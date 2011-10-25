#
# $Id$
# 

=head1 NAME

JazzHands::DBI - database authentication abstraction for Perl

=head1 SYNOPSIS

use JazzHands::DBI;
my $dbh = JazzHands::DBI->connect($app, [ $instance, ] [ $flags ]);

=head1 DESCRIPTION

I totally need to write this.

=head1 FILES

/etc/jazzhands/dbauth.json - configuration file
/var/lib/jazzhands/dbauth-info - Default Location for Auth Files

=head1 AUTHORS

Todd Kover (kovert@omniscient.com)

=cut

package JazzHands::DBI;

use strict;
use warnings;
use Exporter;
use DBI;
use FileHandle;
use JSON::PP;
use Data::Dumper;
use vars qw(@EXPORT_OK @ISA $VERSION $db_config);

$VERSION = '$Revision$';

@ISA       = qw(DBI Exporter);
@EXPORT_OK = qw(do_database_connect);


#
# places where the auth files may live
#
my (@searchdirs);

# Parsed JSON config
my $dbi_config;

#
# If the environment variable is set, require it, otherwise use an optional
# config file
#
BEGIN {
	my $fn;
	if(defined($ENV{'DBAUTH_CONFIG'})) {
		$fn = $ENV{'DBAUTH_CONFIG'};
	}
	if(defined($fn)) { 
		if(! -r $fn) {
			die "$fn is unreadable or nonexistance.\n";
		}
	} else {
		$fn = "/etc/jazzhands/dbauth.json";
	}
	if(-r $fn) {
		my $fh = new FileHandle($fn) || die "$fn: $!\n";
		my $json = join("", $fh->getlines);
		$fh->close;
		$dbi_config = decode_json($json) || die "Unable to parse config file";
		if(exists($dbi_config->{'onload'})) {
			if(defined($dbi_config->{'onload'}->{'environment'})) {
				foreach my $e (@{$dbi_config->{'onload'}->{'environment'}}) {
					foreach my $k (keys %$e) {
						$ENV{'$k'} = $e->{$k};
					}
				}
			}
		}
		my $dirname = $fn;
		$dirname =~ s,/[^/]+$,,;
		if(exists($dbi_config->{'search_dirs'})) {
			foreach my $d (@{$dbi_config->{'search_dirs'}}) {
				#
				# Translate . by itself to the directory that the file is
				# in.  If someone actually wants the current directory, use ./
				if($d eq '.') {
					push(@searchdirs, $dirname);
				} else {
					push(@searchdirs, $d);
				}
			}
		}
	}
}

#
# Parse a JSON file and return the variable/value pairs to be used by other
# operations
#
sub parse_json_auth {
	my $fn = shift;

	my $fh = new FileHandle($fn) || die "$fn: $!\n";
	my $json = join("", $fh->getlines);
	$fh->close;
	my $thing = decode_json($json) || die "Unable to parse config file";
	$thing;
}

#
# Find an authfile on the search paths and return the procssed version
#
sub find_and_parse_auth {
	my $app = shift @_;
	my $instance = shift @_;

	#
	# This implementation only supports json, but others may want something
	# in XML, plantext or something else.
	#
	#
	# If instance is set, then look for $d/$app/$instance.json.  if that is
	# not there, check to see if sloppy_instance_match is set to no, in which
	# case don't check for a non-instance version.
	#
	if($instance) {
		foreach my $d (@searchdirs) {
			if(-f "$d/$app/$instance.json") {
				return(parse_json_auth("$d/$app.$instance/json"));
			}
		}

		if(defined($dbi_config->{'sloppy_instance_match'}) &&
			$dbi_config->{'sloppy_instance_match'} =~ /^n(o)?$/i) {
				return undef;
		}
	}

	foreach my $d (@searchdirs) {
		if(-f "$d/$app.json") {
			return(parse_json_auth("$d/$app.json"));
		} elsif(-f "$d/$app") {
			return(parse_json_auth("$d/$app"));
		}
	}

	undef;
}

#
# our implemenation of DBI::connect.
#
# We provide no defaults, but the underlying DBD module may provide something.
#
sub do_database_connect {
	my $app = shift;
	my $instance = shift;
	my $dbiflags = shift;

	# Mapping from what the db is called to how to set parameters
	# parameter names preceeded by an r: are required, everything else is
	# optional.  NOTE:  Have not implemented anything required yet.
	#
	# Most of these have not yet been tested.
	#
	my $dbdmap = {
		'oracle' => { 
					'_DBD' => 'Oracle' 
			},
		'postgresql' => {
					'_DBD' => 'Pg',
					'DBName' => 'dbname',
					'DBHost' => 'host',
					'DBPort' => 'port',
					'Options' => 'options',
					'Service' => 'service',
					'SSLMode' => 'sslmode',
			},
		'mysql' => {
					DBD => 'mysql',
			},
		'tds' => {
					DBD => 'Sybase'
		},
		'sqlite' => {
					DBD => 'SQLite',
		},
	};

	my $auth = find_and_parse_auth($app, $instance);

	return undef if(!defined($auth) || !defined($auth->{'Method'}));
	return undef if(!defined($auth->{'DBType'}));

	#
	# only support Password at the moment
	#
	my($user,$pass);

	my $dbtype = $auth->{'DBType'};
	$dbtype =~ tr/A-Z/a-z/;

	my $dbd = $dbdmap->{$dbtype}->{_DBD};

	my @vals;
	foreach my $k (keys(% {$auth} )) {
		if($k eq 'Username') {
			$user = $auth->{'Username'};
		} elsif($k eq 'Password') {
			$pass = $auth->{'Password'};
		} else {
			next if(!exists($dbdmap->{$dbtype}->{$k}));
			my $pk = $dbdmap->{$dbtype}->{$k};
			my $v = $auth->{$k};
			push(@vals, "$pk=$v");
		}
	}

	if(lc($auth->{'Method'}) ne 'password') {
		return undef;
	}

	my $dbstr = "dbi:${dbd}:". join(";", @vals);

	return DBI->connect($dbstr, $user, $pass, $dbiflags);
}


#
# Our implementation of connect.  In order to accomadate the instance being an
# optionalish thing, this will accept flags instead of an instance and DTRT.
#
sub connect {
	my $class = shift;
	my $app = shift;
	my $instance = shift;
	my $flags = shift;

	if(ref($instance)) {
		return do_database_connect($app, undef, $instance);
	} else {
		return do_database_connect($app, $instance, $flags);
	}
}

1;