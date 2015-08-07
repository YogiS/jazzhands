#!/usr/bin/env perl
# Copyright (c) 2015, Todd M. Kover
# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

use strict;
use warnings;
use LWP::Protocol::https;
use LWP::UserAgent;
use HTTP::Cookies;
use LWP::Debug qw(+);
use Data::Dumper;
use JSON::PP;
use JazzHands::DBI;
use Net::SSLeay;
use Pod::Usage;
use Getopt::Long;
use JazzHands::AppAuthAL;

=head1 NAME

process-jira - 
=head1 SYNOPSIS

process-jira [ --dry-run | -n ] 

=head1 DESCRIPTION

=head1 AUTHORS

Todd M. Kover <kovert@omniscient.com>

=cut 

my $project   = 'CSI';
my $priority  = 'Critical';
my $issuetype = 'Task';
my @labels;
my $oneemail;
my $dryrun;
my $service = "jira-attestation";
my ($jiraroot, $jirauser, $jirapass);

#
# Setup defaults that can be overridden on the command line
#
my $appauth = JazzHands::AppAuthAL::find_and_parse_auth($service, undef, 'web');
if($appauth) {
	if ( ref($appauth) eq 'ARRAY' ) {
		$appauth = $appauth->[0];
	}
	$jiraroot = $appauth->{'URL'};
	$jirauser = $appauth->{'Username'};
	$jirapass = $appauth->{'Password'};
}

GetOptions(
	"dry-run|n"       => \$dryrun,
	"jira-root=s"     => \$jiraroot,
	"jira-user=s"     => \$jirauser,
	"jira-password=s" => \$jirapass,
	"project=s"       => \$project,
	"label=s"        => \@labels,
	"priority=s"      => \$priority,
	"issue-type=s"    => \$issuetype,
	"one-email"       => \$oneemail,
) || die pod2usage();

#if ( !scalar @labels ) {
#	push( @labels, 'physical-access-verify' );
#}

die "Must have a jira root" if (! $jiraroot);
die "Must have a jira username" if (! $jirauser);
die "Must have a jira password" if (! $jirapass);

# This script basically makes a post like this:
# curl --insecure -D- -u 'user:pw' -X POST --data @json.txt -H "Content-Type: application/json" https://jira.example.com/rest/api/2/issue/

$ENV{'PERL_LWP_SSL_VERIFY_HOSTNAME'} = 0;
$ENV{'HTTPS_DEBUG'}                  = 0;
my $jiraapiroot = "$jiraroot/rest/api/2";

sub open_jira_issue($$$;$) {
	my ( $login, $msg, $summary, $dryrun ) = @_;

	my $jira = {
		'fields' => {
			'project'     => { 'key'  => $project },
			'summary'     => $summary,
			# 'labels'      => \@labels,
			'description' => $msg,
			'priority'    => { 'name' => $priority },
			'issuetype'   => { 'name' => $issuetype },
			'reporter'	  => { 'name' => $login },
		},
	};
	my $j = new JSON::PP;
	my $p = $j->pretty->encode($jira);
	if ( !$dryrun ) {
		my $n = jira_req( "issue/", 'POST', $p );
		return $n;
	} else {
		print "posting: $p";
	}
}

sub jira_req($$;$) {
	my ( $what, $action, $body ) = @_;

	$action = 'GET' if ( !$action );

	my $url = "$jiraapiroot/$what";
	my $ua  = LWP::UserAgent->new(
		ssl_opts => {
			SSL_verify_mode => Net::SSLeay->VERIFY_NONE(),
			verify_hostname => 0
		}
	) || die "UA: $!";
	$ua->agent('myfirstscript/1.0');

	my $req = HTTP::Request->new( $action => $url ) || die "$!";

	if ($body) {
		$req->content($body);
	}

	$req->authorization_basic( $jirauser, $jirapass );
	$req->header( "Content-Type" => "application/json" );

	my $res = $ua->request($req);
	if ( $res->is_success ) {
		return decode_json( $res->content );
	}
	die $url, ": ", $res->status_line, "\n";
}

my $dbh = JazzHands::DBI->connect($service, {AutoCommit => 0}) || die $JazzHands::DBI::errstr;

sub open_new_issues {
	#
	# Build $map up to have a list of dcs and a human readable list of names that
	# should have access
	#
	my $map = {};
	my $sth = $dbh->prepare_cached(qq{
	       SELECT approver_account_id, aii.*, ais.is_completed, a.login
	        FROM    approval_instance ai
	                INNER JOIN approval_instance_step ais
	                    USING (approval_instance_id)
	                INNER JOIN approval_instance_item aii
	                    USING (approval_instance_step_id)
	                INNER JOIN approval_instance_link ail
	                    USING (approval_instance_link_id)
			INNER JOIN account a ON
				a.account_id = ais.approver_account_id
	        Where     approval_type = 'jira-hr'
	        AND     ais.is_completed = 'N'
			AND		ais.external_reference_name IS NULL
	        ORDER BY approval_instance_step_id, approved_lhs, approved_label
	}
	) || die $dbh->errstr;

	$sth->execute || die $sth->errstr;

	while ( my $hr = $sth->fetchrow_hashref ) {
		my $step = $hr->{approval_instance_step_id};
		my $lhs = $hr->{approved_lhs};

		if(!defined( $map->{$step}->{ $lhs })) {
			$map->{$step}->{login} = $hr->{login};
			$map->{$step}->{approval_instance_step_id} = $step;
		}

		$map->{$step}->{changes}->{$lhs}->{ $hr->{approved_label} } =
				$hr->{approved_rhs};

		#push(@ {$map->{$step}->{changes}->{$lhs}},
		#			{ $hr->{approved_label} => $hr->{approved_rhs} }
		#);

	}

	$sth->finish;

	my $header = qq{
		During the regular audit of ADP information, the following changes
		needed to be made.  Please update ADP, accordingly.
	};

	my $wsth = $dbh->prepare_cached(qq{
		UPDATE approval_instance_step
		SET external_reference_name = :name
		WHERE approval_instance_step_id = :id
	}) || die $dbh->errstr;

	foreach my $step ( sort keys( %{$map} ) ) {
		my $msg = "$header\n";
		my $login = $map->{$step}->{login};
		foreach my $dude ( sort keys %{$map->{$step}->{changes}} ) {
			$msg .= "\nChanges for $dude:\n";
			my $x = $map->{$step}->{changes}->{$dude};
			$msg .= join("\n", map{ "* $_ becomes '".$x->{$_}."'"
					} keys %{$x})."\n";
		}
		$msg =~ s/^\t{1,3}//mg;
		my $summary = "Organizational Corrections for $login";
		my $jresp = open_jira_issue( $login, $msg, $summary, $dryrun );
		my $jid = $jresp->{key};
		$wsth->bind_param(':name', $jid) || die $sth->errstr;
		$wsth->bind_param(':id', $step) || die $sth->errstr;
		$wsth->execute || die $sth->errstr;
		$wsth->finish;
	}
}

sub check_pending_issues {
	my $sth = $dbh->prepare_cached(qq{
	       SELECT approver_account_id, aii.*, ais.is_completed, a.login,
					ais.external_reference_name
	        FROM    approval_instance ai
	                INNER JOIN approval_instance_step ais
	                    USING (approval_instance_id)
	                INNER JOIN approval_instance_item aii
	                    USING (approval_instance_step_id)
	                INNER JOIN approval_instance_link ail
	                    USING (approval_instance_link_id)
			INNER JOIN account a ON
				a.account_id = ais.approver_account_id
	        Where     approval_type = 'jira-hr'
	        AND     ais.is_completed = 'N'
			AND		ais.external_reference_name IS NOT NULL
	        ORDER BY approval_instance_step_id, approved_lhs, approved_label
	}
	) || die $dbh->errstr;

	$sth->execute || die $sth->errstr;

	my $cache  = {};
	while(my $hr = $sth->fetchrow_hashref) {
		my $key = $hr->{external_reference_name};

		my $status;
		if(exists( $cache->{$key} )) {
			$status = $cache->{$key};
		} else {
			my $r = jira_req("issue/$key", 'GET');


			if($r->{fields} && $r->{fields}->{status}) {
				my $stat = $r->{fields}->{status};
				if($stat->{name} eq 'Closed') {
					$status = $cache->{$key} = $stat->{name};
				}
			}
		}
		warn "status is $status";
	}

	$sth->finish;
}

check_pending_issues();
open_new_issues();


$dbh->commit;

END {
	if ($dbh) {
		$dbh->disconnect;
		$dbh = undef;
	}
}
