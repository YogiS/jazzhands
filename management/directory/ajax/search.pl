#!/usr/bin/env perl

use strict;
use warnings;
use CGI;
use JSON::PP;

BEGIN { unshift(@INC, "/Users/kovert"); };

use JazzHands::DBI;

exit do_work();

sub do_work {
	my $dbh = JazzHands::DBI->connect('directory', {AutoCommit => 0}) ||
		die $JazzHands::DBI::errstr;

	my $cgi = new CGI;

	my $searchfor = $cgi->param('find');

	my $sth = $dbh->prepare_cached(qq{
		select	p.person_id,
				coalesce(p.preferred_first_name, p.first_name) as first_name,
				coalesce(p.preferred_last_name, p.last_name) as last_name,
				p.nickname,
				pi.person_image_id
		 from	person p
		 		inner join person_company pc
					on pc.person_id = p.person_id
				inner join v_person_company_expanded pce
					on p.person_id = pce.person_id
				left join (
					select	person_image_id, person_id
					  from	person_image
					  	inner join person_image_usage  using (person_image_id)
					 where	person_image_usage = 'corpdirectory'
				) pi on pi.person_id = p.person_id
	   where	pce.company_id = (
				select	property_value_company_id
				  from	property
				 where	property_name = '_rootcompanyid'
				   and	property_type = 'Defaults'
			)
		and	pc.person_company_status = 'enabled'
		and	(
				lower(p.first_name || ' ' || p.last_name) like :search
			or
				lower(p.preferred_first_name || ' ' || p.preferred_last_name) like :search
			or
				lower(p.first_name || ' ' || p.preferred_last_name) like :search
			or
				lower(p.preferred_first_name || ' ' || p.last_name) like :search
			or
				lower(p.last_name) like :search
--			or
--				lower(p.preferred_first_name) like :search
			or
				lower(p.preferred_last_name) like :search
--			or
--				lower(p.first_name) like :search
			or
				lower(p.nickname) like :search
			)
	   order by	last_name, first_name
	   LIMIT 10
	}) || die $dbh->errstr;

	my $s = "$searchfor%";
	$s =~ s/\s+/ /g;
	$s =~ tr/A-Z/a-z/;


	$sth->bind_param(':search', $s) || die $sth->errstr;

	# XXX - need to process nicknames and maybe add pictures
	$sth->execute || die $sth->errstr;

	my $r = {};
	$r->{type} = 'person';
	while(my $hr = $sth->fetchrow_hashref) {
		my $x = {};
		if($hr->{person_image_id}) {
			$x->{img} = "picture.php?person_id=$hr->{person_id}&person_image_id=$hr->{person_image_id}&class=thumb";
		}
		$x->{name} = join(" ", $hr->{first_name}, $hr->{last_name});
		$x->{link} = "contact.php?person_id=" . $hr->{person_id};
		push(@ {$r->{people}}, $x);
	}

	$r->{error} = 'Error!';
	$r->{error} = undef;

	print $cgi->header( -type => 'application/json', -charset => 'utf-8');
	print encode_json ( $r );

	$dbh->rollback;
	$dbh->disconnect;
}
