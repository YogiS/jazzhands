#!/usr/bin/env perl
#
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
#


#
# $Id$
#

use strict;
use warnings;
use POSIX;
use Data::Dumper;
use Carp;
use JazzHands::STAB;
use JazzHands::Common qw(_dbx);
use Net::IP;

return process_attestment();

sub process_attestment {
	my $stab = new JazzHands::STAB || die "Could not create STAB";
	my $cgi  = $stab->cgi	  || die "Could not create cgi";

	# print $cgi->header, $cgi->start_html, $cgi->Dump, $cgi->end_html; exit;

	# XXX - need to validate that this is ok.
	my $acctid = $stab->cgi_parse_param('accting_as_account');

	my $myacctid = $stab->get_account_id() || die $stab->error_return("I was not able to determine who you are. This should not happen.");

	#
	#
	# NOTE:  THis query is shared with index.pl.  May want to do something
	# about that...
	my $sth = $stab->prepare(qq{
		SELECT approver_account_id, aii.*
		FROM	approval_instance ai
				INNER JOIN approval_instance_step ais
					USING (approval_instance_id)
				INNER JOIN approval_instance_item aii 
					USING (approval_instance_step_id)
				INNER JOIN approval_instance_link ail 
					USING (approval_instance_link_id)
		WHERE	approver_account_id = ?
	}) || return $stab->return_db_err;

	$sth->execute($acctid) || return $stab->return_db_err($sth);

	my $wsth = $stab->prepare(qq{
		PERFORM approval_utils.approve(
			approval_instance_item_id := ?,
			approved := ?,
			approving_account_id := ?,
			new_value := ?
		);
	}) || return $stab->return_db_err;

	my $count = 0;
	while(my $hr = $sth->fetchrow_hashref) {
		my $id = $hr->{_dbx('approval_instance_item_id')};

		my $yes = $cgi->param("app_$id");
		my $no = $cgi->param("dis_$id");
		
		my $fix;
		my $approved;
		# Javascript prevents this from happening.
		if($yes && $no) {
			$stab->error_return("All users must have Y or N checked, not both");
		} elsif(!$yes && !$no) {
			$stab->error_return("All users must have Y or N checked, not none");
		} elsif($yes) {
			$approved = 'Y';
			print $cgi->li("yes to $id");
		} elsif($no) {
			$fix = $cgi->param("fix_$id");
			if(!$fix) {
				$stab->error_return("All rejected users must have a correction");
			}
			print $cgi->li("no to $id, fix is $fix");
			$approved = 'N';
		}

		$wsth->execute($id, $approved, $myacctid, $fix) || return $stab->return_db_err;
		$wsth->finish;
		$count++;
	}

	$stab->commit;
	$stab->msg_return("Submitted $count items Succesfully.");
	0;
}
