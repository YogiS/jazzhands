#!/usr/bin/env perl

#
# Copyright (c) 2010-2014 Todd Kover
# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#
# Copyright (c) 2013 Matthew Ragan
# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Copyright (c) 2005-2010, Vonage Holdings Corp.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#	 * Redistributions of source code must retain the above copyright
#	   notice, this list of conditions and the following disclaimer.
#	 * Redistributions in binary form must reproduce the above copyright
#	   notice, this list of conditions and the following disclaimer in the
#	   documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY VONAGE HOLDINGS CORP. ''AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL VONAGE HOLDINGS CORP. BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use strict;
use warnings;
use JazzHands::STAB;

do_style_dump();

sub do_style_dump {
	my $stab = new JazzHands::STAB || die "Could not create STAB";

	my $cgi = $stab->cgi || die "Could not create cgi";

	print $cgi->header( { -type => 'text/css' } ), "\n";

	my $root = $stab->guess_stab_root;

	# this could be smarter.  It also apperas in STAB.pm for
	# the title bar
	if ( $root !~ m,://stab.[^/]+/?$, && $root !~ /dev/ ) {
		print <<END;

BODY { 
	background: url($root/images/development-background.png ); 
}
a { color: blue }
a:visited { color: purple }

END
	}

	print <<END;

BODY {
	Font-Family: Verdana, Arial, Helvetica, MS Sans Serif;
}

div.introblurb {
	margin-left: auto;
	margin-right: auto;
	text-align: center;
	width: 60%;
}

div.navbar {
	margin: auto;
	font-size: 75%;
	text-align: center;
}

div.errmsg {
	margin: auto;
	color: red;
	text-align: center;
}

div.notemsg {
	margin: auto;
	color: green;
	text-align: center;
}

/* device box pretty */

TABLE.dev_checkboxes { background-color: lightyellow; border: 1px solid;}
TABLE.rack_summary { background-color: lightyellow; border: 1px solid;}

/* physical port popup box */

TABLE.primaryinterface { background-color: lightyellow; }
TD.header { background-color: orange; }
div.searchPopup {
	background: white;
	border: 1px solid black;
	border-top: none;
	font-size: 50%;
}
div.physPortPopup {
	background: lightblue;
	border: 1px solid black;
	font-size: 50%;
}
table.physPortTitle {
	background: lightgreen;
	border: 1px solid black;
	font-size: 100%;
	width: 600px;
}

div.dnsvalueref {
	margin-left: auto;
	margin-right: auto;
	background: lightblue;
	text-align: center;
	border: 1px solid;
}

/* Tabs */

div.tabgroup_pending {
	padding-top: 10px;
}

div#verifybox {
	border: 2px solid black;
	padding: 5px;
	background: red;
	text-align: center;
	width: 60%;
	margin-left: 20%;
}

div.maindiv {
	border: 2px solid black;
	padding: 3px;
}

/* #tabthis is the "please select a tab" content */
div#tabthis {
	border: 1px solid black;
	padding-top: 10px;
	margin-top: 3px;
	background: #DDE;
}

div.tabthis {
	background: #DDE;
}

tabgroup_pending {
	color: lightgrey
}

tabgrouptab { padding: 5px 0; }

a.tabgroupactive {
	margin-top: 10px;
	padding: 3px 0.5em;
	padding-bottom: 4px;
	margin-left: 3px;
	border: 1px solid black;
	border-bottom-width: 0px;
	background: #DDE;
	text-decoration: none;
	border-radius: 20px 20px 0px 0px;
}

a.tabgrouptab {
	margin-top: 10px;
	padding: 3px 0.5em;
	margin-left: 3px;
	border: 1px solid black;
	border-bottom-width: 1px;
	background: #AAB;
	text-decoration: none;
	border-radius: 20px 20px 0px 0px;
}

a:hover.tabgrouptab {
	background: lightblue;
	color: black;
	border-bottom: 5px;
}

input#submitdevice { font-size: 60%;}

/***************** START OF GENERIC JQUERY TABS ****************************/

/*
 * tabs default to 'off', but the tab bar item needs class stabbar_off set
 * because of the hover bits
 *
 * to to
 */
div.stabtabcontent {
	background: #DDE;
	border: 1px solid black;
	border-radius: 10px;
	width: 100%;
	margin:auto;
	min-width: 100ex;
}

div.stabtab { 
	padding: 5px 0;
	display: none;
	visibility: hidden;
}

a.stabtab {
	margin-top: 10px;
	padding: 3px 0.5em;
	margin-left: 3px;
	border: 2px solid black;
	text-decoration: none;
	border-radius: 20px 20px 0px 0px;
	background: #AAB;
	border-bottom-width: 1px;
	color: white;
	padding-top: 10px;
}

a.stabtab_on {
	padding-bottom: 4px;
	background: #DDE;
	border-bottom-width: 0px;
	color: blue;
}

.stabtabbar a:hover.stabtab_off {
	background: lightblue;
	color: black;
	border-bottom: 5px;
}

div.stabtab_on { 
	display: block;
	visibility: visible;
}

/******************************* END OF NEW TABS **************************/


/* 	Rack display */

table.rackit {
	border: 1px solid black;
	background: #B0B0FF;
}

td.rackit_even {
	border: 1px solid black;
	background: #FFAA33;
}
td.rackit_odd {
	border: 1px solid black;
	background: \#FFFF33;
}
td.rackit_infrastructure {
	border: 1px solid black;
	background: grey;
}
td.rackit_vertical {
	width: 10px;
	text-align: center;
	border: 1px solid black;
	background: lightgrey;
}

/* DNS display */

form.dnspage, form.dnspage table {
	text-align: center;
	margin: auto;
}

table.dnsgentable {
	text-align: center;
	margin: auto;
	border: 2px solid;
}

table.soatable {
	text-align: center;
	margin: auto;
	background: lightgrey;
	border: 2px solid;
}

tr.even {
	background: lightgrey;
}

tr.odd {
	background: transparent;
}

/* App display */

div.approle_inside {
	text-align: center;
	border: 1px solid;
}
div.approle {
	background: #DDE;
	border: 1px solid;
}
div.approle_leaf {
	color: green;
}
div.approle_undecided {
	/* color: yellow; */
	color: green;
}
a.approle_addchild {
	font-size: 50%;
	color: red; 
}

/* Ports */

span.port_label {
	color: green;
}

table.center_table, form.center_form {
	text-align: center;
}

table.center_table tbody {
	text-align: left;
}

div.approles {
	margin-left: auto;
	margin-right: auto;
	background: orange;
	text-align: center;
	border: 1px solid;
}

div.approles > div {
	text-align: left;
}

label {
	font-weight: bold;
}

/* Editable descriptions */

.hinttext, .hint {
	color: #a9a9a9;
	font-size: 75%;
	font-style: italic;
}

input.editabletext {
	min-width: 200px;
}

input.srvnum {
	width: 5em;
}

/*
	 this exists for approval because display: none confuses the chosen
	jquery plugin
 */
.hidecorrection {
	visibility: hidden;
}

.irrelevant {
	display: none;
	visibility: hidden;
}

div.chosen-workaround {
	width: 100% !important;
}

div.ipdiv {
	width: 100%;
}

ul.collection {
	list-style-type: lower-roman;		/* should be none */
}

span.netblocksite {
	width: 8ex;
	float: left;
}

span.netblocklink {
	min-width: 30ex;
	float: left;
}

#SOA_MNAME, #SOA_RNAME {
	width: 35ex; 
}

tr.intadd {
	background: grey;
}

table.dnstable {
	text-align: center;
	margin: auto;
	border: 1px solid;
}

table.dnstable tr.dnsrecord {
	text-align: left;
}

table.dnstable input.dnsttl {
	width: 6em;
}

table.dnstable input.dnsvalue {
	width: 20em;
}

table.dnstable input.dnsname {
	width: 10em;
}


.autocomplete-suggestions { border: 1px solid #999; background: lightyellow; overflow: auto; font-size: 80%;}
.autocomplete-suggestion { padding: 2px 5px; white-space: nowrap; overflow: hidden; }
.autocomplete-selected { background: #F0F0F0; }
.autocomplete-suggestions strong { font-weight: normal; color: #3399FF; }
.autocomplete-group { padding: 2px 5px; }
.autocomplete-group strong { display: block; border-bottom: 1px solid #000; }


.dnssubmit {
	display: block;
	margin: auto;
	text-align: center;
}

/* netblock management (pre rewrite */
table.nblk_ipallocation tr, table.nblk_ipallocation td {
	outline: 1px solid;
}

img.subnet {
	width: 1em;
}

table.intmoretable {
	background: lightyellow;
}

.intableheader {
	text-align: center;
	font-weight: bold;
	background: orange;
}

table.interfacetable {
	display: inline;
	margin: auto;
	text-align: center;
}

table.interfacetable tr {
	outline: 1px solid;
}

table.interfacetable td {
	vertical-align: top;
	margin: auto;
	text-align: center;
}

#verifybox li {
	list-style-type: none;
}

div.ncmanip {
	text-align: center;
	width: 100%;
	display: inline-block;
}

div.attestbox {
	min-width: 20ex;
	text-align: center;
}


table.attest {
	border: 2px solid;
	margin: auto;
	background: grey;
}

table.attest tbody tr.odd {
	background: lightgrey;
	border: 1px solid;
}

table.attest td {
	background: grey;
}

table.attest tbody th {
	background: lightgrey;
}
	
table.attest tbody tr.even {
	background: white;
}
	
table.attest tbody tr.even {
	background: white;
}

table.attest tbody tr.even td {
	background-color: white;
}
table.attest tbody tr.odd td {
	background-color: lightgrey;
}

.error {
	background-color: red;
	color: white;
}

.disabled  {
	/*pointer-events: none;*/
	opacity: .9;
	text-decoration: line-through;
}

.off  {
	pointer-events: none;
	opacity: .9;
	background-color: lightgrey;
}

img.plusbutton {
	height: 2ex;
	border: 1px solid;
	display: block;
	text-align:center;
}

img.button {
	height: 2ex;
	vertical-align: middle;
}

div.description {
	border: 3px solid;
	text-align: center;
	margin: auto;
	min-width: 75%;
	
}

div.directions {
	text-align: center;
	margin: auto;
	width: 60%;
	
}

div.process { background: orange; }
div.chain { background: lightgrey; }

div.attestsubmit {
	width: 100%;
	text-align: center;
}

td.correction {
	min-width: 30ex;
}

td.correction input { 
	width: 100%;
}

input.attestsubmit { 
	background-color: green;
	color: white;
	border-radius: 20px;
	margin: auto;
	font-size: 130%;
	
}

.approveall {
	border: 1px solid;
	border-radius: 20px;
	margin: 2px;
	background: lightyellow;
}

.attesttoggle {
	border: 1px solid;
	border-radius: 20px;
	margin: 0px;
	margin: 0px;
	text-decoration: bold;
	background: lightyellow;
}

.approvaldue {
	margin: auto;
	text-align: center;
}

.collectionbox {
	margin: auto;
	text-align: center;
}

.duesoon {
	background: yellow;
}

.overdue {
	background: red;
	color: white;
}

tr.rowrm {
	background: red;
	text-decoration: line-through;
}

li.rmrow {
	background: red;
	text-decoration: line-through;
}

td.pendingrm * {
	pointer-events: none;
	opacity: .5;
	text-decoration: line-through;
}

.buttonon {
	background: lightblue;
}

div.reporting {
	width: 100%;
	margin: auto;
}

table.reporting {
	border: 1px solid;
	margin: auto;
	text-align:center;
}

table.reporting >tbody{
	text-align:left;
}

table.reporting > tbody td {
	border: 1px solid;
	border-color: grey;
}


div.centeredlist {
	text-align: center;
	margin: auto;
}

ul.nbhier {
	padding-top: 0;
}

ul.nbhier > li.nbkids {
	list-style-type: none;
	margin-left: 0ex;
}

ul.nbhier > li.nbnokids {
	list-style-type: none;
	padding-left: 19px;
	margin-left: 5ex;
}

/* account (and eventually other) collection manipulation */

ul.collectionbox a.collpad {
	pointer-events: none;
	visibility: hidden;
}

ul.collectionbox li {
	width: 100%;
}

ul.collectionbox li.plus {
	list-style-type: none;
}

ul.collectionbox input {
	width: 50%;
	mid-width: 25ex;
}

form.picker ul {
	text-align: left;
	margin: auto;
}

h2.objectdetails {
	text-align: center;
	margin: auto;
}

div.collectionname {
	text-align: center;
	margin: auto;
}

div.collectionexpandview {
	position: absolute;
	border: 2px double;
	z-index: 2;
	left: 0;
	right: 0;
	margin: auto;
	text-align: center;
	vertical-align:center;
	min-width: 50ex;
	min-height: 20em;
	width: 50%;
	height: 50%;
	overflow-y: auto;
	background: lightgrey;
}
div.collectionexpandview ul {
	text-align: left;
}

END
	undef $stab;
}
