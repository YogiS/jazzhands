#!/bin/sh
# Copyright (c) 2005-2010, Vonage Holdings Corp.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
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

# Copyright (c) 2013, Todd M. Kover
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
# $Id$
#

PATH=usr/local/bin:/usr/local/sbin:/usr/vendor/bin:/usr/kerberos/bin:/usr/bin:/bin
export PATH

ZG_ROOT=/var/lib/zonegen

if [ ! -r ${ZG_ROOT} ] ; then
	mkdir -p ${ZG_ROOT}
	mkdir -p ${ZG_ROOT}/run
	mkdir -p ${ZG_ROOT}/auto-gen
	mkdir -p ${ZG_ROOT}/auto-gen/perserver
fi

# redirect debugging to /dev/null unless this is a tty
exec 3>/dev/null
tty -s
if [ $? = 0 ] ; then
	exec 3>&2
fi

LOCKFILE=${ZG_ROOT}/run/zonegen.lock

cleanup() {
        echo 1>&2 cleaning up lockfile after signal
        rm -f $LOCKFILE
	exit 1
}

trap cleanup HUP INT TERM

tty >/dev/null 2>&1 
#
# if we're not a tty, do away with output
#
if [ $? = 1 ] ; then
	exec >/dev/null
	exec 2>/dev/null
fi

list=`find 2>/dev/null $LOCKFILE -mmin +180`
if [ ! -z "$list" ] ; then
	echo 1>&3 removing lockfile $LOCKFILE as three hours has past.
	rm -f $LOCKFILE
fi

umask 022

lockingon=no
if [ -z "$*" ] ; then
	if [ -r $LOCKFILE ] ; then
		echo 1>&2 Locked, Skipping.
		exit 0
	fi
	echo Locking
	lockingon=yes
	touch $LOCKFILE
fi

TMPFILE=/tmp/zonegenzonelist.$$
PERSERVER_HOSTFILE=/etc/jazzhands/zonegen-perserver.conf
ALLZONE_HOSTFILE=/etc/jazzhands/zonegen-allzone.conf

LISTGEN=/usr/libxec/jazzhands/zonegen/generate-list

SRC_ROOT=${ZG_ROOT}/auto-gen/perserver
DST_ROOT=/var/named/chroot/auto-gen
RSYNC_RSH=/usr/libexec/jazzhands/zonegen/ssh-wrap

export RSYNC_RSH

if [ ! -x ${RSYNC_RSH} ] ; then
	RSYNC_RSH=ssh
fi

UNLINK_ALLZONE=no
if [ -x ${LISTGEN} ] ; then
	ALLZONE_HOSTFILE=${TMPFILE}
	${LISTGEN} > ${TMPFILE}
	UNLINK_ALLZONE=yes
fi

KRB5CCNAME=/tmp/krb5cc_zonegen_$$_do_zonegen
export KRB5CCNAME

if [ -x  /usr/libexec/jazzhands/zonegen/generate-zones ] ; then
	echo 1>&3  "Generating Zones (This may take a while)..."
	/usr/libexec/jazzhands/zonegen/generate-zones "$@" >&3

	if [ -f /etc/krb5.keytab.zonegen ] ; then
		kinit -k -t /etc/krb5.keytab.zonegen zonegen
	fi
	if [ -r $PERSERVER_HOSTFILE ] ; then
		echo 1>&3 Rsyncing to pesrserver hosts listed in $ALLZONE_HOSTFILE
		sed -e 's/#.*//' $PERSERVER_HOSTFILE | 
		while read ns servers ; do
			if [ "$servers" = "" ] ; then
				servers="$ns"
			fi
			if [ "$ns" != "" ] ; then
				servers=`echo $servers | sed 's/,/ /'`
				for host in $servers ; do
					if [ x"$host" != "x" ] ;then
						echo 1>&3  "Rsyncing to $host (in $ns) ..."
						rsync </dev/null -rLpt --delete-after $SRC_ROOT/$ns/ ${host}:$DST_ROOT
						$RSYNC_RSH </dev/null >/dev/null $host sh $DST_ROOT/etc/zones-changed.rndc
					fi
				done
			fi
		done
	fi
	if [ -r $ALLZONE_HOSTFILE ] ; then
		echo 1>&3 Rsyncing to allzone hosts listed in $ALLZONE_HOSTFILE
		sed -e 's/#.*//' $ALLZONE_HOSTFILE | 
		while read host ; do
			if [ x"$host" != "x" ] ;then
				echo 1>&3 ++ Rsyncing to $host
				rsync </dev/null -rLpt --delete-after $SRC_ROOT/../zones $SRC_ROOT/../etc ${host}:$DST_ROOT
				$RSYNC_RSH </dev/null >/dev/null $host sh $DST_ROOT/etc/zones-changed.rndc
			fi
		done
	fi
fi

if [ "$lockingon" = "yes" ] ; then
	echo 1>&3 Unlocking
	rm -f $LOCKFILE
fi

if [ x"$UNLINK_ALLZONE" = xyes ] ; then
	rm -f $ALLZONE_HOSTFILE
fi

kdestroy >/dev/null 2>&1

exit 0
