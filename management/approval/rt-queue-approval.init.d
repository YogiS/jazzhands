#!/bin/bash
#
# chkconfig: 2345 70 80
# jira-issues daemon
# description: Monitor RT tickets for Approval System
#
# $Revision: 1.0 $

# Source function library.
. /etc/init.d/functions
PATH=/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/bin:/usr/bin
PROG=/usr/libexec/jazzhands/approval/process-rt
PIDFILE=/var/run/process-rt.pid

SHORTPROG=`basename $PROG`

RETVAL=0

start() {
    echo -n $"Starting ${SHORTPROG}: "
    if [ -s $PIDFILE ] && [ -e /proc/`cat ${PIDFILE}` ]; then
        echo -n $"already running.";
        failure $"already running.";
        echo
        return 1
    fi

    if [ "$1" == "debug" ]; then
        daemon $PROG --debug
    elif [ "$1" == "nodaemon" ]; then
        daemon $PROG --nodaemon
    else
        daemon $PROG
    fi

    RETVAL=$?
	sleep 1
    pgrep -f $PROG > $PIDFILE
	[ $RETVAL -eq 0 ] && echo -n "Started ${SHORTPROG}: "
	echo
    return $RETVAL
}

stop() {
    echo -n $"Stopping ${SHORTPROG}: "
	if [ -f $PIDFILE ]; then
		PID=`cat $PIDFILE`
		if [ "$PID" ]; then
			kill $PID >/dev/null 2>&1
		fi
	fi
    RETVAL=$?
	[ $RETVAL -eq 0 ] && success "Stopped ${SHORTPROG}: "
    echo
    [ $RETVAL -eq 0 ] && rm -f $PIDFILE
    return $RETVAL
}

restart() {
    stop
    start
}

# See how we were called.
case "$1" in
  start)
        start
        ;;
  startdebug)
        start debug
        ;;
  startnodaemon)
        start nodaemon
        ;;
  stop)
        stop
        ;;
  restart)
        restart
        ;;
  status)
        status $PROG
        RETVAL=$?
        ;;
  *)
        echo $"Usage: $0 {start|startdebug|startnodaemon|stop|restart|status}"
        exit 1
        ;;
esac

exit $RETVAL

