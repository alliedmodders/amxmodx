#!/bin/bash
#
# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

cd "$(dirname "$0")"

if [ ! -f ./amxxpc ]; then
	echo // Could not find amxxpc
	exit 1
fi

test -e compiled || mkdir compiled

if [[ $# -ne 0 ]]; then
	for sourcefile in "$@"; 
	do
		outfile="$(echo "$sourcefile" | sed -e 's/\.sma$/\.amxx/')";
		echo -e "// Compiling $sourcefile...";
		./amxxpc "$sourcefile" -ocompiled/"$outfile"
		RETVAL=$?
		if [ $RETVAL -ne 0 ]; then
			exit 1;
		fi
	done
else
	for sourcefile in *.sp
	do
		outfile="$(echo "$sourcefile" | sed -e 's/\.sma$/\.amxx/')"
		echo -e "// Compiling $sourcefile ..."
		./amxxpc "$sourcefile" -ocompiled/"$outfile"
		RETVAL=$?
		if [ $RETVAL -ne 0 ]; then
			exit 1;
		fi
	done
fi
