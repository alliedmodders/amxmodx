#!/bin/bash -e
cd "$(dirname "$0")"

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

# new code contributed by \malex\

test -e compiled || mkdir compiled
rm -f temp.txt
if [[ $# -ne 0 ]]; then
	for sourcefile in "$@"
	do
        amxxfile="`echo $sourcefile | sed -e 's/\.sma$/.amxx/'`"
        echo -n "Compiling $sourcefile ..."
        ./amxxpc $sourcefile -ocompiled/$amxxfile >> temp.txt
        echo "done"
	done
else
    for sourcefile in *.sma
    do
        amxxfile="`echo $sourcefile | sed -e 's/\.sma$/.amxx/'`"
        echo -n "Compiling $sourcefile ..."
        ./amxxpc $sourcefile -ocompiled/$amxxfile >> temp.txt
        echo "done"
        done
fi

less temp.txt
rm temp.txt
