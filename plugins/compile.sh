#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

echo // AMX Mod X Batch Compiler
echo // by the AMX Mod X Dev Team
echo

if [ ! -f ./amxxpc ]; then
	echo // Could not find amxxpc
	exit 1
fi

test -e compiled || mkdir compiled

for sourcefile in *.sma
do
	amxxfile="$(echo "$sourcefile" | sed -e 's/\.sma$/.amxx/')"

	echo -n "// Compiling $sourcefile ..."

	./amxxpc "$sourcefile" -ocompiled/"$amxxfile"
done
