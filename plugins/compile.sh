#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

# to compile all source files in the directory
#  execute the script without any parameters
# to compile specific or multiple source files
#  execute the script with parameters set as source files' name
#  e.g ./compile.sh admin.sma admincmd.sma adminchat.sma

test -e compiled || mkdir compiled
rm -f temp.txt

if [ $# != 0 ]
	then
		for src in "$@"
		do
			amxxfile="`echo $src | sed -e 's/\.sma$/.amxx/'`"
			echo -n "Compiling $src ..."
			./amxxpc $src -ocompiled/$amxxfile >> temp.txt
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
