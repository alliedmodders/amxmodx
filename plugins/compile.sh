# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

cd "$(dirname "$0")"

test -e compiled || mkdir compiled

if [[ $# -ne 0 ]]; then
	for sourcefile in "$@"
	do
		amxxfile="`echo $sourcefile | sed -e 's/\.sma$/\.amxx/'`"
		echo -e "\nCompiling $sourcefile..."
		./amxxpc $sourcefile -ocompiled/$amxxfile
	done
else
	for sourcefile in *.sma
	do
		amxxfile="`echo $sourcefile | sed -e 's/\.sma$/\.amxx/'`"
		echo -e "\nCompiling $sourcefile ..."
		./amxxpc $sourcefile -ocompiled/$amxxfile
	done
fi
