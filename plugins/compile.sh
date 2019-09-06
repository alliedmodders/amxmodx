#!/bin/bash -e
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
