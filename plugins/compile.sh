#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

test -e compiled || mkdir compiled
rm -f temp.txt

if [ $# != 0 ]
	then
		case $1 in
			"--help" | "-h" )
				echo "  Usage:"
				echo "--help    (-h) : This menu"
				echo "--version (-v) : AMX MOD X Compiler version"
				echo -e "--latest  (-l) : Latest AMX MOD X version\n"
				echo "To compile all source files in the directory"
				echo "  execute the script without any parameters"
				echo "To compile specific or multiple source files"
				echo "  execute the script with parameters set as source files' name"
				echo "  e.g ./compile.sh admin.sma admincmd.sma adminchat.sma"
				;;
			 "--version" | "-v" )
				./amxxpc -q | head -n 3
	 			;;
	 		"--latest" | "-l" )
				echo "Latest AMX MOD X version"
				os="$(uname -s)"
				
				case "$os" in
					"Linux"  ) os="linux" ;;
					"Darwin" ) os="mac" ;;
				*)
				esac
				
				latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/1.9/amxmodx-latest-base-$os)
				
				if [ $? -eq 0 ]
				then
					echo -n "1.9 Build: "
					echo $latest | grep -o -P '(?<=git).*(?=-base)'
					echo -n "1.10 Build: "
					wget -qO- https://www.amxmodx.org/amxxdrop/1.10/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)'
				else
					echo "Internet connection is required"
				fi
				;;
			*)
		esac
		
		for src in "$@"
		do
			if [[ $src == *.sma ]]
			then
				amxxfile="`echo $src | sed -e 's/\.sma$/.amxx/'`"
				echo -n "Compiling $src ..."
				./amxxpc $src -ocompiled/$amxxfile >> temp.txt
				echo "done"
			fi
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

if [ -f temp.txt ]
then
	less temp.txt
	rm temp.txt
fi
