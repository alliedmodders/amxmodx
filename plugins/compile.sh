#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

##################### 
# GENERAL FUNCTIONS #
#####################

# Quit script
quit_script () {
	if (( $# != 0 )); then
		echo "$@"; fi
	
	echo "Exiting..."
	exit 1
}

# Checks if folder exists
exists () {
	test -e $1
}

##################### 
# ARGUMENT CHECKING #
#####################

# Checks if exists any argument
check_for_arguments () {
	if [ $# != 0 ]; then
		return 0; # Argument exist
	else return 1; fi # Argument doesn't exist
}

# Check if argument is a command
check_for_commands () {
	if [[ $1 == -* && $1 != *.* ]]; then
		return 0; # Argument is command
	else return 1; fi # Argument is file
}

#####################
# COMMAND FUNCTIONS #
#####################

# HELP
__help () {
	echo "  Usage:"
	echo "--help    (-h) : This menu"
	echo -e "--version (-v) : AMX MOD X Compiler version\n"
	echo "To compile all source files in the directory"
	echo "  execute the script without any parameters"
	echo "To compile specific or multiple source files"
	echo "  execute the script with parameters set as source files' name"
	echo "  e.g ./compile.sh admin.sma admincmd.sma adminchat.sma"
}

# VERSION
__version () {
	if exists amxxpc; then
	 	./amxxpc -q | head -n 3
	else quit_script "amxxpc does not exists"; fi
}

# Execute command
execute_command () {
	case "$1" in
		"--help"    | "-h" ) __help    ;;
		"--version" | "-v" ) __version ;;
	*) 
	echo "  $1 is not a valid command"
	echo "  Type \"$0 --help\" to see more information"
	esac
}

#####################
#  COMPILE SOURCE   #
#####################

# Compile source file
compile_source_file () {
	amxxfile="`echo $1 | sed -e 's/\.sma$/.amxx/'`"
	echo -n "Compiling $1 ..."
	./amxxpc $1 -ocompiled/$amxxfile >> temp.txt
	echo "done"
}

#####################
#   MAIN FUNCTION   #
#####################

# Main function
main () {
	if check_for_arguments "$@"; then
		for arg in "$@"; do
			if check_for_commands $arg; then
				execute_command $arg
				exit
			else
				test -e amxxpc || quit_script "amxxpc does not exists"
				if [[ $arg == *.sma ]]; then
					test -e temp.txt && rm -f temp.txt
					test -e compiled || mkdir compiled
					compile_source_file $arg
					less temp.txt && rm temp.txt
				else
					echo "  $arg not a valid file"
					echo "  Type \"$0 --help\" to see more information"
				fi
			fi
		done
	else 
		test -e compiled || mkdir compiled
		test -e amxxpc || quit_script "amxxpc does not exists"
		test -e temp.txt && rm -f temp.txt
		for sourcefile in *.sma; do
			compile_source_file $sourcefile
		done
		less temp.txt && rm temp.txt
	fi
}	

#####################
# EXECUTE MAIN FUNC #
#####################
main "$@"
#####################
