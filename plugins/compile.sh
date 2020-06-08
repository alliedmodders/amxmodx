#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

##################### 
# REQUIRED PACKAGES #
#####################
# wget
# tar

##################### 
# GENERAL FUNCTIONS #
#####################

# Quit script
quit_script () {
	if (( $# != 0 )); then
		echo $@; fi
	
	echo -e "Exiting..."
	exit 1
}

# Get network status
network_status () {
	echo -ne "Please wait\r"
	connection=$(wget -qO- google.com)
	
	if [ $? -eq 0 ]; then
		return 0;
	else quit_script "Internet connection is required"; fi
}

# Checks if folder exists
folder_exists () {
	if [ -d "$1" ]; then
		return 0;
	else return 1; fi;
}

# Checks if file exists
file_exists () {
	if [ -f "$1" ]; then
		return 0;
	else return 1; fi;
}

# Returns OS' name
get_os_name () {
	os="$(uname -s)"
	
	case "$os" in
		"Linux"  ) os="linux" ;;
		"Darwin" ) os="mac" ;;
	*) esac
}

# Checks if app is installed
check_for_app () {
	if [ $(which $1) ]; then
		return 0;
	else quit_script "Script requires $1 package"; fi
}

# Checks if variable is number
is_number () {
	if ! [[ $1 =~ ^-?[0-9]+([.][0-9]+)?$ ]]; then 
		quit_script "Invalid number"
	fi
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
# UPDATE FUNCTIONS  #
#####################

# unused ; Reads value
#__update_read_value () {
#	echo -n "Number: "; read -r $1
#}

# END
__update_finish () {
	path="update_files/addons/amxmodx"
	
	for ((i = 0; i < length; i++)); do
		if [ "${modify[i]}" == "0" ]; then
			file_del="${dir}/${files[i]}"
			rm -f $file_del
		fi
	done
	
	rm -rf $path/logs
	cp -r $path/* ../
	if file_exists "index.html"; then rm -f "index.html"; fi
	echo -e "\n#################### (100%)"
	echo -e "\nSuccessfully installed"
	exit 1
}

# Reload table
__update_configs_reload_table () {
	for ((i = 0; i < length; i++)); do
		current=${#files[i]}
		
		file_num=$((i+1))
		if (( file_num < 10)); then
			echo -n " $file_num) ${files[i]}"
		else echo -n "$file_num) ${files[i]}"; fi
		
		for ((j = 0; j < $(($largest-$current+1)); j++)); do
			echo -n " "; done
	
		if [ "${modify[i]}" -eq "1" ]; then
			echo "[replace]"
		else echo "[x]"; fi
	done
	
	__update_config_key_select
}

# Multiselect modification
__update_configs_select_type () {
	for ((i = 0; i < length; i++)); do
		modify[i]=$1;
	done
	
	__update_configs_reload_table
}

# Single file modification
__update_configs_file () {
	if (( $1 < 1 )) || (( $1 > length+1 )); then
		echo -e "\n      Number out of boundaries\n";
	else
		arr=$(($1-1))
		
		if [ "${modify[arr]}" == "1" ]; then
			modify[arr]=0
		else modify[arr]=1; fi
		
		echo -e "\n      Modified ${files[arr]} ${modify[arr]}\n"
	fi
	
	__update_configs_reload_table
}

# Read key
__update_config_key_select () {
	echo -e "\nPlease select which files you want to replace or not"
	echo -n "00) Continue | 88) Deselect All | 99) Select All | Number: "; read -r fileedit
	
	is_number $fileedit
	case $fileedit in
		00 ) __update_finish                 ;;
		88 ) __update_configs_select_type 0  ;;
		99 ) __update_configs_select_type 1  ;;
		 * ) __update_configs_file $fileedit ;;
	esac
}

# configs folder Settings
__update_configs_table () {
	dir="update_files/addons/amxmodx/configs"
	
	files=()
	modify=()
	largest=0
	
	while IFS= read -r line; do
		files+=("$line")
	done< <(ls $dir| sort)
	length=${#files[@]}

	for ((i = 0; i < length; i++)); do
		current=${#files[i]}
	
		if [[ "${files[i]}" == "hamdata.ini" ]]; then
			modify[i]=1
		else modify[i]=0; fi
	
		if (( $current > $largest )); then
			largest=$current; fi
	done

	for ((i = 0; i < length; i++)); do
		current=${#files[i]}
		
		num=$((i+1))
		if (( num < 10)); then
			echo -n " $num) ${files[i]}"
		else echo -n "$num) ${files[i]}"; fi
		
		for ((j = 0; j < $(($largest-$current+1)); j++)); do
			echo -n " "; done
	
		if [ "${modify[i]}" -eq "1" ]; then
			echo "[replace]"
		else echo "[x]"; fi
	done
	
	__update_config_key_select
}

# Replace specific files in configs directory
__update_configs () {
	echo -ne "\n\nReplace specific files in configs folder ? "
	echo -ne "[Y/n] : "; read -r configsedit
	
	if [ "$configsedit" == "Y" ]; then
		echo && __update_configs_table
	else return 0; fi
}

# Install Game package
__update_game_files () {
	if [ -n "$1" ]; then 
		echo -ne "############         (60%)\r"
		latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-$1-$os)
		if file_exists $latest; then rm -f $latest; fi
		echo -ne "##############       (70%)\r"
		wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
		echo -ne "################     (80%)\r"
		tar -xzf $latest -C update_files
		rm -f $latest
		echo -ne "################     (90%)\r"
	fi
}

# Install Base package
__update_get_base () {
	check_for_app tar
	
	if folder_exists "update_files"; then
		rm -rf update_files; fi
	mkdir update_files
	
	latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-base-$os)
	if file_exists $latest; then rm -f $latest; fi
	echo -ne "####                 (20%)\r"
	wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
	echo -ne "########             (40%)\r"
	tar -xzf $latest -C update_files
	echo -ne "##########           (50%)\r"
	rm -f $latest
}

# Update Game files
__update_game () {
	echo -ne "\nUpdating AMXX $version "
	
	case $game in
		1 ) echo -e "Counter-Strike\n";         game="cstrike" ;;
		2 ) echo -e "Day of Defeat\n";          game="dod"     ;;
		3 ) echo -e "Earth's Special Forces\n"; game="esf"     ;;
		4 ) echo -e "Half-Life\n";                             ;;
		5 ) echo -e "Natural Selection\n";      game="ns"      ;;
		6 ) echo -e "Team Fortress Classic\n";  game="tfc"     ;;
		7 ) echo -e "The Specialists\n";        game="ts"      ;;
	*) esac
	
	echo -ne "##                   (10%)\r"
}

# Lists Notes before updating
__update_list_notes () {
	echo -e "\nBefore updating files, note that all modifications"
	echo "  of default files will be reset"
	echo -e "Default files are shown below\r"
	
	if (( ver != 2 )); then
	echo "      configs   / hamdata.ini"; fi
	echo "      data      / all default files"
	echo "      dlls      / all default files"
	echo "      modules   / all default files"
	echo "      plugins   / all default files"
	echo "      scripting / all default files"
	echo -ne "\nContinue ? [Y/n] : "; read -r answer
	
	if [ ! "$answer" == "Y" ]; then
		quit_script
	fi
}

# Lists AMX MOD X Versions
__update_amxx_version () {
	echo -e "\nAMX MOD X Version"
	echo "  1) 1.9"
	echo "  2) 1.10"
	echo -n "Number: "; read -r ver
	
	is_number $ver
	case $ver in
		1 ) version="1.9"   ;;
		2 ) version="1.10"  ;;
	*) esac
}

# Lists all Games
__update_list_games () {
	check_for_app wget
	network_status

	echo -e "Please select the Game"
	echo "  1) Counter-Strike"
	echo "  2) Day of Defeat"
	echo "  3) Earth's Special Forces"
	echo "  4) Half-Life"
	echo "  5) Natural Selection"
	echo "  6) Team Fortress Classic"
	echo "  7) The Specialists"
	echo "  0) Exit"
	
	echo -n "Number: "; read -r game
	
	is_number $game
	if (( game <= 0 )) || (( game > 7 )); then
		quit_script
	fi
}

# Check if directories exists
__update_check_if_gameserver () {
	if [ ! -d "../configs" -a ! -d "../data" -a ! -d "../dlls" -a ! -d "../modules" -a ! -d "../plugins" ]; then
		echo "Make sure that folders configs, data, dlls, modules and plugins"
		echo "  exist and run the script again"
		quit_script
	fi
}

# UPDATE
__update () {
	__update_check_if_gameserver
	__update_list_games
	__update_amxx_version
	__update_list_notes
	__update_game
	__update_get_base
	__update_game_files $game
	__update_configs
	__update_finish
}

#####################
# COMMAND FUNCTIONS #
#####################

# HELP
__help () {
	echo "  Usage:"
	echo "--help    (-h) : This menu"
	echo "--version (-v) : AMX MOD X Compiler version"
	echo "--latest  (-l) : Latest AMX MOD X version"
	echo -e "--update  (-u) : Update files\n"
	echo "To compile all source files in the directory"
	echo "  execute the script without any parameters"
	echo "To compile specific or multiple source files"
	echo "  execute the script with parameters set as source files' name"
	echo "  e.g ./compile.sh admin.sma admincmd.sma adminchat.sma"
}

# VERSION
__version () {
	if file_exists amxxpc; then
		./amxxpc -q | head -n 3
	else quit_script "amxxpc does not exists"; fi
}

# LATEST
__latest () {
	network_status

	echo -e "Latest AMX MOD X version"
	echo -n "1.9 Build: "
	echo $(wget -qO- https://www.amxmodx.org/amxxdrop/1.9/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)')
	echo -n "1.10 Build: "
	echo $(wget -qO- https://www.amxmodx.org/amxxdrop/1.10/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)')
}

# Execute command
execute_command () {
	case "$1" in
		"--help"    | "-h" ) __help    ;;
		"--version" | "-v" ) __version ;;
		"--latest"  | "-l" ) __latest  ;;
		"--update"  | "-u" ) __update  ;;
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
	get_os_name
	if check_for_arguments $@; then
		for arg in "$@"; do
			if check_for_commands $arg; then
				execute_command $arg
				exit
			else
				if [[ $arg == *.sma ]]; then
					compile_source_file $arg
				else
					echo "  $arg not a valid file or command"
					echo "  Type \"$0 --help\" to see more information"
				fi
			fi
		done
	else 
		for sourcefile in *.sma; do
			compile_source_file $sourcefile
		done
	fi
}

#####################
# EXECUTE MAIN FUNC #
#####################
main $@
#####################
