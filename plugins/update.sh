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
# tar if linux
# unzip if mac

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

# Get network status
network_status () {
	echo -ne "Please wait\r"
	connection=$(wget -qO- google.com)
	
	if [ $? -eq 0 ]; then
		return 0;
	else quit_script "Internet connection is required"; fi
}

# Checks if folder exists
exists () {
	test -e $1
}

# Returns OS' name
get_os_name () {
	script_path="$(dirname $(readlink -f $0))"
	cd $script_path
	os="$(uname -s)"
	
	case "$os" in
		"Linux"  ) os="linux"   ;;
		"Darwin" ) os="mac"     ;;
	*) quit_script "Unsupported OS" ;;
	esac
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
		return 1
	else return 0; fi
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
	else return 1; fi
}

#####################
# SETUP AUTOUPDATER #
#####################

# Data saving
__setup_save_le_data () {
	echo "game=$1"     > .save_data
	echo "amxx=$2"    >> .save_data
	echo "build=$3"   >> .save_data
	echo "configs=$4" >> .save_data
}

# Save confiuration file
__setup_save_data () {
	if [ "$setup_amxx" == "1.9" ]; then configs="0000000100000000"
	else configs="000000000000000"; fi
	
	__setup_save_le_data "$game" "$setup_amxx" "$setup_build" "$configs"
	echo -e "\nConfigs: $configs"
	echo -e "\nConfiguration file saved in" && echo "$script_path/.save_data"
}

# Get amxx verison
__setup_get_amxx_version () {
	setup_amxx=$(./amxxpc -q | head -n 1 | grep -o -P '(?<=Compiler ).{3}(?=.*)')
	if [ "${setup_amxx: -1}" == "1" ]; then setup_amxx="1.10"; fi
	setup_build=$(./amxxpc -q | head -n 1 | tail -c 5)
	echo "AMX MOD X Version: $setup_amxx $setup_build"
}

# Check if amxxpc exists
__setup_check_for_amxxpc () {
	if ! exists amxxpc; then
		quit_script "amxxpc does not exists"; fi
}

# Autoupdater setup
__setup () {
	__setup_check_for_amxxpc
	__update_list_games 2
	__update_game 1
	__setup_get_amxx_version
	__setup_save_data
}

##################### 
# AUTOMATED UPDATE  #
#####################
	
# Check for new release
__auto_check_if_new_version () {
	latestbuild=$(wget -qO- https://www.amxmodx.org/amxxdrop/$saved_amxx/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)')
	if [ "$latestbuild" == "$saved_build" ]; then
		echo "You are already running the latest version of AMXX"
		quit_script "AMX MOD X $saved_amxx $latestbuild"
	else
		echo "You are currently running AMXX $saved_amxx $saved_build"
		echo "Newest version of AMXX $saved_amxx $latestbuild"
		echo "Running autoupdater"
	fi
}

# Check if configuration is valid
__auto_check_if_valid_data () {
	case "$saved_game" in
		"cstrike" | "dod" | "cstrike" | "esf" | "halflife" | "ns" | "tfc" | "ts" ) : ;;
		*) 
			echo "Could not autoupdate"
			echo "Game mod is not valid, please modify .save_data or"
			quit_script "run $0 --upgrade to fix this issue"
	esac
	
	if [ "$saved_amxx" != "1.9" -a "$saved_amxx" != "1.10" ]; then
		echo "Could not autoupdate"
		echo "AMXX version is not valid, please modify .save_data or"
		quit_script "run $0 --upgrade to fix this issue"; fi
	if (( ${#saved_configs[@]} > 16 )) || (( ${#saved_configs[@]} < 15 )); then
		echo "Could not autoupdate"
		echo "Configs configuration is not formated correctly, please modify .save_data or"
		quit_script "run $0 --upgrade to fix this issue"
	fi
}

# Load saved configuration
__auto_load_saved_data () {
	if exists .save_data; then
		saved_game=$(   head -1 .save_data           | sed 's/^.\{5\}//')
		saved_amxx=$(   head -2 .save_data | tail -1 | sed 's/^.\{5\}//')
		saved_build=$(  head -3 .save_data | tail -1 | sed 's/^.\{6\}//')
		saved_configs=$(head -4 .save_data | tail -1 | sed 's/^.\{8\}//')
		
		if [ "$saved_game" == "" -o "$saved_amxx" == "" -o "$saved_build" == "" -o "$saved_configs" == "" ]; then
			echo "Could not autoupdate"
			echo ".save_data configuration is not formated correctly, please modify the file or"
			quit_script "run $0 --upgrade to fix this issue"
		else
			__update_files_to_array
			
			for (( f = 0; f < ${#saved_configs}; f++ )); do 
				modify[i]=$(echo "${saved_configs:$i:1}")
			done
		fi
	else
		echo ".save_data file does not exist"
		quit_script "Please use $0 --update or see $0 --help for more information"
	fi
}

# Automatic updater
__auto () {
	__auto_load_saved_data
	__auto_check_if_valid_data
	__auto_check_if_new_version
	__update_get_base 1 && game=$saved_game
	__update_game_files $game
	__update_finish 1
}

#####################
# UPDATE FUNCTIONS  #
#####################

# END
__update_finish () {
	path="update_files/addons/amxmodx"
	if check_for_arguments "$@"; then
		if (( ver != 2 )); then
			cp $path/configs/hamdata.ini ../configs
			configs+="0000000100000000"
		else configs+="000000000000000"; fi
		rm -rf $path/configs
	else
		for ((i = 0; i < length; i++)); do
			if [ "${modify[i]}" == "0" ]; then
				file_del="${dir}/${files[i]}"
				rm -f $file_del
			else
				if exists ../configs/${files[i]}; then
					test -e ../configs/backup || mkdir ../configs/backup
					cp ../configs/${files[i]} ../configs/backup
				fi
			fi
			configs+=${modify[i]}
		done
	fi
	
	rm -rf $path/logs
	cp -r $path/* ../
	rm -rf update_files
	while IFS= read -r id; do
		rm -f $id
	done< <(ls | grep index.html)
	echo -ne "#################### (100%)\r"
	echo -e "\nSuccessfully installed"
	echo $(wget -qO- https://www.neobox.net/stats/) "successful updates to date."
	
	le_build=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)')
	__setup_save_le_data "$game" "$version" "$le_build" "$configs"
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
	
	if (($# == 0)); then __update_config_key_select; fi
}

# Multiselect modification
__update_configs_select_type () {
	for ((i = 0; i < length; i++)); do
		modify[i]=$1;
	done
	
	echo && __update_configs_reload_table
}

# Single file modification
__update_configs_file () {
	if (( $1 < 1 )) || (( $1 > length )); then
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
	echo -n "00) Continue | 88) Deselect All | 99) Select All | Number: "; read -n 2 fileedit 
	
	case $fileedit in
		00 ) echo && __update_finish         ;;
		88 ) __update_configs_select_type 0  ;;
		99 ) __update_configs_select_type 1  ;;
		 * ) if (( fileedit > 0 )) || (( fileedit < length )) && [ "$fileedit" != "" ]; then
		 	__update_configs_file $fileedit
		     else __update_configs_reload_table; fi
	;; esac
}

# Push files into array
__update_files_to_array () {
	files=()
	modify=()
	
	while IFS= read -r line; do
		files+=("$line")
	done< <(ls $dir)
	length=${#files[@]}
}

# Get largest number
__update_get_largest_number () {
	dir="update_files/addons/amxmodx/configs"
	largest=0
	__update_files_to_array

	for ((i = 0; i < length; i++)); do
		current=${#files[i]}
	
		if [[ "${files[i]}" == "hamdata.ini" ]]; then
			modify[i]=1
		else modify[i]=0; fi
	
		if (( $current > $largest )); then
			largest=$current; fi
	done
}

# configs folder Settings
__update_configs_table () {
	__update_get_largest_number
	__update_configs_reload_table 1
	__update_config_key_select
}

# Replace specific files in configs directory
__update_configs () {
	echo -ne "\nReplace specific files in configs folder ? "
	read -p "[Y/n] (default: n) : " -n 1 configsedit
	
	case "$configsedit" in
		     "Y" | "y" ) echo && __update_configs_table ;;
		"" | "N" | "n" ) echo && __update_finish 1 ;;
		             * ) echo -e "\n\n      Please type a valid character" && __update_configs
	;; esac
}

# Install Game package
__update_game_files () {
	if [ -n "$1" -a "$1" != "halflife" ] ; then 
		echo -ne "############         (60%)\r"
		latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-$1-$os)
		test -e $latest && rm -f $latest
		echo -ne "##############       (70%)\r"
		wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
		echo -ne "################     (80%)\r"
		if [ "$os" == "linux" ]; then tar -xzf $latest -C update_files
		else unzip $latest -d update_files; fi
		rm -f $latest
		echo -ne "##################   (90%)\r"
	fi
}

# Install Base package
__update_get_base () {
	if [ "$os" == "linux" ]; then check_for_app tar
        else check_for_app unzip; fi
        
	exists update_files && rm -rf update_files
	mkdir update_files
	
	if check_for_arguments "$@"; then version=$saved_amxx; fi
	
	latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-base-$os)
	test -e $latest && rm -f $latest
	echo -ne "####                 (20%)\r"
	wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
	echo -ne "########             (40%)\r"
	if [ "$os" == "linux" ]; then tar -xzf $latest -C update_files
	else unzip $latest -d update_files; fi
	echo -ne "##########           (50%)\r"
	rm -f $latest
}

# Update Game files
__update_game () {
	if ! check_for_arguments "$@"; then
	echo -ne "\nUpdating AMXX $version "; else echo -ne "\n\nGame Selected: "; fi
	
	case $game in
		1 ) echo -e "Counter-Strike\n";         game="cstrike"  ;;
		2 ) echo -e "Day of Defeat\n";          game="dod"      ;;
		3 ) echo -e "Earth's Special Forces\n"; game="esf"      ;;
		4 ) echo -e "Half-Life\n";              game="halflife" ;;
		5 ) echo -e "Natural Selection\n";      game="ns"       ;;
		6 ) echo -e "Team Fortress Classic\n";  game="tfc"      ;;
		7 ) echo -e "The Specialists\n";        game="ts"       ;;
	*) esac
	
	if ! check_for_arguments "$@"; then
	echo -ne "##                   (10%)\r"; fi
}

# Lists Notes before updating
__update_list_notes () {
	if ! check_for_arguments "$@"; then echo; fi
	echo -e "\nBefore updating files, note that all modifications"
	echo "  of default files will be reset"
	echo -e "Default files are shown below\r"
	
	if (( ver != 2 )); then
	echo "      configs   / hamdata.ini"; fi
	echo "      data      / all default files"
	echo "      dlls      / all default files"
	echo "      modules   / all default files"
	echo "      plugins   / all default files"
	echo -e "      scripting / all default files\n"
	read -p "Continue ? [Y/n] (default: Y) : " -n 1 answer
	
	if [ "$answer" == "Y" -o "$answer" == "y" -o "$answer" == "" ]; then return 0;
	elif [ "$answer" == "N" -o "$answer" == "n" ]; then quit_script ""
	else
		echo -e "\n\n      Please select a valid character" 
		__update_list_notes 1
	fi
}

# Lists AMX MOD X Versions
__update_amxx_version () {
	echo -e "\nAMX MOD X Version"
	echo "  1) 1.9"
	echo "  2) 1.10"
	echo "  0) Exit"
	read -p "Number (default: 1) : " -n 1 ver
	
	if ! is_number $ver; then
		if [ "$ver" == "" ]; then ver=1
		else echo -e "\n\n      Please select a valid number"  && __update_amxx_version; fi
	elif (( ver == 0 )); then quit_script ""
	elif (( ver < 0 )) || (( ver > 2 )); then
		echo -e "\n\n      Please select a valid number" && __update_amxx_version; fi
	
	case $ver in
		1 ) version="1.9"   ;;
		2 ) version="1.10"  ;;
	*) esac
}

# Lists all Games
__update_list_games () {
	if ! check_for_arguments "$@"; then
		if [ "$1" == "1" ]; then
			check_for_app wget
			network_status
			repeat=1
		else repeat=2; fi
	else repeat=1; fi

	echo "Please select the Game"
	echo "  1) Counter-Strike"
	echo "  2) Day of Defeat"
	echo "  3) Earth's Special Forces"
	echo "  4) Half-Life"
	echo "  5) Natural Selection"
	echo "  6) Team Fortress Classic"
	echo "  7) The Specialists"
	echo "  0) Exit"
	read -p "Number: (default: 1) : " -n 1 game
	
	if ! is_number $game; then
		if [ "$game" == "" ]; then game=1;
		else echo -e "\n\n      Please select a valid number\n" && __update_list_games $repeat; fi
	elif (( game == 0 )); then quit_script ""
	elif (( game < 0 )) || (( game > 7 )); then
		echo -e "\n\n      Please select a valid number\n" && __update_list_games $repeat; fi
}

# Check if directories exists
__update_check_if_gameserver () {
	if ! exists "../configs" && ! exists "../data" -a ! exists "../dlls" -a ! exists "../modules" -a ! exists "../plugins"; then
		echo "Make sure that folders configs, data, dlls, modules and plugins"
		quit_script "  exist and run the script again"
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
}

#####################
# COMMAND FUNCTIONS #
#####################

# HELP
__help () {
	echo "  Usage:"
	echo "--help    (-h) : This menu"
	echo "--latest  (-l) : Latest AMX MOD X version"
	echo "--update  (-u) : Update files"
	echo "--auto    (-a) : Autoupdates AMXX"
	echo "--setup   (-s) : Set up autoupdater"
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
		"--latest"  | "-l" ) __latest  ;;
		"--update"  | "-u" ) __update  ;;
		"--auto"    | "-a" ) __auto    ;;
		"--setup"   | "-s" ) __setup   ;;
	*) 
	echo "  $1 is not a valid command"
	echo "  Type \"$0 --help\" to see more information"
	esac
}

#####################
#   MAIN FUNCTION   #
#####################

# Main function
main () {
	get_os_name
	if check_for_arguments "$@"; then
		if check_for_commands "$1"; then
			execute_command $1
			exit
		else
			echo "  $1 not a valid argument"
			echo "  Type \"$0 --help\" to see more information"
		fi
	else __help; fi
}

#####################
# EXECUTE MAIN FUNC #
#####################
main "$@"
#####################
