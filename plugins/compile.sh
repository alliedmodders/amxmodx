#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

test -e compiled || mkdir compiled
rm -f temp.txt

os="$(uname -s)"
case "$os" in
	"Linux"  ) os="linux" ;;
	"Darwin" ) os="mac" ;;
	*)
esac

check_if_gameserver () {
	if [ ! -d "../configs" -a ! -d "../data" -a ! -d "../dlls" -a ! -d "../modules" -a ! -d "../plugins" ]
	then
		echo "Make sure that folders configs, data, dlls, modules and plugins"
		echo "  exist and run the script again"
		exit 1
	fi
}

get_base () {
	tar=(which tar)
	if (( tar == -1 ))
	then
		echo "Please install \"tar\" and run the script again"
		
		if [ "$os" == "linux" ]
		then
			echo "  type: sudo apt-get install tar"
		else
			echo "  type: brew install tar"
		fi
		 
		echo "Exiting..."
		exit 1
	fi
	
	ran=$RANDOM
	update=$RANDOM
	rm -rf update_files
	test -e update_files || mkdir update_files
	
	latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-base-$os)
	
	if [ $? -ne 0 ]
	then
		echo "Internet connection is required"
		echo "Exiting..."
		exit 1
	fi
	echo -ne "####                 (20%)\r"
	wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
	echo -ne "########             (40%)\r"
	tar -xzf $latest -C update_files
	echo -ne "##########           (50%)\r"
	rm -f $latest
}

prepare_update () {
	echo -ne "##################   (90%)\r"
	dir="update_files/addons/amxmodx"
	if [ $ver -eq 1 ]
	then
		mv $dir/configs/hamdata.ini ../configs/hamdata.ini
	fi
	rm -rf $dir/configs
	rm -rf $dir/logs
	
	cp -r $dir ../../
	echo -ne "#################### (100%)\r"
}

get_game_files () {
	if [ -n "$gamemod" ]
	then 
		echo -ne "############         (60%)\r"
		latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/$version/amxmodx-latest-$gamemod-$os)
		echo -ne "##############       (70%)\r"
		wget -q https://www.amxmodx.org/amxxdrop/$version/$latest
		echo -ne "################     (80%)\r"
		tar -xzf $latest -C update_files
		rm -f $latest
	fi
	
	prepare_update
}

update_game () {
	echo -en "\nUpdating AMXX $version "
	case $1 in
		1 )
			echo -e "Counter-Strike\n"
			gamemod="cstrike"
			;;
		2 )
			echo -e "Day of Defeat\n"
			gamemod="dod"
			;;
		3 )
			echo -e "Earth's Special Forces\n"
			gamemod="esf"
			;;
		4 )
			echo -e "Half-Life\n"
			;;
		5 )
			echo -e "Natural Selection\n"
			gamemod="ns"
			;;
		6 )
			echo -e "Team Fortress Classic\n"
			gamemod="tfc"
			;;
		7 )
			echo -e "The Specialists\n"
			gamemod="ts"
			;;
		*)
	esac
	
	echo -ne "##                   (10%)\r"
	
	get_base
	get_game_files $id
}

get_game () {
	echo "Please select the Game"
	echo "  1) Counter-Strike"
	echo "  2) Day of Defeat"
	echo "  3) Earth's Special Forces"
	echo "  4) Half-Life"
	echo "  5) Natural Selection"
	echo "  6) Team Fortress Classic"
	echo "  7) The Specialists"
	echo "  0) Exit"
	echo -n "Number: "
	read -r id
	
	if (( id == 0 )) || (( id > 7 ))
	then
		echo "Exiting..."
		exit 1
	fi
	
	echo -e "\nAMX MOD X Version"
	echo "  1) 1.9"
	echo "  2) 1.10"
	echo -n "Version: "
	read -r ver
	
	if (( ver == 1 ))
	then
		version="1.9"
	elif (( ver == 2 ))
	then
		version="1.10"
	else
		echo "Exiting..."
		exit 1
	fi
	
	echo -e "\nBefore updating files, note that all modifications"
	echo "  of default files will be reset"
	echo "Default files are shown below"
	echo "      configs   / hamdata.ini"
	echo "      data      / all default files"
	echo "      dlls      / amxmodx_mm_i386.so"
	echo "      modules   / all default files"
	echo "      plugins   / all default files"
	echo "      scripting / all default files"
	echo -en "\nContinue ? [Y/n] : "
	read -r answer
	
	if [ ! "$answer" == "Y" ]
	then
		echo "Exiting..."
		exit 1
	fi
}

check_for_updates () {
	check_if_gameserver
	get_game
	update_game $id
	echo -e "\nSucessfully updated"
	mv $dir/scripting/compile.sh compile.sh
	rm -rf update_files
	exit 1
}
	

if [ $# != 0 ]
	then
		case $1 in
			"--help" | "-h" )
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
				;;
			 "--version" | "-v" )
			 	./amxxpc -q | head -n 3
	 			;;
	 		"--latest" | "-l" )
				latest=$(wget -qO- https://www.amxmodx.org/amxxdrop/1.9/amxmodx-latest-base-$os)
				
				if [ $? -eq 0 ]
				then
					echo "Latest AMX MOD X version"
					echo -n "1.9 Build: "
					echo $latest | grep -o -P '(?<=git).*(?=-base)'
					echo -n "1.10 Build: "
					wget -qO- https://www.amxmodx.org/amxxdrop/1.10/amxmodx-latest-base-$os | grep -o -P '(?<=git).*(?=-base)'
				else
					echo "Internet connection is required"
				fi
				;;
			"--update" | "-u" )
				check_for_updates
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
