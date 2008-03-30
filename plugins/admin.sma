/* AMX Mod X script.
*   Admin Base Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is part of AMX Mod X.
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

// Uncomment for SQL version
// #define USING_SQL

#include <amxmodx>
#include <amxmisc>
#if defined USING_SQL
#include <sqlx>
#endif

//new Vector:AdminList;

new AdminCount;

new PLUGINNAME[] = "AMX Mod X"

#define ADMIN_LOOKUP	(1<<0)
#define ADMIN_NORMAL	(1<<1)
#define ADMIN_STEAM		(1<<2)
#define ADMIN_IPADDR	(1<<3)
#define ADMIN_NAME		(1<<4)

new g_cmdLoopback[16]
new bool:g_CaseSensitiveName[33];

// pcvars
new amx_mode;
new amx_password_field;
new amx_default_access;

public plugin_init()
{
#if defined USING_SQL
	register_plugin("Admin Base (SQL)", AMXX_VERSION_STR, "AMXX Dev Team")
#else
	register_plugin("Admin Base", AMXX_VERSION_STR, "AMXX Dev Team")
#endif
	register_dictionary("admin.txt")
	register_dictionary("common.txt")
	amx_mode=register_cvar("amx_mode", "1")
	amx_password_field=register_cvar("amx_password_field", "_pw")
	amx_default_access=register_cvar("amx_default_access", "")

	register_cvar("amx_vote_ratio", "0.02")
	register_cvar("amx_vote_time", "10")
	register_cvar("amx_vote_answers", "1")
	register_cvar("amx_vote_delay", "60")
	register_cvar("amx_last_voting", "0")
	register_cvar("amx_show_activity", "2")
	register_cvar("amx_votekick_ratio", "0.40")
	register_cvar("amx_voteban_ratio", "0.40")
	register_cvar("amx_votemap_ratio", "0.40")

	set_cvar_float("amx_last_voting", 0.0)

#if defined USING_SQL
	register_srvcmd("amx_sqladmins", "adminSql")
	register_cvar("amx_sql_table", "admins")
#endif
	register_cvar("amx_sql_host", "127.0.0.1")
	register_cvar("amx_sql_user", "root")
	register_cvar("amx_sql_pass", "")
	register_cvar("amx_sql_db", "amx")
	register_cvar("amx_sql_type", "mysql")

	register_concmd("amx_reloadadmins", "cmdReload", ADMIN_CFG)
	register_concmd("amx_addadmin", "addadminfn", ADMIN_RCON, "<playername|auth> <accessflags> [password] [authtype] - add specified player as an admin to users.ini")

	format(g_cmdLoopback, 15, "amxauth%c%c%c%c", random_num('A', 'Z'), random_num('A', 'Z'), random_num('A', 'Z'), random_num('A', 'Z'))

	register_clcmd(g_cmdLoopback, "ackSignal")

	remove_user_flags(0, read_flags("z"))		// Remove 'user' flag from server rights

	new configsDir[64]
	get_configsdir(configsDir, 63)
	
	server_cmd("exec %s/amxx.cfg", configsDir)	// Execute main configuration file
	server_cmd("exec %s/sql.cfg", configsDir)

	// Create a vector of 5 cells to store the info.
	//AdminList=vector_create(5);

	
#if defined USING_SQL
	server_cmd("amx_sqladmins")
#else
	format(configsDir, 63, "%s/users.ini", configsDir)
	loadSettings(configsDir)					// Load admins accounts
#endif
}
public client_connect(id)
{
	g_CaseSensitiveName[id] = false;
}
public addadminfn(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED
		
	new idtype = ADMIN_STEAM | ADMIN_LOOKUP

	if (read_argc() >= 5)
	{
		new t_arg[16]
		read_argv(4, t_arg, 15)
		
		if (equali(t_arg, "steam") || equali(t_arg, "steamid") || equali(t_arg, "auth"))
		{
			idtype = ADMIN_STEAM
		}
		else if (equali(t_arg, "ip"))
		{
			idtype = ADMIN_IPADDR
		}
		else if (equali(t_arg, "name") || equali(t_arg, "nick"))
		{
			idtype = ADMIN_NAME
			
			if (equali(t_arg, "name"))
				idtype |= ADMIN_LOOKUP
		} else {
			console_print(id, "[%s] Unknown id type ^"%s^", use one of: steamid, ip, name", PLUGINNAME, t_arg)
			return PLUGIN_HANDLED
		}
	}

	new arg[33]
	read_argv(1, arg, 32)
	new player = -1
	
	if (idtype & ADMIN_STEAM)
	{
		if (containi(arg, "STEAM_0:") == -1)
		{
			idtype |= ADMIN_LOOKUP
			player = cmd_target(id, arg, CMDTARGET_ALLOW_SELF | CMDTARGET_NO_BOTS)
		} else {
			new _steamid[44]
			static _players[32], _num, _pv
			get_players(_players, _num)
			for (new _i=0; _i<_num; _i++)
			{
				_pv = _players[_i]
				get_user_authid(_pv, _steamid, sizeof(_steamid)-1)
				if (!_steamid[0])
					continue
				if (equal(_steamid, arg))
				{
					player = _pv
					break
				}
			}	
			if (player < 1)
			{
				idtype &= ~ADMIN_LOOKUP
			}		
		}
	}
	else if (idtype & ADMIN_NAME)
	{
		player = cmd_target(id, arg, CMDTARGET_ALLOW_SELF | CMDTARGET_NO_BOTS)
		
		if (player)
			idtype |= ADMIN_LOOKUP
		else
			idtype &= ~ADMIN_LOOKUP
	}
	else if (idtype & ADMIN_IPADDR)
	{
		new len = strlen(arg)
		new dots, chars
		
		for (new i = 0; i < len; i++)
		{
			if (arg[i] == '.')
			{
				if (!chars || chars > 3)
					break
				
				if (++dots > 3)
					break
				
				chars = 0
			} else {
				chars++
			}
			
			if (dots != 3 || !chars || chars > 3)
			{
				idtype |= ADMIN_LOOKUP
				player = find_player("dh", arg)
			}
		}
	}
	
	if (idtype & ADMIN_LOOKUP && !player)
	{
		console_print(id, "%L", id, "CL_NOT_FOUND")
		return PLUGIN_HANDLED
	}
	
	new flags[64]
	read_argv(2, flags, 63)

	new password[64]
	if (read_argc() >= 4)
		read_argv(3, password, 63)

	new auth[33]
	new Comment[33]; // name of player to pass to comment field
	if (idtype & ADMIN_LOOKUP)
	{
		get_user_name(player, Comment, sizeof(Comment)-1)
		if (idtype & ADMIN_STEAM)
		{
			get_user_authid(player, auth, 32)
		}
		else if (idtype & ADMIN_IPADDR)
		{
			get_user_ip(player, auth, 32)
		}
		else if (idtype & ADMIN_NAME)
		{
			get_user_name(player, auth, 32)
		}
	} else {
		copy(auth, 32, arg)
	}
	
	new type[16], len
	
	if (idtype & ADMIN_STEAM)
		len += format(type[len], 15-len, "c")
	else if (idtype & ADMIN_IPADDR)
		len += format(type[len], 15-len, "d")
	
	if (strlen(password) > 0)
		len += format(type[len], 15-len, "a")
	else
		len += format(type[len], 15-len, "e")
	
	AddAdmin(id, auth, flags, password, type, Comment)
	cmdReload(id, ADMIN_CFG, 0)

	if (player > 0)
	{
		new name[32]
		get_user_info(player, "name", name, 31)
		accessUser(player, name)
	}

	return PLUGIN_HANDLED
}

AddAdmin(id, auth[], accessflags[], password[], flags[], comment[]="")
{
#if defined USING_SQL
	new error[128], errno

	new Handle:info = SQL_MakeStdTuple()
	new Handle:sql = SQL_Connect(info, errno, error, 127)
	
	if (sql == Empty_Handle)
	{
		server_print("[AMXX] %L", LANG_SERVER, "SQL_CANT_CON", error)
		//backup to users.ini
#endif
		// Make sure that the users.ini file exists.
		new configsDir[64]
		get_configsdir(configsDir, 63)
		format(configsDir, 63, "%s/users.ini", configsDir)

		if (!file_exists(configsDir))
		{
			console_print(id, "[%s] File ^"%s^" doesn't exist.", PLUGINNAME, configsDir)
			return
		}

		// Make sure steamid isn't already in file.
		new line = 0, textline[256], len
		const SIZE = 63
		new line_steamid[SIZE + 1], line_password[SIZE + 1], line_accessflags[SIZE + 1], line_flags[SIZE + 1], parsedParams
		
		// <name|ip|steamid> <password> <access flags> <account flags>
		while ((line = read_file(configsDir, line, textline, 255, len)))
		{
			if (len == 0 || equal(textline, ";", 1))
				continue // comment line

			parsedParams = parse(textline, line_steamid, SIZE, line_password, SIZE, line_accessflags, SIZE, line_flags, SIZE)
			
			if (parsedParams != 4)
				continue	// Send warning/error?
			
			if (containi(line_flags, flags) != -1 && equal(line_steamid, auth))
			{
				console_print(id, "[%s] %s already exists!", PLUGINNAME, auth)
				return
			}
		}

		// If we came here, steamid doesn't exist in users.ini. Add it.
		new linetoadd[512]
		
		if (comment[0]==0)
		{
			formatex(linetoadd, 511, "^r^n^"%s^" ^"%s^" ^"%s^" ^"%s^"", auth, password, accessflags, flags)
		}
		else
		{
			formatex(linetoadd, 511, "^r^n^"%s^" ^"%s^" ^"%s^" ^"%s^" ; %s", auth, password, accessflags, flags, comment)
		}
		console_print(id, "Adding:^n%s", linetoadd)

		if (!write_file(configsDir, linetoadd))
			console_print(id, "[%s] Failed writing to %s!", PLUGINNAME, configsDir)
#if defined USING_SQL
	}
	
	new table[32]
	
	get_cvar_string("amx_sql_table", table, 31)
	
	new Handle:query = SQL_PrepareQuery(sql, "SELECT * FROM `%s` WHERE (`auth` = '%s')", table, auth)

	if (!SQL_Execute(query))
	{
		SQL_QueryError(query, error, 127)
		server_print("[AMXX] %L", LANG_SERVER, "SQL_CANT_LOAD_ADMINS", error)
		console_print(id, "[AMXX] %L", LANG_SERVER, "SQL_CANT_LOAD_ADMINS", error)
	} else if (SQL_NumResults(query)) {
		console_print(id, "[%s] %s already exists!", PLUGINNAME, auth)
	} else {
		console_print(id, "Adding to database:^n^"%s^" ^"%s^" ^"%s^" ^"%s^"", auth, password, accessflags, flags)
	
		SQL_QueryAndIgnore(sql, "REPLACE INTO `%s` (`auth`, `password`, `access`, `flags`) VALUES ('%s', '%s', '%s', '%s')", table, auth, password, accessflags, flags)
	}
	
	SQL_FreeHandle(query)
	SQL_FreeHandle(sql)
	SQL_FreeHandle(info)
#endif

}
public plugin_cfg()
{
	set_task(6.1, "delayed_load")
}

public delayed_load()
{
	new configFile[128], curMap[64], configDir[128]

	get_configsdir(configDir, sizeof(configDir)-1)
	get_mapname(curMap, sizeof(curMap)-1)

	new i=0;
	
	while (curMap[i] != '_' && curMap[i++] != '^0') {/*do nothing*/}
	
	if (curMap[i]=='_')
	{
		// this map has a prefix
		curMap[i]='^0';
		formatex(configFile, sizeof(configFile)-1, "%s/maps/prefix_%s.cfg", configDir, curMap);

		if (file_exists(configFile))
		{
			server_cmd("exec %s", configFile);
		}
	}

	get_mapname(curMap, sizeof(curMap)-1)

	
	formatex(configFile, sizeof(configFile)-1, "%s/maps/%s.cfg", configDir, curMap)

	if (file_exists(configFile))
	{
		server_cmd("exec %s", configFile)
	}
	
}

loadSettings(szFilename[])
{
	new File=fopen(szFilename,"r");
	
	if (File)
	{
		new Text[512];
		new Flags[32];
		new Access[32]
		new AuthData[44];
		new Password[32];
		
		while (!feof(File))
		{
			fgets(File,Text,sizeof(Text)-1);
			
			trim(Text);
			
			// comment
			if (Text[0]==';') 
			{
				continue;
			}
			
			Flags[0]=0;
			Access[0]=0;
			AuthData[0]=0;
			Password[0]=0;
			
			// not enough parameters
			if (parse(Text,AuthData,sizeof(AuthData)-1,Password,sizeof(Password)-1,Access,sizeof(Access)-1,Flags,sizeof(Flags)-1) < 2)
			{
				continue;
			}
			
			admins_push(AuthData,Password,read_flags(Access),read_flags(Flags));

			AdminCount++;
		}
		
		fclose(File);
	}

	if (AdminCount == 1)
	{
		server_print("[AMXX] %L", LANG_SERVER, "LOADED_ADMIN");
	}
	else
	{
		server_print("[AMXX] %L", LANG_SERVER, "LOADED_ADMINS", AdminCount);
	}
	
	return 1;
}

#if defined USING_SQL
public adminSql()
{
	new table[32], error[128], type[12], errno
	
	new Handle:info = SQL_MakeStdTuple()
	new Handle:sql = SQL_Connect(info, errno, error, 127)
	
	get_cvar_string("amx_sql_table", table, 31)
	
	SQL_GetAffinity(type, 11)
	
	if (sql == Empty_Handle)
	{
		server_print("[AMXX] %L", LANG_SERVER, "SQL_CANT_CON", error)
		
		//backup to users.ini
		new configsDir[64]
		
		get_configsdir(configsDir, 63)
		format(configsDir, 63, "%s/users.ini", configsDir)
		loadSettings(configsDir) // Load admins accounts

		return PLUGIN_HANDLED
	}

	new Handle:query
	
	if (equali(type, "sqlite"))
	{
		if (!sqlite_TableExists(sql, table))
		{
			SQL_QueryAndIgnore(sql, "CREATE TABLE %s ( auth TEXT NOT NULL DEFAULT '', password TEXT NOT NULL DEFAULT '', access TEXT NOT NULL DEFAULT '', flags TEXT NOT NULL DEFAULT '' )", table)
		}

		query = SQL_PrepareQuery(sql, "SELECT auth, password, access, flags FROM %s", table)
	} else {
		SQL_QueryAndIgnore(sql, "CREATE TABLE IF NOT EXISTS `%s` ( `auth` VARCHAR( 32 ) NOT NULL, `password` VARCHAR( 32 ) NOT NULL, `access` VARCHAR( 32 ) NOT NULL, `flags` VARCHAR( 32 ) NOT NULL ) COMMENT = 'AMX Mod X Admins'", table)
		query = SQL_PrepareQuery(sql,"SELECT `auth`,`password`,`access`,`flags` FROM `%s`", table)
	}

	if (!SQL_Execute(query))
	{
		SQL_QueryError(query, error, 127)
		server_print("[AMXX] %L", LANG_SERVER, "SQL_CANT_LOAD_ADMINS", error)
	} else if (!SQL_NumResults(query)) {
		server_print("[AMXX] %L", LANG_SERVER, "NO_ADMINS")
	} else {
		
		AdminCount = 0
		
		/** do this incase people change the query order and forget to modify below */
		new qcolAuth = SQL_FieldNameToNum(query, "auth")
		new qcolPass = SQL_FieldNameToNum(query, "password")
		new qcolAccess = SQL_FieldNameToNum(query, "access")
		new qcolFlags = SQL_FieldNameToNum(query, "flags")
		
		new AuthData[44];
		new Password[44];
		new Access[32];
		new Flags[32];
		
		while (SQL_MoreResults(query))
		{
			SQL_ReadResult(query, qcolAuth, AuthData, sizeof(AuthData)-1);
			SQL_ReadResult(query, qcolPass, Password, sizeof(Password)-1);
			SQL_ReadResult(query, qcolAccess, Access, sizeof(Access)-1);
			SQL_ReadResult(query, qcolFlags, Flags, sizeof(Flags)-1);
	
			admins_push(AuthData,Password,read_flags(Access),read_flags(Flags));
	
			++AdminCount;
			SQL_NextRow(query)
		}
	
		if (AdminCount == 1)
		{
			server_print("[AMXX] %L", LANG_SERVER, "SQL_LOADED_ADMIN")
		}
		else
		{
			server_print("[AMXX] %L", LANG_SERVER, "SQL_LOADED_ADMINS", AdminCount)
		}
		
		SQL_FreeHandle(query)
		SQL_FreeHandle(sql)
		SQL_FreeHandle(info)
	}
	
	return PLUGIN_HANDLED
}
#endif

public cmdReload(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	//strip original flags (patch submitted by mrhunt)
	remove_user_flags(0, read_flags("z"))
	
	admins_flush();

#if !defined USING_SQL
	new filename[128]
	
	get_configsdir(filename, 127)
	format(filename, 63, "%s/users.ini", filename)

	AdminCount = 0;
	loadSettings(filename);		// Re-Load admins accounts

	if (id != 0)
	{
		if (AdminCount == 1)
		{
			console_print(id, "[AMXX] %L", LANG_SERVER, "LOADED_ADMIN");
		}
		else
		{
			console_print(id, "[AMXX] %L", LANG_SERVER, "LOADED_ADMINS", AdminCount);
		}
	}
#else
	AdminCount = 0
	adminSql()

	if (id != 0)
	{
		if (AdminCount == 1)
			console_print(id, "[AMXX] %L", LANG_SERVER, "SQL_LOADED_ADMIN")
		else
			console_print(id, "[AMXX] %L", LANG_SERVER, "SQL_LOADED_ADMINS", AdminCount)
	}
#endif

	new players[32], num, pv
	new name[32]
	get_players(players, num)
	for (new i=0; i<num; i++)
	{
		pv = players[i]
		get_user_name(pv, name, 31)
		accessUser(pv, name)
	}

	return PLUGIN_HANDLED
}

getAccess(id, name[], authid[], ip[], password[])
{
	new index = -1
	new result = 0
	
	static Count;
	static Flags;
	static Access;
	static AuthData[44];
	static Password[32];
	
	g_CaseSensitiveName[id] = false;

	Count=admins_num();
	for (new i = 0; i < Count; ++i)
	{
		Flags=admins_lookup(i,AdminProp_Flags);
		admins_lookup(i,AdminProp_Auth,AuthData,sizeof(AuthData)-1);
		
		if (Flags & FLAG_AUTHID)
		{
			if (equal(authid, AuthData))
			{
				index = i
				break
			}
		}
		else if (Flags & FLAG_IP)
		{
			new c = strlen(AuthData)
			
			if (AuthData[c - 1] == '.')		/* check if this is not a xxx.xxx. format */
			{
				if (equal(AuthData, ip, c))
				{
					index = i
					break
				}
			}									/* in other case an IP must just match */
			else if (equal(ip, AuthData))
			{
				index = i
				break
			}
		} 
		else 
		{
			if (Flags & FLAG_CASE_SENSITIVE)
			{
				if (Flags & FLAG_TAG)
				{
					if (contain(name, AuthData) != -1)
					{
						index = i
						g_CaseSensitiveName[id] = true
						break
					}
				}
				else if (equal(name, AuthData))
				{
					index = i
					g_CaseSensitiveName[id] = true
					break
				}
			}
			else
			{
				if (Flags & FLAG_TAG)
				{
					if (containi(name, AuthData) != -1)
					{
						index = i
						break
					}
				}
				else if (equali(name, AuthData))
				{
					index = i
					break
				}
			}
		}
	}

	if (index != -1)
	{
		Access=admins_lookup(index,AdminProp_Access);

		if (Flags & FLAG_NOPASS)
		{
			result |= 8
			new sflags[32]
			
			get_flags(Access, sflags, 31)
			set_user_flags(id, Access)
			
			log_amx("Login: ^"%s<%d><%s><>^" became an admin (account ^"%s^") (access ^"%s^") (address ^"%s^")", name, get_user_userid(id), authid, AuthData, sflags, ip)
		}
		else 
		{
		
			admins_lookup(index,AdminProp_Password,Password,sizeof(Password)-1);

			if (equal(password, Password))
			{
				result |= 12
				set_user_flags(id, Access)
				
				new sflags[32]
				get_flags(Access, sflags, 31)
				
				log_amx("Login: ^"%s<%d><%s><>^" became an admin (account ^"%s^") (access ^"%s^") (address ^"%s^")", name, get_user_userid(id), authid, AuthData, sflags, ip)
			} 
			else 
			{
				result |= 1
				
				if (Flags & FLAG_KICK)
				{
					result |= 2
					log_amx("Login: ^"%s<%d><%s><>^" kicked due to invalid password (account ^"%s^") (address ^"%s^")", name, get_user_userid(id), authid, AuthData, ip)
				}
			}
		}
	}
	else if (get_pcvar_float(amx_mode) == 2.0)
	{
		result |= 2
	} 
	else 
	{
		new defaccess[32]
		
		get_pcvar_string(amx_default_access, defaccess, 31)
		
		if (!strlen(defaccess))
		{
			copy(defaccess, 32, "z")
		}
		
		new idefaccess = read_flags(defaccess)
		
		if (idefaccess)
		{
			result |= 8
			set_user_flags(id, idefaccess)
		}
	}
	
	return result
}

accessUser(id, name[] = "")
{
	remove_user_flags(id)
	
	new userip[32], userauthid[32], password[32], passfield[32], username[32]
	
	get_user_ip(id, userip, 31, 1)
	get_user_authid(id, userauthid, 31)
	
	if (name[0])
	{
		copy(username, 31, name)
	}
	else
	{
		get_user_name(id, username, 31)
	}
	
	get_pcvar_string(amx_password_field, passfield, 31)
	get_user_info(id, passfield, password, 31)
	
	new result = getAccess(id, username, userauthid, userip, password)
	
	if (result & 1)
	{
		client_cmd(id, "echo ^"* %L^"", id, "INV_PAS")
	}
	
	if (result & 2)
	{
		client_cmd(id, "%s", g_cmdLoopback)
		return PLUGIN_HANDLED
	}
	
	if (result & 4)
	{
		client_cmd(id, "echo ^"* %L^"", id, "PAS_ACC")
	}
	
	if (result & 8)
	{
		client_cmd(id, "echo ^"* %L^"", id, "PRIV_SET")
	}
	
	return PLUGIN_CONTINUE
}

public client_infochanged(id)
{
	if (!is_user_connected(id) || !get_pcvar_num(amx_mode))
	{
		return PLUGIN_CONTINUE
	}

	new newname[32], oldname[32]
	
	get_user_name(id, oldname, 31)
	get_user_info(id, "name", newname, 31)

	if (g_CaseSensitiveName[id])
	{
		if (!equal(newname, oldname))
		{
			accessUser(id, newname)
		}
	}
	else
	{
		if (!equali(newname, oldname))
		{
			accessUser(id, newname)
		}
	}
	return PLUGIN_CONTINUE
}

public ackSignal(id)
{
	server_cmd("kick #%d ^"%L^"", get_user_userid(id), id, "NO_ENTRY")
	return PLUGIN_HANDLED
}

public client_authorized(id)
	return get_pcvar_num(amx_mode) ? accessUser(id) : PLUGIN_CONTINUE

public client_putinserver(id)
{
	if (!is_dedicated_server() && id == 1)
		return get_pcvar_num(amx_mode) ? accessUser(id) : PLUGIN_CONTINUE
	
	return PLUGIN_CONTINUE
}
