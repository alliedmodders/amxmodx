/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#include <time.h>
#include "amxmodx.h"
#include "fakemeta.h"

plugin_info_t Plugin_info = {
  META_INTERFACE_VERSION, // ifvers
  "AMX Mod X",  // name
  AMX_VERSION,  // version
  __DATE__, // date
  "AMX Mod X Dev Team", // author
  "http://www.amxmodx.org",  // url
  "AMXX",  // logtag
  PT_ANYTIME,// (when) loadable
  PT_ANYTIME,// (when) unloadable
};

meta_globals_t *gpMetaGlobals;
gamedll_funcs_t *gpGamedllFuncs;
mutil_funcs_t *gpMetaUtilFuncs;
enginefuncs_t g_engfuncs;
globalvars_t  *gpGlobals;

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);

CLog g_log;
CForwardMngr  g_forwards;
CList<CPlayer*> g_auth;
CList<CCVar> g_cvars;
CList<ForceObject> g_forcemodels;
CList<ForceObject> g_forcesounds;
CList<ForceObject> g_forcegeneric;
CPlayer g_players[33];
CPlayer* mPlayer;
CPluginMngr g_plugins;
CTaskMngr g_tasksMngr;
CmdMngr g_commands;
EventsMngr g_events;
Grenades g_grenades;
LogEventsMngr g_logevents;
MenuMngr g_menucmds;
CLangMngr g_langMngr;
String g_log_dir;
String g_mod_name;
XVars g_xvars;
bool g_bmod_cstrike;
bool g_bmod_dod;
bool g_dontprecache;
bool g_forcedmodules;
bool g_forcedsounds;
bool g_initialized;
fakecmd_t g_fakecmd;
float g_game_restarting;
float g_game_timeleft;
float g_task_time;
float g_auth_time;

#ifdef MEMORY_TEST
float g_next_memreport_time;
unsigned int g_memreport_count;
String g_memreport_dir;
bool g_memreport_enabled;
#define MEMREPORT_INTERVAL 300.0f	/* 5 mins */
#endif // MEMORY_TEST

hudtextparms_t g_hudset;
//int g_edict_point;
int g_players_num;
int mPlayerIndex;
int mState;
int g_srvindex;

cvar_t init_amxmodx_version = {"amxmodx_version", "", FCVAR_SERVER | FCVAR_SPONLY};
cvar_t init_amxmodx_modules = {"amxmodx_modules", "", FCVAR_SPONLY};
cvar_t* amxmodx_version = NULL;
cvar_t* amxmodx_modules = NULL;
cvar_t* hostname = NULL;
cvar_t* mp_timelimit = NULL;

// main forwards
int FF_ClientCommand = -1;
int FF_ClientConnect = -1;
int FF_ClientDisconnect = -1;
int FF_ClientInfoChanged = -1;
int FF_ClientPutInServer = -1;
int FF_PluginInit = -1;
int FF_PluginCfg = -1;
int FF_PluginPrecache = -1;
int FF_PluginLog = -1;
int FF_PluginEnd = -1;
int FF_InconsistentFile = -1;
int FF_ClientAuthorized = -1;
int FF_ChangeLevel = -1;

// fake metamod api
CFakeMeta g_FakeMeta;

// Precache	stuff from force consistency calls
// or check	for	pointed	files won't	be done
int	C_PrecacheModel(char *s) {
  if ( !g_forcedmodules	){
	g_forcedmodules	= true;
	for(CList<ForceObject>::iterator a =  g_forcemodels.begin(); a ; ++a){
	  PRECACHE_MODEL((char*)(*a).getFilename());
	  ENGINE_FORCE_UNMODIFIED((*a).getForceType(),(*a).getMin(),(*a).getMax(),(*a).getFilename());
	}
  }
  RETURN_META_VALUE(MRES_IGNORED, 0);
}

int	C_PrecacheSound(char *s) {
  if ( !g_forcedsounds ) {
	g_forcedsounds = true;
	for(CList<ForceObject>::iterator a =  g_forcesounds.begin(); a ; ++a){
	  PRECACHE_SOUND((char*)(*a).getFilename());
	  ENGINE_FORCE_UNMODIFIED((*a).getForceType(),(*a).getMin(),(*a).getMax(),(*a).getFilename());
	}
	if (!g_bmod_cstrike){
	  PRECACHE_SOUND("weapons/cbar_hitbod1.wav");
	  PRECACHE_SOUND("weapons/cbar_hitbod2.wav");
	  PRECACHE_SOUND("weapons/cbar_hitbod3.wav");
	}
  }
  RETURN_META_VALUE(MRES_IGNORED, 0);
}

// On InconsistentFile call	forward	function from plugins
int	C_InconsistentFile( const	edict_t	*player, const char	*filename, char	*disconnect_message	)
{
	if (FF_InconsistentFile < 0)
		RETURN_META_VALUE(MRES_IGNORED,	FALSE);

	if ( MDLL_InconsistentFile(player,filename,disconnect_message) )
	{
		CPlayer	*pPlayer = GET_PLAYER_POINTER((edict_t *)player);

#ifdef ENABLEEXEPTIONS
		try
		{
#endif
			if (executeForwards(FF_InconsistentFile, pPlayer->index, filename, disconnect_message) == 1)
				RETURN_META_VALUE(MRES_SUPERCEDE, FALSE);
#ifdef ENABLEEXEPTIONS
		}
		catch(	...	)
		{
			AMXXLOG_Log( "[AMXX] Fatal error at inconsistent file forward execution");
		}
#endif
		RETURN_META_VALUE(MRES_SUPERCEDE, TRUE );
	}

	RETURN_META_VALUE(MRES_IGNORED, FALSE);
}

const char*	get_localinfo( const char* name	, const	char* def )
{
  const	char* b	= LOCALINFO( (char*)name );
  if ( b ==	0 || *b	== 0 )
	SET_LOCALINFO((char*)name,(char*)(b	= def) );
  return b;
}

// Very	first point	at map load
// Load	AMX	modules	for	new	native functions
// Initialize AMX stuff	and	load it's plugins from plugins.ini list
// Call	precache forward function from plugins
int	C_Spawn( edict_t *pent ) {

  if ( g_initialized ) RETURN_META_VALUE(MRES_IGNORED, 0);

  g_initialized	= true;
  g_forcedmodules =	false;
  g_forcedsounds = false;

  g_srvindex = IS_DEDICATED_SERVER() ? 0 : 1;

  hostname = CVAR_GET_POINTER("hostname");
  mp_timelimit = CVAR_GET_POINTER("mp_timelimit");


  g_log.MapChange();

  // ######	Initialize task	manager
  g_tasksMngr.registerTimers( &gpGlobals->time,	&mp_timelimit->value,  &g_game_timeleft		);

  //  ###### Load lang
  g_langMngr.LoadCache(build_pathname("%s/dictionary.cache", get_localinfo("amxx_datadir", "addons/amxx/data")));
//  g_langMngr.Load(build_pathname("%s/languages.dat", get_localinfo("amxx_datadir", "addons/amxx/data")));
  // ######	Initialize commands	prefixes
  g_commands.registerPrefix( "amx" );
  g_commands.registerPrefix( "amxx"	);
  g_commands.registerPrefix( "say" );
  g_commands.registerPrefix( "admin_" );
  g_commands.registerPrefix( "sm_" );
  g_commands.registerPrefix( "cm_" );

  // make sure localinfos are set
  get_localinfo("amxx_basedir",	"addons/amxx");
  get_localinfo("amxx_pluginsdir", "addons/amxx/plugins");
  get_localinfo("amxx_modulesdir", "addons/amxx/modules");
  get_localinfo("amxx_configsdir", "addons/amxx/configs");
  get_localinfo("amxx_customdir", "addons/amxx/custom");

  //  ###### Load modules
  loadModules(get_localinfo("amxx_modules",	"addons/amxx/configs/modules.ini"));
  attachModules();
  int loaded = countModules(CountModules_Running);	// Call	after attachModules	so all modules don't have pending stat
  // Set some info about amx version and modules
  CVAR_SET_STRING(init_amxmodx_version.name, AMX_VERSION
#ifdef JIT
	  "J"
#endif
	  );
  char buffer[32];
  sprintf(buffer, "%d", loaded);
  CVAR_SET_STRING(init_amxmodx_modules.name, buffer);

  //  ######  Load Vault
  g_vault.setSource( build_pathname("%s", get_localinfo("amxx_vault", "addons/amxx/configs/vault.ini"))	);
  g_vault.loadVault( );
  if (strlen(g_vault.get("server_language")) < 1)
  {
     g_vault.put("server_language", "en");
	 g_vault.saveVault();
  }

  //  ###### Init time and freeze tasks
  g_game_timeleft =	g_bmod_dod ? 1 : 0;
  g_task_time =	gpGlobals->time	+ 99999.0;
  g_auth_time =	gpGlobals->time	+ 99999.0;
#ifdef MEMORY_TEST
  g_next_memreport_time = gpGlobals->time + 99999.0;
#endif

  g_players_num	= 0;

  // Set server	flags
  memset(g_players[0].flags,-1,sizeof(g_players[0].flags));

  //  ###### Load AMX scripts
  g_plugins.loadPluginsFromFile( get_localinfo("amxx_plugins", "addons/amxx/configs/plugins.ini") );

  // Register forwards
  FF_PluginInit = registerForward("plugin_init", ET_IGNORE, FP_DONE);
  FF_ClientCommand = registerForward("client_command", ET_IGNORE, FP_CELL, FP_DONE);
  FF_ClientConnect = registerForward("client_connect", ET_IGNORE, FP_CELL, FP_DONE);
  FF_ClientDisconnect = registerForward("client_disconnect", ET_IGNORE, FP_CELL, FP_DONE);
  FF_ClientInfoChanged = registerForward("client_infochanged", ET_IGNORE, FP_CELL, FP_DONE);
  FF_ClientPutInServer = registerForward("client_putinserver", ET_IGNORE, FP_CELL, FP_DONE);
  FF_PluginCfg = registerForward("plugin_cfg", ET_IGNORE, FP_DONE);
  FF_PluginPrecache = registerForward("plugin_precache", ET_IGNORE, FP_DONE);
  FF_PluginLog = registerForward("plugin_log", ET_IGNORE, FP_DONE);
  FF_PluginEnd = registerForward("plugin_end", ET_IGNORE, FP_DONE);
  FF_InconsistentFile = registerForward("inconsistent_file", ET_STOP, FP_CELL, FP_STRING, FP_STRINGEX, FP_DONE);
  FF_ClientAuthorized = registerForward("client_authorized", ET_IGNORE, FP_CELL, FP_DONE);
  FF_ChangeLevel = registerForward("server_changelevel", ET_STOP, FP_STRING, FP_DONE);

  modules_callPluginsLoaded();

  //  ###### Call precache forward function
  g_dontprecache = false;
  executeForwards(FF_PluginPrecache);
  g_dontprecache = true;

  for(CList<ForceObject>::iterator a =	g_forcegeneric.begin();	a ;	++a){
	  PRECACHE_GENERIC((char*)(*a).getFilename());
	  ENGINE_FORCE_UNMODIFIED((*a).getForceType(),
	  (*a).getMin(),(*a).getMax(),(*a).getFilename());
  }


  RETURN_META_VALUE(MRES_IGNORED, 0);
}

struct sUserMsg	{
  const	char* name;
  int* id;
  funEventCall func;
  bool endmsg;
  bool cstrike;
} g_user_msg[] = {
  {	"CurWeapon"	, &gmsgCurWeapon , Client_CurWeapon, false,false },
  {	"Damage" , &gmsgDamage,Client_DamageEnd, true ,	true },
  {	"DeathMsg" , &gmsgDeathMsg,	Client_DeathMsg, false,true	},
  {	"TextMsg" ,	&gmsgTextMsg,Client_TextMsg	, false,false},
  {	"TeamInfo" , &gmsgTeamInfo,Client_TeamInfo , false,false},
  {	"WeaponList" , &gmsgWeaponList,	Client_WeaponList, false, false},
  {	"MOTD" , &gmsgMOTD,	0 ,	false,false},
  {	"ServerName" , &gmsgServerName,	0 ,	false, false},
  {	"Health" , &gmsgHealth,	0 ,	false,false	},
  {	"Battery" ,	&gmsgBattery, 0	, false,false},
  {	"ShowMenu" , &gmsgShowMenu,Client_ShowMenu , false,false},
  {	"SendAudio"	, &gmsgSendAudio, 0, false,false},
  {	"AmmoX"	, &gmsgAmmoX, Client_AmmoX , false,false },
  {	"ScoreInfo"	, &gmsgScoreInfo, Client_ScoreInfo,	false, false},
  {	"VGUIMenu" , &gmsgVGUIMenu,Client_VGUIMenu,	false,false	},
  {	"AmmoPickup" , &gmsgAmmoPickup,	Client_AmmoPickup ,	false,false	},
  {	"WeapPickup" , &gmsgWeapPickup,0, false,false },
  {	"ResetHUD" , &gmsgResetHUD,0, false,false },
  {	"RoundTime"	, &gmsgRoundTime,0,	false, false},
  {	0 ,	0,0,false,false	}
};


int	C_RegUserMsg_Post(const char *pszName, int iSize)
{
  for (int i = 0; g_user_msg[ i	].name;	++i	)
  {
	if ( strcmp( g_user_msg[ i ].name ,	pszName	 ) == 0	)
	{
	  int id = META_RESULT_ORIG_RET( int );

	  *g_user_msg[ i ].id =	id;

	  if ( !g_user_msg[	i ].cstrike	|| g_bmod_cstrike  )
	  {
		if ( g_user_msg[ i ].endmsg	)
		  modMsgsEnd[ id  ]	= g_user_msg[ i	].func;
		else
		  modMsgs[ id  ] = g_user_msg[ i ].func;
	  }

	  break;
	}
  }

  RETURN_META_VALUE(MRES_IGNORED, 0);
}

/*
Much more later	after precache.	All	is precached, server
will be	flaged as ready	to use so call
plugin_init	forward	function from plugins
*/
void C_ServerActivate( edict_t *pEdictList, int edictCount, int clientMax	){

  int id;
  for (int i = 0; g_user_msg[ i	].name;	++i	)
  {
	if ( (*g_user_msg[ i ].id == 0)	&&
	  (id =	GET_USER_MSG_ID(PLID, g_user_msg[ i	].name , NULL ))!=0)
	{
	  *g_user_msg[ i ].id =	id;

	  if ( !g_user_msg[	i ].cstrike	|| g_bmod_cstrike  )
	  {
		if ( g_user_msg[ i ].endmsg	)
		  modMsgsEnd[ id  ]	= g_user_msg[ i	].func;
		else
		  modMsgs[ id  ] = g_user_msg[ i ].func;
	  }
	}
  }

  RETURN_META(MRES_IGNORED);
}

void C_ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){

//	g_edict_point =	(int)pEdictList;

  for(int i	= 1; i <= gpGlobals->maxClients; ++i) {
	CPlayer	*pPlayer = GET_PLAYER_POINTER_I(i);
	pPlayer->Init( pEdictList +	i ,	i );
  }

  executeForwards(FF_PluginInit);
  executeForwards(FF_PluginCfg);

  //  ###### Save lang
  g_langMngr.SaveCache(build_pathname("%s/dictionary.cache", get_localinfo("amxx_datadir", "addons/amxx/data")));
  g_langMngr.Save(build_pathname("%s/languages.dat", get_localinfo("amxx_datadir", "addons/amxx/data")));

// Correct time in Counter-Strike	and	other mods (except DOD)
  if ( !g_bmod_dod)	 g_game_timeleft = 0;

  g_task_time =	gpGlobals->time;
  g_auth_time =	gpGlobals->time;

#ifdef MEMORY_TEST
  g_next_memreport_time = gpGlobals->time + MEMREPORT_INTERVAL;
  g_memreport_count = 0;
  g_memreport_enabled = true;
#endif

  RETURN_META(MRES_IGNORED);
}

// Call	plugin_end forward function	from plugins.
void C_ServerDeactivate()	{

  for(int i	= 1; i <= gpGlobals->maxClients; ++i){
	CPlayer	*pPlayer = GET_PLAYER_POINTER_I(i);
	if (pPlayer->ingame){

	  executeForwards(FF_ClientDisconnect, pPlayer->index);

	  pPlayer->Disconnect();
	  --g_players_num;
	}
  }

  g_players_num	= 0;
  executeForwards(FF_PluginEnd);

  RETURN_META(MRES_IGNORED);
}

// After all clear whole AMX configuration
// However leave AMX modules which are loaded only once
void C_ServerDeactivate_Post() {

  g_initialized	= false;

  detachReloadModules();

  g_auth.clear();
  g_forwards.clear();
  g_commands.clear();
  g_forcemodels.clear();
  g_forcesounds.clear();
  g_forcegeneric.clear();
  g_grenades.clear();
  g_tasksMngr.clear();
  g_logevents.clearLogEvents();
  g_events.clearEvents();
  g_menucmds.clear();
  g_vault.clear();
  g_xvars.clear();
  g_plugins.clear();
  g_langMngr.SaveCache(build_pathname("%s/dictionary.cache", get_localinfo("amxx_datadir", "addons/amxx/data")));
  g_langMngr.Save(build_pathname("%s/languages.dat", get_localinfo("amxx_datadir", "addons/amxx/data")));
  g_langMngr.Clear();
  // last memreport
#ifdef MEMORY_TEST
	if (g_memreport_enabled)
	{
		if (g_memreport_count == 0)
		{
			// make new directory
			time_t td;
			time(&td);
			tm *curTime = localtime(&td);
			int i = 0;
#ifdef __linux__
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxx")), 0700);
#else
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxx")));
#endif
			while (true)
			{
				char buffer[256];
				sprintf(buffer, "%s/memreports/D%02d%02d%03d", get_localinfo("amxx_basedir", "addons/amxx"), curTime->tm_mon + 1, curTime->tm_mday, i);
#ifdef __linux__
				mkdir(build_pathname("%s", g_log_dir.c_str()), 0700);
				if (mkdir(build_pathname(buffer), 0700) < 0)
#else
				mkdir(build_pathname("%s", g_log_dir.c_str()));
				if (mkdir(build_pathname(buffer)) < 0)
#endif
				{
					if (errno == EEXIST)
					{
						// good
						++i;
						continue;
					}
					else
					{
						// bad
						g_memreport_enabled = false;
						AMXXLOG_Log("[AMXX] Fatal error: Can't create directory for memreport files (%s)", buffer);
						break;
					}
				}
				g_memreport_dir.assign(buffer);
				// g_memreport_dir should be valid now
				break;
			}
		}
		m_dumpMemoryReport(build_pathname("%s/r%03d", g_memreport_dir.c_str(), g_memreport_count));
		AMXXLOG_Log("Memreport #%d created (file \"%s/r%03d\") (interval %f)", g_memreport_count + 1, g_memreport_dir.c_str(), g_memreport_count, MEMREPORT_INTERVAL);
		g_memreport_count++;
	}
#endif // MEMORY_TEST

  RETURN_META(MRES_IGNORED);
}

BOOL C_ClientConnect_Post( edict_t *pEntity, const char *pszName,	const char *pszAddress,	char szRejectReason[ 128 ]	){
  CPlayer* pPlayer = GET_PLAYER_POINTER(pEntity);
  if (!pPlayer->bot) {

	bool a = pPlayer->Connect(pszName,pszAddress);

	executeForwards(FF_ClientConnect, pPlayer->index);

	if ( a )
	{
	  CPlayer**	aa = new CPlayer*(pPlayer);
	  if ( aa )	g_auth.put(	aa );
	}
	else
	{
	  pPlayer->Authorize();
	  executeForwards(FF_ClientAuthorized, pPlayer->index);
	}
  }
  RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void C_ClientDisconnect( edict_t *pEntity	) {
  CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
  if (pPlayer->ingame)	{
	executeForwards(FF_ClientDisconnect, pPlayer->index);
	--g_players_num;
  }
  pPlayer->Disconnect();
  RETURN_META(MRES_IGNORED);
}

void C_ClientPutInServer_Post( edict_t *pEntity )	{
  CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
  if (!pPlayer->bot) {
	pPlayer->PutInServer();
	++g_players_num;

	executeForwards(FF_ClientPutInServer, pPlayer->index);

  }
  RETURN_META(MRES_IGNORED);
}

void C_ClientUserInfoChanged_Post( edict_t *pEntity, char	*infobuffer	) {
  CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

  executeForwards(FF_ClientInfoChanged, pPlayer->index);

  const	char* name = INFOKEY_VALUE(infobuffer,"name");

  // Emulate bot connection	and	putinserver
  if ( pPlayer->ingame )
  {
	pPlayer->name.assign(name); //	Make sure player have name up to date
  }
  else if (	pPlayer->IsBot() )
  {
	pPlayer->Connect( name ,"127.0.0.1"/*CVAR_GET_STRING("net_address")*/);

	executeForwards(FF_ClientConnect, pPlayer->index);

	pPlayer->Authorize();
	executeForwards(FF_ClientAuthorized, pPlayer->index);


	pPlayer->PutInServer();
	++g_players_num;

	executeForwards(FF_ClientPutInServer, pPlayer->index);
  }

  RETURN_META(MRES_IGNORED);
}

void C_ClientCommand(	edict_t	*pEntity ) {
  CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
  META_RES result =	MRES_IGNORED;
  cell ret = 0;
  int err;

#ifdef ENABLEEXEPTIONS
  try
  {
#endif
	  if (executeForwards(FF_ClientCommand, pPlayer->index) > 0)
		  RETURN_META(MRES_SUPERCEDE);

#ifdef ENABLEEXEPTIONS
  }
  catch( ... )
  {
	AMXXLOG_Log( "[AMXX] Fatal error at commmand forward execution");
  }
#endif


  /* check for command and if needed also for first	argument and call proper function */
  const	char* cmd =	CMD_ARGV(0);
  const	char* arg =	CMD_ARGV(1);

#ifdef ENABLEEXEPTIONS
  try{
#endif

	CmdMngr::iterator aa = g_commands.clcmdprefixbegin(	cmd	);
	if ( !aa ) aa =	g_commands.clcmdbegin();

	while (	aa )
	{
	  if ( (*aa).matchCommandLine( cmd , arg  )	&&
		(*aa).getPlugin()->isExecutable(  (*aa).getFunction() )	)
	  {

		if ((err =amx_Exec((*aa).getPlugin()->getAMX(),	&ret , (*aa).getFunction()	 , 3, pPlayer->index, (*aa).getFlags(),(*aa).getId()  )) !=	AMX_ERR_NONE)
		  AMXXLOG_Log("[AMXX] Run time error %d on line	%ld (plugin \"%s\")",
		  err,(*aa).getPlugin()->getAMX()->curline,(*aa).getPlugin()->getName());

		if ( ret & 2 )	result = MRES_SUPERCEDE;
		if ( ret & 1 )	RETURN_META(MRES_SUPERCEDE);
	  }

	  ++aa;
	}

#ifdef ENABLEEXEPTIONS
  }catch( ... )
  {
	AMXXLOG_Log( "[AMXX] fatal error at client commmand execution");
  }
#endif
  /* check menu	commands */

  if (!strcmp(cmd,"menuselect"))
  {
	int	pressed_key	= atoi(	arg	) -	1;
	int	bit_key	= (1<<pressed_key);

	if (pPlayer->keys &	bit_key)
	{

	  int menuid = pPlayer->menu;
	  pPlayer->menu	= 0;

#ifdef ENABLEEXEPTIONS
	  try{
#endif
		MenuMngr::iterator a = g_menucmds.begin();

		while( a )
		{
		  if ( (*a).matchCommand(  menuid ,	bit_key	 ) && (*a).getPlugin()->isExecutable( (*a).getFunction() ) )
		  {

			if ( ( err = amx_Exec((*a).getPlugin()->getAMX(), &ret ,(*a).getFunction() , 2,	pPlayer->index,pressed_key)) !=	AMX_ERR_NONE)
			  AMXXLOG_Log("[AMXX] Run time error %d on line %ld (plugin \"%s\")",
			  err,(*a).getPlugin()->getAMX()->curline,(*a).getPlugin()->getName());

			if ( ret & 2 ) result =	MRES_SUPERCEDE;
			if ( ret & 1 ) RETURN_META(MRES_SUPERCEDE);
		  }

		  ++a;
		}

#ifdef ENABLEEXEPTIONS
	  }
	  catch( ... )
	  {
		AMXXLOG_Log( "[AMXX] Fatal error at menu commmand execution");
	  }
#endif
	}
  }
  /* check for PLUGIN_HANDLED_MAIN and block hl	call if	needed */
  RETURN_META( result );
}

void C_StartFrame_Post( void ) {

  if (g_auth_time <	gpGlobals->time	)
  {
  g_auth_time =	gpGlobals->time	+ 0.7;

  CList<CPlayer*>::iterator	a =	g_auth.begin();

  while	( a	)
  {
	const char*	auth = GETPLAYERAUTHID(	(*a)->pEdict );

	if ( (auth == 0) ||	(*auth == 0) ) {
	  a.remove();
	  continue;
	}

	if ( strcmp( auth, "STEAM_ID_PENDING" )	)
	{
	  (*a)->Authorize();
	  executeForwards(FF_ClientAuthorized, (*a)->index);
	  a.remove();
	  continue;
	}

	++a;
  }

  }


#ifdef MEMORY_TEST
	if (g_memreport_enabled && g_next_memreport_time <= gpGlobals->time)
	{
		g_next_memreport_time = gpGlobals->time + MEMREPORT_INTERVAL;
		if (g_memreport_count == 0)
		{
			// make new directory
			time_t td;
			time(&td);
			tm *curTime = localtime(&td);
			int i = 0;
#ifdef __linux__
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxx")), 0700);
#else
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxx")));
#endif
			while (true)
			{
				char buffer[256];
				sprintf(buffer, "%s/memreports/D%02d%02d%03d", get_localinfo("amxx_basedir", "addons/amxx"), curTime->tm_mon + 1, curTime->tm_mday, i);
#ifdef __linux__
				mkdir(build_pathname("%s", g_log_dir.c_str()), 0700);
				if (mkdir(build_pathname(buffer), 0700) < 0)
#else
				mkdir(build_pathname("%s", g_log_dir.c_str()));
				if (mkdir(build_pathname(buffer)) < 0)
#endif
				{
					if (errno == EEXIST)
					{
						// good
						++i;
						continue;
					}
					else
					{
						// bad
						g_memreport_enabled = false;
						AMXXLOG_Log("[AMXX] Fatal error: Can't create directory for memreport files (%s)", buffer);
						break;
					}
				}
				g_memreport_dir.assign(buffer);
				// g_memreport_dir should be valid now
				break;
			}
		}
		m_dumpMemoryReport(build_pathname("%s/r%03d", g_memreport_dir.c_str(), g_memreport_count));
		AMXXLOG_Log("Memreport #%d created (file \"%s/r%03d\") (interval %f)", g_memreport_count + 1, g_memreport_dir.c_str(), g_memreport_count, MEMREPORT_INTERVAL);
		g_memreport_count++;
	}
#endif // MEMORY_TEST

  if (g_task_time >	gpGlobals->time)
	RETURN_META(MRES_IGNORED);

  g_task_time =	gpGlobals->time	+ 0.1;

  g_tasksMngr.startFrame();

  RETURN_META(MRES_IGNORED);
}

void C_MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
  if (ed)
  {

	if (gmsgBattery==msg_type&&g_bmod_cstrike)
	{
	  void*	ptr	= GET_PRIVATE(ed);
#ifdef __linux__
	  int *z = (int*)ptr + 0x171;
#else
	  int *z = (int*)ptr + 0x16C;
#endif
	  int stop = (int)ed->v.armorvalue;
	  *z = stop;
	  ed->v.armorvalue = stop;
	}

	mPlayerIndex = ENTINDEX(ed);
	mPlayer	= GET_PLAYER_POINTER_I(mPlayerIndex);
  }
  else
  {
	mPlayerIndex = 0;
	mPlayer	= 0;
  }
  if ( msg_type	< 0	|| msg_type	>= MAX_REG_MSGS	)
	msg_type = 0;

  mState = 0;
  function=modMsgs[msg_type];
  endfunction=modMsgsEnd[msg_type];
  g_events.parserInit(msg_type,	&gpGlobals->time, mPlayer ,mPlayerIndex);
  RETURN_META(MRES_IGNORED);
}
void C_WriteByte_Post(int	iValue)	{
  g_events.parseValue(iValue);
  if (function)	(*function)((void *)&iValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteChar_Post(int	iValue)	{
  g_events.parseValue(iValue);
  if (function)	(*function)((void *)&iValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteShort_Post(int iValue) {
  g_events.parseValue(iValue);
  if (function)	(*function)((void *)&iValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteLong_Post(int	iValue)	{
  g_events.parseValue(iValue);
  if (function)	(*function)((void *)&iValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteAngle_Post(float flValue)	{
  g_events.parseValue(flValue);
  if (function)	(*function)((void *)&flValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteCoord_Post(float flValue)	{
  g_events.parseValue(flValue);
  if (function)	(*function)((void *)&flValue);
  RETURN_META(MRES_IGNORED);
}
void C_WriteString_Post(const	char *sz) {
  g_events.parseValue(sz);
  if (function)	(*function)((void *)sz);
  RETURN_META(MRES_IGNORED);
}
void C_WriteEntity_Post(int iValue) {
  g_events.parseValue(iValue);
  if (function)	(*function)((void *)&iValue);
  RETURN_META(MRES_IGNORED);
}
void C_MessageEnd_Post(void) {
  g_events.executeEvents();

#if	0 // ######### this	is done	by call	above
  EventsMngr::iterator a = g_events.begin();
  int err;
#ifdef ENABLEEXEPTIONS
  try
  {
#endif

	while (	a )
	{

	  if ((err = amx_Exec((*a).getPlugin()->getAMX(), NULL ,  (*a).getFunction() , 1, mPlayerIndex	/*g_events.getArgInteger(0)*/ )) !=	AMX_ERR_NONE)
		AMXXLOG_Log("[AMXX] Run time error %d on line %ld (plugin \"%s\")",err,(*a).getPlugin()->getAMX()->curline,(*a).getPlugin()->getName());


	  ++a;

	}

#ifdef ENABLEEXEPTIONS
  }
  catch( ... )
  {
	AMXXLOG_Log( "[AMXX] Fatal error at event execution");
  }
#endif
#endif

  if (endfunction) (*endfunction)(NULL);
  RETURN_META(MRES_IGNORED);
}

void C_ChangeLevel(char* s1, char* s2)
{
	if (FF_ChangeLevel) {
		int retVal = 0;
		char *map = s1;
		retVal = executeForwards(FF_ChangeLevel, map);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

const char *C_Cmd_Args( void )
{
	// if the global "fake" flag is set, which means that engclient_cmd was used, supercede the function
	if (g_fakecmd.fake)
		RETURN_META_VALUE(MRES_SUPERCEDE, (g_fakecmd.argc > 1) ? g_fakecmd.args : g_fakecmd.argv[0]);
	// otherwise ignore it
	RETURN_META_VALUE(MRES_IGNORED, NULL);
}

const char *C_Cmd_Argv(int argc)
{
	// if the global "fake" flag is set, which means that engclient_cmd was used, supercede the function
	if (g_fakecmd.fake)
		RETURN_META_VALUE(MRES_SUPERCEDE,	(argc < 3) ? g_fakecmd.argv[argc] : "");
	// otherwise ignore it
	RETURN_META_VALUE(MRES_IGNORED, NULL);
}

int	C_Cmd_Argc( void )
{
	// if the global "fake" flag is set, which means that engclient_cmd was used, supercede the function
	if (g_fakecmd.fake)
		RETURN_META_VALUE(MRES_SUPERCEDE, g_fakecmd.argc);
	// otherwise ignore it
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

// Grenade has been	thrown.
// Only	here we	may	find out who is	an owner.
void C_SetModel(edict_t *e, const	char *m){
  if(e->v.owner&&m[7]=='w'&&m[8]=='_'&&m[9]=='h')
	g_grenades.put(	e ,	1.75, 4, GET_PLAYER_POINTER(e->v.owner)	);
  RETURN_META(MRES_IGNORED);
}

// Save	at what	part of	body a player is aiming
void C_TraceLine_Post(const float	*v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr) {
  if ( e &&	( e->v.flags & (FL_CLIENT |	FL_FAKECLIENT) ) ) {
	CPlayer* pPlayer = GET_PLAYER_POINTER(e);
	if (ptr->pHit&&(ptr->pHit->v.flags&	(FL_CLIENT | FL_FAKECLIENT)	))
	  pPlayer->aiming =	ptr->iHitgroup;
	pPlayer->lastTrace = pPlayer->thisTrace;
	pPlayer->thisTrace = ptr->vecEndPos;
  }
  RETURN_META(MRES_IGNORED);
}

void C_AlertMessage_Post(ALERT_TYPE atype, char *szFmt, ...)
{
	if (atype != at_logged)
		RETURN_META(MRES_IGNORED);

	/*  There	are	also more messages but we want only	logs
	at_notice,
	at_console,		// same	as at_notice, but forces a ConPrintf, not a	message	box
	at_aiconsole,	// same	as at_console, but only	shown if developer level is	2!
	at_warning,
	at_error,
	at_logged		// Server print to console ( only in multiplayer games ).
	*/

	// execute logevents and plugin_log forward
	if (g_logevents.logEventsExist() || FF_PluginLog >= 0)
	{
		va_list	logArgPtr;
		va_start ( logArgPtr , szFmt );
		g_logevents.setLogString( szFmt	, logArgPtr	);
		va_end ( logArgPtr );
		g_logevents.parseLogString(	);
		if (g_logevents.logEventsExist())
			g_logevents.executeLogEvents( );
		executeForwards(FF_PluginLog);
	}

	RETURN_META(MRES_IGNORED);
}

C_DLLEXPORT	int	Meta_Query(char	*ifvers, plugin_info_t **pPlugInfo,	mutil_funcs_t *pMetaUtilFuncs) {
  gpMetaUtilFuncs=pMetaUtilFuncs;
  *pPlugInfo=&Plugin_info;
  if(strcmp(ifvers,	Plugin_info.ifvers)) {
	int	mmajor=0, mminor=0,	pmajor=0, pminor=0;
	LOG_MESSAGE(PLID, "WARNING:	meta-interface version mismatch; requested=%s ours=%s",	Plugin_info.logtag,	ifvers);
	sscanf(ifvers, "%d:%d",	&mmajor, &mminor);
	sscanf(META_INTERFACE_VERSION, "%d:%d",	&pmajor, &pminor);
	if(pmajor >	mmajor || (pmajor==mmajor && pminor	> mminor)) {
	  LOG_ERROR(PLID, "metamod version is too old for this plugin; update metamod");
	  return(FALSE);
	}
	else if(pmajor < mmajor) {
	  LOG_ERROR(PLID, "metamod version is incompatible with	this plugin; please	find a newer version of	this plugin");
	  return(FALSE);
	}
	else if(pmajor==mmajor && pminor < mminor)
	  LOG_MESSAGE(PLID,	"WARNING: metamod version is newer than	expected; consider finding a newer version of this plugin");
	else
	  LOG_ERROR(PLID, "unexpected version comparison; metavers=%s, mmajor=%d, mminor=%d; plugvers=%s, pmajor=%d, pminor=%d", ifvers, mmajor, mminor, META_INTERFACE_VERSION, pmajor, pminor);
  }
  // :NOTE: Don't call modules query here (g_FakeMeta.Meta_Query), because we don't know modules yet. Do it in Meta_Attach
  return(TRUE);
}

static META_FUNCTIONS gMetaFunctionTable;
C_DLLEXPORT	int	Meta_Attach(PLUG_LOADTIME now, META_FUNCTIONS *pFunctionTable, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs) {
  if(now > Plugin_info.loadable) {
	LOG_ERROR(PLID,	"Can't load	plugin right now");
	return(FALSE);
  }
  gpMetaGlobals=pMGlobals;
  gMetaFunctionTable.pfnGetEntityAPI2 =	GetEntityAPI2;
  gMetaFunctionTable.pfnGetEntityAPI2_Post = GetEntityAPI2_Post;
  gMetaFunctionTable.pfnGetEngineFunctions = GetEngineFunctions;
  gMetaFunctionTable.pfnGetEngineFunctions_Post	= GetEngineFunctions_Post;
  //gMetaFunctionTable.pfnGetNewDLLFunctions = GetNewDLLFunctions;
  //gMetaFunctionTable.pfnGetNewDLLFunctions_Post = GetNewDLLFunctions_Post;

  memcpy(pFunctionTable, &gMetaFunctionTable, sizeof(META_FUNCTIONS));
  gpGamedllFuncs=pGamedllFuncs;
  CVAR_REGISTER(&init_amxmodx_version);
  CVAR_REGISTER(&init_amxmodx_modules);
  amxmodx_version =	CVAR_GET_POINTER(init_amxmodx_version.name);
  REG_SVR_COMMAND("amxx",amx_command);

  char gameDir[512];
  GET_GAME_DIR(gameDir);
  char *a =	gameDir;
  int i	= 0;
  while	( gameDir[i] )
	if (gameDir[i++] ==	'/')
	  a	= &gameDir[i];
  g_mod_name.assign(a);

  // ###### Print short GPL
  print_srvconsole(	"\n  AMX Mod X version %s Copyright (c) 2004 AMX Mod X Development Team \n"
					"  AMX Mod X comes with ABSOLUTELY NO WARRANTY; for details type `amxx gpl'.\n", AMX_VERSION);
  print_srvconsole(	"  This is free software and you are welcome to redistribute it under \n"
					"  certain conditions; type 'amxx gpl' for details.\n  \n");

  // ######	Load custom	path configuration
  Vault	amx_config;
  amx_config.setSource(build_pathname("%s",	get_localinfo("amxx_cfg", "addons/amxx/configs/core.ini")));

  if ( amx_config.loadVault() ){
	Vault::iterator	a =	amx_config.begin();
	while (	a != amx_config.end() )	{
	  SET_LOCALINFO( (char*)a.key().c_str(), (char*)a.value().c_str() );
	  ++a;
	}
	amx_config.clear();
  }

  // ######	Initialize logging here
  g_log_dir.assign(get_localinfo("amxx_logs", "addons/amxx/logs"));

  //  ###### Now attach	metamod	modules
  // This will also call modules Meta_Query and Meta_Attach functions
  attachMetaModModules(now,	get_localinfo("amxx_modules", "addons/amxx/configs/modules.ini") );

  return(TRUE);
}

C_DLLEXPORT	int	Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON	reason)	{
  if(now > Plugin_info.unloadable && reason	!= PNL_CMD_FORCED) {
	LOG_ERROR(PLID,	"Can't unload plugin right now");
	return(FALSE);
  }
  g_auth.clear();
  g_forwards.clear();
  g_commands.clear();
  g_forcemodels.clear();
  g_forcesounds.clear();
  g_forcegeneric.clear();
  g_grenades.clear();
  g_tasksMngr.clear();
  g_logevents.clearLogEvents();
  g_events.clearEvents();
  g_menucmds.clear();
  g_vault.clear();
  g_xvars.clear();
  g_plugins.clear();
  g_cvars.clear();

  detachModules();

  //  ###### Now detach metamod modules
  g_FakeMeta.Meta_Detach(now, reason);
  g_FakeMeta.ReleasePlugins();

  g_log.CloseFile();

  return(TRUE);
}

#ifdef __linux__
// linux prototype
C_DLLEXPORT void GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals ) {

#else
#ifdef _MSC_VER
// MSVC: Simulate __stdcall calling convention
C_DLLEXPORT __declspec(naked) void GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals )
{
	__asm			// Prolog
	{
		// Save ebp
		push		ebp
		// Set stack frame pointer
		mov			ebp, esp
		// Allocate space for local variables
		// The MSVC compiler gives us the needed size in __LOCAL_SIZE.
		sub			esp, __LOCAL_SIZE
		// Push registers
		push		ebx
		push		esi
		push		edi
	}
#else	// _MSC_VER
#ifdef __GNUC__
// GCC can also work with this
C_DLLEXPORT void __stdcall GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals )
{
#else	// __GNUC__
// compiler not known
#error There is no support (yet) for your compiler. Please use MSVC or GCC compilers or contact the AMX Mod X dev team.
#endif	// __GNUC__
#endif // _MSC_VER
#endif // __linux__

	// ** Function core <--
	memcpy(&g_engfuncs, pengfuncsFromEngine, sizeof(enginefuncs_t));
	gpGlobals = pGlobals;
	// --> ** Function core

#ifdef _MSC_VER
	// Epilog
	if (sizeof(int*) == 8)
	{	// 64 bit
		__asm
		{
			// Pop registers
			pop	edi
			pop	esi
			pop	ebx
			// Restore stack frame pointer
			mov	esp, ebp
			// Restore ebp
			pop	ebp
			// 2 * sizeof(int*) = 16 on 64 bit
			ret 16
		}
	}
	else
	{	// 32 bit
		__asm
		{
			// Pop registers
			pop	edi
			pop	esi
			pop	ebx
			// Restore stack frame pointer
			mov	esp, ebp
			// Restore ebp
			pop	ebp
			// 2 * sizeof(int*) = 8 on 32 bit
			ret 8
		}
	}
#endif // #ifdef _MSC_VER
}

DLL_FUNCTIONS gFunctionTable;
C_DLLEXPORT	int	GetEntityAPI2( DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion	){
  gFunctionTable.pfnSpawn =	C_Spawn;
  gFunctionTable.pfnClientCommand =	C_ClientCommand;
  gFunctionTable.pfnServerDeactivate = C_ServerDeactivate;
  gFunctionTable.pfnClientDisconnect = C_ClientDisconnect;
  gFunctionTable.pfnInconsistentFile = C_InconsistentFile;
  gFunctionTable.pfnServerActivate = C_ServerActivate;

  return g_FakeMeta.GetEntityAPI2(pFunctionTable, interfaceVersion, &gFunctionTable);
}

DLL_FUNCTIONS gFunctionTable_Post;
C_DLLEXPORT	int	GetEntityAPI2_Post(	DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion ) {
  gFunctionTable_Post.pfnClientPutInServer = C_ClientPutInServer_Post;
  gFunctionTable_Post.pfnClientUserInfoChanged = C_ClientUserInfoChanged_Post;
  gFunctionTable_Post.pfnServerActivate	= C_ServerActivate_Post;
  gFunctionTable_Post.pfnClientConnect = C_ClientConnect_Post;
  gFunctionTable_Post.pfnStartFrame	= C_StartFrame_Post;
  gFunctionTable_Post.pfnServerDeactivate =	C_ServerDeactivate_Post;

  return g_FakeMeta.GetEntityAPI2_Post(pFunctionTable, interfaceVersion, &gFunctionTable_Post);
}

enginefuncs_t meta_engfuncs;
C_DLLEXPORT	int	GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion ) {

  if ( stricmp(g_mod_name.c_str(),"cstrike") == 0	|| stricmp(g_mod_name.c_str(),"czero")==0 )
  {
	meta_engfuncs.pfnSetModel =	C_SetModel;
	g_bmod_cstrike = true;
  }
  else
  {
	g_bmod_cstrike = false;
	g_bmod_dod = !stricmp(g_mod_name.c_str(),"dod");
  }


  meta_engfuncs.pfnCmd_Argc	= C_Cmd_Argc;
  meta_engfuncs.pfnCmd_Argv	= C_Cmd_Argv;
  meta_engfuncs.pfnCmd_Args	= C_Cmd_Args;
  meta_engfuncs.pfnPrecacheModel = C_PrecacheModel;
  meta_engfuncs.pfnPrecacheSound = C_PrecacheSound;
  meta_engfuncs.pfnChangeLevel = C_ChangeLevel;

  return g_FakeMeta.GetEngineFunctions(pengfuncsFromEngine, interfaceVersion, &meta_engfuncs);
  /*
  if(*interfaceVersion!=ENGINE_INTERFACE_VERSION) {
	LOG_ERROR(PLID,	"GetEngineFunctions	version	mismatch; requested=%d ours=%d", *interfaceVersion,	ENGINE_INTERFACE_VERSION);
	*interfaceVersion =	ENGINE_INTERFACE_VERSION;
	return(FALSE);
  }
  memcpy(pengfuncsFromEngine, &meta_engfuncs, sizeof(enginefuncs_t));
  return(TRUE);
  */
}

enginefuncs_t meta_engfuncs_post;
C_DLLEXPORT	int	GetEngineFunctions_Post(enginefuncs_t *pengfuncsFromEngine,	int	*interfaceVersion )	{
  meta_engfuncs_post.pfnTraceLine =	C_TraceLine_Post;
  meta_engfuncs_post.pfnMessageBegin = C_MessageBegin_Post;
  meta_engfuncs_post.pfnMessageEnd = C_MessageEnd_Post;
  meta_engfuncs_post.pfnWriteByte =	C_WriteByte_Post;
  meta_engfuncs_post.pfnWriteChar =	C_WriteChar_Post;
  meta_engfuncs_post.pfnWriteShort = C_WriteShort_Post;
  meta_engfuncs_post.pfnWriteLong =	C_WriteLong_Post;
  meta_engfuncs_post.pfnWriteAngle = C_WriteAngle_Post;
  meta_engfuncs_post.pfnWriteCoord = C_WriteCoord_Post;
  meta_engfuncs_post.pfnWriteString	= C_WriteString_Post;
  meta_engfuncs_post.pfnWriteEntity	=  C_WriteEntity_Post;
  meta_engfuncs_post.pfnAlertMessage =	C_AlertMessage_Post;
  meta_engfuncs_post.pfnRegUserMsg = C_RegUserMsg_Post;


  CList<int, int> list;
  list.put(new int (8));
  list.put_back(new int(10));
  list.put_front(new int(6));
  list.put(new int (12));
  CList<int,int>::iterator iter;
  iter = list.begin();
  while (iter)
  {
	  if (*iter == 10)
		  iter.remove();
	  else if (*iter == 8)
		  iter.put(new int (9));
	  else
		++iter;
  }
  iter = list.begin();
  while (iter)
  {
	  AMXXLOG_Log("%d", *iter);
	  ++iter;
  }
  return g_FakeMeta.GetEngineFunctions_Post(pengfuncsFromEngine, interfaceVersion, &meta_engfuncs_post);
  /*
  if(*interfaceVersion!=ENGINE_INTERFACE_VERSION) {
	LOG_ERROR(PLID,	"GetEngineFunctions_Post version mismatch; requested=%d	ours=%d", *interfaceVersion, ENGINE_INTERFACE_VERSION);
	*interfaceVersion =	ENGINE_INTERFACE_VERSION;
	return(FALSE);
  }
  memcpy(pengfuncsFromEngine, &meta_engfuncs_post, sizeof(enginefuncs_t));
  return(TRUE);
  */
}

NEW_DLL_FUNCTIONS gNewDLLFunctionTable;
C_DLLEXPORT int GetNewDLLFunctions(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion)
{
	return g_FakeMeta.GetNewDLLFunctions(pNewFunctionTable, interfaceVersion, &gNewDLLFunctionTable);
}

NEW_DLL_FUNCTIONS gNewDLLFunctionTable_Post;
C_DLLEXPORT int GetNewDLLFunctions_Post(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion)
{
	return g_FakeMeta.GetNewDLLFunctions_Post(pNewFunctionTable, interfaceVersion, &gNewDLLFunctionTable_Post);
}
