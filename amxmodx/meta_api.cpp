// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <time.h>
#include "amxmodx.h"
#include "fakemeta.h"
#include "CMenu.h"
#include "newmenus.h"
#include "natives.h"
#include "binlog.h"
#include "optimizer.h"
#include "libraries.h"
#include "messages.h"

#include "datastructs.h"
#include "CFlagManager.h"
#include <amxmodx_version.h>
#include "trie_natives.h"
#include "CDataPack.h"
#include "textparse.h"
#include "CvarManager.h"
#include "CLibrarySys.h"
#include "CFileSystem.h"
#include "gameconfigs.h"
#include "CGameConfigs.h"
#include <engine_strucs.h>
#include <CDetour/detours.h>
#include "CoreConfig.h"
#include <resdk/mod_rehlds_api.h>
#include <amtl/am-utility.h>

plugin_info_t Plugin_info =
{
	META_INTERFACE_VERSION,		// ifvers
	"AMX Mod X",				// name
	AMXX_VERSION,			// version
	__DATE__,					// date
	"AMX Mod X Dev Team",		// author
	"http://www.amxmodx.org",	// url
	"AMXX",						// logtag
	PT_STARTUP,					// (when) loadable
	PT_ANYTIME,					// (when) unloadable
};

meta_globals_t *gpMetaGlobals;
gamedll_funcs_t *gpGamedllFuncs;
mutil_funcs_t *gpMetaUtilFuncs;
enginefuncs_t g_engfuncs;
globalvars_t *gpGlobals;

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];

void (*function)(void*);
void (*endfunction)(void*);

extern List<AUTHORIZEFUNC> g_auth_funcs;
extern ke::Vector<CAdminData *> DynamicAdmins;

CLog g_log;
CForwardMngr g_forwards;
ke::Vector<ke::AutoPtr<CPlayer *>> g_auth;
ke::Vector<ke::AutoPtr<ForceObject>> g_forcemodels;
ke::Vector<ke::AutoPtr<ForceObject>> g_forcesounds;
ke::Vector<ke::AutoPtr<ForceObject>> g_forcegeneric;
CPlayer g_players[33];
CPlayer* mPlayer;
CPluginMngr g_plugins;
CTaskMngr g_tasksMngr;
CFrameActionMngr g_frameActionMngr;
CmdMngr g_commands;
CFlagManager FlagMan;
EventsMngr g_events;
Grenades g_grenades;
LogEventsMngr g_logevents;
MenuMngr g_menucmds;
CLangMngr g_langMngr;
ke::AString g_log_dir;
ke::AString g_mod_name;
XVars g_xvars;

bool g_bmod_tfc;
bool g_bmod_cstrike;
bool g_bmod_dod;
bool g_bmod_dmc;
bool g_bmod_ricochet;
bool g_bmod_valve;
bool g_bmod_gearbox;
bool g_official_mod;
bool g_dontprecache;
bool g_forcedmodules;
bool g_forcedsounds;

fakecmd_t g_fakecmd;

float g_game_restarting;
float g_game_timeleft;
float g_task_time;
float g_auth_time;

bool g_initialized = false;
bool g_coloredmenus;
bool g_activated = false;
bool g_NewDLL_Available = false;

#ifdef MEMORY_TEST
	float g_next_memreport_time;
	unsigned int g_memreport_count;
	ke::AString g_memreport_dir;
	bool g_memreport_enabled;
	#define MEMREPORT_INTERVAL 300.0f	/* 5 mins */
#endif // MEMORY_TEST

hudtextparms_t g_hudset;
//int g_edict_point;
int g_players_num;
int mPlayerIndex;
int mState;
int g_srvindex;

CDetour *DropClientDetour;
bool g_isDropClientHookEnabled = false;
bool g_isDropClientHookAvailable = false;
void SV_DropClient_RH(IRehldsHook_SV_DropClient *chain, IGameClient *cl, bool crash, const char *format);

cvar_t init_amxmodx_version = {"amxmodx_version", "", FCVAR_SERVER | FCVAR_SPONLY};
cvar_t init_amxmodx_modules = {"amxmodx_modules", "", FCVAR_SPONLY};
cvar_t init_amxmodx_debug = {"amx_debug", "1", FCVAR_SPONLY};
cvar_t init_amxmodx_mldebug = {"amx_mldebug", "", FCVAR_SPONLY};
cvar_t init_amxmodx_language = {"amx_language", "en", FCVAR_SERVER};
cvar_t init_amxmodx_cl_langs = {"amx_client_languages", "1", FCVAR_SERVER};
cvar_t init_amxmodx_perflog = { "amx_perflog_ms", "1.0", FCVAR_SPONLY };

cvar_t* amxmodx_version = NULL;
cvar_t* amxmodx_modules = NULL;
cvar_t* amxmodx_debug = NULL;
cvar_t* amxmodx_language = NULL;
cvar_t* amxmodx_perflog = NULL;

cvar_t* hostname = NULL;
cvar_t* mp_timelimit = NULL;

// main forwards
int FF_ClientCommand = -1;
int FF_ClientConnect = -1;
int FF_ClientDisconnect = -1;
int FF_ClientDisconnected = -1;
int FF_ClientRemove = -1;
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
int FF_ClientConnectEx = -1;

IFileSystem* g_FileSystem;
HLTypeConversion TypeConversion;

bool ColoredMenus(const char *ModName)
{
	const char * pModNames[] = { "cstrike", "czero", "dmc", "dod", "tfc", "valve" };
	const size_t ModsCount = sizeof(pModNames) / sizeof(const char *);

	for (size_t i = 0; i < ModsCount; ++i)
	{
		if (strcmp(ModName, pModNames[i]) == 0)
			return true; // this game modification currently supports colored menus
	}

	return false; // no colored menus are supported for this game modification
}

void ParseAndOrAdd(CStack<ke::AString *> & files, const char *name)
{
	if (strncmp(name, "plugins-", 8) == 0)
	{
#if !defined WIN32
		size_t len = strlen(name);
		if (strcmp(&name[len-4], ".ini") == 0)
		{
#endif
			ke::AString *pString = new ke::AString(name);
			files.push(pString);
#if !defined WIN32
		}
#endif
	}
}

void BuildPluginFileList(const char *initialdir, CStack<ke::AString *> & files)
{
	char path[PLATFORM_MAX_PATH];
#if defined WIN32
	build_pathname_r(path, sizeof(path), "%s/*.ini", initialdir);
	_finddata_t fd;
	intptr_t handle = _findfirst(path, &fd);

	if (handle < 0)
	{
		return;
	}

	while (!_findnext(handle, &fd))
	{
		ParseAndOrAdd(files, fd.name);
	}

	_findclose(handle);
#elif defined(__linux__) || defined(__APPLE__)
	build_pathname_r(path, sizeof(path), "%s/", initialdir);
	struct dirent *ep;
	DIR *dp;

	if ((dp = opendir(path)) == NULL)
	{
		return;
	}

	while ( (ep=readdir(dp)) != NULL )
	{
		ParseAndOrAdd(files, ep->d_name);
	}

	closedir (dp);
#endif
}

//Loads a plugin list into the Plugin Cache and Load Modules cache
void LoadExtraPluginsToPCALM(const char *initialdir)
{
	CStack<ke::AString *> files;
	BuildPluginFileList(initialdir, files);
	char path[255];
	while (!files.empty())
	{
		ke::AString *pString = files.front();
		ke::SafeSprintf(path, sizeof(path), "%s/%s",
			initialdir,
			pString->chars());
		g_plugins.CALMFromFile(path);
		delete pString;
		files.pop();
	}
}

void LoadExtraPluginsFromDir(const char *initialdir)
{
	CStack<ke::AString *> files;
	char path[255];
	BuildPluginFileList(initialdir, files);
	while (!files.empty())
	{
		ke::AString *pString = files.front();
		ke::SafeSprintf(path, sizeof(path), "%s/%s",
			initialdir,
			pString->chars());
		g_plugins.loadPluginsFromFile(path);
		delete pString;
		files.pop();
	}
}

// Precache	stuff from force consistency calls
// or check	for	pointed	files won't	be done
int	C_PrecacheModel(const char *s)
{
	if (!g_forcedmodules)
	{
		g_forcedmodules	= true;
		for (auto &model : g_forcemodels)
		{
			PRECACHE_MODEL(model->getFilename());
			ENGINE_FORCE_UNMODIFIED(model->getForceType(), model->getMin(), model->getMax(), model->getFilename());
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

int	C_PrecacheSound(const char *s)
{
	if (!g_forcedsounds)
	{
		g_forcedsounds = true;
		for (auto &sound : g_forcesounds)
		{
			PRECACHE_SOUND(sound->getFilename());
			ENGINE_FORCE_UNMODIFIED(sound->getForceType(), sound->getMin(), sound->getMax(), sound->getFilename());
		}

		if (!g_bmod_cstrike)
		{
			PRECACHE_SOUND("weapons/cbar_hitbod1.wav");
			PRECACHE_SOUND("weapons/cbar_hitbod2.wav");
			PRECACHE_SOUND("weapons/cbar_hitbod3.wav");
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

// On InconsistentFile call	forward	function from plugins
int	C_InconsistentFile(const edict_t *player, const char *filename, char *disconnect_message)
{
	if (FF_InconsistentFile < 0)
		RETURN_META_VALUE(MRES_IGNORED,	FALSE);

	if (MDLL_InconsistentFile(player, filename, disconnect_message))
	{
		CPlayer	*pPlayer = GET_PLAYER_POINTER((edict_t *)player);

		if (executeForwards(FF_InconsistentFile, static_cast<cell>(pPlayer->index),
			filename, disconnect_message) == 1)
			RETURN_META_VALUE(MRES_SUPERCEDE, FALSE);

		RETURN_META_VALUE(MRES_SUPERCEDE, TRUE);
	}

	RETURN_META_VALUE(MRES_IGNORED, FALSE);
}

const char*	get_localinfo(const char* name, const char* def)
{
	const char* b = LOCALINFO((char*)name);

	if (b == 0 || *b == 0)
	{
		SET_LOCALINFO((char*)name, (char*)(b = def));
	}

	return b;
}

const char*	get_localinfo_r(const char *name, const char *def, char buffer[], size_t maxlength)
{
	const char* b = LOCALINFO((char*)name);

	if (b == 0 || *b == 0)
	{
		SET_LOCALINFO((char*)name, (char*)(b = def));
	}

	ke::SafeSprintf(buffer, maxlength, "%s", b);

	return buffer;
}

// Very	first point	at map load
// Load	AMX	modules	for	new	native functions
// Initialize AMX stuff	and	load it's plugins from plugins.ini list
// Call	precache forward function from plugins
int	C_Spawn(edict_t *pent)
{
	if (g_initialized)
	{
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	g_activated = false;
	g_initialized = true;
	g_forcedmodules = false;
	g_forcedsounds = false;

	g_srvindex = IS_DEDICATED_SERVER() ? 0 : 1;

	hostname = CVAR_GET_POINTER("hostname");
	mp_timelimit = CVAR_GET_POINTER("mp_timelimit");

	// Fix for crashing on mods that do not have mp_timelimit
	if (mp_timelimit == NULL)
	{
		static cvar_t timelimit_holder;

		timelimit_holder.name = "mp_timelimit";
		timelimit_holder.string = "0";
		timelimit_holder.flags = 0;
		timelimit_holder.value = 0.0;

		CVAR_REGISTER(&timelimit_holder);

		mp_timelimit = &timelimit_holder;

	}

	g_forwards.clear();

	g_log.MapChange();

	// ###### Initialize task manager
	g_tasksMngr.registerTimers(&gpGlobals->time, &mp_timelimit->value, &g_game_timeleft);

	// ###### Initialize commands prefixes
	g_commands.registerPrefix("amx");
	g_commands.registerPrefix("amxx");
	g_commands.registerPrefix("say");
	g_commands.registerPrefix("admin_");
	g_commands.registerPrefix("sm_");
	g_commands.registerPrefix("cm_");

	// make sure localinfos are set
	get_localinfo("amxx_basedir", "addons/amxmodx");
	get_localinfo("amxx_pluginsdir", "addons/amxmodx/plugins");
	get_localinfo("amxx_modulesdir", "addons/amxmodx/modules");
	get_localinfo("amxx_configsdir", "addons/amxmodx/configs");
	get_localinfo("amxx_customdir", "addons/amxmodx/custom");

	// make sure bcompat localinfos are set
	get_localinfo("amx_basedir", "addons/amxmodx");
	get_localinfo("amx_configdir", "addons/amxmodx/configs");
	get_localinfo("amx_langdir", "addons/amxmodx/data/amxmod-lang");
	get_localinfo("amx_modulesdir", "addons/amxmodx/modules");
	get_localinfo("amx_pluginsdir", "addons/amxmodx/plugins");
	get_localinfo("amx_logdir", "addons/amxmodx/logs");

	FlagMan.LoadFile();

	ArrayHandles.clear();
	TrieHandles.clear();
	TrieIterHandles.clear();
	TrieSnapshotHandles.clear();
	DataPackHandles.clear();
	TextParsersHandles.clear();
	GameConfigHandle.clear();

	char map_pluginsfile_path[256];
	char prefixed_map_pluginsfile[256];
	char configs_dir[256];

	// ###### Load modules
	loadModules(get_localinfo("amxx_modules", "addons/amxmodx/configs/modules.ini"), PT_ANYTIME);

	get_localinfo_r("amxx_configsdir", "addons/amxmodx/configs", configs_dir, sizeof(configs_dir)-1);
	g_plugins.CALMFromFile(get_localinfo("amxx_plugins", "addons/amxmodx/configs/plugins.ini"));
	LoadExtraPluginsToPCALM(configs_dir);
	char temporaryMap[64], *tmap_ptr;

	ke::SafeSprintf(temporaryMap, sizeof(temporaryMap), "%s", STRING(gpGlobals->mapname));

	prefixed_map_pluginsfile[0] = '\0';
	if ((tmap_ptr = strchr(temporaryMap, '_')) != NULL)
	{
		// this map has a prefix

		*tmap_ptr = '\0';
		ke::SafeSprintf(prefixed_map_pluginsfile,
			sizeof(prefixed_map_pluginsfile),
			"%s/maps/plugins-%s.ini",
			configs_dir,
			temporaryMap);
		g_plugins.CALMFromFile(prefixed_map_pluginsfile);
	}

	ke::SafeSprintf(map_pluginsfile_path,
		sizeof(map_pluginsfile_path),
		"%s/maps/plugins-%s.ini",
		configs_dir,
		STRING(gpGlobals->mapname));
	g_plugins.CALMFromFile(map_pluginsfile_path);

	int loaded = countModules(CountModules_Running); // Call after attachModules so all modules don't have pending stat

	// Set some info about amx version and modules
	CVAR_SET_STRING(init_amxmodx_version.name, AMXX_VERSION);
	char buffer[32];
	sprintf(buffer, "%d", loaded);
	CVAR_SET_STRING(init_amxmodx_modules.name, buffer);

	// ###### Load Vault
	char file[PLATFORM_MAX_PATH];
	g_vault.setSource(build_pathname_r(file, sizeof(file), "%s", get_localinfo("amxx_vault", "addons/amxmodx/configs/vault.ini")));
	g_vault.loadVault();

	// ###### Init time and freeze tasks
	g_game_timeleft = g_bmod_dod ? 1.0f : 0.0f;
	g_task_time = gpGlobals->time + 99999.0f;
	g_auth_time = gpGlobals->time + 99999.0f;
#ifdef MEMORY_TEST
	g_next_memreport_time = gpGlobals->time + 99999.0f;
#endif
	g_players_num = 0;

	// Set server flags
	memset(g_players[0].flags, -1, sizeof(g_players[0].flags));

	g_opt_level = atoi(get_localinfo("optimizer", "7"));
	if (!g_opt_level)
		g_opt_level = 7;

	// ###### Load AMX Mod X plugins
	g_plugins.loadPluginsFromFile(get_localinfo("amxx_plugins", "addons/amxmodx/configs/plugins.ini"));
	LoadExtraPluginsFromDir(configs_dir);
	g_plugins.loadPluginsFromFile(map_pluginsfile_path, false);
	if (prefixed_map_pluginsfile[0] != '\0')
	{
		g_plugins.loadPluginsFromFile(prefixed_map_pluginsfile, false);
	}

	g_plugins.Finalize();
	g_plugins.InvalidateCache();

	// Register forwards
	FF_PluginInit = registerForward("plugin_init", ET_IGNORE, FP_DONE);
	FF_ClientCommand = registerForward("client_command", ET_STOP, FP_CELL, FP_DONE);
	FF_ClientConnect = registerForward("client_connect", ET_IGNORE, FP_CELL, FP_DONE);
	FF_ClientDisconnect = registerForward("client_disconnect", ET_IGNORE, FP_CELL, FP_DONE);
	FF_ClientDisconnected = registerForward("client_disconnected", ET_IGNORE, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
	FF_ClientRemove = registerForward("client_remove", ET_IGNORE, FP_CELL, FP_CELL, FP_STRING, FP_DONE);
	FF_ClientInfoChanged = registerForward("client_infochanged", ET_IGNORE, FP_CELL, FP_DONE);
	FF_ClientPutInServer = registerForward("client_putinserver", ET_IGNORE, FP_CELL, FP_DONE);
	FF_PluginCfg = registerForward("plugin_cfg", ET_IGNORE, FP_DONE);
	FF_PluginPrecache = registerForward("plugin_precache", ET_IGNORE, FP_DONE);
	FF_PluginLog = registerForward("plugin_log", ET_STOP, FP_DONE);
	FF_PluginEnd = registerForward("plugin_end", ET_IGNORE, FP_DONE);
	FF_InconsistentFile = registerForward("inconsistent_file", ET_STOP, FP_CELL, FP_STRING, FP_STRINGEX, FP_DONE);
	FF_ClientAuthorized = registerForward("client_authorized", ET_IGNORE, FP_CELL, FP_STRING, FP_DONE);
	FF_ChangeLevel = registerForward("server_changelevel", ET_STOP, FP_STRING, FP_DONE);
	FF_ClientConnectEx = registerForward("client_connectex", ET_STOP, FP_CELL, FP_STRING, FP_STRING, FP_ARRAY, FP_DONE);

	CoreCfg.OnAmxxInitialized();

#if defined BINLOG_ENABLED
	if (!g_BinLog.Open())
	{
		LOG_ERROR(PLID, "Binary log failed to open.");
	}
	g_binlog_level = atoi(get_localinfo("bin_logging", "17"));
	g_binlog_maxsize = atoi(get_localinfo("max_binlog_size", "20"));
#endif

	modules_callPluginsLoaded();

	TypeConversion.init();

	// ###### Call precache forward function
	g_dontprecache = false;
	executeForwards(FF_PluginPrecache);
	g_dontprecache = true;

	for (auto &generic : g_forcegeneric)
	{
		PRECACHE_GENERIC(generic->getFilename());
		ENGINE_FORCE_UNMODIFIED(generic->getForceType(), generic->getMin(), generic->getMax(), generic->getFilename());
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

struct sUserMsg
{
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
	bool cstrike;
} g_user_msg[] =
{
	{"CurWeapon", &gmsgCurWeapon, Client_CurWeapon, false, false},
	{"Damage", &gmsgDamage, Client_DamageEnd, true, true},
	{"DeathMsg", &gmsgDeathMsg,	Client_DeathMsg, false, true},
	{"TextMsg", &gmsgTextMsg, Client_TextMsg, false, false},
	{"TeamInfo", &gmsgTeamInfo, Client_TeamInfo, false, false},
	{"WeaponList", &gmsgWeaponList, Client_WeaponList, false, false},
	{"MOTD", &gmsgMOTD,	0, false, false},
	{"ServerName", &gmsgServerName,	0, false, false},
	{"Health", &gmsgHealth,	0, false, false},
	{"Battery", &gmsgBattery, 0, false, false},
	{"ShowMenu", &gmsgShowMenu, Client_ShowMenu, false, false},
	{"SendAudio", &gmsgSendAudio, 0, false, false},
	{"AmmoX", &gmsgAmmoX, Client_AmmoX, false, false},
	{"ScoreInfo", &gmsgScoreInfo, Client_ScoreInfo,	false, false},
	{"VGUIMenu", &gmsgVGUIMenu, Client_VGUIMenu, false, false},
	{"AmmoPickup", &gmsgAmmoPickup,	Client_AmmoPickup, false, false},
	{"WeapPickup", &gmsgWeapPickup, 0, false, false},
	{"ResetHUD", &gmsgResetHUD, 0, false, false},
	{"RoundTime", &gmsgRoundTime, 0, false, false},
	{"SayText", &gmsgSayText, 0, false, false},
	{"InitHUD", &gmsgInitHUD, Client_InitHUDEnd, true, false},
	{0, 0, 0, false, false}
};

int	C_RegUserMsg_Post(const char *pszName, int iSize)
{
	for (int i = 0; g_user_msg[i].name;	++i)
	{
		if (strcmp(g_user_msg[i].name, pszName) == 0)
		{
			int id = META_RESULT_ORIG_RET(int);
			*g_user_msg[i].id =	id;

			if (!g_user_msg[i].cstrike || g_bmod_cstrike)
			{
				if (g_user_msg[i].endmsg)
					modMsgsEnd[id] = g_user_msg[i].func;
				else
					modMsgs[id] = g_user_msg[i].func;
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
void C_ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	int id;

	for (int i = 0; g_user_msg[i].name;	++i)
	{
		if ((*g_user_msg[i].id == 0) && (id = GET_USER_MSG_ID(PLID, g_user_msg[i].name, NULL)) != 0)
		{
			*g_user_msg[i].id =	id;

			if (!g_user_msg[i].cstrike || g_bmod_cstrike)
			{
				if (g_user_msg[i].endmsg)
					modMsgsEnd[id] = g_user_msg[i].func;
				else
					modMsgs[id] = g_user_msg[i].func;
			}
		}
	}

	if (g_isDropClientHookAvailable)
	{
		if (!g_isDropClientHookEnabled)
		{
			if (RehldsApi)
			{
				RehldsHookchains->SV_DropClient()->registerHook(SV_DropClient_RH);
			}
			else
			{
				DropClientDetour->EnableDetour();
			}
			g_isDropClientHookEnabled = true;
		}
	}

	RETURN_META(MRES_IGNORED);
}

void C_ServerActivate_Post(edict_t *pEdictList, int edictCount, int clientMax)
{
	if (g_activated)
		RETURN_META(MRES_IGNORED);

	for (int i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer	*pPlayer = GET_PLAYER_POINTER_I(i);
		pPlayer->Init(pEdictList + i, i);
	}

	CoreCfg.ExecuteMainConfig();    // Execute amxx.cfg

	executeForwards(FF_PluginInit);
	executeForwards(FF_PluginCfg);

	CoreCfg.ExecuteAutoConfigs();   // Execute configs created with AutoExecConfig native.
	CoreCfg.SetMapConfigTimer(6.1); // Prepare per-map configs to be executed 6.1 seconds later.
	                                // Original value which was used in admin.sma.

	// Correct time in Counter-Strike and other mods (except DOD)
	if (!g_bmod_dod)
		g_game_timeleft = 0;

	g_task_time = gpGlobals->time;
	g_auth_time = gpGlobals->time;

#ifdef MEMORY_TEST
	g_next_memreport_time = gpGlobals->time + MEMREPORT_INTERVAL;
	g_memreport_count = 0;
	g_memreport_enabled = true;
#endif

	g_activated = true;

	RETURN_META(MRES_IGNORED);
}

// Call	plugin_end forward function	from plugins.
void C_ServerDeactivate()
{
	if (!g_activated)
		RETURN_META(MRES_IGNORED);

	for (int i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer	*pPlayer = GET_PLAYER_POINTER_I(i);

		if (pPlayer->initialized)
		{
			// deprecated
			executeForwards(FF_ClientDisconnect, static_cast<cell>(pPlayer->index));

			if (g_isDropClientHookAvailable && !pPlayer->disconnecting)
			{
				executeForwards(FF_ClientDisconnected, static_cast<cell>(pPlayer->index), FALSE, prepareCharArray(const_cast<char*>(""), 0), 0);
			}
		}

		if (pPlayer->ingame)
		{
			auto wasDisconnecting = pPlayer->disconnecting;

			pPlayer->Disconnect();
			--g_players_num;

			if (!wasDisconnecting && g_isDropClientHookAvailable)
			{
				executeForwards(FF_ClientRemove, static_cast<cell>(pPlayer->index), FALSE, const_cast<char*>(""));
			}
		}
	}

	if (g_isDropClientHookAvailable)
	{
		if (g_isDropClientHookEnabled)
		{
			if (RehldsApi)
			{
				RehldsHookchains->SV_DropClient()->unregisterHook(SV_DropClient_RH);
			}
			else
			{
				DropClientDetour->DisableDetour();
			}
			g_isDropClientHookEnabled = false;
		}
	}

	g_players_num	= 0;
	executeForwards(FF_PluginEnd);

	RETURN_META(MRES_IGNORED);
}

extern ke::Vector<cell *> g_hudsync;

// After all clear whole AMX configuration
// However leave AMX modules which are loaded only once
void C_ServerDeactivate_Post()
{
	if (!g_initialized)
		RETURN_META(MRES_IGNORED);

	modules_callPluginsUnloading();

	CoreCfg.Clear();

	g_auth.clear();
	g_commands.clear();
	g_forcemodels.clear();
	g_forcesounds.clear();
	g_forcegeneric.clear();
	g_grenades.clear();
	g_tasksMngr.clear();
	g_forwards.clear();
	g_logevents.clearLogEvents();
	g_events.clearEvents();
	g_menucmds.clear();
	ClearMenus();
	g_vault.clear();
	g_xvars.clear();
	g_plugins.clear();
	g_langMngr.Clear();

	ArrayHandles.clear();
	TrieHandles.clear();
	TrieIterHandles.clear();
	TrieSnapshotHandles.clear();
	DataPackHandles.clear();
	TextParsersHandles.clear();
	GameConfigHandle.clear();

	g_CvarManager.OnPluginUnloaded();

	ClearPluginLibraries();
	modules_callPluginsUnloaded();

	detachReloadModules();

	ClearMessages();

	// Flush the dynamic admins list
	for (size_t iter=DynamicAdmins.length();iter--; )
	{
		delete DynamicAdmins[iter];
	}

	DynamicAdmins.clear();
	for (unsigned int i=0; i<g_hudsync.length(); i++)
		delete [] g_hudsync[i];
	g_hudsync.clear();

	FlagMan.WriteCommands();

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
#if defined(__linux__) || defined(__APPLE__)
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxmodx")), 0700);
#else
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxmodx")));
#endif
			while (true)
			{
				char buffer[256];
				sprintf(buffer, "%s/memreports/D%02d%02d%03d", get_localinfo("amxx_basedir", "addons/amxmodx"), curTime->tm_mon + 1, curTime->tm_mday, i);
#if defined(__linux__) || defined(__APPLE__)
				mkdir(build_pathname("%s", g_log_dir.chars()), 0700);
				if (mkdir(build_pathname(buffer), 0700) < 0)
#else
				mkdir(build_pathname("%s", g_log_dir.chars()));
				if (mkdir(build_pathname(buffer)) < 0)
#endif
				{
					if (errno == EEXIST)
					{
						// good
						++i;
						continue;
					} else {
						// bad
						g_memreport_enabled = false;
						AMXXLOG_Log("[AMXX] Fatal error: Can't create directory for memreport files (%s)", buffer);
						break;
					}
				}
				g_memreport_dir = buffer;

				// g_memreport_dir should be valid now
				break;
			}
		}

		m_dumpMemoryReport(build_pathname("%s/r%03d.txt", g_memreport_dir.chars(), g_memreport_count));
		AMXXLOG_Log("Memreport #%d created (file \"%s/r%03d.txt\") (interval %f)", g_memreport_count + 1, g_memreport_dir.chars(), g_memreport_count, MEMREPORT_INTERVAL);

		g_memreport_count++;
	}
#endif // MEMORY_TEST

#if defined BINLOG_ENABLED
	g_BinLog.Close();
#endif

	g_initialized = false;

	RETURN_META(MRES_IGNORED);
}

BOOL C_ClientConnect_Post(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	CPlayer* pPlayer = GET_PLAYER_POINTER(pEntity);
	if (!pPlayer->IsBot())
	{
		bool a = pPlayer->Connect(pszName, pszAddress);
		executeForwards(FF_ClientConnect, static_cast<cell>(pPlayer->index));

		if (a)
		{
			auto playerToAuth = ke::AutoPtr<CPlayer *>(new CPlayer*(pPlayer));
			if (playerToAuth)
				g_auth.append(ke::Move(playerToAuth));
		} else {
			pPlayer->Authorize();
			const char* authid = GETPLAYERAUTHID(pEntity);
			if (g_auth_funcs.size())
			{
				List<AUTHORIZEFUNC>::iterator iter, end=g_auth_funcs.end();
				AUTHORIZEFUNC fn;
				for (iter=g_auth_funcs.begin(); iter!=end; iter++)
				{
					fn = (*iter);
					fn(pPlayer->index, authid);
				}
			}
			executeForwards(FF_ClientAuthorized, static_cast<cell>(pPlayer->index), authid);
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

BOOL C_ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	CPlayer* pPlayer = GET_PLAYER_POINTER(pEntity);

	if(executeForwards(FF_ClientConnectEx, static_cast<cell>(pPlayer->index), pszName, pszAddress, prepareCharArray(szRejectReason, 128, true)))
		RETURN_META_VALUE(MRES_SUPERCEDE, FALSE);

	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void C_ClientDisconnect(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	if (pPlayer->initialized)
	{
		// deprecated
		executeForwards(FF_ClientDisconnect, static_cast<cell>(pPlayer->index));

		if (g_isDropClientHookAvailable && !pPlayer->disconnecting)
		{
			executeForwards(FF_ClientDisconnected, static_cast<cell>(pPlayer->index), FALSE, prepareCharArray(const_cast<char*>(""), 0), 0);
		}
	}

	if (pPlayer->ingame)
	{
		--g_players_num;
	}

	auto wasDisconnecting = pPlayer->disconnecting;

	pPlayer->Disconnect();

	if (!wasDisconnecting && g_isDropClientHookAvailable)
	{
		executeForwards(FF_ClientRemove, static_cast<cell>(pPlayer->index), FALSE, const_cast<char*>(""));
	}

	RETURN_META(MRES_IGNORED);
}

CPlayer* SV_DropClient_PreHook(edict_s *client, qboolean crash, const char *buffer, size_t buffer_size)
{
	auto pPlayer = client ? GET_PLAYER_POINTER(client) : nullptr;

	if (pPlayer)
	{
		if (pPlayer->initialized)
		{
			pPlayer->disconnecting = true;
			executeForwards(FF_ClientDisconnected, pPlayer->index, TRUE, prepareCharArray(const_cast<char*>(buffer), buffer_size, true), buffer_size - 1);
		}
	}

	return pPlayer;
}

void SV_DropClient_PostHook(CPlayer *pPlayer, qboolean crash, const char *buffer)
{
	if (pPlayer)
	{
		pPlayer->Disconnect();
		executeForwards(FF_ClientRemove, pPlayer->index, TRUE, buffer);
	}
}

// void SV_DropClient(client_t *cl, qboolean crash, const char *fmt, ...);
DETOUR_DECL_STATIC3_VAR(SV_DropClient, void, client_t*, cl, qboolean, crash, const char*, format)
{
	char buffer[1024];

	va_list ap;
	va_start(ap, format);
	ke::SafeVsprintf(buffer, sizeof(buffer) - 1, format, ap);
	va_end(ap);

	auto pPlayer = SV_DropClient_PreHook(cl->edict, crash, buffer, ARRAY_LENGTH(buffer));

	DETOUR_STATIC_CALL(SV_DropClient)(cl, crash, "%s", buffer);

	SV_DropClient_PostHook(pPlayer, crash, buffer);
}

void SV_DropClient_RH(IRehldsHook_SV_DropClient *chain, IGameClient *cl, bool crash, const char *format)
{
	char buffer[1024];
	ke::SafeStrcpy(buffer, sizeof(buffer), format);

	auto pPlayer = SV_DropClient_PreHook(cl->GetEdict(), crash, buffer, ARRAY_LENGTH(buffer));

	chain->callNext(cl, crash, buffer);

	SV_DropClient_PostHook(pPlayer, crash, buffer);
}

void C_ClientPutInServer_Post(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (!pPlayer->IsBot())
	{
		pPlayer->PutInServer();
		++g_players_num;
		executeForwards(FF_ClientPutInServer, static_cast<cell>(pPlayer->index));
	}

	RETURN_META(MRES_IGNORED);
}

void C_ClientUserInfoChanged_Post(edict_t *pEntity, char *infobuffer)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	executeForwards(FF_ClientInfoChanged, static_cast<cell>(pPlayer->index));
	const char* name = INFOKEY_VALUE(infobuffer, "name");

	// Emulate bot connection and putinserver
	if (pPlayer->ingame)
	{
		pPlayer->name =name;			//	Make sure player have name up to date
	} else if (pPlayer->IsBot()) {
		pPlayer->Connect(name, "127.0.0.1"/*CVAR_GET_STRING("net_address")*/);

		executeForwards(FF_ClientConnect, static_cast<cell>(pPlayer->index));

		pPlayer->Authorize();
		const char* authid = GETPLAYERAUTHID(pEntity);
		if (g_auth_funcs.size())
		{
			List<AUTHORIZEFUNC>::iterator iter, end=g_auth_funcs.end();
			AUTHORIZEFUNC fn;
			for (iter=g_auth_funcs.begin(); iter!=end; iter++)
			{
				fn = (*iter);
				fn(pPlayer->index, authid);
			}
		}
		executeForwards(FF_ClientAuthorized, static_cast<cell>(pPlayer->index), authid);

		pPlayer->PutInServer();
		++g_players_num;

		executeForwards(FF_ClientPutInServer, static_cast<cell>(pPlayer->index));
	}

	RETURN_META(MRES_IGNORED);
}

void C_ClientCommand(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	META_RES result = MRES_IGNORED;
	cell ret = 0;

	const char* cmd = CMD_ARGV(0);
	const char* arg = CMD_ARGV(1);

	// Handle "amxx" if not on listenserver
	if (IS_DEDICATED_SERVER())
	{
		if (cmd && stricmp(cmd, "amxx") == 0)
		{
			// Print version
			static char buf[1024];
			size_t len = 0;

			sprintf(buf, "%s %s\n", Plugin_info.name, Plugin_info.version);
			CLIENT_PRINT(pEntity, print_console, buf);
			len = sprintf(buf, "Authors: \n         David \"BAILOPAN\" Anderson, Pavol \"PM OnoTo\" Marko, Felix \"SniperBeamer\" Geyer\n");
			len += sprintf(&buf[len], "         Jonny \"Got His Gun\" Bergstrom, Lukasz \"SidLuke\" Wlasinski\n");
			CLIENT_PRINT(pEntity, print_console, buf);
			len = sprintf(buf, "         Christian \"Basic-Master\" Hammacher, Borja \"faluco\" Ferrer\n");
			len += sprintf(&buf[len], "         Scott \"DS\" Ehlert\n");
			len += sprintf(&buf[len], "Compiled: %s\nURL:http://www.amxmodx.org/\n", __DATE__ ", " __TIME__);
			CLIENT_PRINT(pEntity, print_console, buf);
#ifdef JIT
			sprintf(buf, "Core mode: JIT\n");
#else
#ifdef ASM32
			sprintf(buf, "Core mode: ASM\n");
#else
			sprintf(buf, "Core mode: Normal\n");
#endif
#endif
			CLIENT_PRINT(pEntity, print_console, buf);
			RETURN_META(MRES_SUPERCEDE);
		}
	}

	if (executeForwards(FF_ClientCommand, static_cast<cell>(pPlayer->index)) > 0)
		RETURN_META(MRES_SUPERCEDE);

	/* check for command and if needed also for first argument and call proper function */

	CmdMngr::iterator aa = g_commands.clcmdprefixbegin(cmd);

	if (!aa)
		aa = g_commands.clcmdbegin();

	while (aa)
	{
		if ((*aa).matchCommandLine(cmd, arg) && (*aa).getPlugin()->isExecutable((*aa).getFunction()))
		{
			ret = executeForwards((*aa).getFunction(), static_cast<cell>(pPlayer->index),
				static_cast<cell>((*aa).getFlags()), static_cast<cell>((*aa).getId()));
			if (ret & 2) result = MRES_SUPERCEDE;
			if (ret & 1) RETURN_META(MRES_SUPERCEDE);
		}
		++aa;
	}

	/* check menu commands */

	if (!strcmp(cmd, "menuselect"))
	{
		int	pressed_key	= atoi(arg) - 1;
		int	bit_key	= (1<<pressed_key);

		if (pPlayer->keys &	bit_key)
		{
			if (gpGlobals->time > pPlayer->menuexpire)
			{
				if (Menu *pMenu = get_menu_by_id(pPlayer->newmenu))
				{
					pMenu->Close(pPlayer->index);

					RETURN_META(MRES_SUPERCEDE);
				}
				else if (pPlayer->menu > 0 && !pPlayer->vgui)
				{
					pPlayer->menu = 0;
					pPlayer->keys = 0;

					RETURN_META(MRES_SUPERCEDE);
				}
			}

			int menuid = pPlayer->menu;
			pPlayer->menu = 0;

			/* First, do new menus */
			int func_was_executed = -1;
			if (pPlayer->newmenu != -1)
			{
				int menu = pPlayer->newmenu;
				pPlayer->newmenu = -1;
				if (Menu *pMenu = get_menu_by_id(menu))
				{
					int item = pMenu->PagekeyToItem(pPlayer->page, pressed_key+1);
					if (item == MENU_BACK)
					{
						if (pMenu->pageCallback >= 0)
							executeForwards(pMenu->pageCallback, static_cast<cell>(pPlayer->index), static_cast<cell>(MENU_BACK), static_cast<cell>(menu));

						pMenu->Display(pPlayer->index, pPlayer->page - 1);
					} else if (item == MENU_MORE) {
						if (pMenu->pageCallback >= 0)
							executeForwards(pMenu->pageCallback, static_cast<cell>(pPlayer->index), static_cast<cell>(MENU_MORE), static_cast<cell>(menu));

						pMenu->Display(pPlayer->index, pPlayer->page + 1);
					} else {
						ret = executeForwards(pMenu->func, static_cast<cell>(pPlayer->index), static_cast<cell>(menu), static_cast<cell>(item));
						if (ret & 2)
						{
							result = MRES_SUPERCEDE;
						} else if (ret & 1) {
							RETURN_META(MRES_SUPERCEDE);
						}
					}
					/**
					 * No matter what we marked it as executed, since the callback styles are
					 * entirely different.  After all, this is a backwards compat shim.
					 */
					func_was_executed = pMenu->func;
				}
			}

			/* Now, do old menus */
			MenuMngr::iterator a = g_menucmds.begin();

			while (a)
			{
				g_menucmds.SetWatchIter(a);
				if ((*a).matchCommand(menuid, bit_key)
					&& (*a).getPlugin()->isExecutable((*a).getFunction())
					&& (func_was_executed == -1
						|| !g_forwards.isSameSPForward(func_was_executed, (*a).getFunction()))
					)
				{
					ret = executeForwards((*a).getFunction(), static_cast<cell>(pPlayer->index),
						static_cast<cell>(pressed_key), 0);

					if (ret & 2) result = MRES_SUPERCEDE;
					if (ret & 1) RETURN_META(MRES_SUPERCEDE);
				}
				if (g_menucmds.GetWatchIter() != a)
				{
					a = g_menucmds.GetWatchIter();
				} else {
					++a;
				}
			}
		}
	}

	/* check for PLUGIN_HANDLED_MAIN and block hl call if needed */
	RETURN_META(result);
}

void C_StartFrame_Post(void)
{
	if (g_auth_time < gpGlobals->time)
	{
		g_auth_time = gpGlobals->time + 0.7f;

		size_t i = 0;
		while (i < g_auth.length())
		{
			auto player = g_auth[i].get();
			const char*	auth = GETPLAYERAUTHID((*player)->pEdict);

			if ((auth == 0) || (*auth == 0))
			{
				g_auth.remove(i);
				continue;
			}

			if (strcmp(auth, "STEAM_ID_PENDING"))
			{
				(*player)->Authorize();
				if (g_auth_funcs.size())
				{
					List<AUTHORIZEFUNC>::iterator iter, end=g_auth_funcs.end();
					AUTHORIZEFUNC fn;
					for (iter=g_auth_funcs.begin(); iter!=end; iter++)
					{
						fn = (*iter);
						fn((*player)->index, auth);
					}
				}
				executeForwards(FF_ClientAuthorized, static_cast<cell>((*player)->index), auth);
				g_auth.remove(i);

				continue;
			}
			i++;
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
#if defined(__linux__) || defined(__APPLE__)
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxmodx")), 0700);
#else
			mkdir(build_pathname("%s/memreports", get_localinfo("amxx_basedir", "addons/amxmodx")));
#endif
			while (true)
			{
				char buffer[256];
				sprintf(buffer, "%s/memreports/D%02d%02d%03d", get_localinfo("amxx_basedir", "addons/amxmodx"), curTime->tm_mon + 1, curTime->tm_mday, i);
#if defined(__linux__) || defined(__APPLE__)
				mkdir(build_pathname("%s", g_log_dir.chars()), 0700);
				if (mkdir(build_pathname(buffer), 0700) < 0)
#else
				mkdir(build_pathname("%s", g_log_dir.chars()));
				if (mkdir(build_pathname(buffer)) < 0)
#endif
				{
					if (errno == EEXIST)
					{
						// good
						++i;
						continue;
					} else {
						// bad
						g_memreport_enabled = false;
						AMXXLOG_Log("[AMXX] Fatal error: Can't create directory for memreport files (%s)", buffer);
						break;
					}
				}
				g_memreport_dir = buffer;
				// g_memreport_dir should be valid now
				break;
			}
		}

		m_dumpMemoryReport(build_pathname("%s/r%03d.txt", g_memreport_dir.chars(), g_memreport_count));
		AMXXLOG_Log("Memreport #%d created (file \"%s/r%03d.txt\") (interval %f)", g_memreport_count + 1, g_memreport_dir.chars(), g_memreport_count, MEMREPORT_INTERVAL);

		g_memreport_count++;
	}
#endif // MEMORY_TEST

	g_frameActionMngr.ExecuteFrameCallbacks();

	if (g_task_time > gpGlobals->time)
		RETURN_META(MRES_IGNORED);

	g_task_time = gpGlobals->time + 0.1f;
	g_tasksMngr.startFrame();

	CoreCfg.OnMapConfigTimer();

	RETURN_META(MRES_IGNORED);
}

void C_MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (ed)
	{
		mPlayerIndex = ENTINDEX(ed);
		mPlayer	= GET_PLAYER_POINTER_I(mPlayerIndex);
	} else {
		mPlayerIndex = 0;
		mPlayer	= 0;
	}

	if (msg_type < 0 || msg_type >= MAX_REG_MSGS)
		msg_type = 0;

	mState = 0;
	function = modMsgs[msg_type];
	endfunction = modMsgsEnd[msg_type];

	g_events.parserInit(msg_type, &gpGlobals->time, mPlayer, mPlayerIndex);

	RETURN_META(MRES_IGNORED);
}

void C_WriteByte_Post(int iValue)
{
	g_events.parseValue(iValue);
	if (function) (*function)((void *)&iValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteChar_Post(int iValue)
{
	g_events.parseValue(iValue);
	if (function) (*function)((void *)&iValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteShort_Post(int iValue)
{
	g_events.parseValue(iValue);
	if (function) (*function)((void *)&iValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteLong_Post(int iValue)
{
	g_events.parseValue(iValue);
	if (function) (*function)((void *)&iValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteAngle_Post(float flValue)
{
	g_events.parseValue(flValue);
	if (function) (*function)((void *)&flValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteCoord_Post(float flValue)
{
	g_events.parseValue(flValue);
	if (function) (*function)((void *)&flValue);

	RETURN_META(MRES_IGNORED);
}

void C_WriteString_Post(const char *sz)
{
	g_events.parseValue(sz);
	if (function) (*function)((void *)sz);

	RETURN_META(MRES_IGNORED);
}

void C_WriteEntity_Post(int iValue)
{
	g_events.parseValue(iValue);
	if (function) (*function)((void *)&iValue);

	RETURN_META(MRES_IGNORED);
}

void C_MessageEnd_Post(void)
{
	g_events.executeEvents();
	if (endfunction) (*endfunction)(NULL);

	RETURN_META(MRES_IGNORED);
}

const char *C_Cmd_Args(void)
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
		RETURN_META_VALUE(MRES_SUPERCEDE, (argc < 3) ? g_fakecmd.argv[argc] : "");

	// otherwise ignore it
	RETURN_META_VALUE(MRES_IGNORED, NULL);
}

int	C_Cmd_Argc(void)
{
	// if the global "fake" flag is set, which means that engclient_cmd was used, supercede the function
	if (g_fakecmd.fake)
		RETURN_META_VALUE(MRES_SUPERCEDE, g_fakecmd.argc);

	// otherwise ignore it
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

// Grenade has been	thrown.
// Only	here we	may	find out who is	an owner.
void C_SetModel(edict_t *e, const char *m)
{
	if (!m || strcmp(m, "models/w_hegrenade.mdl") != 0)
	{
		RETURN_META(MRES_IGNORED);
	}

	if (e->v.owner)
	{
		g_grenades.put(e, 1.75f, 4, GET_PLAYER_POINTER(e->v.owner));
	}

	RETURN_META(MRES_IGNORED);
}

// Save	at what	part of	body a player is aiming
void C_TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr)
{
	if (e && (e->v.flags & (FL_CLIENT | FL_FAKECLIENT)))
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER(e);

		if (ptr->pHit && (ptr->pHit->v.flags & (FL_CLIENT | FL_FAKECLIENT)))
			pPlayer->aiming = ptr->iHitgroup;

		pPlayer->lastTrace = ptr->vecEndPos;
	}

	RETURN_META(MRES_IGNORED);
}

void C_AlertMessage(ALERT_TYPE atype, const char *szFmt, ...)
{
	if (atype != at_logged)
	{
		RETURN_META(MRES_IGNORED);
	}

	/* There are also more messages but we want only logs
	at_notice,
	at_console,		// same	as at_notice, but forces a ConPrintf, not a	message	box
	at_aiconsole,	// same	as at_console, but only	shown if developer level is	2!
	at_warning,
	at_error,
	at_logged		// Server print to console ( only in multiplayer games ).
	*/

	cell retVal = 0;

	// execute logevents and plugin_log forward
	if (g_logevents.logEventsExist()
		|| g_forwards.getFuncsNum(FF_PluginLog))
	{
		va_list	logArgPtr;
		va_start(logArgPtr, szFmt);
		g_logevents.setLogString(szFmt, logArgPtr);
		va_end(logArgPtr);
		g_logevents.parseLogString();

		if (g_logevents.logEventsExist())
		{
			g_logevents.executeLogEvents();
		}

		retVal = executeForwards(FF_PluginLog);
	}

	if (retVal)
	{
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_ChangeLevel(const char *map, const char *what)
{
	int ret = executeForwards(FF_ChangeLevel,  map);
	if (ret)
		RETURN_META(MRES_SUPERCEDE);
	RETURN_META(MRES_IGNORED);
}

void C_CvarValue2(const edict_t *pEdict, int requestId, const char *cvar, const char *value)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEdict);
	if (pPlayer->queries.empty())
		RETURN_META(MRES_IGNORED);

	List<ClientCvarQuery_Info *>::iterator iter, end=pPlayer->queries.end();
	ClientCvarQuery_Info *info;
	for (iter=pPlayer->queries.begin(); iter!=end; iter++)
	{
		info = (*iter);
		if ( info->requestId == requestId )
		{
			if (info->paramLen)
			{
				cell arr = prepareCellArray(info->params, info->paramLen);
				executeForwards(info->resultFwd, static_cast<cell>(ENTINDEX(pEdict)),
					cvar, value, arr);
			} else {
				executeForwards(info->resultFwd, static_cast<cell>(ENTINDEX(pEdict)),
					cvar, value);
			}
			unregisterSPForward(info->resultFwd);
			pPlayer->queries.erase(iter);
			delete [] info->params;
			delete info;

			break;
		}
	}

	RETURN_META(MRES_HANDLED);
}

C_DLLEXPORT	int	Meta_Query(const char	*ifvers, plugin_info_t **pPlugInfo,	mutil_funcs_t *pMetaUtilFuncs)
{
	gpMetaUtilFuncs = pMetaUtilFuncs;
	*pPlugInfo = &Plugin_info;

	int	mmajor = 0, mminor = 0,	pmajor = 0, pminor = 0;

	sscanf(ifvers, "%d:%d",	&mmajor, &mminor);
	sscanf(Plugin_info.ifvers, "%d:%d",	&pmajor, &pminor);

	if (strcmp(ifvers, Plugin_info.ifvers))
	{
		LOG_MESSAGE(PLID, "warning: ifvers mismatch (pl \"%s\") (mm \"%s\")", Plugin_info.ifvers, ifvers);
		if (pmajor > mmajor)
		{
			LOG_ERROR(PLID, "metamod version is too old for this plugin; update metamod");
			return (FALSE);
		} else if (pmajor < mmajor) {
			LOG_ERROR(PLID, "metamod version is incompatible with this plugin; please find a newer version of this plugin");
			return (FALSE);
		} else if (pmajor == mmajor) {
			if (pminor > mminor)
			{
				LOG_ERROR(PLID, "metamod version is incompatible with this plugin; please find a newer version of this plugin");
				return FALSE;
			} else if (pminor < mminor) {
				LOG_MESSAGE(PLID, "warning: there may be a newer version of metamod available");
			}
		}
	}

	// :NOTE: Don't call modules query here (g_FakeMeta.Meta_Query), because we don't know modules yet. Do it in Meta_Attach
	return (TRUE);
}

static META_FUNCTIONS gMetaFunctionTable;
C_DLLEXPORT	int	Meta_Attach(PLUG_LOADTIME now, META_FUNCTIONS *pFunctionTable, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs)
{
	if (now > Plugin_info.loadable)
	{
		LOG_ERROR(PLID,	"Can't load	plugin right now");
		return (FALSE);
	}

	gpMetaGlobals = pMGlobals;
	gMetaFunctionTable.pfnGetEntityAPI2 = GetEntityAPI2;
	gMetaFunctionTable.pfnGetEntityAPI2_Post = GetEntityAPI2_Post;
	gMetaFunctionTable.pfnGetEngineFunctions = GetEngineFunctions;
	gMetaFunctionTable.pfnGetEngineFunctions_Post = GetEngineFunctions_Post;
#if !defined AMD64
	gMetaFunctionTable.pfnGetNewDLLFunctions = GetNewDLLFunctions;
#endif

	memcpy(pFunctionTable, &gMetaFunctionTable, sizeof(META_FUNCTIONS));
	gpGamedllFuncs=pGamedllFuncs;

	Module_CacheFunctions();

	CVAR_REGISTER(&init_amxmodx_version);
	CVAR_REGISTER(&init_amxmodx_modules);
	CVAR_REGISTER(&init_amxmodx_debug);
	CVAR_REGISTER(&init_amxmodx_mldebug);
	CVAR_REGISTER(&init_amxmodx_language);
	CVAR_REGISTER(&init_amxmodx_cl_langs);
	CVAR_REGISTER(&init_amxmodx_perflog);

	amxmodx_version = CVAR_GET_POINTER(init_amxmodx_version.name);
	amxmodx_debug = CVAR_GET_POINTER(init_amxmodx_debug.name);
	amxmodx_language = CVAR_GET_POINTER(init_amxmodx_language.name);
	amxmodx_perflog = CVAR_GET_POINTER(init_amxmodx_perflog.name);

	REG_SVR_COMMAND("amxx", amx_command);

	char gameDir[512];
	GET_GAME_DIR(gameDir);
	char *a = gameDir;
	int i = 0;

	while (gameDir[i])
		if (gameDir[i++] ==	'/')
			a = &gameDir[i];

	g_mod_name = a;

	g_coloredmenus = ColoredMenus(g_mod_name.chars()); // whether or not to use colored menus

	// ###### Print short GPL
	print_srvconsole("\n   AMX Mod X version %s Copyright (c) 2004-2015 AMX Mod X Development Team \n"
					 "   AMX Mod X comes with ABSOLUTELY NO WARRANTY; for details type `amxx gpl'.\n", AMXX_VERSION);
	print_srvconsole("   This is free software and you are welcome to redistribute it under \n"
					 "   certain conditions; type 'amxx gpl' for details.\n  \n");

	// ###### Load custom path configuration
	Vault amx_config;
	amx_config.setSource(build_pathname("%s", get_localinfo("amxx_cfg", "addons/amxmodx/configs/core.ini")));

	if (amx_config.loadVault())
	{
		Vault::iterator	a =	amx_config.begin();

		while (a != amx_config.end())
		{
			SET_LOCALINFO((char*)a.key().chars(), (char*)a.value().chars());
			++a;
		}
		amx_config.clear();
	}

	// ###### Initialize logging here
	g_log_dir = get_localinfo("amxx_logs", "addons/amxmodx/logs");
	g_log.SetLogType("amxx_logging");

	// ###### Now attach metamod modules
	// This will also call modules Meta_Query and Meta_Attach functions
	loadModules(get_localinfo("amxx_modules", "addons/amxmodx/configs/modules.ini"), now);

	GET_HOOK_TABLES(PLID, &g_pEngTable, NULL, NULL);

	FlagMan.SetFile("cmdaccess.ini");

	ConfigManager.OnAmxxStartup();

	if (RehldsApi_Init())
	{
		RehldsHookchains->SV_DropClient()->registerHook(SV_DropClient_RH);
		g_isDropClientHookAvailable = true;
		g_isDropClientHookEnabled = true;
	}
	else
	{
		void *address = nullptr;

		if (CommonConfig && CommonConfig->GetMemSig("SV_DropClient", &address) && address)
		{
			DropClientDetour = DETOUR_CREATE_STATIC_FIXED(SV_DropClient, address);
			g_isDropClientHookAvailable = true;
			g_isDropClientHookEnabled = true;
		}
		else
		{
			auto reason = RehldsApi ? "update ReHLDS" : "check your gamedata files";
			AMXXLOG_Log("client_disconnected and client_remove forwards have been disabled - %s.", reason);
		}
	}

	g_CvarManager.CreateCvarHook();

	GET_IFACE<IFileSystem>("filesystem_stdio", g_FileSystem, FILESYSTEM_INTERFACE_VERSION);

	return (TRUE);
}

C_DLLEXPORT	int	Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON	reason)
{
	if (now > Plugin_info.unloadable && reason != PNL_CMD_FORCED)
	{
		LOG_ERROR(PLID,	"Can't unload plugin right now");
		return (FALSE);
	}

	modules_callPluginsUnloading();

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
	ClearMenus();
	g_vault.clear();
	g_xvars.clear();
	g_plugins.clear();
	g_langMngr.Clear();

	ArrayHandles.clear();
	TrieHandles.clear();
	TrieIterHandles.clear();
	TrieSnapshotHandles.clear();
	DataPackHandles.clear();
	TextParsersHandles.clear();
	GameConfigHandle.clear();

	ClearMessages();

	modules_callPluginsUnloaded();

	detachModules();

	g_log.CloseFile();

	Module_UncacheFunctions();

	ClearLibraries(LibSource_Plugin);
	ClearLibraries(LibSource_Module);

	if (g_isDropClientHookAvailable)
	{
		if (RehldsApi)
		{
			if (g_isDropClientHookEnabled)
			{
				RehldsHookchains->SV_DropClient()->unregisterHook(SV_DropClient_RH);
			}
		}
		else
		{
			DropClientDetour->Destroy();
		}
		g_isDropClientHookAvailable = false;
		g_isDropClientHookEnabled = false;
	}

	return (TRUE);
}

C_DLLEXPORT void WINAPI GiveFnptrsToDll(enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals)
{
	memcpy(&g_engfuncs, pengfuncsFromEngine, sizeof(enginefuncs_t));
	gpGlobals = pGlobals;
}

DLL_FUNCTIONS gFunctionTable;
C_DLLEXPORT	int	GetEntityAPI2(DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion)
{
	memset(&gFunctionTable, 0, sizeof(DLL_FUNCTIONS));

	gFunctionTable.pfnSpawn = C_Spawn;
	gFunctionTable.pfnClientCommand = C_ClientCommand;
	gFunctionTable.pfnServerDeactivate = C_ServerDeactivate;
	gFunctionTable.pfnClientDisconnect = C_ClientDisconnect;
	gFunctionTable.pfnInconsistentFile = C_InconsistentFile;
	gFunctionTable.pfnServerActivate = C_ServerActivate;
	gFunctionTable.pfnClientConnect = C_ClientConnect;

	memcpy(pFunctionTable, &gFunctionTable, sizeof(DLL_FUNCTIONS));

	return 1;
}

DLL_FUNCTIONS gFunctionTable_Post;
C_DLLEXPORT	int	GetEntityAPI2_Post(DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion)
{
	memset(&gFunctionTable_Post, 0, sizeof(DLL_FUNCTIONS));

	gFunctionTable_Post.pfnClientPutInServer = C_ClientPutInServer_Post;
	gFunctionTable_Post.pfnClientUserInfoChanged = C_ClientUserInfoChanged_Post;
	gFunctionTable_Post.pfnServerActivate = C_ServerActivate_Post;
	gFunctionTable_Post.pfnClientConnect = C_ClientConnect_Post;
	gFunctionTable_Post.pfnStartFrame = C_StartFrame_Post;
	gFunctionTable_Post.pfnServerDeactivate = C_ServerDeactivate_Post;

	memcpy(pFunctionTable, &gFunctionTable_Post, sizeof(DLL_FUNCTIONS));

	return 1;
}

enginefuncs_t meta_engfuncs;

C_DLLEXPORT	int	GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion)
{
	memset(&meta_engfuncs, 0, sizeof(enginefuncs_t));

	if (stricmp(g_mod_name.chars(), "cstrike") == 0 || stricmp(g_mod_name.chars(), "czero") == 0)
	{
		meta_engfuncs.pfnSetModel =	C_SetModel;
		g_bmod_cstrike = true;
	} else {
		g_bmod_cstrike  = false;
		g_bmod_dod      = !stricmp(g_mod_name.chars(), "dod");
		g_bmod_dmc      = !stricmp(g_mod_name.chars(), "dmc");
		g_bmod_tfc      = !stricmp(g_mod_name.chars(), "tfc");
		g_bmod_ricochet = !stricmp(g_mod_name.chars(), "ricochet");
		g_bmod_valve    = !stricmp(g_mod_name.chars(), "valve");
		g_bmod_gearbox  = !stricmp(g_mod_name.chars(), "gearbox");
	}

	g_official_mod = g_bmod_cstrike || g_bmod_dod || g_bmod_dmc || g_bmod_ricochet || g_bmod_tfc || g_bmod_valve || g_bmod_gearbox;

	meta_engfuncs.pfnCmd_Argc = C_Cmd_Argc;
	meta_engfuncs.pfnCmd_Argv = C_Cmd_Argv;
	meta_engfuncs.pfnCmd_Args = C_Cmd_Args;
	meta_engfuncs.pfnPrecacheModel = C_PrecacheModel;
	meta_engfuncs.pfnPrecacheSound = C_PrecacheSound;
	meta_engfuncs.pfnChangeLevel = C_ChangeLevel;

	/* message stuff from messages.h/cpp */
	meta_engfuncs.pfnMessageBegin = C_MessageBegin;
	meta_engfuncs.pfnMessageEnd = C_MessageEnd;
	meta_engfuncs.pfnWriteAngle = C_WriteAngle;
	meta_engfuncs.pfnWriteByte = C_WriteByte;
	meta_engfuncs.pfnWriteChar = C_WriteChar;
	meta_engfuncs.pfnWriteCoord = C_WriteCoord;
	meta_engfuncs.pfnWriteEntity = C_WriteEntity;
	meta_engfuncs.pfnWriteLong = C_WriteLong;
	meta_engfuncs.pfnWriteShort = C_WriteShort;
	meta_engfuncs.pfnWriteString = C_WriteString;

	meta_engfuncs.pfnAlertMessage = C_AlertMessage;

	memcpy(pengfuncsFromEngine, &meta_engfuncs, sizeof(enginefuncs_t));

	return 1;
}

enginefuncs_t meta_engfuncs_post;
C_DLLEXPORT	int	GetEngineFunctions_Post(enginefuncs_t *pengfuncsFromEngine,	int	*interfaceVersion)
{
	memset(&meta_engfuncs_post, 0, sizeof(enginefuncs_t));

	meta_engfuncs_post.pfnTraceLine = C_TraceLine_Post;
	meta_engfuncs_post.pfnMessageBegin = C_MessageBegin_Post;
	meta_engfuncs_post.pfnMessageEnd = C_MessageEnd_Post;
	meta_engfuncs_post.pfnWriteByte = C_WriteByte_Post;
	meta_engfuncs_post.pfnWriteChar = C_WriteChar_Post;
	meta_engfuncs_post.pfnWriteShort = C_WriteShort_Post;
	meta_engfuncs_post.pfnWriteLong = C_WriteLong_Post;
	meta_engfuncs_post.pfnWriteAngle = C_WriteAngle_Post;
	meta_engfuncs_post.pfnWriteCoord = C_WriteCoord_Post;
	meta_engfuncs_post.pfnWriteString = C_WriteString_Post;
	meta_engfuncs_post.pfnWriteEntity = C_WriteEntity_Post;
	meta_engfuncs_post.pfnRegUserMsg = C_RegUserMsg_Post;

	memcpy(pengfuncsFromEngine, &meta_engfuncs_post, sizeof(enginefuncs_t));

	return 1;
}

//quick hack - disable all newdll stuff for AMD64
// until VALVe gets their act together!
#if !defined AMD64
NEW_DLL_FUNCTIONS gNewDLLFunctionTable;
C_DLLEXPORT int GetNewDLLFunctions(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion)
{
	memset(&gNewDLLFunctionTable, 0, sizeof(NEW_DLL_FUNCTIONS));

	// default metamod does not call this if the gamedll doesn't provide it
	if (g_engfuncs.pfnQueryClientCvarValue2)
	{
		gNewDLLFunctionTable.pfnCvarValue2 = C_CvarValue2;
		g_NewDLL_Available = true;
	}

	memcpy(pNewFunctionTable, &gNewDLLFunctionTable, sizeof(NEW_DLL_FUNCTIONS));

	return 1;
}
#endif
