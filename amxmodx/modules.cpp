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

#include "amxmodx.h"
#include "osdep.h"			// sleep, etc
#include "CFile.h"
#include "amxxfile.h"

CList<CModule> g_modules;
CList<CScript,AMX*> g_loadedscripts;

CModule *g_CurrentlyCalledModule = NULL;	// The module we are in at the moment; NULL otherwise
// also NULL for non-amxx modules
// This is needed so we know which module called a function
ModuleCallReason g_ModuleCallReason;

extern const char* no_function; // stupid work around


void report_error( int code, char* fmt, ... )
{
	va_list argptr;
	char string[256];
	*string = 0;
	va_start (argptr, fmt);
	vsnprintf (string, 255, fmt,argptr);
	string[255] = 0;
	va_end (argptr);
	if ( *string ) {
		AMXXLOG_Log("Error:");
		AMXXLOG_Log(string);
	}
	else
	{
		AMXXLOG_Log("!!! There was an unexpected module error.");
		AMXXLOG_Log("The server may not work correctly.");
	}
}

void print_srvconsole( char *fmt, ... )
{
	va_list argptr;
	char string[256];
	va_start (argptr, fmt);
	vsnprintf (string, 255, fmt,argptr);
	string[255] = 0;
	va_end (argptr);
	SERVER_PRINT(string);
}

void* alloc_amxmemory(void** p, int size)
{
	*p = new unsigned char[ size ];
	return *p;
}

void free_amxmemory(void **ptr)
{
	delete[] *ptr;
	*ptr = 0;
}

int load_amxscript(AMX *amx, void **program, const char *filename, char error[64])
{
	*error = 0;
	CAmxxReader reader(filename, SMALL_CELL_SIZE / 8);
	if (reader.GetStatus() == CAmxxReader::Err_None)
	{
		size_t bufSize = reader.GetBufferSize();
		if (bufSize != 0)
		{
			*program = (void*) (new char[bufSize]);
			if (!*program)
			{
				strcpy(error, "Failed to allocate memory");
				return (amx->error = AMX_ERR_MEMORY);
			}
			reader.GetSection(*program);
		}
	}

	switch (reader.GetStatus())
	{
	case CAmxxReader::Err_None:
		break;
	case CAmxxReader::Err_FileOpen:
		strcpy(error, "Plugin file open error");
		return (amx->error = AMX_ERR_NOTFOUND);
	case CAmxxReader::Err_FileRead:
		strcpy(error, "Plugin file read error");
		return (amx->error = AMX_ERR_NOTFOUND);
	case CAmxxReader::Err_InvalidParam:
		strcpy(error, "Internal error: Invalid parameter");
		return (amx->error = AMX_ERR_NOTFOUND);
	case CAmxxReader::Err_FileInvalid:
		strcpy(error, "Invalid Plugin");
		return (amx->error = AMX_ERR_FORMAT);
	case CAmxxReader::Err_SectionNotFound:
		strcpy(error, "Searched section not found (.amxx)");
		return (amx->error = AMX_ERR_NOTFOUND);
	case CAmxxReader::Err_DecompressorInit:
		strcpy(error, "Decompressor initialization failed");
		return (amx->error = AMX_ERR_INIT);
	case CAmxxReader::Err_Decompress:
		strcpy(error, "Internal error: Decompress");
		return (amx->error = AMX_ERR_NOTFOUND);
	default:
		strcpy(error, "Unknown error");
		return (amx->error = AMX_ERR_NOTFOUND);
	}

	// check for magic
	AMX_HEADER *hdr = (AMX_HEADER*)*program;
	uint16_t magic = hdr->magic;
	amx_Align16(&magic);
	if (magic != AMX_MAGIC)
	{
		strcpy(error, "Invalid Plugin");
		return (amx->error = AMX_ERR_FORMAT);
	}

	int err;
	memset(amx, 0, sizeof(*amx));
	if ((err = amx_Init( amx, *program )) != AMX_ERR_NONE)
	{
		sprintf(error,"Load error %d (invalid file format or version)", err);
		return (amx->error = AMX_ERR_INIT);
	}


#ifdef JIT
	void *np = new char[ amx->code_size ];
	void *rt = new char[ amx->reloc_size ];
	if ( !np || (!rt && amx->reloc_size > 0) )
	{
		delete[] np;
		delete[] rt;
		strcpy(error,"Failed to initialize plugin");
		return (amx->error = AMX_ERR_INIT);
	}

	if (amx_InitJIT(amx, rt, np) == AMX_ERR_NONE) 
	{
		//amx->base = (unsigned char FAR *)realloc( np, amx->code_size );
		amx->base = new unsigned char[ amx->code_size ];
		if ( amx->base )
			memcpy( amx->base , np , amx->code_size );
		delete[] np;
		delete[] rt;
		delete[] *program;
		(*program) = amx->base;
		if ( *program == 0 ){
			strcpy(error,"Failed to allocate memory");
			return (amx->error = AMX_ERR_MEMORY);
		}
	}
	else 
	{
		delete[] np;
		delete[] rt;
		strcpy(error,"Failed to initialize plugin");
		return (amx->error = AMX_ERR_INIT_JIT);
	}

#endif

	CScript* aa  =  new CScript(amx,*program,filename);

	if ( aa == 0 )
	{
		strcpy(error,"Failed to allocate memory");
		return (amx->error = AMX_ERR_MEMORY);
	}

	g_loadedscripts.put( aa );
	amx->sysreq_d = 0;
	return set_amxnatives(amx,error);
}

int set_amxnatives(AMX* amx,char error[64])
{
	for ( CList<CModule>::iterator  a  = g_modules.begin(); a ; ++a )
	{
		for( CList<AMX_NATIVE_INFO*>::iterator cc = 
			(*a).m_Natives.begin(); cc; ++cc )
			amx_Register(amx, *cc , -1);
	}

	amx_Register(amx, string_Natives, -1);
	amx_Register(amx, float_Natives, -1);
	amx_Register(amx, file_Natives, -1);
	amx_Register(amx, amxmod_Natives, -1);
	amx_Register(amx, power_Natives, -1);
	amx_Register(amx, time_Natives, -1);
	amx_Register(amx, vault_Natives, -1);

	if ( amx_Register(amx, core_Natives, -1) != AMX_ERR_NONE )
	{
		sprintf(error,"Function not found (name \"%s\")",no_function);
		return (amx->error = AMX_ERR_NATIVE);
	}

	return AMX_ERR_NONE;
}


int unload_amxscript(AMX* amx, void** program)
{
	CList<CScript,AMX*>::iterator a = g_loadedscripts.find( amx  );
	if ( a ) a.remove();
	delete[] *program;
	*program = 0;
	return AMX_ERR_NONE;
}


AMX* get_amxscript(int id , void** code, const char** filename)
{
	CList<CScript,AMX*>::iterator a = g_loadedscripts.begin();
	while ( a && id-- )
		++a;
	if ( a ){
		*filename = (*a).getName();
		*code = (*a).getCode();
		return (*a).getAMX();
	}
	return 0;
}

const char* get_amxscriptname(AMX* amx)
{
	CList<CScript,AMX*>::iterator a = g_loadedscripts.find( amx  );
	return a ? (*a).getName() : "";
}

void get_modname(char* buffer )
{
	strcpy( buffer , g_mod_name.c_str() );
}

char* build_pathname(char *fmt, ... )
{
	static char string[256];

	int b;

	int a = b = snprintf(string , 255 ,

#ifndef __linux__
		"%s\\",
#else
		"%s/",
#endif
		g_mod_name.c_str());

	va_list argptr;
	va_start (argptr, fmt);
	a += vsnprintf (&string[a], 255 - a , fmt,argptr);
	string[ a ] = 0;
	va_end (argptr);

	char* path = &string[b];

	while (*path) 
	{
#ifndef __linux__
		if (*path == '/') *path = '\\';
#else
		if (*path == '\\') *path = '/';
#endif
		++path;
	}

	return string;
}


// build pathname based on addons dir
char* build_pathname_addons(char *fmt, ... )
{
	static char string[256];

	va_list argptr;
	va_start (argptr, fmt);
	vsnprintf (string, 255, fmt, argptr);
	va_end (argptr);

	char* path = string;

	while (*path) 
	{
#ifndef __linux__
		if (*path == '/') *path = '\\';
#else
		if (*path == '\\') *path = '/';
#endif
		++path;
	}

	return string;
}

int add_amxnatives(module_info_s* info,AMX_NATIVE_INFO*natives)
{
	CList<CModule>::iterator  a  = g_modules.begin();

	while ( a )
	{
		if (  (*a).getInfo() == info )
		{
			AMX_NATIVE_INFO** aa = new AMX_NATIVE_INFO*(natives);
			if ( aa == 0 ) return AMX_ERR_NATIVE;
			(*a).m_Natives.put( aa  );
			return AMX_ERR_NONE;
		}

		++a;
	}

	return AMX_ERR_NATIVE;
}


bool validFile(const char* file)
{
	const char* a = 0;
	while(*file)
		if (*file++=='.')
			a = file;
#ifndef __linux__
	return (a && !strcmp(a,"dll"));
#else
	return (a && !strcmp(a,"so"));
#endif
}

int loadModules(const char* filename)
{
	File fp( build_pathname("%s",filename), "r"  );

	if ( !fp )
	{
		AMXXLOG_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
		return 0;
	}

	char line[256], moduleName[256];
	int loaded = 0;

	while ( fp.getline( line ,  255  ) )
	{
		*moduleName = 0;
		sscanf(line,"%s",moduleName);
		if (!isalnum(*moduleName) || !validFile(moduleName) )  
			continue;

		char* pathname = build_pathname("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxx/modules"), line);

		CList<CModule>::iterator a = g_modules.find(  pathname  );

		if ( a ) continue; // already loaded

		CModule* cc = new CModule( pathname  );

		if ( cc == 0 ) return loaded;

		cc->queryModule();

		switch(  cc->getStatusValue()  )  {
	  case MODULE_BADLOAD:
		  report_error( 1 , "[AMXX] Module is not a valid library (file \"%s\")",pathname );
		  break;
	  case MODULE_NOINFO:
		  report_error( 1 ,"[AMXX] Couldn't find info. about module (file \"%s\")",pathname );
		  break;
	  case MODULE_NOQUERY:
		  report_error( 1 , "[AMXX] Couldn't find \"AMX_Query\" or \"AMXX_Query\" (file \"%s\")", pathname );
		  break;
	  case MODULE_NOATTACH:
		  report_error( 1 , "[AMXX] Couldn't find \"%s\" (file \"%s\")", cc->isAmxx() ? "AMXX_Attach" : "AMX_Attach", pathname );
		  break;
	  case MODULE_OLD:
		  report_error( 1 , "[AMXX] Module has a different interface version (file \"%s\")",pathname );
		  break;
	  case MODULE_NEWER:
		  report_error(1, "[AMXX] Module has a newer interface version (file \"%s\"). Please download a new amxmodx.", pathname);
		  break;
	  case MODULE_INTERROR:
		  report_error(1, "[AMXX] Internal error during module load (file \"%s\")", pathname);
		  break;
	  case MODULE_NOT64BIT:
		  report_error(1, "[AMXX] Module \"%s\" is not 64 bit compatible.", pathname);
		  break;
	  default:
		  ++loaded; 
		}

		g_modules.put( cc );
	}

	return loaded;
}

void detachModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();

	while ( a )
	{
		(*a).detachModule();
		a.remove();
	}
}

void detachReloadModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();

	while ( a )
	{
		if ( (*a).isReloadable() )
		{
			(*a).detachModule();
			a.remove();
			continue;
		}

		++a;
	}

}

void attachModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();

	while ( a )
	{
		bool retVal = (*a).attachModule();
		if ((*a).isAmxx() && !retVal)
		{
			switch ((*a).getStatusValue())
			{
			case MODULE_FUNCNOTPRESENT:
				report_error(1, "[AMXX] Module requested a not exisitng function (file \"%s\")%s%s%s", (*a).getFilename(), (*a).getMissingFunc() ? " (func \"" : "",
					(*a).getMissingFunc() ? (*a).getMissingFunc() : "", (*a).getMissingFunc() ? "\")" : "");
				break;
			case MODULE_INTERROR:
				report_error(1, "[AMXX] Internal error during module load (file \"%s\")", (*a).getFilename());
				break;
			case MODULE_BADLOAD:
				report_error( 1 , "[AMXX] Module is not a valid library (file \"%s\")", (*a).getFilename());
				break;
			default:
				break;
			}
		}

		++a;
	}
}

const char* strip_name( const char* a )
{
	const char* ret = a;
	while(*a){
		if ( *a== '/' || *a=='\\' ){
			ret = ++a;
			continue;
		}
		++a;
	}
	return ret;
}

void attachMetaModModules(PLUG_LOADTIME now, const char* filename)
{
	File fp( build_pathname("%s",filename), "r"  );

	if ( !fp )
	{
		AMXXLOG_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
		return;
	}

	char line[256], moduleName[256];
	DLHANDLE module;

	while ( fp.getline( line ,  255  ) )
	{
		*moduleName = 0;
		sscanf(line,"%s",moduleName);

		if (!isalnum(*moduleName) || !validFile(moduleName) )  
			continue;

		char* pathname = build_pathname("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxx/modules"), line);
		char* mmpathname = build_pathname_addons("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxx/modules"), line);
		module = DLLOAD( pathname ); // link dll

		if ( module )
		{
			int a = (int)DLPROC(module,"Meta_Attach");
			DLFREE(module);

			if ( a )
			{
				g_FakeMeta.AddPlugin(mmpathname);
			}
		}
	}
	g_FakeMeta.Meta_Query(gpMetaUtilFuncs);
	g_FakeMeta.Meta_Attach(now, gpMetaGlobals, gpGamedllFuncs);
}



// Get the number of running modules
int countModules(CountModulesMode mode)
{
	CList<CModule>::iterator iter;
	int num;
	switch (mode)
	{
	case CountModules_All:
		return g_modules.size();
	case CountModules_Running:
		iter = g_modules.begin();
		num = 0;
		while (iter)
		{
			if ((*iter).getStatusValue() == MODULE_LOADED)
				++num;
			++iter;
		}
		return num;
	case CountModules_Stopped:
		iter = g_modules.begin();
		num = 0;
		while (iter)
		{
			if ((*iter).getStatusValue() != MODULE_LOADED)
				++num;
			++iter;
		}
		return num;
	}
	return 0;
}

// Call all modules' AMXX_PluginsLoaded functions
void modules_callPluginsLoaded()
{
	CList<CModule>::iterator iter = g_modules.begin();
	while (iter)
	{
		(*iter).CallPluginsLoaded();
		++iter;
	}
}

// new functions

int MNF_AddNatives(AMX_NATIVE_INFO* natives)
{
	CList<CModule>::iterator a = g_modules.begin();

	if (!g_CurrentlyCalledModule || g_ModuleCallReason != ModuleCall_Attach)
		return FALSE;				// may only be called from attach

	// This is needed so that CList can free it ;]
	AMX_NATIVE_INFO** pPtr = new AMX_NATIVE_INFO*(natives);
	if (!pPtr)
		return FALSE;

	g_CurrentlyCalledModule->m_Natives.put(pPtr);
	return TRUE;
}

const char *MNF_GetModname(void)
{
	// :TODO: Do we have to do this??
	static char buffer[64];
	strcpy(buffer, g_mod_name.c_str());
	return buffer;
}

AMX *MNF_GetAmxScript(int id)
{
	CList<CScript,AMX*>::iterator iter = g_loadedscripts.begin();
	while (iter && id--)
		++iter;

	return (*iter).getAMX();
}

const char *MNF_GetAmxScriptName(int id)
{
	CList<CScript,AMX*>::iterator iter = g_loadedscripts.begin();
	while (iter && id--)
		++iter;

	return (*iter).getName();
}

int MNF_FindAmxScriptByName(const char *name)
{
	CList<CScript,AMX*>::iterator iter = g_loadedscripts.begin();
	bool found = false;
	int i = 0;
	while (iter)
	{
		if (stricmp((*iter).getName(), name) == 0)
		{
			found = true;
			break;
		}
		++iter;
		++i;
	}
	if (!found)
		return -1;
	return i;
}

int MNF_FindAmxScriptByAmx(const AMX *amx)
{
	CList<CScript,AMX*>::iterator iter = g_loadedscripts.begin();
	bool found = false;
	int i = 0;
	while (iter)
	{
		if (amx == (*iter).getAMX())
		{
			found = true;
			break;
		}
		++iter;
		++i;
	}
	if (!found)
		return -1;
	return i;
}

char *MNF_GetAmxString(AMX *amx, cell amx_addr, int bufferId, int *pLen)
{
	int len;
	char *retVal = get_amxstring(amx, amx_addr, bufferId, len);
	if (pLen)
		*pLen = len;
	return retVal;
}

int MNF_GetAmxStringLen(const cell *ptr)
{
	register int c = 0;
	while(ptr[c])
		++c;
	return c;
}

char *MNF_FormatAmxString(AMX *amx, cell *params, int startParam, int *pLen)
{
	int len;
	char *retVal = format_amxstring(amx, params, startParam, len);
	if (pLen)
		*pLen = len;
	return retVal;
}

void MNF_CopyAmxMemory(cell * dest, const cell * src, int len)
{
	memcpy((void*)dest, (const void *)src, (size_t)len*sizeof(cell));
}

int MNF_IsPlayerValid(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	return (pPlayer->initialized) ? 1 : 0;
}
const char * MNF_GetPlayerName(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;
	return GET_PLAYER_POINTER_I(id)->name.c_str();
}
const char * MNF_GetPlayerIP(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;
	return GET_PLAYER_POINTER_I(id)->ip.c_str();
}
int MNF_IsPlayerInGame(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->ingame ? 1 : 0;
}
int MNF_IsPlayerBot(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->IsBot() ? 1 : 0;
}
int MNF_IsPlayerAuthorized(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->authorized ? 1 : 0;
}
float MNF_GetPlayerTime(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0.0f;
	return GET_PLAYER_POINTER_I(id)->time;
}
float MNF_GetPlayerPlayTime(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0.0f;
	return GET_PLAYER_POINTER_I(id)->playtime;
}
int MNF_GetPlayerCurweapon(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->current;
}
int MNF_GetPlayerTeamID(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->teamId;
}
int MNF_GetPlayerDeaths(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->deaths;
}
int MNF_GetPlayerMenu(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->menu;
}
int MNF_GetPlayerKeys(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->keys;
}
int MNF_IsPlayerAlive(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return GET_PLAYER_POINTER_I(id)->IsAlive() ? 1 : 0;
}
float MNF_GetPlayerFrags(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0.0f;
	return GET_PLAYER_POINTER_I(id)->pEdict->v.frags;
}
int MNF_IsPlayerConnecting(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer * pPlayer = GET_PLAYER_POINTER_I(id);
	return (!pPlayer->ingame && pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0)) ? 1 : 0;
}
int MNF_IsPlayerHLTV(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return (GET_PLAYER_POINTER_I(id)->pEdict->v.flags & FL_PROXY) ? 1 : 0;
}
float MNF_GetPlayerArmor(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0.0f;
	return (GET_PLAYER_POINTER_I(id)->pEdict->v.armorvalue);
}
float MNF_GetPlayerHealth(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	return (GET_PLAYER_POINTER_I(id)->pEdict->v.health);
}

void MNF_HiddenStuff()
{
	// :TODO:
}

cell MNF_RealToCell(REAL x)
{
	return *(cell*)&x;
}

REAL MNF_CellToReal(cell x)
{
	return *(REAL*)&x;
}

void MNF_Log(const char *fmt, ...)
{
	// :TODO: Overflow possible here
	char msg[3072];
	va_list arglst;
	va_start(arglst, fmt);
	vsprintf(msg, fmt, arglst);
	va_end(arglst);
	AMXXLOG_Log("%s", msg);
}

void MNF_MergeDefinitionFile(const char *file)
{
	g_langMngr.MergeDefinitionFile(file);
}

// Fnptr Request function for the new interface
const char *g_LastRequestedFunc = NULL;
#define REGISTER_FUNC(name, func) { name, (void*)func },
void *Module_ReqFnptr(const char *funcName)
{
	// func table
	struct Func_s
	{
		const char *name;
		void *ptr;
	};
	static Func_s functions[] = {
		// Misc
			REGISTER_FUNC("BuildPathname", build_pathname)
			REGISTER_FUNC("PrintSrvConsole", print_srvconsole)
			REGISTER_FUNC("GetModname", MNF_GetModname)
			REGISTER_FUNC("Log", MNF_Log)
			REGISTER_FUNC("MergeDefinitionFile", MNF_MergeDefinitionFile)

			// Amx scripts loading / unloading / managing
			REGISTER_FUNC("GetAmxScript", MNF_GetAmxScript)
			REGISTER_FUNC("GetAmxScriptName", MNF_GetAmxScriptName)
			REGISTER_FUNC("FindAmxScriptByName", MNF_FindAmxScriptByName)
			REGISTER_FUNC("FindAmxScriptByAmx", MNF_FindAmxScriptByAmx)
			REGISTER_FUNC("LoadAmxScript", load_amxscript)
			REGISTER_FUNC("UnloadAmxScript", unload_amxscript)

			// String / mem in amx scripts support
			REGISTER_FUNC("SetAmxString", set_amxstring)
			REGISTER_FUNC("GetAmxString", MNF_GetAmxString)
			REGISTER_FUNC("GetAmxStringLen", MNF_GetAmxStringLen)
			REGISTER_FUNC("FormatAmxString", MNF_FormatAmxString)
			REGISTER_FUNC("CopyAmxMemory", MNF_CopyAmxMemory)
			REGISTER_FUNC("GetAmxAddr", get_amxaddr)

			// other amx stuff
			REGISTER_FUNC("amx_Exec", amx_Exec)
			REGISTER_FUNC("amx_Execv", amx_Execv)
			REGISTER_FUNC("amx_Allot", amx_Allot)
			REGISTER_FUNC("amx_FindPublic", amx_FindPublic)

			// Natives / Forwards
			REGISTER_FUNC("AddNatives", MNF_AddNatives)
			REGISTER_FUNC("RaiseAmxError", amx_RaiseError)
			REGISTER_FUNC("RegisterForward", registerForward)
			REGISTER_FUNC("RegisterSPForward", registerSPForward)
			REGISTER_FUNC("RegisterSPForwardByName", registerSPForwardByName)
			REGISTER_FUNC("UnregisterSPForward", unregisterSPForward)
			REGISTER_FUNC("ExecuteForward", executeForwards)
			REGISTER_FUNC("PrepareCellArray", prepareCellArray)
			REGISTER_FUNC("PrepareCharArray", prepareCharArray)

			// Player
			REGISTER_FUNC("IsPlayerValid", MNF_IsPlayerValid)
			REGISTER_FUNC("GetPlayerName", MNF_GetPlayerName)
			REGISTER_FUNC("GetPlayerIP", MNF_GetPlayerIP)
			REGISTER_FUNC("IsPlayerInGame", MNF_IsPlayerInGame)
			REGISTER_FUNC("IsPlayerBot", MNF_IsPlayerBot)
			REGISTER_FUNC("IsPlayerAuthorized", MNF_IsPlayerAuthorized)
			REGISTER_FUNC("GetPlayerTime", MNF_GetPlayerTime)
			REGISTER_FUNC("GetPlayerPlayTime", MNF_GetPlayerPlayTime)
			REGISTER_FUNC("GetPlayerCurweapon", MNF_GetPlayerCurweapon)
			REGISTER_FUNC("GetPlayerTeamID", MNF_GetPlayerTeamID)
			REGISTER_FUNC("GetPlayerDeaths", MNF_GetPlayerDeaths)
			REGISTER_FUNC("GetPlayerFrags", MNF_GetPlayerFrags)
			REGISTER_FUNC("GetPlayerMenu", MNF_GetPlayerMenu)
			REGISTER_FUNC("GetPlayerKeys", MNF_GetPlayerKeys)
			REGISTER_FUNC("IsPlayerAlive", MNF_IsPlayerAlive)
			REGISTER_FUNC("IsPlayerConnecting", MNF_IsPlayerConnecting)
			REGISTER_FUNC("IsPlayerHLTV", MNF_IsPlayerHLTV)
			REGISTER_FUNC("GetPlayerArmor", MNF_GetPlayerArmor)
			REGISTER_FUNC("GetPlayerHealth", MNF_GetPlayerHealth)
			REGISTER_FUNC("CellToReal", MNF_CellToReal)
			REGISTER_FUNC("RealToCell", MNF_RealToCell)

#ifdef MEMORY_TEST
			REGISTER_FUNC("Allocator", m_allocator)
			REGISTER_FUNC("Deallocator", m_deallocator)
			REGISTER_FUNC("Reallocator", m_reallocator)
#endif // MEMORY_TEST

			REGISTER_FUNC("Haha_HiddenStuff", MNF_HiddenStuff)
	};

	// code
	if (!g_CurrentlyCalledModule || g_ModuleCallReason != ModuleCall_Attach)
	{
		return NULL;
	}

	g_LastRequestedFunc = funcName;
	for (unsigned int i = 0; i < (sizeof(functions) / sizeof(Func_s)); ++i)
	{
		if (strcmp(funcName, functions[i].name) == 0)
			return functions[i].ptr;
	}
	return NULL;
}