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

CList<CModule,const char*> g_modules;
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

int load_amxscript(AMX *amx, void **program, const char *filename, char error[64], int debug)
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
	case CAmxxReader::Err_OldFile:
		strcpy(error, "Plugin uses deprecated format.  Update compiler");
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

	if ( (int)CVAR_GET_FLOAT("amx_debug") >= 2 || debug)
	{
		//automatic debug mode
		hdr->flags |= AMX_FLAG_LINEOPS;
		hdr->flags |= AMX_FLAG_DEBUG;
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

const char *StrCaseStr(const char *as, const char *bs)
{
	static char a[256];
	static char b[256];
	unsigned int i = 0;
	unsigned int len = strlen(as);

	if (len > 254)
		len = 254;

	for (i=0; i<len; i++)
	{
		a[i] = tolower(as[i]);
	}
	a[len] = 0;

	len = strlen(bs);

	if (len > 254)
		len = 254;

	for (i=0; i<len; i++)
	{
		b[i] = tolower(bs[i]);
	}
	b[len] = 0;

	return strstr(a,b);
}

//BAILOPAN
int CheckModules(AMX *amx, char error[64])
{
	int idx = 0, flag = -1;
	if (amx_FindPublic(amx, "plugin_modules", &idx) == AMX_ERR_NONE)
	{
		cell retVal = 0;
		int err = 0;
		if ( (err = amx_Exec(amx, &retVal, idx, 0)) == AMX_ERR_NONE )
		{
			unsigned int i = 0;
			while (!CurModuleList.empty())
			{
				if (!flag)
				{
					CurModuleList.pop();
					continue;
				}
				//assume module is not found
				flag = 0;
				for (CList<CModule,const char *>::iterator pMod = g_modules.begin(); pMod; ++pMod)
				{
					if (strcmpi(CurModuleList.front().c_str(), "dbi") == 0)
					{
						if (StrCaseStr( (*pMod).getName(), "sql") || strstr( (*pMod).getName(), "dbi" ))
						{
							// the module checks in
							flag = 1;
							break;
						}
					} else {
						if (strcmpi( (*pMod).getName(), CurModuleList.front().c_str() ) == 0)
						{
							flag = 1;
							break;
						}
					}
				}
				//module was not found
				if (!flag)
				{
					sprintf(error, "Module \"%s\" required for plugin.  Check modules.ini.", CurModuleList.front().c_str());
				}
				CurModuleList.pop();
			}
		} else {
			AMXXLOG_Log("[AMXX] Run time error %d on line %ld during module check.", err, amx->curline);
			//could not execute
			return -1;	//bad! very bad!
		}
	} else {
		return -1;
	}

	return flag;
}

int set_amxnatives(AMX* amx,char error[64])
{
	for ( CList<CModule,const char *>::iterator  a  = g_modules.begin(); a ; ++a )
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
		//HACKHACK - if we get here, nullify the plugin's native table
		//then reregister the one native we need
		// - BAILOPAN
		String save;
		save.assign(no_function);
		amx_NullNativeTable(amx);
		AMX_NATIVE_INFO p[] = {
			{ "require_module",	require_module },
			{ NULL,				NULL },
		};
		amx_Register(amx, p, -1);
		if (CheckModules(amx, error) == -1 || *error == 0)
		{
			sprintf(error,"Plugin uses an unknown function (name \"%s\") - check your modules.ini.",save.c_str());
		}
		return (amx->error = AMX_ERR_NATIVE);
	}

	CheckModules(amx, error);

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
	CList<CModule,const char *>::iterator  a  = g_modules.begin();

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

void ConvertModuleName(const char *pathString, String &path)
{
#if SMALL_CELL_SIZE==64
	char *ptr = strstr(pathString, "i386");
	if (ptr)
	{
		//attempt to fix the binary name
		*ptr = 0;
		path.assign(pathString);
		path.append("amd64.so");
	} else {
		ptr = strstr(pathString, ".dll");
		if (ptr)
		{
			*ptr = 0;
			path.assign(pathString);
			path.append("_amd64.so");
		} else {
			ptr = strstr(pathString, ".so");
			if (ptr)
			{
				path.assign(pathString);
			} else {
				//no extension at all
				path.assign(pathString);
				path.append("_amd64.so");
			}
		}
	}
#else
#ifdef __linux__
	char *ptr = strstr(pathString, "amd64");
	if (ptr) 
	{
		//attempt to fix the binary name
		*ptr = 0;
		path.assign(pathString);
		path.append("i386.so");
	} else {
		ptr = strstr(pathString, ".dll");
		if (ptr)
		{
			*ptr = 0;
			path.assign(pathString);
			path.append("_i386.so");
		} else {
			//check to see if this file even has an extension
			ptr = strstr(pathString, ".so");
			if (ptr)
			{
				path.assign(pathString);
			} else {
				path.assign(pathString);
				path.append("_i386.so");
			}
		}
	}
#else
	char *ptr = strstr(pathString, ".dll");
	if (ptr)
	{
		path.assign(pathString);
	} else {
		//prevent this from loading .so too
		ptr = strstr(pathString, ".so");
		if (ptr)
		{
			int i = 0, len = strlen(pathString), c = -1;
			for (i=len-1; i>=0; i--)
			{
				//cut off at first _
				if (pathString[i] == '_')
				{
					//make sure this is a valid _
					if (i == len-1 || strncmp(&(pathString[i+1]), "amxx", 4) == 0)
						break;
					c = i;
					break;
				}
			}
			*ptr = 0;
			if (c == -1)
			{
				path.assign(pathString);
				path.append(".dll");
			} else {
				ptr = (char *)&(pathString[c]);
				*ptr = 0;
				path.assign(pathString);
				path.append(".dll");
			}
		} else {
			path.assign(pathString);
			path.append(".dll");
		}
	}
#endif //__linux__
#endif //SMALL_CELL_SIZE==64
}

int loadModules(const char* filename)
{
	FILE *fp = fopen(build_pathname("%s",filename), "rt");

	if ( !fp )
	{
		AMXXLOG_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
		return 0;
	}

	char moduleName[256];
	char pathString[512];
	String line;
	int loaded = 0;

	String path;

	while (!feof(fp))
	{
		if (!line._fread(fp) || line.size() < 1)
			continue;
		line.trim();
		*moduleName = 0;
		if (sscanf(line.c_str(),"%s",moduleName) == EOF)
			continue;
		if (moduleName[0] == ';')  
			continue;


		char* pathname = build_pathname("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxmodx/modules"), moduleName);
		strcpy(pathString, pathname);

		path.assign("");

		ConvertModuleName(pathString, path);	

		if (!validFile(path.c_str()))
			continue;

		CList<CModule,const char *>::iterator a = g_modules.find(  path.c_str() );

		if ( a ) continue; // already loaded

		CModule* cc = new CModule( path.c_str() );

		if ( cc == 0 )
		{
			fclose(fp);
			return loaded;
		}

		cc->queryModule();

		switch(  cc->getStatusValue()  )  {
	  case MODULE_BADLOAD:
		  report_error( 1 , "[AMXX] Module is not a valid library (file \"%s\")", path.c_str());
		  break;
	  case MODULE_NOINFO:
		  report_error( 1 ,"[AMXX] Couldn't find info. about module (file \"%s\")", path.c_str());
		  break;
	  case MODULE_NOQUERY:
		  report_error( 1 , "[AMXX] Couldn't find \"AMX_Query\" or \"AMXX_Query\" (file \"%s\")", path.c_str());
		  break;
	  case MODULE_NOATTACH:
		  report_error( 1 , "[AMXX] Couldn't find \"%s\" (file \"%s\")", cc->isAmxx() ? "AMXX_Attach" : "AMX_Attach", path.c_str());
		  break;
	  case MODULE_OLD:
		  report_error( 1 , "[AMXX] Module has a different interface version (file \"%s\")",path.c_str());
		  break;
	  case MODULE_NEWER:
		  report_error(1, "[AMXX] Module has a newer interface version (file \"%s\"). Please download a new amxmodx.", path.c_str());
		  break;
	  case MODULE_INTERROR:
		  report_error(1, "[AMXX] Internal error during module load (file \"%s\")", path.c_str());
		  break;
	  case MODULE_NOT64BIT:
		  report_error(1, "[AMXX] Module \"%s\" is not 64 bit compatible.", path.c_str());
		  break;
	  default:
		  ++loaded; 
		}

		g_modules.put( cc );

	}

	fclose(fp);

	return loaded;
}

void detachModules()
{
	CList<CModule,const char *>::iterator  a  = g_modules.begin();

	while ( a )
	{
		(*a).detachModule();
		a.remove();
	}
}

void detachReloadModules()
{
	CList<CModule,const char *>::iterator  a  = g_modules.begin();

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
	CList<CModule,const char *>::iterator  a  = g_modules.begin();

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
	String modPath, mmPath;
	DLHANDLE module;

	while ( fp.getline( line ,  255  ) )
	{
		*moduleName = 0;
		sscanf(line,"%s",moduleName);

		if (!isalnum(*moduleName))  
			continue;

		char* pathname = build_pathname("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxmodx/modules"), line);
		char* mmpathname = build_pathname_addons("%s/%s", get_localinfo("amxx_modulesdir", "addons/amxmodx/modules"), line);

		ConvertModuleName(pathname, modPath);
		ConvertModuleName(mmpathname, mmPath);

		CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin();

		//prevent double loading
		int foundFlag = 0;

		while (iter)
		{
			if ( strcmp( (*iter).GetPath(), mmPath.c_str() ) == 0 )
			{
				foundFlag = 1;
				break;
			}
			++iter;
		}

		if (foundFlag)
			continue;
		
		module = DLLOAD( modPath.c_str() ); // link dll

		if ( module )
		{
			int a = (int)DLPROC(module,"Meta_Attach");
			DLFREE(module);

			if ( a )
			{
				g_FakeMeta.AddPlugin(mmPath.c_str());
			}
		}
	}
	g_FakeMeta.Meta_Query(gpMetaUtilFuncs);
	g_FakeMeta.Meta_Attach(now, gpMetaGlobals, gpGamedllFuncs);
}



// Get the number of running modules
int countModules(CountModulesMode mode)
{
	CList<CModule,const char *>::iterator iter;
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
	CList<CModule,const char *>::iterator iter = g_modules.begin();
	while (iter)
	{
		(*iter).CallPluginsLoaded();
		++iter;
	}
}

// new functions

int MNF_AddNatives(AMX_NATIVE_INFO* natives)
{
	CList<CModule,const char *>::iterator a = g_modules.begin();

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

	if (iter == NULL)
		return NULL;
	return (*iter).getAMX();
}

const char *MNF_GetAmxScriptName(int id)
{
	CList<CScript,AMX*>::iterator iter = g_loadedscripts.begin();
	while (iter && id--)
		++iter;

	if (iter == NULL)
		return NULL;
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

int MNF_GetPlayerFlags(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	return (pPlayer->flags[0]);
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

//by BAILOPAN
//  generic error printing routine
void GenericError(AMX *amx, int err, int line, char buf[], const char *file)
{
	static const char *amx_errs[] =
	{
		NULL,
		"forced exit",
		"assertion failed",
		"stack error",
		"index out of bounds",
		"memory access",
		"invalid instruction",
		"stack low",
		"heap low",
		"callback",
		"native",
		"divide",
		"sleep",
		NULL,
		NULL,
		NULL,
		"out of memory", //16
		"bad file format",
		"bad file version",
		"function not found",
		"invalid entry point",
		"debugger cannot run",
		"plugin un or re-initialized",
		"userdata table full",
		"JIT failed to initialize",
		"parameter error",
		"domain error",
	};
	//does this plugin have line ops?
	const char *geterr = NULL;
	if (err > 26 || err < 0)
		geterr = NULL;
	else
		geterr = amx_errs[err];
	if (!(amx->flags & AMX_FLAG_LINEOPS))
	{
		if (geterr == NULL)
		{
			sprintf(buf, "Run time error %d (plugin \"%s\" - debug not enabled).", err, g_plugins.findPluginFast(amx)->getName());
		} else {
			sprintf(buf, "Run time error %d (%s) (plugin \"%s\") - debug not enabled.", err, geterr, g_plugins.findPluginFast(amx)->getName());
		}
	} else {
		if (geterr == NULL)
		{
			sprintf(buf, "Run time error %d on line %d (%s \"%s\").", err, line, (file?"file":"plugin"), (file?file:g_plugins.findPluginFast(amx)->getName()));
		} else {
			sprintf(buf, "Run time error %d (%s) on line %d (%s \"%s\").", err, geterr, line, (file?"file":"plugin"), (file?file:g_plugins.findPluginFast(amx)->getName()));
		}
	}
}

//by BAILOPAN
// debugger engine front end
void LogError(AMX *amx, int err, const char *fmt, ...)
{
	//does this plugin have debug info?
	va_list arg;
	AMX_DBG *dbg = (AMX_DBG *)(amx->userdata[0]);
	static char buf[1024];
	static char vbuf[1024];
	*buf = 0;
	*vbuf = 0;

	va_start(arg, fmt);
	vsprintf(vbuf, fmt, arg);
	va_end(arg);

	if (!dbg || !(dbg->tail))
	{
		if (dbg && amx->curfile < dbg->numFiles && amx->curfile >= 0)
		{
			GenericError(amx, err, amx->curline, buf, dbg->files[amx->curfile]);
		} else {
			GenericError(amx, err, amx->curline, buf, NULL);
		}
		AMXXLOG_Log("[AMXX] %s", buf);
		if (*vbuf)
		{
			AMXXLOG_Log("%s", vbuf);
		}
	} else {
		AMX_TRACE *t = dbg->tail;
		AMX_DEBUGCALL tracer = (AMX_DEBUGCALL)(amx->userdata[1]);
		//actuall
		cell line = amx->curline;
		cell file = amx->curfile;
		int i = 0;
		GenericError(amx, err, line, buf, NULL);
		AMXXLOG_Log("[AMXX] %s", buf);
		if (*vbuf)
		{
			AMXXLOG_Log("%s", vbuf);
		}
		AMXXLOG_Log("[AMXX] Debug Trace =>");
		//log the error right away
		if (file >= dbg->numFiles || file < 0)
		{
            AMXXLOG_Log("[AMXX]       [%d] Line %d, File \"%s\"", i++, line, g_plugins.findPluginFast(amx)->getName());
		} else {
			AMXXLOG_Log("[AMXX]       [%d] Line %d, File \"%s\"", i++, line, dbg->files[file]);
		}
		while (t != NULL)
		{
			line = t->line;
			file = t->file;
			if (file >= dbg->numFiles)
			{
				AMXXLOG_Log("[AMXX]       [%d] Line %d, File \"%s\"", i++, line, g_plugins.findPluginFast(amx)->getName());	
			} else {
				AMXXLOG_Log("[AMXX]       [%d] Line %d, File \"%s\"", i++, line, dbg->files[file]);	
			}
			if (tracer)
				(tracer)(amx, 1);		//pop	
			t = dbg->tail;
		}
	}
}

void MNF_MergeDefinitionFile(const char *file)
{
	g_langMngr.MergeDefinitionFile(file);
}

edict_t* MNF_GetPlayerEdict(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;
	return (GET_PLAYER_POINTER_I(id)->pEdict);
}

const char *MNF_Format(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	const char *retVal = g_langMngr.FormatString(fmt, ap);
	va_end(ap);
	return retVal;
}

const char *MNF_GetPlayerTeam(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;

	return (GET_PLAYER_POINTER_I(id)->team.c_str());
}

#ifndef MEMORY_TEST
void *MNF_Allocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc, const unsigned int allocationType, const size_t reportedSize)
{
	return malloc(reportedSize);
}

void *MNF_Reallocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc, const unsigned int reallocationType, const size_t reportedSize, void *reportedAddress)
{
	return realloc(reportedAddress, reportedSize);
}

void MNF_Deallocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc, const unsigned int deallocationType, void *reportedAddress)
{
	free(reportedAddress);
}
#endif

// 09/18/2004 : added these two funcs that default to copyBack=false so we don't break all modules
cell MNF_PrepareCellArray(cell *ptr, unsigned int size)
{
	return prepareCellArray(ptr, size, false);
}

cell MNF_PrepareCharArray(char *ptr, unsigned int size)
{
	return prepareCharArray(ptr, size, false);
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
			REGISTER_FUNC("LogError", LogError)
			REGISTER_FUNC("MergeDefinitionFile", MNF_MergeDefinitionFile)
			REGISTER_FUNC("Format", MNF_Format)

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
			REGISTER_FUNC("amx_FindNative", amx_FindNative)

			// Natives / Forwards
			REGISTER_FUNC("AddNatives", MNF_AddNatives)
			REGISTER_FUNC("RaiseAmxError", amx_RaiseError)
			REGISTER_FUNC("RegisterForward", registerForward)
			REGISTER_FUNC("RegisterSPForward", registerSPForward)
			REGISTER_FUNC("RegisterSPForwardByName", registerSPForwardByName)
			REGISTER_FUNC("UnregisterSPForward", unregisterSPForward)
			REGISTER_FUNC("ExecuteForward", executeForwards)
			REGISTER_FUNC("PrepareCellArray", MNF_PrepareCellArray)
			REGISTER_FUNC("PrepareCharArray", MNF_PrepareCharArray)
			REGISTER_FUNC("PrepareCellArrayA", prepareCellArray)
			REGISTER_FUNC("PrepareCharArrayA", prepareCharArray)

			// Player
			REGISTER_FUNC("GetPlayerFlags", MNF_GetPlayerFlags)
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
			REGISTER_FUNC("GetPlayerTeam", MNF_GetPlayerTeam)
			REGISTER_FUNC("GetPlayerDeaths", MNF_GetPlayerDeaths)
			REGISTER_FUNC("GetPlayerFrags", MNF_GetPlayerFrags)
			REGISTER_FUNC("GetPlayerMenu", MNF_GetPlayerMenu)
			REGISTER_FUNC("GetPlayerKeys", MNF_GetPlayerKeys)
			REGISTER_FUNC("IsPlayerAlive", MNF_IsPlayerAlive)
			REGISTER_FUNC("IsPlayerConnecting", MNF_IsPlayerConnecting)
			REGISTER_FUNC("IsPlayerHLTV", MNF_IsPlayerHLTV)
			REGISTER_FUNC("GetPlayerArmor", MNF_GetPlayerArmor)
			REGISTER_FUNC("GetPlayerHealth", MNF_GetPlayerHealth)
			REGISTER_FUNC("GetPlayerEdict", MNF_GetPlayerEdict)
			REGISTER_FUNC("CellToReal", MNF_CellToReal)
			REGISTER_FUNC("RealToCell", MNF_RealToCell)

#ifdef MEMORY_TEST
			REGISTER_FUNC("Allocator", m_allocator)
			REGISTER_FUNC("Deallocator", m_deallocator)
			REGISTER_FUNC("Reallocator", m_reallocator)
#else
			REGISTER_FUNC("Allocator", MNF_Allocator)
			REGISTER_FUNC("Deallocator", MNF_Deallocator)
			REGISTER_FUNC("Reallocator", MNF_Reallocator)
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
