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

#ifdef __linux__
#include <malloc.h>
#include <stdlib.h>
#include <sys/mman.h>
#include "sclinux.h"
#endif

#include "amxmodx.h"
#include "osdep.h"			// sleep, etc
#include "CFile.h"
#include "amxxfile.h"
#include "amxdbg.h"
#include "newmenus.h"
#include "natives.h"
#include "debugger.h"
#include "optimizer.h"
#include "binlog.h"
#include "libraries.h"
#include "messages.h"

CList<CModule, const char*> g_modules;
CList<CScript, AMX*> g_loadedscripts;

CModule *g_CurrentlyCalledModule = NULL;	// The module we are in at the moment; NULL otherwise

// also NULL for non-amxx modules
// This is needed so we know which module called a function
ModuleCallReason g_ModuleCallReason;

extern const char* no_function;				// stupid work around

bool DirExists(const char *dir)
{
#if defined WIN32 || defined _WIN32
	DWORD attr = GetFileAttributes(dir);
	
	if (attr == INVALID_FILE_ATTRIBUTES)
		return false;
	
	if (attr & FILE_ATTRIBUTE_DIRECTORY)
		return true;
	
#else
	struct stat s;
	
	if (stat(dir, &s) != 0)
		return false;
	
	if (S_ISDIR(s.st_mode))
		return true;
#endif

	return false;
}

void report_error(int code, char* fmt, ...)
{
	va_list argptr;
	char string[256];
	*string = 0;
	va_start(argptr, fmt);
	vsnprintf(string, 255, fmt, argptr);
	string[255] = 0;
	va_end(argptr);
	
	if (*string)
	{
		AMXXLOG_Log("Error:");
		AMXXLOG_Log(string);
	} else {
		AMXXLOG_Log("!!! There was an unexpected module error.");
		AMXXLOG_Log("The server may not work correctly.");
	}
}

void print_srvconsole(char *fmt, ...)
{
	va_list argptr;
	static char string[384];
	va_start(argptr, fmt);
	vsnprintf(string, sizeof(string) - 1, fmt, argptr);
	string[sizeof(string) - 1] = '\0';
	va_end(argptr);
	
	SERVER_PRINT(string);
}

void* alloc_amxmemory(void** p, int size)
{
	*p = new unsigned char[size];
	return *p;
}

void free_amxmemory(void **ptr)
{
	delete[] (unsigned char *)(*ptr);
	*ptr = 0;
}

#if defined BINLOG_ENABLED
void BinLog_LogNative(AMX *amx, int native, int params)
{
	CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
	if (pl)
		g_BinLog.WriteOp(BinLog_NativeCall, pl->getId(), native, params);
}
void BinLog_LogReturn(AMX *amx, cell retval)
{
	CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
	if (pl)
		g_BinLog.WriteOp(BinLog_NativeRet, pl->getId(), retval);
}

void BinLog_LogParams(AMX *amx, cell *params)
{
	if (g_binlog_level & 8)
	{
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
		if (pl)
			g_BinLog.WriteOp(BinLog_NativeParams, pl->getId(), params);
	}
}

static binlogfuncs_t logfuncs = 
{
	BinLog_LogNative,
	BinLog_LogReturn,
	BinLog_LogParams
};
#endif

int load_amxscript(AMX *amx, void **program, const char *filename, char error[64], int debug)
{
	*error = 0;
	size_t bufSize;
	*program = (void *)g_plugins.ReadIntoOrFromCache(filename, bufSize);
	if (!*program)
	{
		CAmxxReader reader(filename, PAWN_CELL_SIZE / 8);
		
		if (reader.GetStatus() == CAmxxReader::Err_None)
		{
			bufSize = reader.GetBufferSize();
			
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
				strcpy(error, "Plugin uses deprecated format. Update compiler");
			default:
				strcpy(error, "Unknown error");
				return (amx->error = AMX_ERR_NOTFOUND);
		}
	} else {
		g_plugins.InvalidateFileInCache(filename, false);
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
	bool will_be_debugged = false;
	tagAMX_DBG *pDbg = NULL;

	if ((int)CVAR_GET_FLOAT("amx_debug") >= 2 || debug)
	{
		if ((hdr->file_version < CUR_FILE_VERSION))
		{
			sprintf(error, "Plugin needs newer debug version info");
			return (amx->error = AMX_ERR_VERSION);
		}
		else if ((hdr->flags & AMX_FLAG_DEBUG) != 0)
		{
			will_be_debugged = true;
			
			char *addr = (char *)hdr + hdr->size;
			pDbg = new tagAMX_DBG;
			
			memset(pDbg, 0, sizeof(AMX_DBG));

			int err = dbg_LoadInfo(pDbg, addr);

			if (err != AMX_ERR_NONE)
			{
				dbg_FreeInfo(pDbg);
				delete pDbg;
				sprintf(error, "Debug loading error %d", err);
				return (amx->error = AMX_ERR_INIT);
			}

			amx->flags |= AMX_FLAG_DEBUG;
		} else {
			sprintf(error, "Plugin not compiled with debug option");
			return (amx->error = AMX_ERR_INIT);
		}
	} else {
#ifdef JIT
		//if (hdr->file_version == CUR_FILE_VERSION)
		amx->flags |= AMX_FLAG_JITC;
#endif
	}

	if (g_opt_level != 65536)
	{
		SetupOptimizer(amx);
	}

	if ((err = amx_Init(amx, *program)) != AMX_ERR_NONE)
	{
		if (pDbg)
		{
			dbg_FreeInfo(pDbg);
			delete pDbg;
		}
		
		sprintf(error, "Load error %d (invalid file format or version)", err);
		return (amx->error = AMX_ERR_INIT);
	}

	Handler *pHandler = new Handler(amx);
	amx->userdata[UD_HANDLER] = (void *)pHandler;

#if defined BINLOG_ENABLED
	amx->usertags[UT_BINLOGS] = (void *)&logfuncs;
#endif

	if (will_be_debugged)
	{
		amx->flags |= AMX_FLAG_DEBUG;
		amx->flags &= (~AMX_FLAG_JITC);
		amx_SetDebugHook(amx, &Debugger::DebugHook);

		Debugger *pDebugger = new Debugger(amx, pDbg);
		amx->userdata[UD_DEBUGGER] = pDebugger;
	} else {
#ifdef JIT
		//set this again because amx_Init() erases it!
		amx->flags |= AMX_FLAG_JITC;
		amx->flags &= (~AMX_FLAG_DEBUG);
		amx->sysreq_d = NULL;
#endif
	}

#ifdef JIT
	if (amx->flags & AMX_FLAG_JITC)
	{
		char *np = new char[amx->code_size];
		char *rt = new char[amx->reloc_size];
		
		if (!np || (!rt && amx->reloc_size > 0))
		{
			delete[] np;
			delete[] rt;
			strcpy(error, "Failed to initialize JIT'd plugin");
			
			return (amx->error = AMX_ERR_INIT);
		}
	
		if ((err = amx_InitJIT(amx, (void *)rt, (void *)np)) == AMX_ERR_NONE) 
		{
			//amx->base = (unsigned char FAR *)realloc(np, amx->code_size);
#ifndef __linux__
			amx->base = (unsigned char *)VirtualAlloc(NULL, amx->code_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#else
			//posix_memalign((void **)&(amx->base), sysconf(_SC_PAGESIZE), amx->code_size);
			amx->base = (unsigned char *)memalign(sysconf(_SC_PAGESIZE), amx->code_size);
			mprotect((void *)amx->base, amx->code_size, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif
			if (amx->base)
				memcpy(amx->base, np, amx->code_size);
			
			delete [] np;
			delete [] rt;
			
			char *prg = (char *)(*program);
			
			delete [] prg;
			(*program) = amx->base;
			
			if (*program == 0)
			{
				strcpy(error, "Failed to allocate memory");
				return (amx->error = AMX_ERR_MEMORY);
			}
		} else {
			delete[] np;
			delete[] rt;
			
			sprintf(error, "Failed to initialize plugin (%d)", err);
			
			return (amx->error = AMX_ERR_INIT_JIT);
		}
	}
#endif

	CScript* aa = new CScript(amx, *program, filename);

	g_loadedscripts.put(aa);

	set_amxnatives(amx, error);

	if (g_plugins.m_Finalized)
	{
		amx_Register(amx, g_plugins.pNatives, -1);
		
		if (CheckModules(amx, error))
		{
			if (amx_Register(amx, core_Natives, -1) != AMX_ERR_NONE)
			{
				sprintf(error, "Plugin uses an unknown function (name \"%s\") - check your modules.ini.", no_function);
				return (amx->error = AMX_ERR_NOTFOUND);
			}
		} else {
			return (amx->error = AMX_ERR_NOTFOUND);
		}
	}

	return (amx->error = AMX_ERR_NONE);
}

const char *StrCaseStr(const char *as, const char *bs)
{
	static char a[256];
	static char b[256];
	unsigned int i = 0;
	unsigned int len = strlen(as);

	if (len > 254)
		len = 254;

	for (i = 0; i < len; i++)
	{
		a[i] = tolower(as[i]);
	}
	
	a[len] = 0;

	len = strlen(bs);

	if (len > 254)
		len = 254;

	for (i = 0; i < len; i++)
	{
		b[i] = tolower(bs[i]);
	}
	
	b[len] = 0;

	return strstr(a, b);
}

//returns 0 for module not found, 1 for "everything's okay"
int CheckModules(AMX *amx, char error[128])
{
	int numLibraries = amx_GetLibraries(amx);
	char buffer[64];
	LibType expect;
	bool found;
	
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	/** decode old style plugins */
	for (int i = 0; i < numLibraries; i++)
	{
		amx_GetLibrary(amx, i, buffer, sizeof(buffer) - 1);
		
		if (stricmp(buffer, "float") == 0)
			continue;

		if (stricmp(buffer, "dbi") == 0)
		{
			expect = LibType_Class;
		} else {
			expect = LibType_Library;
		}

		found = FindLibrary(buffer, expect);

		/* for binary compat */
		if (!found)
		{
			CList<CModule, const char *>::iterator a = g_modules.begin();
			while (a)
			{
				CModule &cm = (*a);
				if (cm.getInfoNew() && 
					cm.getInfoNew()->logtag && 
					!strcasecmp(cm.getInfoNew()->logtag, buffer))
				{
					found = true;
					break;
				}
				++a;
			}
		}
			
		if (!found)
		{
			if (pHandler->HandleModule(buffer, (expect == LibType_Class)))
				found = true;
		}
		
		if (!found)
		{
			const char *type = "Module/Library";
			if (expect == LibType_Class)
				type = "Module/Library Class";
			sprintf(error, "%s \"%s\" required for plugin. Check modules.ini.", type, buffer);
			return 0;
		}
	}

	/** decode new style plugins */
	amx_NumTags(amx, &numLibraries);
	cell notused;
	LibDecoder dec;
	LibError err;
	for (int i=0; i<numLibraries; i++)
	{
		amx_GetTag(amx, i, buffer, &notused);
		if (buffer[0] != '?')
			continue;
		if (DecodeLibCmdString(buffer, &dec))
		{
			if (dec.cmd == LibCmd_ReqClass || dec.cmd == LibCmd_ReqLib)
			{
				if ( (err=RunLibCommand(&dec)) != LibErr_None )
				{
					if (!pHandler->HandleModule(buffer, (err == LibErr_NoClass)))
					{
						const char *type = "Module/Library";
						if (err == LibErr_NoClass)
							type = "Module/Library Class";
						sprintf(error, "%s \"%s\" required for plugin.  Check modules.ini.", type, dec.param1);
						return 0;
					}
				}
			}
		}
	}

	return 1;
}

int set_amxnatives(AMX* amx, char error[128])
{
	CModule *cm;
	for (CList<CModule, const char *>::iterator a = g_modules.begin(); a ; ++a)
	{
		cm = &(*a);
		for (size_t i=0; i<cm->m_Natives.size(); i++)
		{
			amx_Register(amx, cm->m_Natives[i], -1);
		}
	}

	amx_Register(amx, string_Natives, -1);
	amx_Register(amx, float_Natives, -1);
	amx_Register(amx, file_Natives, -1);
	amx_Register(amx, amxmodx_Natives, -1);
	amx_Register(amx, power_Natives, -1);
	amx_Register(amx, time_Natives, -1);
	amx_Register(amx, vault_Natives, -1);
	amx_Register(amx, g_NewMenuNatives, -1);
	amx_Register(amx, g_NativeNatives, -1);
	amx_Register(amx, g_DebugNatives, -1);
	amx_Register(amx, msg_Natives, -1);

	//we're not actually gonna check these here anymore
	amx->flags |= AMX_FLAG_PRENIT;

	int idx, err;
	cell retval;

	Debugger *pd;
	pd = DisableDebugHandler(amx);
	
	if (amx_FindPublic(amx, "plugin_natives", &idx) == AMX_ERR_NONE)
	{
		if ((err = amx_Exec(amx, &retval, idx)) != AMX_ERR_NONE)
		{
			Debugger::GenericMessage(amx, err);
			AMXXLOG_Log("An error occurred in plugins_native. This is dangerous!");
		}
	}

	EnableDebugHandler(amx, pd);

	amx->flags &= ~(AMX_FLAG_PRENIT);

	return (amx->error = AMX_ERR_NONE);
}

int unload_amxscript(AMX* amx, void** program)
{
	int flags = amx->flags;
	
	Debugger *pDebugger = (Debugger *)amx->userdata[UD_DEBUGGER];
	if (pDebugger)
		delete pDebugger;
	
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];
	if (pHandler)
		delete pHandler;

	optimizer_s *opt = (optimizer_s *)amx->usertags[UT_OPTIMIZER];
	if (opt)
		delete opt;
	
	CList<CScript, AMX*>::iterator a = g_loadedscripts.find(amx);
	
	if (a)
		a.remove();
	
	char *prg = (char *)*program;
	
	if (!prg)
		return AMX_ERR_NONE;

#if defined JIT
#if defined __linux__
	if ((flags & AMX_FLAG_JITC) != AMX_FLAG_JITC)
	{
		delete [] prg;
	} else {
#ifdef free
#undef free
		free(prg);
#define free(ptr)	m_deallocator(__FILE__, __LINE__, __FUNCTION__, m_alloc_free, ptr)
#else
		free(prg);
#endif
	}
#elif defined WIN32
	
	if ((flags & AMX_FLAG_JITC) != AMX_FLAG_JITC)
	{
		delete [] prg;
	}
	else if (!VirtualFree((LPVOID)prg, 0, MEM_RELEASE))
	{
		AMXXLOG_Log("[AMXX] Could not free plugin memory, failure %d.", GetLastError());
		return AMX_ERR_PARAMS;
	}
#endif //OS support
#else
	//delete normally
	delete [] prg;
#endif
	*program = 0;
	return AMX_ERR_NONE;
}

AMX* get_amxscript(int id, void** code, const char** filename)
{
	CList<CScript, AMX*>::iterator a = g_loadedscripts.begin();
	
	while (a && id--)
		++a;
	
	if (a)
	{
		*filename = (*a).getName();
		*code = (*a).getCode();
		
		return (*a).getAMX();
	}
	
	return 0;
}

const char* GetFileName(AMX *amx)
{
	const char *filename = "";
	CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
		
	if (pl)
	{
		filename = pl->getName();
	} else {
		CList<CScript,AMX*>::iterator a = g_loadedscripts.find(amx);
		if (a)
			filename = (*a).getName();
	}

	return filename;
}

const char* get_amxscriptname(AMX* amx)
{
	CList<CScript, AMX*>::iterator a = g_loadedscripts.find(amx);
	return a ? (*a).getName() : "";
}

void get_modname(char* buffer)
{
	strcpy(buffer, g_mod_name.c_str());
}

char* build_pathname(char *fmt, ...)
{
	static char string[256];
	int b;
	int a = b = snprintf(string, 255, 
#ifndef __linux__
		"%s\\", 
#else
		"%s/", 
#endif
		g_mod_name.c_str());

	va_list argptr;
	va_start(argptr, fmt);
	a += vsnprintf (&string[a], 255 - a, fmt, argptr);
	string[a] = 0;
	va_end(argptr);

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

char *build_pathname_r(char *buffer, size_t maxlen, char *fmt, ...)
{
	snprintf(buffer, maxlen, 
#ifdef __linux__
		"%s/", 
#else
		"%s\\", 
#endif
		g_mod_name.c_str());

	size_t len = strlen(buffer);
	char *ptr = buffer + len;

	va_list argptr;
	va_start(argptr, fmt);
	vsnprintf (ptr, maxlen-len, fmt, argptr);
	va_end (argptr);

	while (*ptr) 
	{
#ifndef __linux__
		if (*ptr == '/') *ptr = '\\';
#else
		if (*ptr == '\\') *ptr = '/';
#endif
		++ptr;
	}

	return buffer;
}

// build pathname based on addons dir
char* build_pathname_addons(char *fmt, ...)
{
	static char string[256];

	va_list argptr;
	va_start(argptr, fmt);
	vsnprintf (string, 255, fmt, argptr);
	va_end(argptr);

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

#if defined WIN32
#define SEPCHAR '\\'
#elif defined __linux__
#define SEPCHAR '/'
#endif

bool ConvertModuleName(const char *pathString, String &path)
{
	String local;

	local.assign(pathString);
	char *tmpname = const_cast<char *>(local.c_str());
	char *orig_path = tmpname;

	path.clear();

	size_t len = local.size();
	if (!len)
		return false;

	/* run to filename instead of dir */
	char *ptr = tmpname;
	ptr = tmpname + len - 1;
	while (ptr >= tmpname && *ptr != SEPCHAR)
		ptr--;
	if (ptr >= tmpname)
	{
		*ptr++ = '\0';
		tmpname = ptr;
	}

	bool foundAmxx = false;
	int iDigit = '3';
	ptr = tmpname;
	while (*ptr)
	{
		while (*ptr && *ptr != '_')
			ptr++;
		if (strncmp(ptr, "_amxx", 5) == 0)
		{
			char *p = ptr + 5;
			if (strncmp(p, ".dll", 4) == 0)
			{
				foundAmxx = true;
				break;
			} else if (p[0] == '_') {
				p++;
				if (strncmp(p, "amd64.so", 8) == 0)
				{
					foundAmxx = true;
					break;
				} else if (p[0] == 'i') {
					p++;
					if (isdigit(p[0]) && p[1] == '8' && p[2] == '6')
					{
						iDigit = p[0];
						foundAmxx = true;
						break;
					}
				}
			} else if (p[0] == '\0') {
				foundAmxx = true;
				break;
			}
		} else {
			while (*ptr && *ptr == '_')
				ptr++;
		}
	}

	if (!foundAmxx)
	{
		ptr = tmpname + strlen(tmpname) - 1;
		while (ptr >= tmpname && *ptr != '.')
			ptr--;
		if (ptr > tmpname && *ptr == '.')
		{
			*ptr = '\0';
		}
	} else {
		*ptr = '\0';
	}

	path.assign(orig_path);
	path.append(SEPCHAR);
	path.append(tmpname);
	path.append("_amxx");
#if defined __linux__
 #if defined AMD64 || PAWN_CELL_SIZE==64
	path.append("amd64");
 #else
	path.append("i");
	path.append(iDigit);
	path.append("86");
 #endif
#endif
#if defined WIN32
	path.append(".dll");
#elif defined __linux__
	path.append(".so");
#endif

	return true;
}

bool LoadModule(const char *shortname, PLUG_LOADTIME now, bool simplify, bool noFileBail)
{
	char pathString[512];
	String path;

	build_pathname_r(
		pathString, 
		sizeof(pathString)-1, 
		"%s/%s",
		get_localinfo("amxx_modulesdir", "addons/amxmodx/modules"),
		shortname);

	if (simplify)
	{
		if (!ConvertModuleName(pathString, path))
			return false;
	} else {
		path.assign(pathString);
	}

	if (noFileBail)
	{
		FILE *fp = fopen(path.c_str(), "rb");
		if (!fp)
			return false;
		fclose(fp);
	}

	CList<CModule, const char *>::iterator a = g_modules.find(path.c_str());

	if (a)
		return false;

	CModule* cc = new CModule(path.c_str());

	cc->queryModule();

	switch (cc->getStatusValue())
	{
	case MODULE_BADLOAD:
		report_error(1, "[AMXX] Module is not a valid library (file \"%s\")", path.c_str());
		break;
	case MODULE_NOINFO:
		report_error(1, "[AMXX] Couldn't find info about module (file \"%s\")", path.c_str());
		break;
	case MODULE_NOQUERY:
		report_error(1, "[AMXX] Couldn't find \"AMX_Query\" or \"AMXX_Query\" (file \"%s\")", path.c_str());
		break;
	case MODULE_NOATTACH:
		report_error(1, "[AMXX] Couldn't find \"%s\" (file \"%s\")", cc->isAmxx() ? "AMXX_Attach" : "AMX_Attach", path.c_str());
		break;
	case MODULE_OLD:
		report_error(1, "[AMXX] Module has a different interface version (file \"%s\")", path.c_str());
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
	}

	g_modules.put(cc);

	if (cc->IsMetamod())
	{
		char *mmpathname = build_pathname_addons(
							"%s/%s", 
							get_localinfo("amxx_modulesdir", "addons/amxmodx/modules"), 
							shortname);
		ConvertModuleName(mmpathname, path);
		cc->attachMetamod(path.c_str(), now);
	}

	bool retVal = cc->attachModule();

	if (cc->isAmxx() && !retVal)
	{
		switch (cc->getStatusValue())
		{
		case MODULE_FUNCNOTPRESENT:
			report_error(1, "[AMXX] Module requested a not exisitng function (file \"%s\")%s%s%s", cc->getFilename(), cc->getMissingFunc() ? " (func \"" : "", 
				cc->getMissingFunc() ? cc->getMissingFunc() : "", cc->getMissingFunc() ? "\")" : "");
			break;
		case MODULE_INTERROR:
			report_error(1, "[AMXX] Internal error during module load (file \"%s\")", cc->getFilename());
			break;
		case MODULE_BADLOAD:
			report_error(1, "[AMXX] Module is not a valid library (file \"%s\")", cc->getFilename());
			break;
		}

		return false;
	}

	return true;
}

int loadModules(const char* filename, PLUG_LOADTIME now)
{
	FILE *fp = fopen(build_pathname("%s", filename), "rt");

	if (!fp)
	{
		AMXXLOG_Log("[AMXX] Modules list not found (file \"%s\")", filename);
		return 0;
	}

	String line;
	char moduleName[256];
	char buffer[255];
	int loaded = 0;

	String path;

	while (!feof(fp))
	{
		fgets(buffer, sizeof(buffer)-1, fp);

		if (buffer[0] == ';' || buffer[0] == '\n')
			continue;

		bool simplify = true;
		if (buffer[0] == '>')
		{
			simplify = false;
			line.assign(&buffer[1]);
		} else {
			line.assign(buffer);
		}
		
		line.trim();

		*moduleName = 0;
		
		if (sscanf(line.c_str(), "%s", moduleName) == EOF)
			continue;
		
		if (LoadModule(moduleName, now, simplify))
			loaded++;
	}

	fclose(fp);

	return loaded;
}

void detachModules()
{
	CList<CModule, const char *>::iterator a = g_modules.begin();

	while (a)
	{
		(*a).detachModule();
		a.remove();
	}
}

void detachReloadModules()
{
	CList<CModule, const char *>::iterator a = g_modules.begin();

	while (a)
	{
		if ((*a).isReloadable() && !(*a).IsMetamod())
		{
			(*a).detachModule();
			a.remove();
			
			continue;
		}
		++a;
	}
}

const char* strip_name(const char* a)
{
	const char* ret = a;
	
	while (*a)
	{
		if (*a == '/' || *a == '\\')
		{
			ret = ++a;
			continue;
		}
		++a;
	}
	
	return ret;
}

// Get the number of running modules
int countModules(CountModulesMode mode)
{
	CList<CModule, const char *>::iterator iter;
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
			num	= 0;
			
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
	CList<CModule, const char *>::iterator iter = g_modules.begin();
	
	while (iter)
	{
		(*iter).CallPluginsLoaded();
		++iter;
	}
}

//same for unloaded
void modules_callPluginsUnloaded()
{
	CList<CModule, const char *>::iterator iter = g_modules.begin();

	while (iter)
	{
		(*iter).CallPluginsUnloaded();
		++iter;
	}
}

void modules_callPluginsUnloading()
{
	CList<CModule, const char *>::iterator iter = g_modules.begin();

	while (iter)
	{
		(*iter).CallPluginsUnloading();
		++iter;
	}
}

// new functions
int MNF_AddNatives(AMX_NATIVE_INFO* natives)
{
	CList<CModule, const char *>::iterator a = g_modules.begin();

	if (!g_CurrentlyCalledModule || g_ModuleCallReason != ModuleCall_Attach)
		return FALSE;				// may only be called from attach

	g_CurrentlyCalledModule->m_Natives.push_back(natives);
	
	return TRUE;
}

const char *MNF_GetModname(void)
{
	// :TODO: Do we have to do this?? 
	// I dunno who wrote the above comment but no
#if 0
	static char buffer[64];
	strcpy(buffer, g_mod_name.c_str());
	return buffer;
#endif
	return g_mod_name.c_str();
}

AMX *MNF_GetAmxScript(int id)
{
	CList<CScript, AMX*>::iterator iter = g_loadedscripts.begin();
	
	while (iter && id--)
		++iter;

	if (iter == NULL)
		return NULL;
	
	return (*iter).getAMX();
}

const char *MNF_GetAmxScriptName(int id)
{
	CList<CScript, AMX*>::iterator iter = g_loadedscripts.begin();
	
	while (iter && id--)
		++iter;

	if (iter == NULL)
		return NULL;
	
	return (*iter).getName();
}

int MNF_FindAmxScriptByName(const char *name)
{
	CList<CScript, AMX*>::iterator iter = g_loadedscripts.begin();
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
	CList<CScript, AMX*>::iterator iter = g_loadedscripts.begin();
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

extern "C" char *MNF_GetAmxString(AMX *amx, cell amx_addr, int bufferId, int *pLen)
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
	
	while (ptr[c])
		++c;
	
	return c;
}

List<AUTHORIZEFUNC> g_auth_funcs;
void MNF_RegAuthorizeFunc(AUTHORIZEFUNC fn)
{
	g_auth_funcs.push_back(fn);
}

void MNF_UnregAuthorizeFunc(AUTHORIZEFUNC fn)
{
	g_auth_funcs.remove(fn);
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
	memcpy((void*)dest, (const void *)src, (size_t)len * sizeof(cell));
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

void MNF_OverrideNatives(AMX_NATIVE_INFO *natives, const char *name)
{
	//HACKHACK - we should never have had to do this
	//find a better solution for SourceMod!!!
	for (CList<CModule, const char *>::iterator a = g_modules.begin(); a ; ++a)
	{
		CModule &cm = (*a);
		if (cm.getStatusValue() != MODULE_LOADED)
			continue;
		const amxx_module_info_s *p = cm.getInfoNew();
		if (!p || !p->name)
			continue;
		if (strcmp(p->name, name)==0)
			continue;
		cm.rewriteNativeLists(natives);
	}
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
	char msg[3072];
	va_list arglst;
	va_start(arglst, fmt);
	_vsnprintf(msg, sizeof(msg) - 1, fmt, arglst);
	//vsprintf(msg, fmt, arglst);
	va_end(arglst);
	
	AMXXLOG_Log("%s", msg);
}

//by BAILOPAN
// debugger engine front end
extern "C" void LogError(AMX *amx, int err, const char *fmt, ...)
{
	Debugger *pDebugger = (Debugger *)amx->userdata[UD_DEBUGGER];

	amx->error = err;

	char msg_buffer[2048];

	msg_buffer[0] = '\0';
	
	if (fmt != NULL)
	{
		va_list ap;
		va_start(ap, fmt);
		_vsnprintf(msg_buffer, sizeof(msg_buffer) - 1, fmt, ap);
		va_end(ap);
	}

#if defined BINLOG_ENABLED
	CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
	if (pl)
		g_BinLog.WriteOp(BinLog_NativeError, pl->getId(), err, msg_buffer);
#endif

	//give the plugin first chance to handle any sort of error
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (pHandler->InNativeFilter())
	{
		if (pDebugger)
			pDebugger->EndExec();
	} else {
		if (pHandler)
		{
			if (pHandler->IsHandling())
			{
				if (fmt != NULL)
					pHandler->SetErrorMsg(msg_buffer);
				return;
			}
			
			//give the user a first-chance at blocking the error from displaying
			if (pHandler->HandleError(fmt ? msg_buffer : NULL) != 0)
			{
				amx->error = -1;
				return;
			}
		}
	}

	if (!pDebugger)
	{
		if (fmt)
			AMXXLOG_Error("%s", msg_buffer);
		
		Debugger::GenericMessage(amx, err);
		AMXXLOG_Error("[AMXX] To enable debug mode, add \"debug\" after the plugin name in plugins.ini (without quotes).");
		//destroy original error code so the original is not displayed again
	} else {
		pDebugger->SetTracedError(err);
		//we can display error now
		pDebugger->DisplayTrace(fmt ? msg_buffer : NULL);
	}

	amx->error = -1;
}

void MNF_MergeDefinitionFile(const char *file)
{
	g_langMngr.MergeDefinitionFile(file);
}

int MNF_FindLibrary(const char *name, LibType type)
{
	return FindLibrary(name, type) ? 1 : 0;
}

size_t MFN_AddLibraries(const char *name, LibType type, void *parent)
{
	return AddLibrariesFromString(name, type, LibSource_Module, parent) ? 1 : 0;
}

size_t MNF_RemoveLibraries(void *parent)
{
	return RemoveLibraries(parent);
}

edict_t* MNF_GetPlayerEdict(int id)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;
	
	return (GET_PLAYER_POINTER_I(id)->pEdict);
}

const char *MNF_Format(const char *fmt, ...)
{
	return "";
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

inline bool operator ==(func_s &arg1, const char *desc)
{
	if (strcmp(arg1.desc, desc) == 0)
		return true;
	
	return false;
}

CList<func_s, const char *> g_functions;

// Fnptr Request function for the new interface
const char *g_LastRequestedFunc = NULL;
#define REGISTER_FUNC(name, func) \
	{ \
		pFunc = new func_s; \
		pFunc->pfn = (void *)func; \
		pFunc->desc = name; \
		g_functions.put(pFunc); \
	}

void MNF_RegisterFunction(void *pfn, const char *description)
{
	func_s *pFunc;

	REGISTER_FUNC(description, pfn);
}

void *MNF_RegisterFunctionEx(void *pfn, const char *description)
{
	func_s *pFunc;
	CList<func_s, const char *>::iterator iter;

	for (iter = g_functions.begin(); iter; ++iter)
	{
		pFunc = &(*iter);
		if (strcmp(description, pFunc->desc) == 0)
		{
			void *pOld = pFunc->pfn;
			pFunc->pfn = pfn;
			return pOld;
		}
	}

	return NULL;
}

void Module_UncacheFunctions()
{
	g_functions.clear();
}

int MNF_SetPlayerTeamInfo(int player, int teamid, const char *teamname)
{
	if (player < 1 || player > gpGlobals->maxClients)
		return 0;

    CPlayer *pPlayer = GET_PLAYER_POINTER_I(player);

	if (!pPlayer->ingame)
		return 0;

	pPlayer->teamId = teamid;
	if (teamname != NULL)
		pPlayer->team.assign(teamname);

	return 1;
}

const char *MNF_GetLocalInfo(char *name, const char *def)
{
	return get_localinfo(name, def);
}

void *MNF_PlayerPropAddr(int id, int prop)
{
	if (id < 1 || id > gpGlobals->maxClients)
		return NULL;

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);

	switch (prop)
	{
	case Player_Name:
		return &pPlayer->name;
	case Player_Ip:
		return &pPlayer->ip;
	case Player_Team:
		return &pPlayer->team;
	case Player_Ingame:
		return &pPlayer->ingame;
	case Player_Authorized:
		return &pPlayer->authorized;
	case Player_Vgui:
		return &pPlayer->vgui;
	case Player_Time:
		return &pPlayer->time;
	case Player_Playtime:
		return &pPlayer->playtime;
	case Player_MenuExpire:
		return &pPlayer->menuexpire;
	case Player_Weapons:
		return &pPlayer->weapons[0];
	case Player_CurrentWeapon:
		return &pPlayer->current;
	case Player_TeamID:
		return &pPlayer->teamId;
	case Player_Deaths:
		return &pPlayer->deaths;
	case Player_Aiming:
		return &pPlayer->aiming;
	case Player_Menu:
		return &pPlayer->menu;
	case Player_Keys:
		return &pPlayer->keys;
	case Player_Flags:
		return &pPlayer->flags[0];
	case Player_Newmenu:
		return &pPlayer->newmenu;
	case Player_NewmenuPage:
		return &pPlayer->page;
	default:
		return NULL;
	}

	return NULL;
}

int amx_Execv()
{
	return AMX_ERR_NOTFOUND;
}

void Module_CacheFunctions()
{
	func_s *pFunc;

	REGISTER_FUNC("BuildPathname", build_pathname)
	REGISTER_FUNC("BuildPathnameR", build_pathname_r)
	REGISTER_FUNC("PrintSrvConsole", print_srvconsole)
	REGISTER_FUNC("GetModname", MNF_GetModname)
	REGISTER_FUNC("Log", MNF_Log)
	REGISTER_FUNC("LogError", LogError)
	REGISTER_FUNC("MergeDefinitionFile", MNF_MergeDefinitionFile)
	REGISTER_FUNC("Format", MNF_Format)
	REGISTER_FUNC("RegisterFunction", MNF_RegisterFunction);
	REGISTER_FUNC("RegisterFunctionEx", MNF_RegisterFunctionEx);

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
	REGISTER_FUNC("AmxReregister", amx_Reregister);

	// other amx stuff
	REGISTER_FUNC("amx_Exec", amx_Exec)
	REGISTER_FUNC("amx_Push", amx_Push)
	REGISTER_FUNC("amx_Execv", amx_Execv)		//I HOPE NO ONE USES THIS!!!!
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
	REGISTER_FUNC("SetPlayerTeamInfo", MNF_SetPlayerTeamInfo)
	REGISTER_FUNC("PlayerPropAddr", MNF_PlayerPropAddr)

	REGISTER_FUNC("RegAuthFunc", MNF_RegAuthorizeFunc);
	REGISTER_FUNC("UnregAuthFunc", MNF_UnregAuthorizeFunc);

	REGISTER_FUNC("FindLibrary", MNF_FindLibrary);
	REGISTER_FUNC("AddLibraries", MFN_AddLibraries);
	REGISTER_FUNC("RemoveLibraries", MNF_RemoveLibraries);
	REGISTER_FUNC("OverrideNatives", MNF_OverrideNatives);
	REGISTER_FUNC("GetLocalInfo", MNF_GetLocalInfo);

#ifdef MEMORY_TEST
	REGISTER_FUNC("Allocator", m_allocator)
	REGISTER_FUNC("Deallocator", m_deallocator)
	REGISTER_FUNC("Reallocator", m_reallocator)
#else
	REGISTER_FUNC("Allocator", MNF_Allocator)
	REGISTER_FUNC("Deallocator", MNF_Deallocator)
	REGISTER_FUNC("Reallocator", MNF_Reallocator)
#endif // MEMORY_TEST
}

void *Module_ReqFnptr(const char *funcName)
{
	// code
	// ^---- really? wow!
	
	if (!g_CurrentlyCalledModule)
	{
		return NULL;
	}

	g_LastRequestedFunc = funcName;

	CList<func_s, const char *>::iterator iter;
	
	for (iter = g_functions.begin(); iter; ++iter)
	{
		if (strcmp(funcName, iter->desc) == 0)
			return iter->pfn;
	}

	return NULL;
}

Debugger *DisableDebugHandler(AMX *amx)
{
	Debugger *pd = static_cast<Debugger *>(amx->userdata[UD_DEBUGGER]);
	
	amx->userdata[UD_DEBUGGER] = NULL;
	amx->flags &= ~(AMX_FLAG_DEBUG);
	amx_SetDebugHook(amx, NULL);

	return pd;
}

void EnableDebugHandler(AMX *amx, Debugger *pd)
{
	if (pd)
		amx->flags |= AMX_FLAG_DEBUG;

	amx->userdata[UD_DEBUGGER] = pd;
	amx_SetDebugHook(amx, &Debugger::DebugHook);
}

#if !defined MEMORY_TEST && !defined WIN32
void * operator new(size_t size)
{
	return (calloc(1, size)); 
}

void * operator new[](size_t size)
{
	return (calloc(1, size)); 
}

void operator delete(void * ptr)
{
	if (ptr)
		free(ptr);
}

void operator delete[](void * ptr)
{
	if (ptr)
		free(ptr);
}
#endif
