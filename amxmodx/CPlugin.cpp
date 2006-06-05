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
#include "CPlugin.h"
#include "CForward.h"
#include "CFile.h"
#include "amx.h"
#include "natives.h"
#include "debugger.h"
#include "libraries.h"

extern const char *no_function;

CPluginMngr::CPlugin* CPluginMngr::loadPlugin(const char* path, const char* name, char* error, int debug)
{	
	CPlugin** a = &head;
	
	while (*a)
		a = &(*a)->next;
	
	*a = new CPlugin(pCounter++, path, name, error, debug);
	
	return (*a);
}

void CPluginMngr::unloadPlugin(CPlugin** a)
{
	CPlugin* next = (*a)->next;
	delete *a;
	*a = next;
	--pCounter;
}

void CPluginMngr::Finalize()
{
	if (m_Finalized)
		return;
	
	pNatives = BuildNativeTable();
	CPlugin *a = head;
	
	while (a)
	{
		if (a->getStatusCode() == ps_running)
		{
			amx_Register(a->getAMX(), pNatives, -1);
			a->Finalize();
		}
		a = a->next;
	}
	
	m_Finalized = true;
}

int CPluginMngr::loadPluginsFromFile(const char* filename)
{
	char file[256];
	FILE *fp = fopen(build_pathname_r(file, sizeof(file) - 1, "%s", filename), "rt");

	if (!fp) 
	{
		AMXXLOG_Log("[AMXX] Plugins list not found (file \"%s\")", filename);
		return 1;
	}
	
	// Find now folder
	char pluginName[256], error[256], debug[256];
	int debugFlag = 0;
	const char *pluginsDir = get_localinfo("amxx_pluginsdir", "addons/amxmodx/plugins");
	
	String line;

	while (!feof(fp)) 
	{
		pluginName[0] = '\0';
		
		debug[0] = '\0';
		debugFlag = 0;
		
		line.clear();
		line._fread(fp);
		/** quick hack */
		char *ptr = const_cast<char *>(line.c_str());
		while (*ptr)
		{
			if (*ptr == ';')
			{
				*ptr = '\0';
			} else {
				ptr++;
			}
		}
		sscanf(line.c_str(), "%s %s", pluginName, debug);
		
		if (!isalnum(*pluginName))
			continue;

		if (isalnum(*debug) && strcmp(debug, "debug") == 0)
		{
			debugFlag = 1;
		}

		CPlugin* plugin = loadPlugin(pluginsDir, pluginName, error, debugFlag);
		
		if (plugin->getStatusCode() == ps_bad_load)
		{
			char errorMsg[255];
			sprintf(errorMsg, "%s (plugin \"%s\")", error, pluginName);
			plugin->setError(errorMsg);
			AMXXLOG_Log("[AMXX] %s", plugin->getError());
		}
	}

	fclose(fp);

	InvalidateCache();

	return pCounter;
}

void CPluginMngr::clear()
{
	CPlugin**a = &head;	
	
	while (*a)
		unloadPlugin(a);
	
	m_Finalized = false;
	
	if (pNatives)
	{
		delete [] pNatives;
		pNatives = NULL;
	}
}

CPluginMngr::CPlugin* CPluginMngr::findPlugin(AMX *amx)
{
	CPlugin*a = head;
	
	while (a && &a->amx != amx)
		a = a->next;
	
	return a;
}
	
CPluginMngr::CPlugin* CPluginMngr::findPlugin(int index)
{
	CPlugin*a = head;
	
	while (a && index--)
		a = a->next;
	
	return a;
}
	
CPluginMngr::CPlugin* CPluginMngr::findPlugin(const char* name)
{
	if (!name)
		return 0;
	
	int len = strlen(name);
	
	if (!len)
		return 0;
	
	CPlugin*a = head;
	
	while (a && strncmp(a->name.c_str(), name, len))
		a = a->next;
	
	return a;
}

const char* CPluginMngr::CPlugin::getStatus() const
{
	switch (status)
	{
		case ps_running: 
		{
			if (m_Debug)
			{
				return "debug";
			} else {
				return "running";
			}
			break;
		}
		case ps_paused: return "paused";
		case ps_bad_load: return "bad load";
		case ps_stopped: return "stopped";
		case ps_locked: return "locked";
	}
	
	return "error";
}

CPluginMngr::CPlugin::CPlugin(int i, const char* p, const char* n, char* e, int d) : name(n), title(n)
{
	const char* unk = "unknown";
	
	title.assign(unk);
	author.assign(unk);
	version.assign(unk);
	
	char file[256];
	char* path = build_pathname_r(file, sizeof(file) - 1, "%s/%s", p, n);
	code = 0;
	memset(&amx, 0, sizeof(AMX));
	int err = load_amxscript(&amx, &code, path, e, d);
	
	if (err == AMX_ERR_NONE)
	{
		status = ps_running;
	} else {
		status = ps_bad_load;
	}
	
	amx.userdata[UD_FINDPLUGIN] = this;
	paused_fun = 0;
	next = 0;
	id = i;
	
	if (status == ps_running)
	{
		m_PauseFwd = registerSPForwardByName(&amx, "plugin_pause", FP_DONE);
		m_UnpauseFwd = registerSPForwardByName(&amx, "plugin_unpause", FP_DONE);
		
		if (amx.flags & AMX_FLAG_DEBUG)
		{
			m_Debug = true;
		} else {
			m_Debug = false;
		}
	}
}

CPluginMngr::CPlugin::~CPlugin()
{
	unload_amxscript(&amx, &code);
}

int AMXAPI native_handler(AMX *amx, int index)
{
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	char name[sNAMEMAX + 1];
	amx_GetNative(amx, index, name);

	return pHandler->HandleNative(name, index, 0);
}

static cell AMX_NATIVE_CALL invalid_native(AMX *amx, cell *params)
{
	//A script has accidentally called an invalid native! give them a
	// first chance to block the resulting error.

	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	//this should never happen
	if (!pHandler)
	{
		LogError(amx, AMX_ERR_INVNATIVE, "Invalid native attempt");
		return 0;
	}

	//this should never happen because this native won't be called
	// if the plugin isn't filtering.
	if (!pHandler->IsNativeFiltering())
	{
		LogError(amx, AMX_ERR_INVNATIVE, "Invalid native attempt");
		return 0;
	}

	char name[sNAMEMAX + 1];
	int native = (int)(_INT_PTR)(amx->usertags[UT_NATIVE]);
	int err = amx_GetNative(amx, native, name);

	if (err != AMX_ERR_NONE)
		name[0] = '\0';

	//1 - because we're trapping usage
	if (!pHandler->HandleNative(name, native, 1))
	{
		amx->usertags[UT_NATIVE] = (void *)native;
		LogError(amx, AMX_ERR_INVNATIVE, NULL);
		return 0;
	}

	//Someday maybe allow native filters to write their own return value?
	return 0;
}

void CPluginMngr::CPlugin::Finalize()
{
	char buffer[128];
	int old_status = status;
	
	if (CheckModules(&amx, buffer))
	{
		if (amx_Register(&amx, core_Natives, -1) != AMX_ERR_NONE)
		{
			Handler *pHandler = (Handler *)amx.userdata[UD_HANDLER];
			int res = 0;
			
			if (pHandler->IsNativeFiltering())
				res = amx_CheckNatives(&amx, native_handler);
			
			if (!res)
			{
				status = ps_bad_load;
				sprintf(buffer, "Plugin uses an unknown function (name \"%s\") - check your modules.ini.", no_function);
				errorMsg.assign(buffer);
				amx.error = AMX_ERR_NOTFOUND;
			} else {
				amx_RegisterToAny(&amx, invalid_native);
			}
		}
	} else {
		status = ps_bad_load;
		errorMsg.assign(buffer);
		amx.error = AMX_ERR_NOTFOUND;
	}
	
	if (old_status != status)
	{
		AMXXLOG_Log("[AMXX] Plugin \"%s\" failed to load: %s", name.c_str(), errorMsg.c_str());
	}
}

void CPluginMngr::CPlugin::pauseFunction(int id)
{ 
}

void CPluginMngr::CPlugin::unpauseFunction(int id)
{
}

void CPluginMngr::CPlugin::setStatus(int a)
{ 
	status = a; 
	g_commands.clearBufforedInfo(); // ugly way
}

// Pause a plugin
void CPluginMngr::CPlugin::pausePlugin()
{
	if (isValid())
	{
		// call plugin_pause if provided
		if (m_PauseFwd != -1)
			executeForwards(m_PauseFwd);
	
		setStatus(ps_paused);
	}
}

// Unpause a plugin
void CPluginMngr::CPlugin::unpausePlugin()
{
	if (isValid())
	{
		// set status first so the function will be marked executable
		setStatus(ps_running);
		
		// call plugin_unpause if provided
		if (m_UnpauseFwd != -1)
			executeForwards(m_UnpauseFwd);
	}
}

char *CPluginMngr::ReadIntoOrFromCache(const char *file, size_t &bufsize)
{
	List<plcache_entry *>::iterator iter;
	plcache_entry *pl;

	for (iter=m_plcache.begin(); iter!=m_plcache.end(); iter++)
	{
		pl = (*iter);
		if (pl->path.compare(file) == 0)
		{
			bufsize = pl->bufsize;
			return pl->buffer;
		}
	}

	pl = new plcache_entry;

	pl->file = new CAmxxReader(file, sizeof(cell));
	pl->buffer = NULL;
	if (pl->file->GetStatus() != CAmxxReader::Err_None)
	{
		delete pl->file;
		delete pl;
		return NULL;
	}

	pl->bufsize = pl->file->GetBufferSize();
	if (pl->bufsize)
	{
		pl->buffer = new char[pl->bufsize];
		pl->file->GetSection(pl->buffer);
	}

	if (!pl->buffer || pl->file->GetStatus() != CAmxxReader::Err_None)
	{
		delete [] pl->buffer;
		delete pl->file;
		delete pl;
		return NULL;
	}

	pl->path.assign(file);

	bufsize = pl->bufsize;

	m_plcache.push_back(pl);

	return pl->buffer;
}

void CPluginMngr::InvalidateCache()
{
	List<plcache_entry *>::iterator iter;
	plcache_entry *pl;

	for (iter=m_plcache.begin(); iter!=m_plcache.end(); iter++)
	{
		pl = (*iter);
		delete [] pl->buffer;
		delete pl->file;
		delete pl;
	}

	m_plcache.clear();
}

void CPluginMngr::InvalidateFileInCache(const char *file, bool freebuf)
{
	List<plcache_entry *>::iterator iter;
	plcache_entry *pl;

	for (iter=m_plcache.begin(); iter!=m_plcache.end(); iter++)
	{
		pl = (*iter);
		if (pl->path.compare(file) == 0)
		{
			if (freebuf)
				delete [] pl->buffer;
			delete pl->file;
			delete pl;
			m_plcache.erase(iter);
			return;
		}
	}
}

void CPluginMngr::CacheAndLoadModules(const char *plugin)
{
	size_t progsize;
	char *prog = ReadIntoOrFromCache(plugin, progsize);

	if (!prog)
		return;

	AMX_HEADER hdr;
	memcpy(&hdr, prog, sizeof(AMX_HEADER));

	uint16_t magic = hdr.magic;
	amx_Align16(&magic);

	if (magic != AMX_MAGIC)
	{
		return;
	}

	if (hdr.file_version < MIN_FILE_VERSION ||
		hdr.file_version > CUR_FILE_VERSION)
	{
		return;
	}
	if ((hdr.defsize != sizeof(AMX_FUNCSTUB)) && 
		(hdr.defsize != sizeof(AMX_FUNCSTUBNT)))
	{
		return;
	}

	amx_Align32((uint32_t*)&hdr.nametable);
	uint16_t *namelength=(uint16_t*)((unsigned char*)prog + (unsigned)hdr.nametable);
	amx_Align16(namelength);
	if (*namelength>sNAMEMAX)
	{
		return;
	}
	
	if (hdr.stp <= 0)
	{
		return;
	}

	AMX amx;
	memset(&amx, 0, sizeof(AMX));
	amx.base = (unsigned char *)prog;

	int num;
	char name[sNAMEMAX+1];

	num = amx_GetLibraries(&amx);
	for (int i=0; i<num; i++)
	{
		amx_GetLibrary(&amx, i, name, sNAMEMAX);
		if (stricmp(name, "Float")==0)
			continue;
		//awful backwards compat hack
		if (stricmp(name, "socket")==0)
			strcpy(name, "sockets");
		//we don't want to report failed modules here...
		LoadModule(name, PT_ANYTIME, true, true);
	}

	cell tag_id;
	amx_NumTags(&amx, &num);

	CVector<LibDecoder *> expects;
	CVector<LibDecoder *> defaults;
	CStack<LibDecoder *> delstack;
	for (int i=0; i<num; i++)
	{
		amx_GetTag(&amx, i, name, &tag_id);
		if (name[0] == '?')
		{
			LibDecoder *dc = new LibDecoder;
			delstack.push(dc);
			if (DecodeLibCmdString(name, dc))
			{
				if (dc->cmd == LibCmd_ForceLib)
				{
					RunLibCommand(dc);
				} else if ( (dc->cmd == LibCmd_ExpectClass) ||
							(dc->cmd == LibCmd_ExpectLib) ) 
				{
					expects.push_back(dc);
				} else if (dc->cmd == LibCmd_DefaultLib) {
					defaults.push_back(dc);
				}
			}
		}
	}

	for (size_t i=0; i<expects.size(); i++)
	{
		RunLibCommand(expects[i]);
	}
	for (size_t i=0; i<defaults.size(); i++)
	{
		RunLibCommand(defaults[i]);
	}

	expects.clear();
	defaults.clear();

	while (!delstack.empty())
	{
		delete delstack.front();
		delstack.pop();
	}

	return;
}

void CPluginMngr::CALMFromFile(const char *file)
{
	char filename[256];
	FILE *fp = fopen(build_pathname_r(filename, sizeof(filename) - 1, "%s", file), "rt");

	if (!fp) 
	{
		return;
	}

	// Find now folder
	char pluginName[256];
	char line[256];
	String rline;

	while (!feof(fp)) 
	{
		fgets(line, sizeof(line)-1, fp);
		if (line[0] == ';' || line[0] == '\n' || line[0] == '\0')
			continue;

		/** quick hack */
		char *ptr = line;
		while (*ptr)
		{
			if (*ptr == ';')
			{
				*ptr = '\0';
			} else {
				ptr++;
			}
		}

		rline.assign(line);
		rline.trim();
		pluginName[0] = '\0';
		sscanf(rline.c_str(), "%s", pluginName);

		if (!isalnum(*pluginName))
			continue;

		build_pathname_r(filename, sizeof(filename)-1, "%s/%s", get_localinfo("amxx_pluginsdir", "addons/amxmodx/plugins"), pluginName);

		CacheAndLoadModules(filename);
	}

	fclose(fp);
}
