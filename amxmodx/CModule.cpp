// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "libraries.h"

#ifndef FAR
	#define FAR
#endif

// New
typedef void* (*PFN_REQ_FNPTR)(const char * /*name*/);
typedef int (FAR *QUERYMOD_NEW)(int * /*ifvers*/, amxx_module_info_s * /*modInfo*/);
typedef int (FAR *CHECKGAME_NEW)(const char *);
typedef int (FAR *ATTACHMOD_NEW)(PFN_REQ_FNPTR /*reqFnptrFunc*/);
typedef int (FAR *DETACHMOD_NEW)(void);
typedef void (FAR *PLUGINSLOADED_NEW)(void);
typedef void (*PLUGINSUNLOADED_NEW)(void);
typedef void (*PLUGINSUNLOADING_NEW)(void);

// *****************************************************
// class CModule
// *****************************************************

CModule::CModule(const char* fname)
{
	m_Filename = fname;
	clear(false);
}

CModule::~CModule()
{
	// old & new
	if (m_Handle)
		DLFREE(m_Handle);

	clear();
}

void CModule::clear(bool clearFilename)
{
	// old & new
	m_Metamod = false;
	m_Handle = NULL;
	m_Status = MODULE_NONE;
	
	if (clearFilename)
	{
		m_Filename = "unknown";
	}

	// new
	m_Amxx = false;
	m_InfoNew.author = "unknown";
	m_InfoNew.name = "unknown";
	m_InfoNew.version = "unknown";
	m_InfoNew.reload = 0;
	m_MissingFunc = NULL;

	for (size_t i=0; i<m_DestroyableIndexes.length(); i++)
	{
		delete [] m_Natives[m_DestroyableIndexes[i]];
	}

	m_DestroyableIndexes.clear();
	m_Natives.clear();
	m_NewNatives.clear();
}

bool CModule::attachMetamod(const char *mmfile, PLUG_LOADTIME now)
{
	void **handle;
	void *dummy = NULL;

	if (!m_Handle)
		handle = &dummy;
	else
		handle = (void **)&m_Handle;

	int res = LoadMetamodPlugin(mmfile, handle, now);

	if (!res)
	{
		m_Metamod = false;
	}

	return true;
}

//this ugly function is ultimately something like O(n^4).
//sigh.  it shouldn't be needed.
void CModule::rewriteNativeLists(AMX_NATIVE_INFO *list)
{
	AMX_NATIVE_INFO *curlist;
	for (size_t i=0; i<m_Natives.length(); i++)
	{
		curlist = m_Natives[i];
		bool changed = false;
		bool found = false;
		ke::Vector<size_t> newlist;
		for (size_t j=0; curlist[j].func != NULL; j++)
		{
			found = false;
			for (size_t k=0; list[k].func != NULL; k++)
			{
				if (strcmp(curlist[j].name, list[k].name) == 0)
				{
					found = true;
					break;
				}
			}
			if (found)
			{
				changed = true;
				//don't break, we have to search it all
			} else {
				newlist.append(j);
			}
		}
		if (changed)
		{
			//now build the new list
			AMX_NATIVE_INFO *rlist = new AMX_NATIVE_INFO[newlist.length()+1];
			for (size_t j=0; j<newlist.length(); j++)
			{
				rlist[j].func = curlist[newlist[j]].func;
				rlist[j].name = curlist[newlist[j]].name;
			}
			rlist[newlist.length()].func = NULL;
			rlist[newlist.length()].name = NULL;
			m_Natives[i] = rlist;
			m_DestroyableIndexes.append(i);
		}
	}
}

bool CModule::attachModule()
{
	// old & new
	if (m_Status != MODULE_QUERY || !m_Handle)
		return false;

	if (m_Amxx)
	{
		// new
		ATTACHMOD_NEW AttachFunc_New = (ATTACHMOD_NEW)DLPROC(m_Handle, "AMXX_Attach");

		if (!AttachFunc_New)
			return false;
		
		g_ModuleCallReason = ModuleCall_Attach;
		g_CurrentlyCalledModule = this;
		int retVal = (*AttachFunc_New)(Module_ReqFnptr);
		g_CurrentlyCalledModule = NULL;
		g_ModuleCallReason = ModuleCall_NotCalled;

		switch (retVal)
		{
			case AMXX_OK:
				m_Status = MODULE_LOADED;
				break;
			case AMXX_PARAM:
				AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") returned \"Invalid parameter\" from Attach func.", m_Filename.chars(), getVersion());
				m_Status = MODULE_INTERROR;
				return false;
			case AMXX_FUNC_NOT_PRESENT:
				m_Status = MODULE_FUNCNOTPRESENT;
				m_MissingFunc = g_LastRequestedFunc;
				return false;
			default:
				AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.", m_Filename.chars(), getVersion());
				m_Status = MODULE_BADLOAD;
				return false;
		}
	} else {
		m_Status = MODULE_BADLOAD;
	}

	if (m_Status == MODULE_LOADED)
	{
		AddLibrariesFromString(m_InfoNew.library, LibType_Library, LibSource_Module, this);
		AddLibrariesFromString(m_InfoNew.libclass, LibType_Class, LibSource_Module, this);
		return true;
	}

	return false;
}

bool CModule::queryModule()
{
	if (m_Status != MODULE_NONE)				// don't check if already queried
		return false;

	m_Handle = DLLOAD(m_Filename.chars());		// load file
	if (!m_Handle)
	{
#if defined(__linux__) || defined(__APPLE__)
		AMXXLOG_Log("[AMXX] Module \"%s\" failed to load (%s)", m_Filename.chars(), dlerror());
#endif
		m_Status = MODULE_BADLOAD;
		return false;
	}

	// Check whether the module uses metamod (for auto attach)
	if (DLPROC(m_Handle, "Meta_Attach"))
		m_Metamod = true;

	// Try new interface first
	QUERYMOD_NEW queryFunc_New = (QUERYMOD_NEW)DLPROC(m_Handle, "AMXX_Query");
	
	if (queryFunc_New)
	{
		m_Amxx = true;
		int ifVers = AMXX_INTERFACE_VERSION;
		g_ModuleCallReason = ModuleCall_Query;
		g_CurrentlyCalledModule = this;
		int retVal = (*queryFunc_New)(&ifVers, &m_InfoNew);
		g_CurrentlyCalledModule = NULL;
		g_ModuleCallReason = ModuleCall_NotCalled;
		
		switch (retVal)
		{
			case AMXX_PARAM:
				AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") returned \"Invalid parameter\" from Attach func.", m_Filename.chars(), getVersion());
				m_Status = MODULE_INTERROR;
				return false;
			case AMXX_IFVERS:
				if (ifVers < AMXX_INTERFACE_VERSION)
				{
					//backwards compat for new defs
					if (ifVers == 3)
					{
						g_ModuleCallReason = ModuleCall_Query;
						g_CurrentlyCalledModule = this;
						retVal = (*queryFunc_New)(&ifVers, &m_InfoNew);
						g_CurrentlyCalledModule = NULL;
						g_ModuleCallReason = ModuleCall_NotCalled;
						if (retVal == AMXX_OK)
						{
							m_InfoNew.library = m_InfoNew.logtag;
							if (StrCaseStr(m_InfoNew.library, "sql") 
								|| StrCaseStr(m_InfoNew.library, "dbi"))
							{
								m_InfoNew.libclass = "DBI";
							} else {
								m_InfoNew.libclass = "";
							}
							break;
						}
						return false;
					} else {
						m_Status = MODULE_OLD;
						return false;
					}
				} else {
					m_Status = MODULE_NEWER;
					return false;
				}
			case AMXX_OK:
				break;
			default:
				AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.", m_Filename.chars(), getVersion());
				m_Status = MODULE_BADLOAD;
				return false;
		}

		// Check for attach
		if (!DLPROC(m_Handle, "AMXX_Attach"))
		{
			m_Status = MODULE_NOATTACH;
			return false;
		}


		// Lastly, check to see if this module is able to load on this game mod
		CHECKGAME_NEW checkGame_New = (CHECKGAME_NEW)DLPROC(m_Handle, "AMXX_CheckGame");

		if (checkGame_New)
		{
			// This is an optional check; do not fail modules that do not have it
			int ret = checkGame_New(g_mod_name.chars());

			if (ret != AMXX_GAME_OK)
			{
				switch (ret)
				{
				case AMXX_GAME_BAD:
					AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") reported that it cannot load on game \"%s\"", m_Filename.chars(), getVersion(), g_mod_name.chars());
					m_Status = MODULE_BADGAME;
					break;
				default:
					AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an unknown CheckGame code (value: %d)", m_Filename.chars(), getVersion(), ret);
					m_Status = MODULE_BADLOAD;
					break;
				}

				return false;
			}
		}

		m_Status = MODULE_QUERY;
		return true;
	} else {
		m_Status = MODULE_NOQUERY;
		m_Amxx = false;
		return false;
	}
}

bool CModule::detachModule()
{
	if (m_Status != MODULE_LOADED)
		return false;

	RemoveLibraries(this);

	if (m_Amxx)
	{
		DETACHMOD_NEW detachFunc_New = (DETACHMOD_NEW)DLPROC(m_Handle, "AMXX_Detach");
		
		if (detachFunc_New)
		{
			g_ModuleCallReason = ModuleCall_Detach;
			g_CurrentlyCalledModule = this;
			(*detachFunc_New)();
			g_CurrentlyCalledModule = NULL;
			g_ModuleCallReason = ModuleCall_NotCalled;
		}
	}

#ifndef FAKEMETA
	if (IsMetamod())
	{
		UnloadMetamodPlugin(m_Handle);
	}
#endif
	
	DLFREE(m_Handle);
	clear();
	
	return true;
}

void CModule::CallPluginsUnloaded()
{
	if (m_Status != MODULE_LOADED)
		return;

	if (!m_Handle)
		return;

	PLUGINSUNLOADED_NEW func = (PLUGINSUNLOADED_NEW)DLPROC(m_Handle, "AMXX_PluginsUnloaded");

	if (!func)
		return;

	func();
}

void CModule::CallPluginsUnloading()
{
	if (m_Status != MODULE_LOADED)
		return;

	if (!m_Handle)
		return;

	PLUGINSUNLOADING_NEW func = (PLUGINSUNLOADING_NEW)DLPROC(m_Handle, "AMXX_PluginsUnloading");

	if (!func)
		return;

	func();
}

void CModule::CallPluginsLoaded()
{
	if (m_Status != MODULE_LOADED)
		return;

	if (!m_Handle)
		return;

	PLUGINSLOADED_NEW func = (PLUGINSLOADED_NEW)DLPROC(m_Handle, "AMXX_PluginsLoaded");
	
	if (!func)
		return;
	
	func();
}

const char* CModule::getStatus() const
{
	switch (m_Status)
	{
		case MODULE_NONE:		return "error";
		case MODULE_QUERY:		return "pending";
		case MODULE_BADLOAD:	return "bad load";
		case MODULE_LOADED:		return "running";
		case MODULE_NOINFO:		return "no info";
		case MODULE_NOQUERY:	return "no query";
		case MODULE_NOATTACH:	return "no attach";
		case MODULE_OLD:		return "old";
		case MODULE_FUNCNOTPRESENT:
		case MODULE_NEWER:		return "newer";
		case MODULE_INTERROR:	return "internal err";
		case MODULE_NOT64BIT:	return "not 64bit";
		case MODULE_BADGAME:	return "bad game";
		default:				break;
	}
	
	return "unknown";
}
