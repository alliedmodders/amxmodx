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
#include "libraries.h"

#ifndef FAR
	#define FAR
#endif

// New
typedef void* (*PFN_REQ_FNPTR)(const char * /*name*/);
typedef int (FAR *QUERYMOD_NEW)(int * /*ifvers*/, amxx_module_info_s * /*modInfo*/);
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
	m_Filename.assign(fname);
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
		m_Filename.assign("unknown");

	// new
	m_Amxx = false;
	m_InfoNew.author = "unknown";
	m_InfoNew.name = "unknown";
	m_InfoNew.version = "unknown";
	m_InfoNew.reload = 0;
	m_MissingFunc = NULL;

	for (size_t i=0; i<m_DestroyableIndexes.size(); i++)
	{
		delete [] m_Natives[m_DestroyableIndexes[i]];
	}

	m_DestroyableIndexes.clear();
	m_Natives.clear();
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
	for (size_t i=0; i<m_Natives.size(); i++)
	{
		curlist = m_Natives[i];
		bool changed = false;
		bool found = false;
		CVector<size_t> newlist;
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
				newlist.push_back(j);
			}
		}
		if (changed)
		{
			//now build the new list
			AMX_NATIVE_INFO *rlist = new AMX_NATIVE_INFO[newlist.size()+1];
			for (size_t j=0; j<newlist.size(); j++)
			{
				rlist[j].func = curlist[newlist[j]].func;
				rlist[j].name = curlist[newlist[j]].name;
			}
			rlist[newlist.size()].func = NULL;
			rlist[newlist.size()].name = NULL;
			m_Natives[i] = rlist;
			m_DestroyableIndexes.push_back(i);
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
				AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") retured \"Invalid parameter\" from Attach func.", m_Filename.c_str(), getVersion());
				m_Status = MODULE_INTERROR;
				return false;
			case AMXX_FUNC_NOT_PRESENT:
				m_Status = MODULE_FUNCNOTPRESENT;
				m_MissingFunc = g_LastRequestedFunc;
				return false;
			default:
				AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.", m_Filename.c_str(), getVersion());
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

	m_Handle = DLLOAD(m_Filename.c_str());		// load file
	if (!m_Handle)
	{
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
				AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") retured \"Invalid parameter\" from Attach func.", m_Filename.c_str(), getVersion());
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
				AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.", m_Filename.c_str(), getVersion());
				m_Status = MODULE_BADLOAD;
				return false;
		}

		// Check for attach
		if (!DLPROC(m_Handle, "AMXX_Attach"))
		{
			m_Status = MODULE_NOATTACH;
			return false;
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
		default:				break;
	}
	
	return "unknown";
}
