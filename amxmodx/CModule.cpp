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

#ifndef FAR
#define FAR
#endif

// Old
typedef int (FAR *QUERYMOD)(module_info_s**);
typedef int (FAR *ATTACHMOD)(pfnamx_engine_g*,pfnmodule_engine_g*);
typedef int (FAR *DETACHMOD)(void);

// New
typedef void* (*PFN_REQ_FNPTR)(const char * /*name*/);
typedef int (FAR *QUERYMOD_NEW)(int * /*ifvers*/, amxx_module_info_s * /*modInfo*/);
typedef int (FAR *ATTACHMOD_NEW)(PFN_REQ_FNPTR /*reqFnptrFunc*/);
typedef int (FAR *DETACHMOD_NEW)(void);
typedef void (FAR *PLUGINSLOADED_NEW)(void);

// Old
// These functions are needed since Small Abstract Machine 2.5.0
int wamx_FindPublic(AMX *amx, char *name, int *index)
{ return amx_FindPublic(amx, name, index); }

int wamx_FindPubVar(AMX *amx, char *varname, cell *amx_addr)
{ return amx_FindPubVar(amx, varname, amx_addr); }

int wamx_GetString(char *dest, cell *source)
{ return amx_GetString(dest, source, 0); }

AMX_NATIVE_INFO *wamx_NativeInfo(char *name, AMX_NATIVE func)
{ return amx_NativeInfo(name, func); }

int wamx_SetString(cell *dest, char *source, int pack)
{ return amx_SetString(dest, source, pack, 0); }

pfnamx_engine_g engAmxFunc = {
  amx_Align16,
  amx_Align32,
  amx_Allot,
  amx_Callback,
  amx_Clone,
  amx_Debug,
  amx_Exec,
  amx_Execv,
  wamx_FindPublic,
  wamx_FindPubVar,
  amx_FindTagId,
  amx_Flags,
  amx_GetAddr,
  amx_GetPublic,
  amx_GetPubVar,
  wamx_GetString,
  amx_GetTag,
  amx_GetUserData,
  amx_Init,
  amx_InitJIT,
  amx_MemInfo,
  amx_NameLength,
  wamx_NativeInfo,
  amx_NumPublics,
  amx_NumPubVars,
  amx_NumTags,
  amx_RaiseError,
  amx_Register,
  amx_Release,
  amx_SetCallback,
  amx_SetDebugHook,
  wamx_SetString,
  amx_SetUserData,
  amx_StrLen,
};

pfnmodule_engine_g engModuleFunc = {
  add_amxnatives,
  build_pathname,
  copy_amxmemory,
  format_amxstring,
  get_amxaddr,
  get_amxscript,
  get_amxscriptname,
  get_amxstring,
  get_modname,
  load_amxscript,
  print_srvconsole,
  report_error,
  set_amxnatives,
  set_amxstring,
  amxstring_len,
  unload_amxscript,
  alloc_amxmemory,
  free_amxmemory,
};

// *****************************************************
// class CModule
// *****************************************************

CModule::CModule(const char* fname) : m_Filename(fname)
{
	clear(false);
}

CModule::~CModule()
{
	// old & new
	if ( m_Handle )
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
		m_Filename.set("unknown");

	// old
	m_InfoOld = NULL;
	// new
	m_Amxx = false;
	m_InfoNew.author = "unknown";
	m_InfoNew.name = "unknown";
	m_InfoNew.version = "unknown";
	m_InfoNew.reload = 0;
	m_MissingFunc = NULL;

	m_Natives.clear();
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
			return true;
		case AMXX_PARAM:
			AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") retured \"Invalid parameter\" from Attach func.", m_Filename.str(), getVersion());
			m_Status = MODULE_INTERROR;
			return false;
		case AMXX_FUNC_NOT_PRESENT:
			m_Status = MODULE_FUNCNOTPRESENT;
			m_MissingFunc = g_LastRequestedFunc;
			return false;
		default:
			AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.",  m_Filename.str(), getVersion());
			m_Status = MODULE_BADLOAD;
			return false;
		}
	}
	else
	{
		// old
		ATTACHMOD AttachFunc = (ATTACHMOD)DLPROC(m_Handle, "AMX_Attach");

		if (AttachFunc)
			(*AttachFunc)(&engAmxFunc,&engModuleFunc);
		m_Status = MODULE_LOADED;
		return true;
	}
}

bool CModule::queryModule()
{
	if (m_Status != MODULE_NONE)			// don't check if already queried
		return false;

	m_Handle = DLLOAD(m_Filename.str());		// load file
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
			AMXXLOG_Log("[AMXX] Internal Error: Module \"%s\" (version \"%s\") retured \"Invalid parameter\" from Attach func.", m_Filename.str(), getVersion());
			m_Status = MODULE_INTERROR;
			return false;
		case AMXX_IFVERS:
			if (ifVers < AMXX_INTERFACE_VERSION)
				m_Status = MODULE_OLD;
			else
				m_Status = MODULE_NEWER;
			return false;
		case AMXX_OK:
			break;
		default:
			AMXXLOG_Log("[AMXX] Module \"%s\" (version \"%s\") returned an invalid code.",  m_Filename.str(), getVersion());
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
	}
	else
	{
		// Try old interface
		QUERYMOD queryFunc_Old = (QUERYMOD)DLPROC(m_Handle,"AMX_Query"); // check what version
		if (!queryFunc_Old)
		{
			m_Status = MODULE_NOQUERY;
			return false;
		}

		(*queryFunc_Old)(&m_InfoOld);

		if (!m_InfoOld)
		{
			m_Status = MODULE_NOINFO;
			return false;
		}

		if (m_InfoOld->ivers != AMX_INTERFACE_VERSION)
		{
			m_Status = MODULE_OLD;
			return false;
		}

		// Check for attach
		if (!DLPROC(m_Handle, "AMX_Attach"))
		{
			m_Status = MODULE_NOATTACH;
			return false;
		}

		m_InfoOld->serial = (long int)this;
		m_Status = MODULE_QUERY;
		return true;
	}
}

bool CModule::detachModule()
{
	if (m_Status != MODULE_LOADED)
		return false;

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
	else
	{
		DETACHMOD detachFunc_Old = (DETACHMOD)DLPROC(m_Handle, "AMX_Detach");
		if (detachFunc_Old)
			(*detachFunc_Old)();
	}
	DLFREE(m_Handle);
	clear();
	return true;
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
	switch(m_Status)
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
	}
	return "unknown";
}
