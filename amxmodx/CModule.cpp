/*
 * Copyright (c) 2002-2003 Aleksander Naszko
 *
 *    This file is part of AMX Mod.
 *
 *    AMX Mod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    AMX Mod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with AMX Mod; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#include <extdll.h>
#include <meta_api.h>
#include "amxmod.h"

typedef int (FAR *QUERYMOD)(module_info_s**);
typedef int (FAR *ATTACHMOD)(pfnamx_engine_g*,pfnmodule_engine_g*);
typedef int (FAR *DETACHMOD)(void);

QUERYMOD QueryModule;
ATTACHMOD AttachModule;
DETACHMOD DetachModule;



pfnamx_engine_g engAmxFunc = {
  amx_Align16,
  amx_Align32,
  amx_Allot,
  amx_Callback,
  amx_Clone,
  amx_Debug,
  amx_Exec,
  amx_Execv,
  amx_FindPublic,
  amx_FindPubVar,
  amx_FindTagId,
  amx_Flags,
  amx_GetAddr,
  amx_GetPublic,
  amx_GetPubVar,
  amx_GetString,
  amx_GetTag,
  amx_GetUserData,
  amx_Init,
  amx_InitJIT,
  amx_MemInfo,
  amx_NameLength,
  amx_NativeInfo,
  amx_NumPublics,
  amx_NumPubVars,
  amx_NumTags,
  amx_RaiseError,
  amx_Register,
  amx_Release,
  amx_SetCallback,
  amx_SetDebugHook,
  amx_SetString,
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

CModule::CModule(const char* fname) : filename(fname)
{
	metamod = false;
	info = 0;
	module = 0;
	status = MODULE_NONE;
}
CModule::~CModule()
{
	if ( module ) DLFREE(module);
	natives.clear();
}
bool CModule::attachModule()
{
	if ( status != MODULE_QUERY )
		return false;
	AttachModule = (ATTACHMOD)DLPROC(module,"AMX_Attach");
	if ( AttachModule )	(*AttachModule)(&engAmxFunc,&engModuleFunc);
	status = MODULE_LOADED;
	return true;
}
bool CModule::queryModule()
{
	if ( status != MODULE_NONE ) // don't check if already quried
		return false;
	module = DLLOAD( filename.str() ); // link dll
	if ( !module ){
		status = MODULE_BADLOAD;
		return false;
	}
	int meta = (int)DLPROC(module,"Meta_Attach"); // check if also MM
	if ( meta ) metamod = true;

	QueryModule = (QUERYMOD)DLPROC(module,"AMX_Query"); // check what version
	if (QueryModule == 0) {
		status = MODULE_NOQUERY;
		return false;
	}
	(*QueryModule)( &info );
	if ( info == 0 ){
		status = MODULE_NOINFO;
		return false;
	}
	if ( info->ivers != AMX_INTERFACE_VERSION )	{
		status = MODULE_OLD;
		return false;
	}
	AttachModule = (ATTACHMOD)DLPROC(module,"AMX_Attach"); // check for attach
	if ( AttachModule == 0)	{
		status = MODULE_NOATTACH;
		return false;
	}
	info->serial = (long int)this;
	status = MODULE_QUERY;
	return true;
}
bool CModule::detachModule()
{
	if ( status != MODULE_LOADED )
		return false;
	DetachModule = (DETACHMOD)DLPROC(module,"AMX_Detach");
	if (DetachModule) (*DetachModule)();
	DLFREE(module);
	module = 0;
	natives.clear();
	status = MODULE_NONE;
	return true;
}
const char* CModule::getStatus() const {
	switch(status){
	case MODULE_NONE: return "error";
	case MODULE_QUERY: return "pending";
	case MODULE_BADLOAD:return "bad load";
	case MODULE_LOADED:return "running";
	case MODULE_NOINFO:return "no info";
	case MODULE_NOQUERY:return "no query";
	case MODULE_NOATTACH:return "no attach";
	case MODULE_OLD:return "old";
	}
	return "unknown";
}
