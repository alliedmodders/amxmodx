/*
 * Copyright (c) 2004 Jussi Kivilinna
 *
 *    This file is part of "Metamod All-Mod-Support"-patch for Metamod.
 *
 *    Metamod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    Metamod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with Metamod; if not, write to the Free Software Foundation,
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

#ifndef MM_PEXTENSIONS_H
#define MM_PEXTENSIONS_H

#include "plinfo.h"		// plid_t
#include "meta_api.h"		// PLUG_LOADTIME
/*

	How to use:
		1. Add new export function 'Meta_PExtGiveFnptrs' to your plugin file.
		   'Meta_PExtGiveFnptrs' will be called right after 'Meta_Query' call.
		2. Meta_PExtGiveFnptrs is called with interface version 'META_PEXT_VERSION'
		   and pointer to extension function table.
		3. Meta_PExtGiveFnptrs should return plugin's interface version.
		4. !NOTE! Metamod will not stop loading plugin even if plugin returns 
		   interface version greater than current. Plugin should disable itself in
		   this kind of situation.
	
	Example:
		#include "mm_pextensions.h"
		
		pextension_funcs_t *gpMetaPExtFuncs;
		
		int Meta_PExtGiveFnptrs(int interfaceVersion, pextension_funcs_t *pMetaPExtFuncs) {
			if(interfaceVersion < META_PEXT_VERSION) {
				LOG_DEVELOPER(PLID, "Error! Metamod is too old, please update!");
				gpMetaPExtFuncs = NULL;
			
				return(META_PEXT_VERSION);
			}
			
			gpMetaPExtFuncs = pMetaPExtFuncs;
			
			return(META_PEXT_VERSION);
		}
	
	Callback functions:
		- int PEXT_LOAD_PLUGIN_BY_NAME(PLID, const char *cmdline, PLUG_LOADTIME now, void **plugin_handle);
			Parses 'cmdline' as metamod would parse 'meta load <cmdline>' and loads found 
			plugin. If 'plugin_handle' is set, metamod writes module handle of loaded 
			plugin at it.
			Returns zero on success.
			For error codes see 'META_ERRNO' in 'types_meta.h'.
		
		- int PEXT_UNLOAD_PLUGIN_BY_NAME(PLID, const char *cmdline, PLUG_LOADTIME now, PL_UNLOAD_REASON reason);
			Parses 'cmdline' as metamod would parse 'meta unload <cmdline>' and 
			unloads found plugin.
			Returns zero on success.
			For error codes see 'META_ERRNO' in 'types_meta.h'.
		
		- int PEXT_UNLOAD_PLUGIN_BY_HANDLE(PLID, void *plugin_handle, PLUG_LOADTIME now, PL_UNLOAD_REASON reason);
			Unloads plugin with 'plugin_handle'.
			Returns zero on success.
			For error codes see 'META_ERRNO' in 'types_meta.h'.
		
		!NOTE! Plugin cannot unload itself!
*/

// Interface version
//  1: first version. Used in p13
//  2: Complete remake (p14):
//	pfnLoadMetaPluginByName
//	pfnUnloadMetaPluginByName
//	pfnUnloadMetaPluginByHandle
//  v2 is locked now. Don't modify old functions. If you add new functions, increase META_PEXT_VERSION.
#define META_PEXT_VERSION 2

// Meta PExtension Function table type.
typedef struct pextension_funcs_s {
	int (*pfnLoadMetaPluginByName)(plid_t plid, const char *cmdline, PLUG_LOADTIME now, void **plugin_handle);
	int (*pfnUnloadMetaPluginByName)(plid_t plid, const char *cmdline, PLUG_LOADTIME now, PL_UNLOAD_REASON reason);
	int (*pfnUnloadMetaPluginByHandle)(plid_t plid, void *plugin_handle, PLUG_LOADTIME now, PL_UNLOAD_REASON reason);
} pextension_funcs_t;

// Convenience macros for MetaPExtension functions.
#define PEXT_LOAD_PLUGIN_BY_NAME	(*gpMetaPExtFuncs->pfnLoadMetaPluginByName)
#define PEXT_UNLOAD_PLUGIN_BY_NAME	(*gpMetaPExtFuncs->pfnUnloadMetaPluginByName)
#define PEXT_UNLOAD_PLUGIN_BY_HANDLE	(*gpMetaPExtFuncs->pfnUnloadMetaPluginByHandle)

// Give plugin extension function table.
C_DLLEXPORT int Meta_PExtGiveFnptrs(int interfaceVersion, 
		pextension_funcs_t *pMetaPExtFuncs);
typedef int (*META_GIVE_PEXT_FUNCTIONS_FN) (int interfaceVersion, 
		pextension_funcs_t *pMetaPExtFuncs);

#endif /* MM_PEXTENSIONS_H */
