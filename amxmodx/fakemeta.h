/* AMX Mod X
*
* by the AMX Mod X Development Team
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

#ifndef __FAKEMETA_H__
#define __FAKEMETA_H__

// Fake metamod api for modules

#include <CList.h>

// from mplugin.h (metamod)
// Flags to indicate current "load" state of plugin.
// NOTE: order is important, as greater/less comparisons are made.
typedef enum {
	PL_EMPTY = 0,		// empty slot
	PL_VALID,			// has valid info in it
	PL_BADFILE,			// nonexistent file (open failed), 
						//    or not a valid plugin file (query failed)
	PL_OPENED,			// dlopened and queried
	PL_FAILED,			// opened, but failed to attach or unattach
	PL_RUNNING,			// attached and running
	PL_PAUSED,			// attached but paused
} PLUG_STATUS;

// from h_export.h (metamod)
// Our GiveFnptrsToDll, called by engine.
typedef void (WINAPI *GIVE_ENGINE_FUNCTIONS_FN) (enginefuncs_t 
		*pengfuncsFromEngine, globalvars_t *pGlobals);


// *** CFakeMeta
class CFakeMeta
{
private:
	// Core tables
	DLL_FUNCTIONS m_CoreDllFuncTable;
	enginefuncs_t m_CoreEngineFuncTable;
	NEW_DLL_FUNCTIONS m_CoreNewDllFuncTable;

	DLL_FUNCTIONS m_CoreDllFuncTable_Post;
	enginefuncs_t m_CoreEngineFuncTable_Post;
	NEW_DLL_FUNCTIONS m_CoreNewDllFuncTable_Post;
public:
	class CFakeMetaPlugin
	{
	private:
		// plugin info
		String m_Path;
		PLUG_STATUS m_Status;
		plugin_info_t *m_Info;
		// Function tables
		META_FUNCTIONS m_MetaFuncTable;

		DLL_FUNCTIONS m_DllFuncTable;
		enginefuncs_t m_EngineFuncTable;
		NEW_DLL_FUNCTIONS m_NewDllFuncTable;

		DLL_FUNCTIONS m_DllFuncTable_Post;
		enginefuncs_t m_EngineFuncTable_Post;
		NEW_DLL_FUNCTIONS m_NewDllFuncTable_Post;

		// OS dep handle
		DLHANDLE m_Handle;
	public:
		inline PLUG_STATUS GetStatus() const
		{ return m_Status; }
		inline void SetStatus(PLUG_STATUS newStatus)
		{ m_Status = newStatus; }


		inline plugin_info_t * GetInfo()
		{ return m_Info; }
		inline const plugin_info_t * GetInfo() const
		{ return m_Info; }
		inline void SetInfo(plugin_info_t *newInfo)
		{ m_Info = newInfo; }

		inline const char * GetPath() const
		{ return m_Path.str(); }

		inline const META_FUNCTIONS &GetMetaFunctions() const
		{ return m_MetaFuncTable; }

		// Get
		inline DLL_FUNCTIONS &GetDllFuncTable()
		{ return m_DllFuncTable; }
		inline enginefuncs_t &GetEngineFuncTable()
		{ return m_EngineFuncTable; }
		inline NEW_DLL_FUNCTIONS &GetNewDllFuncTable()
		{ return m_NewDllFuncTable; }
		
		// Get const
		inline const DLL_FUNCTIONS &GetDllFuncTable() const
		{ return m_DllFuncTable; }
		inline const enginefuncs_t &GetEngineFuncTable() const
		{ return m_EngineFuncTable; }
		inline const NEW_DLL_FUNCTIONS &GetNewDllFuncTable() const
		{ return m_NewDllFuncTable; }

		// Get post
		inline DLL_FUNCTIONS &GetDllFuncTable_Post()
		{ return m_DllFuncTable_Post; }
		inline enginefuncs_t &GetEngineFuncTable_Post()
		{ return m_EngineFuncTable_Post; }
		inline NEW_DLL_FUNCTIONS &GetNewDllFuncTable_Post()
		{ return m_NewDllFuncTable_Post; }

		// Get post const
		inline const DLL_FUNCTIONS &GetDllFuncTable_Post() const
		{ return m_DllFuncTable_Post; }
		inline const enginefuncs_t &GetEngineFuncTable_Post() const
		{ return m_EngineFuncTable_Post; }
		inline const NEW_DLL_FUNCTIONS &GetNewDllFuncTable_Post() const
		{ return m_NewDllFuncTable_Post; }

		int Query(mutil_funcs_t *pMetaUtilFuncs);	// Also calls GiveFnPtrsToDll
		int Attach(PLUG_LOADTIME now, meta_globals_t *pMGlobals, gamedll_funcs_t *pGameDllFuncs);
		int Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason);

		int GetEntityAPI2(int interfaceVersion);
		int GetEntityAPI2_Post(int interfaceVersion);
		int GetEngineFunctions(int interfaceVersion);
		int GetEngineFunctions_Post(int interfaceVersion);
		int GetNewDLLFunctions(int interfaceVersion);
		int GetNewDLLFunctions_Post(int interfaceVersion);

		CFakeMetaPlugin(const char *path);
		~CFakeMetaPlugin();
	};	// CFakeMetaPlugin

	CFakeMeta();
	~CFakeMeta();

	bool AddPlugin(const char *path /*path relative to moddir*/);
	void ReleasePlugins();

	// This is public because i don't want to declare all the functions as friends :)
	CList<CFakeMetaPlugin> m_Plugins;

	// ****** Meta functions ******
	// Query all added plugins
	void Meta_Query(mutil_funcs_t *pMetaUtilFuncs);
	// Attach all added plugins
	void Meta_Attach(PLUG_LOADTIME now, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs);
	// Detach all added plugins
	void Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason);
	// :NOTE: Meta_Init currently not supported
	int GetEntityAPI2(DLL_FUNCTIONS *pFunctionTable /*from metamod*/, int *interfaceVersion /*from metamod*/,
		DLL_FUNCTIONS *pAMXXFunctionTable /*Functions amxx needs*/);
	int GetEntityAPI2_Post(DLL_FUNCTIONS *pFunctionTable /*from metamod*/, int *interfaceVersion /*from metamod*/,
		DLL_FUNCTIONS *pAMXXFunctionTable /*Functions amxx needs*/);
	int GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion,
		enginefuncs_t *pAMXXFunctionTable /*Fucntions amxx needs*/);
	int GetEngineFunctions_Post(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion,
		enginefuncs_t *pAMXXFunctionTable /*Fucntions amxx needs*/);
	int GetNewDLLFunctions(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion,
		NEW_DLL_FUNCTIONS *pAMXXFunctionTable);
	int GetNewDLLFunctions_Post(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion,
		NEW_DLL_FUNCTIONS *pAMXXFunctionTable);

	// Get
	inline DLL_FUNCTIONS &GetDllFuncTable()
	{ return m_CoreDllFuncTable; }
	inline enginefuncs_t &GetEngineFuncTable()
	{ return m_CoreEngineFuncTable; }
	inline NEW_DLL_FUNCTIONS &GetNewDllFuncTable()
	{ return m_CoreNewDllFuncTable; }
	
	// Get const
	inline const DLL_FUNCTIONS &GetDllFuncTable() const
	{ return m_CoreDllFuncTable; }
	inline const enginefuncs_t &GetEngineFuncTable() const
	{ return m_CoreEngineFuncTable; }
	inline const NEW_DLL_FUNCTIONS &GetNewDllFuncTable() const
	{ return m_CoreNewDllFuncTable; }

	// Get post
	inline DLL_FUNCTIONS &GetDllFuncTable_Post()
	{ return m_CoreDllFuncTable_Post; }
	inline enginefuncs_t &GetEngineFuncTable_Post()
	{ return m_CoreEngineFuncTable_Post; }
	inline NEW_DLL_FUNCTIONS &GetNewDllFuncTable_Post()
	{ return m_CoreNewDllFuncTable_Post; }

	// Get post const
	inline const DLL_FUNCTIONS &GetDllFuncTable_Post() const
	{ return m_CoreDllFuncTable_Post; }
	inline const enginefuncs_t &GetEngineFuncTable_Post() const
	{ return m_CoreEngineFuncTable_Post; }
	inline const NEW_DLL_FUNCTIONS &GetNewDllFuncTable_Post() const
	{ return m_CoreNewDllFuncTable_Post; }
};	// CFakeMeta

// Fake Metamod
// defined in meta_api.cpp
extern CFakeMeta g_FakeMeta;

#endif // #ifndef __FAKEMETA_H__