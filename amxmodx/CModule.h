// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// *****************************************************
// class CModule
// *****************************************************

#ifndef CMODULE_H
#define CMODULE_H

enum MODULE_STATUS
{
	MODULE_NONE,			// No module loaded
	MODULE_QUERY,			// Query failed
	MODULE_BADLOAD,			// Bad file or the module writer messed something up ;]
	MODULE_LOADED,			// Loaded
	MODULE_NOINFO,			// No info
	MODULE_NOQUERY,			// No query function present
	MODULE_NOATTACH,		// No attach function present
	MODULE_OLD,				// Old interface
	MODULE_NEWER,			// newer interface
	MODULE_INTERROR,		// Internal error
	MODULE_FUNCNOTPRESENT,	// Function not present
	MODULE_NOT64BIT,		// Not 64 bit compatible
	MODULE_BADGAME,			// Module cannot load on the current game mod
};

struct amxx_module_info_s
{
	const char *name;
	const char *author;
	const char *version;
	int reload;				// reload on mapchange when nonzero
	const char *logtag;		//added in version 2
	const char *library;	// added in version 4
	const char *libclass;	// added in version 4
};

#define AMXX_OK					0			/* no error */
#define AMXX_IFVERS				1			/* interface version */
#define AMXX_PARAM				2			/* Invalid parameter */
#define AMXX_FUNC_NOT_PRESENT	3			/* Function not present */

#define AMXX_GAME_OK			0			/* Module can load on this game. */
#define AMXX_GAME_BAD			1			/* Module cannot load on this game. */

#define AMXX_INTERFACE_VERSION	4

class CModule 
{
	ke::AString m_Filename;         // Filename
	
	bool m_Metamod;					// Using metamod?
	bool m_Amxx;					// Using new module interface?
	
	amxx_module_info_s m_InfoNew;	// module info (new module interface)
	DLHANDLE m_Handle;				// handle
	MODULE_STATUS m_Status;			// status
	const char *m_MissingFunc;		// missing function; only set on MODULE_FUNCNOTPRESENT status

	void clear(bool clearFilename = true);
public:
	CModule(const char* fname);
	~CModule();

	// Interface
	
	bool attachModule();
	bool queryModule();
	bool detachModule();
	void rewriteNativeLists(AMX_NATIVE_INFO *list);

#ifndef FAKEMETA
	bool attachMetamod(const char *mmfile, PLUG_LOADTIME now);
#endif

	const char* getStatus() const;
	inline const char* getType() const { return m_Amxx ? "amxx" : (m_Metamod ? "amx&mm" : "amx"); }
	inline const char* getAuthor() const { return m_InfoNew.author; }
	inline const char* getVersion() const { return m_InfoNew.version; }
	inline const char* getName() const { return m_InfoNew.name; }
	inline const amxx_module_info_s* getInfoNew() const { return &m_InfoNew; }	// new
	inline int getStatusValue() { return m_Status; }
	inline bool operator==(const char* fname) { return !strcmp(m_Filename.chars(), fname); }
	inline bool isReloadable() { return ((m_Status == MODULE_LOADED) && (m_InfoNew.reload != 0)); }
	inline bool isAmxx() const { return m_Amxx; }
	inline const char *getMissingFunc() const { return m_MissingFunc; }
	inline const char *getFilename() { return m_Filename.chars(); }
	inline bool IsMetamod() { return m_Metamod; }
	
	void CallPluginsLoaded();
	void CallPluginsUnloaded();
	void CallPluginsUnloading();

	ke::Vector<AMX_NATIVE_INFO*> m_Natives;
	ke::Vector<AMX_NATIVE_INFO*> m_NewNatives; // Natives for new (AMXX, not AMX) plugins only
	ke::Vector<size_t> m_DestroyableIndexes;
};

#endif //CMODULE_H
