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

// *****************************************************
// class CModule
// *****************************************************

#ifndef CMODULE_H
#define CMODULE_H

enum MODULE_STATUS {
  MODULE_NONE,				// No module loaded
  MODULE_QUERY,				// Query failed
  MODULE_BADLOAD,			// Bad file or the module writer messed something up ;]
  MODULE_LOADED,			// Loaded
  MODULE_NOINFO,			// No info
  MODULE_NOQUERY,			// No query function present
  MODULE_NOATTACH,			// No attach function present
  MODULE_OLD,				// Old interface
  MODULE_NEWER,				// newer interface
  MODULE_INTERROR,			// Internal error
  MODULE_FUNCNOTPRESENT,	// Function not present
  MODULE_NOT64BIT			// Not 64 bit compatible
};

struct amxx_module_info_s
{
	const char *name;
	const char *author;
	const char *version;
	int reload;				// reload on mapchange when nonzero
	const char *logtag;		//added in version 2
};


#define AMXX_OK					0			/* no error */
#define AMXX_IFVERS				1			/* interface version */
#define AMXX_PARAM				2			/* Invalid parameter */
#define AMXX_FUNC_NOT_PRESENT	3			/* Function not present */

#define AMXX_INTERFACE_VERSION	2

class CModule 
{
	String m_Filename;				// Filename
	bool m_Metamod;					// Using metamod?
	bool m_Amxx;					// Using new module interface?
	module_info_s* m_InfoOld;		// module info (old module interface)
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
	const char* getStatus() const;
	inline const char* getType() const { return m_Amxx ? "amxx" : (m_Metamod ? "amx&mm" : "amx"); }
	inline const char* getAuthor() const { return m_Amxx ? (m_InfoNew.author) : (m_InfoOld ? m_InfoOld->author : "unknown"); }
	inline const char* getVersion() const { return m_Amxx ? (m_InfoNew.version) : (m_InfoOld ? m_InfoOld->version : "unknown"); }
	inline const char* getName() const { return m_Amxx ? (m_InfoNew.name) : (m_InfoOld ? m_InfoOld->name : "unknown"); }
	inline module_info_s* getInfo() const { return m_InfoOld; }	// old
	inline const amxx_module_info_s* getInfoNew() const { return &m_InfoNew; }	// new
	inline int getStatusValue() { return m_Status; }
	inline bool operator==( const char* fname ) { return !strcmp( m_Filename.c_str() , fname );  }
	inline bool isReloadable() { return m_Amxx ? ((m_Status == MODULE_LOADED) && (m_InfoNew.reload != 0)) : ( (m_Status==MODULE_LOADED) && (m_InfoOld->type==RELOAD_MODULE)); }
	inline bool isAmxx() const { return m_Amxx; }
	inline const char *getMissingFunc() const { return m_MissingFunc; }
	inline const char *getFilename() { return m_Filename.c_str(); }
	void CModule::CallPluginsLoaded();

	CList<AMX_NATIVE_INFO*> m_Natives;
};



#endif



