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

#ifndef PLUGIN_H
#define PLUGIN_H

#include "CString.h"
#include "sh_list.h"
#include "amx.h"
#include "amxxfile.h"

// *****************************************************
// class CPluginMngr
// *****************************************************

enum
{
	ps_bad_load,	//Load failed
	ps_error,	//Erroneous state
	ps_locked,	//UNUSED
	ps_paused,	//Plugin is temporarily paused
	ps_stopped,	//Plugin is ... more temporarily paused
	ps_running,	//Plugin is running
};

class CPluginMngr
{
public:

	class iterator;

	class CPlugin
	{
		friend class iterator;
		friend class CPluginMngr;
	
		AMX amx;
		void* code;
		
		String name;
		String version;
		String title;
		String author;
		String errorMsg;
		
		unsigned int failcounter;
		int m_PauseFwd;
		int m_UnpauseFwd;
		int paused_fun;
		int status;
		CPlugin* next;
		int id;
		
		CPlugin(int i, const char* p, const char* n, char* e, int d);
		~CPlugin();
		
		bool m_Debug;
	public:
		inline const char* getName() { return name.c_str();}
		inline const char* getVersion() { return version.c_str();}
		inline const char* getTitle() { return title.c_str();}
		inline const char* getAuthor() { return author.c_str();}
		inline const char* getError() { return errorMsg.c_str();}
		inline int getStatusCode() { return status; }
		inline int getId() const { return id; }
		inline AMX* getAMX() { return &amx; }
		inline const AMX* getAMX() const { return &amx; }
		inline void setTitle(const char* n) { title.assign(n); }
		inline void setAuthor(const char* n) { author.assign(n); }
		inline void setVersion(const char* n) { version.assign(n); }
		inline void setError(const char* n) { errorMsg.assign(n); }
		inline bool isValid() const { return (status >= ps_paused); }
		inline bool isPaused() const { return ((status == ps_paused) || (status == ps_stopped)); }
		inline bool isStopped() const { return (status == ps_stopped); }
		inline bool isExecutable(int id) const { return (isValid() && !isPaused());	}
		
		void Finalize();
		void AddToFailCounter(unsigned int i);
		void pausePlugin();
		void unpausePlugin();
		void pauseFunction(int id);
		void unpauseFunction(int id);
		void setStatus(int a);
		
		const char* getStatus() const;
		inline bool isDebug() const { return m_Debug; }
	}; 
	
private:	
	CPlugin *head;
	int pCounter;
public:
	CPluginMngr() { head = 0; pCounter = 0; pNatives = NULL; m_Finalized=false;}
	~CPluginMngr() { clear(); InvalidateCache(); }

	bool m_Finalized;
	AMX_NATIVE_INFO *pNatives;

	// Interface

	CPlugin* loadPlugin(const char* path, const char* name, char* error, int debug);
	void unloadPlugin(CPlugin** a);
	int loadPluginsFromFile(const char* filename, bool warn=true);
	
	inline CPlugin* findPluginFast(AMX *amx) { return (CPlugin*)(amx->userdata[UD_FINDPLUGIN]); }
	CPlugin* findPlugin(AMX *amx);
	CPlugin* findPlugin(int index);
	CPlugin* findPlugin(const char* name);
	
	inline int getPluginsNum() const { return pCounter; }
	void Finalize();
	void clear();

	class iterator
	{
		CPlugin *a;
	public:
		iterator(CPlugin*aa) : a(aa) {}
		iterator& operator++() { a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		operator bool () const { return a ? true : false; }
		CPlugin& operator*() { return *a; }
	};
	
	inline iterator begin() const { return iterator(head); }
	inline iterator end() const { return iterator(0); }
public:
	struct plcache_entry
	{
		CAmxxReader *file;
		size_t bufsize;
		char *buffer;
		String path;
	};
	char *ReadIntoOrFromCache(const char *file, size_t &bufsize);
	void InvalidateCache();
	void InvalidateFileInCache(const char *file, bool freebuf);
	void CacheAndLoadModules(const char *plugin);
	void CALMFromFile(const char *file);
private:
	List<plcache_entry *> m_plcache;
	List<String *> m_BlockList;
};

#endif //PLUGIN_H
