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

// *****************************************************
// class CPluginMngr
// *****************************************************

enum {
	ps_bad_load,
	ps_error,
	ps_paused,
	ps_running,
	ps_stopped,
	ps_locked
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
		int paused_fun;
		int status;
		CPlugin* next;
		int id;
		CPlugin(int i , const char* p,const char* n, char* e, int d);
		~CPlugin( );

	public:

		inline const char* getName() { return name.c_str();}
		inline const char* getVersion() { return version.c_str();}
		inline const char* getTitle() { return title.c_str();}
		inline const char* getAuthor() { return author.c_str();}
		inline int getId() const { return id; }
		inline AMX* getAMX() { return &amx; }
		inline void setTitle( const char* n ) { title.assign(n); }
		inline void setAuthor( const char* n ) { author.assign(n); }
		inline void setVersion( const char* n ) { version.assign(n); }
		inline bool isValid() const { return ((status != ps_bad_load) && (status != ps_locked)); }
		inline bool isPaused() const { return ( (status == ps_paused) || (status == ps_stopped)); }
		inline bool isFunctionPaused( int id ) const { return (paused_fun & (1<<id)) ? true : false; 	}
		inline bool isExecutable(int id) const { 	return (isValid() && !isPaused() && !isFunctionPaused(id));	}
		void pausePlugin();
		void unpausePlugin();
		void pauseFunction( int id );
		void unpauseFunction( int id );
		void setStatus( int a   );
		const char* getStatus() const;
	}; 
	
private:	
	CPlugin *head;
	int pCounter;


public:
	CPluginMngr() { head = 0; pCounter = 0; }
	~CPluginMngr() { clear(); }

	// Interface

	CPlugin* loadPlugin(const char* path, const char* name, char* error, int debug);
	void unloadPlugin( CPlugin** a );
	int loadPluginsFromFile( const char* filename );
	CPlugin* findPluginFast(AMX *amx);
	CPlugin* findPlugin(AMX *amx);
	CPlugin* findPlugin(int index);
	CPlugin* findPlugin(const char* name);
	inline int getPluginsNum() const { return pCounter; }
	void clear();

	class iterator {
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
};

#endif


