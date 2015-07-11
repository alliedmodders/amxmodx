// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef MENUS_H
#define MENUS_H

// *****************************************************
// class MenuMngr
// *****************************************************

class MenuMngr
{
	struct MenuIdEle
	{
		ke::AString name;
		AMX* amx;
		MenuIdEle* next;
		
		int id;
		static int uniqueid;
		
		MenuIdEle(const char* n, AMX* a, MenuIdEle* m) : name(n), amx(a), next(m)
		{
			id = ++uniqueid;
		}
	} *headid;

public:
		class iterator;
private:

	class MenuCommand 
	{
		friend class iterator;
		friend class MenuMngr;

		CPluginMngr::CPlugin *plugin;
		int menuid;
		int keys;
		int function;
		int is_new_menu;
		
		MenuCommand* next;
		MenuCommand(CPluginMngr::CPlugin *a, int mi, int k, int f, bool new_menu=false);
	public:
		inline int getFunction() { return function; }
		inline CPluginMngr::CPlugin* getPlugin() { return plugin; }
		inline bool matchCommand(int m, int k)
		{
			return ((m == menuid) && (keys & k));
		}
	} *headcmd;
	
public:
	MenuMngr() : m_watch_iter(end())
		{ headid = NULL; headcmd = NULL; }
	~MenuMngr();

	// Interface

	int findMenuId(const char* name, AMX* a = 0);
	int registerMenuId(const char* n, AMX* a);
	void registerMenuCmd(CPluginMngr::CPlugin *a, int mi, int k, int f, bool from_new_menu=false);
	void clear();

	class iterator
	{
		friend class MenuMngr;
		MenuCommand* a;
	public:
		iterator(MenuCommand*aa) : a(aa) {}
		iterator& operator++() { a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		operator bool () const { return a ? true : false; }
		MenuCommand& operator*() { return *a; }
	};
	
	inline iterator begin() const { return iterator(headcmd); }
	inline iterator end() const { return iterator(0); }

	MenuMngr::iterator SetWatchIter(MenuMngr::iterator iter);
	inline MenuMngr::iterator GetWatchIter() { return m_watch_iter; }
private:
	MenuMngr::iterator m_watch_iter;
};

extern MenuMngr g_menucmds;

#endif //MENUS_H
