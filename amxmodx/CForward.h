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

#ifndef FORWARD_H
#define FORWARD_H

// *****************************************************
// class CmdMngr
// *****************************************************

#define FORWARD_NUM 12

enum {
	FF_ClientCommand,
	FF_ClientConnect,
	FF_ClientDisconnect,
	FF_ClientInfoChanged,
	FF_ClientPutInServer,
	FF_PluginInit,
	FF_PluginCfg,
	FF_PluginPrecache,
	FF_PluginLog,
	FF_PluginEnd,
	FF_InconsistentFile,
	FF_ClientAuthorized,	
};

class CForwardMngr 
{
public:
	
	class iterator;

	class CForward
	{

		friend class iterator;
		friend class CForwardMngr;

		CPluginMngr::CPlugin* plugin;
		int function;
		CForward* next;
		CForward( CPluginMngr::CPlugin* p, int func  ) : plugin(p) , function(func) {next=0;}

	public:
		inline CPluginMngr::CPlugin* getPlugin() { return plugin; }
		inline int getFunction() { return function; }



	};
	


private:
	CForward *head[ FORWARD_NUM ];
	void clearForwards( CForward** a );

public:
	CForwardMngr() {memset( head, 0, sizeof(head));}
	~CForwardMngr() { clear(); }

	// Interface

	void registerForward(  CPluginMngr::CPlugin* p, int func , int type  );
	void executeForwards( int type , int num = 0, int player = 0 );
	void clear();	


	class iterator {
		CForward *a;
	public:
		iterator(CForward*aa) : a(aa) {}
		iterator& operator++() { a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		operator bool () const { return a ? true : false; }
		CForward& operator*() { return *a; }
	};
	inline iterator begin( int type ) const { return iterator(head[(int)type]); }
	inline iterator end() const { return iterator(0); }
	inline bool forwardsExist( int type ) {return head[(int)type]?true:false;}
};


#endif

