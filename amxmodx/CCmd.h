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

#ifndef COMMANDS_H
#define COMMANDS_H

// *****************************************************
// class CmdMngr
// *****************************************************

enum {
	CMD_ConsoleCommand,
	CMD_ClientCommand,
	CMD_ServerCommand
};

class CmdMngr
{
public:
	class Command;
	friend class Command;
	
	class Command {
		friend class CmdMngr;
		CPluginMngr::CPlugin* plugin;
		CmdMngr* parent;
		String command;
		String argument;
		String commandline;
		String info;
		bool listable;
		int function;
		int flags;
		int id;
		int cmdtype;
		int prefix;
		static int uniqueid;
		Command( CPluginMngr::CPlugin* pplugin,const char* pcmd, const char* pinfo , int pflags , int pfunc, bool pviewable, CmdMngr* pparent );
		~Command();
	public:

		inline const char* getCommand() const{ return command.str(); }
		inline const char* getArgument() const{ return argument.str(); }
		inline const char* getCmdInfo() const{ return info.str(); }
		inline const char* getCmdLine() const{ return commandline.str(); }
		inline bool matchCommandLine(const char* cmd, const char* arg) 	{ return (!stricmp(command.str()+prefix, cmd+prefix ) && (argument.empty() || !stricmp(argument.str() , arg ))); }
		inline bool matchCommand(const char* cmd) {	return (!strcmp(command.str(), cmd ));	}
		inline int getFunction() const { return function; }
		inline bool gotAccess(int f) const { return (!flags||((flags & f)==flags)); }
		inline CPluginMngr::CPlugin* getPlugin() { return plugin; }
		inline bool isViewable() const { return listable; }
		inline int getFlags() const { return flags; }
		inline long int getId() const { return (long int)id; }
		const char* getCmdType() const;		
		void setCmdType( int a );

	};

private:

	struct CmdPrefix;
	friend struct CmdPrefix;

	struct CmdLink {
		Command* cmd;
		CmdLink* next;
		CmdLink(Command* c): cmd(c), next(0) {}
	};

	CmdLink* sortedlists[3];
	CmdLink* srvcmdlist;
	CmdLink* clcmdlist;

	struct CmdPrefix {
		CmdMngr* parent;
		String name;
		CmdLink* list;
		CmdPrefix* next;
		CmdPrefix( const char* nn , CmdMngr* pp) : 	name(nn),parent(pp),list(0),next(0){}
		~CmdPrefix(){  parent->clearCmdLink(&list);  }
	} *prefixHead;

	bool registerCmdPrefix( Command* cc );
	CmdPrefix** findPrefix(  const char* nn );
	void clearPrefix();

	void setCmdLink( CmdLink** a , Command* c, bool sorted = true );
	void clearCmdLink( CmdLink** phead, bool pclear = false  );

public:
	CmdMngr();
	~CmdMngr() {clear();}

	// Interface

	void registerPrefix( const char* nn );
	Command* registerCommand( CPluginMngr::CPlugin* plugin , int func , char* cmd ,  char* info , int level , bool listable );
	Command* getCmd( long int id ,int type,  int access);
	int getCmdNum( int type, int access );
	void clearBufforedInfo();
	void clear();

	class iterator {
		CmdLink *a;
	public:
		iterator(CmdLink*aa = 0) : a(aa) {}
		iterator& operator++() { a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		operator bool () const { return a ? true : false; }
		Command& operator*() { return *a->cmd; }
	};
	inline iterator clcmdprefixbegin(const char* nn){
		CmdPrefix* a = *findPrefix(nn);
		return iterator( a ? a->list : 0 );
	}
	inline iterator clcmdbegin() const {return iterator(clcmdlist);}
	inline iterator srvcmdbegin() const {return iterator(srvcmdlist);}
	inline iterator begin( int type ) const { return iterator(sortedlists[type]); }
	inline iterator end() const { return iterator(0); }

private:

	int buf_cmdid;
	int buf_cmdtype;
	int buf_cmdaccess;
	iterator buf_cmdptr;

	int buf_id;
	int buf_type;
	int buf_access;
	int buf_num;

};

#endif

