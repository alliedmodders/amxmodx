// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef COMMANDS_H
#define COMMANDS_H

// *****************************************************
// class CmdMngr
// *****************************************************

enum
{
	CMD_ConsoleCommand,
	CMD_ClientCommand,
	CMD_ServerCommand
};

class CmdMngr
{
public:
	class Command;
	friend class Command;
	
	class Command
	{
		friend class CmdMngr;
		
		CPluginMngr::CPlugin* plugin;
		CmdMngr* parent;
		ke::AString command;
		ke::AString argument;
		ke::AString commandline;
		ke::AString info;
		
		bool listable;
		int function;
		int flags;
		int id;
		int cmdtype;
		int prefix;
		static int uniqueid;
		
		Command(CPluginMngr::CPlugin* pplugin, const char* pcmd, const char* pinfo, int pflags, int pfunc, bool pviewable, CmdMngr* pparent);
		~Command();
	public:
		inline const char* getCommand() { return command.chars(); }
		inline const char* getArgument() { return argument.chars(); }
		inline const char* getCmdInfo() { return info.chars(); }
		inline const char* getCmdLine() { return commandline.chars(); }
		inline bool matchCommandLine(const char* cmd, const char* arg) 	{return (!stricmp(command.chars() + prefix, cmd + prefix) && (!argument.length() || !stricmp(argument.chars(), arg)));}
		inline bool matchCommand(const char* cmd) {	return (!stricmp(command.chars(), cmd)); }
		inline int getFunction() const { return function; }
		inline bool gotAccess(int f) const { return (!flags || ((flags & f) != 0)); }
		inline CPluginMngr::CPlugin* getPlugin() { return plugin; }
		inline bool isViewable() const { return listable; }
		inline int getFlags() const { return flags; }
		inline long int getId() const { return (long int)id; }
		
		const char* getCmdType() const;		
		void setCmdType(int a);
	};

private:
	struct CmdPrefix;
	friend struct CmdPrefix;

	struct CmdLink
	{
		Command* cmd;
		CmdLink* next;
		CmdLink(Command* c): cmd(c), next(0) {}
	};

	CmdLink* sortedlists[3];
	CmdLink* srvcmdlist;
	CmdLink* clcmdlist;

	struct CmdPrefix
	{
		ke::AString name;
		CmdMngr* parent;
		CmdLink* list;
		CmdPrefix* next;
		CmdPrefix(const char* nn, CmdMngr* pp): name(nn), parent(pp), list(0), next(0) {}
		~CmdPrefix() { parent->clearCmdLink(&list); }
	} *prefixHead;

	bool registerCmdPrefix(Command* cc);
	CmdPrefix** findPrefix(const char* nn);
	void clearPrefix();

	void setCmdLink(CmdLink** a, Command* c, bool sorted = true);
	void clearCmdLink(CmdLink** phead, bool pclear = false);

public:
	CmdMngr();
	~CmdMngr() { clear(); }

	// Interface

	void registerPrefix(const char* nn);
	
	Command* registerCommand(CPluginMngr::CPlugin* plugin, int func, char* cmd, char* info, int level, bool listable);
	Command* getCmd(long int id, int type, int access);
	int getCmdNum(int type, int access);
	
	void clearBufforedInfo();
	void clear();

	class iterator
	{
		CmdLink *a;
	public:
		iterator(CmdLink*aa = 0) : a(aa) {}
		iterator& operator++() { a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		operator bool () const { return a ? true : false; }
		Command& operator*() { return *a->cmd; }
	};

	inline iterator clcmdprefixbegin(const char* nn)
	{
		CmdPrefix* a = *findPrefix(nn);
		return iterator(a ? a->list : 0);
	}

	inline iterator clcmdbegin() const { return iterator(clcmdlist); }
	inline iterator srvcmdbegin() const { return iterator(srvcmdlist); }
	inline iterator begin(int type) const { return iterator(sortedlists[type]); }
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

#endif //COMMANDS_H

