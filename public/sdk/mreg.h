#pragma once

// Width required to printf a Reg*List index number, for show() functions.
// This used to correspond to the number of digits in MAX_REG, which was a
// fixed, compile-time limit. However, now that the reg lists are grown
// dynamically, this has less strong correspondance to list sizes. So for
// the moment, it reflects what one might normally expect to be the max
// width needed to print an index number; 4 allows 9999 (which is a damn
// lot, if you ask me).
enum
{
	WIDTH_MAX_REG = 4
};

// Max number of registered user msgs we can manage.
enum
{
	MAX_REG_MSGS = 256
};

// Flags to indicate if given cvar or func is part of a loaded plugin.
enum REG_STATUS
{
	RG_INVALID,
	RG_VALID
};

// Pointer to function registered by AddServerCommand.
typedef void (*REG_CMD_FN)();

class MPlugin;

// An individual registered function/command.
class MRegCmd
{
public:
	MRegCmd(const char* cmd_name, REG_CMD_FN cmd_handler, MPlugin* cmd_plugin);
	~MRegCmd();
	bool call() const;			// try to call the function
	void disable();
	char* getname() const;
	REG_CMD_FN gethandler() const;

private:
	char*		m_name;			// space is malloc'd
	REG_CMD_FN	m_pfunction;		// pointer to the function
	int			m_plugid;			// index id of corresponding plugin
	REG_STATUS	m_status;		// whether corresponding plugin is loaded

	friend class MRegCmdList;
};

// A list of registered commands.
class MRegCmdList
{
public:
	MRegCmdList();
	~MRegCmdList();
	MRegCmd *find(const char *name) const;
	MRegCmd *add(const char *name, REG_CMD_FN cmd_handler, MPlugin* cmd_plugin);
	void remove(char* cmd_name);
	void remove(int owner_plugin_id);
	void show() const;
	void show(int plugin_id) const;

private:
	std::vector<MRegCmd *> m_list;
};

// An individual registered cvar.
class MRegCvar
{
public:
	MRegCvar(cvar_t* cv_ptr, MPlugin* cv_plugin);
	~MRegCvar();
	cvar_t* getcvar() const;

private:
	cvar_t*		m_cvar;
	int			m_plugid;
	REG_STATUS	m_status;

	friend class MRegCvarList;
};

// A list of registered cvars.
class MRegCvarList
{
public:
	MRegCvarList();
	~MRegCvarList();
	MRegCvar *add(cvar_t* src, MPlugin* plugin);
	MRegCvar *find(const char *findname);		// find by MRegCvar->data.name
	void disable(int plugin_id) const;			// change status to Invalid
	void show() const;					// list all cvars to console
	void show(int plugin_id) const;			// list given plugin's cvars to console

private:
	std::vector<MRegCvar *> m_list;
};

// An individual registered user msg, from gamedll.
class MRegMsg
{
public:
	MRegMsg(const char* name, int msgid, int size);
	const char* getname() const;
	int getid() const;
	int getsize() const;

private:
	const char*	m_name;
	int			m_msgid;
	int			m_size;

	friend class MRegMsgList;
};

// A list of registered user msgs.
class MRegMsgList
{
public:
	MRegMsgList();
	~MRegMsgList();
	MRegMsg *add(const char *addname, int addmsgid, int addsize);
	MRegMsg *find(const char *findname);
	MRegMsg *find(int findmsgid);
	void show();

private:
	std::vector<MRegMsg *> m_list;
};
