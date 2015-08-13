// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef CMISC_H
#define CMISC_H

#include "CList.h"
#include "sh_list.h"

// *****************************************************
// class CPlayer
// *****************************************************

struct ClientCvarQuery_Info
{
	int resultFwd;
	int requestId;
	int paramLen;
	cell *params;
};

class CPlayer 
{
public:
	edict_t* pEdict;
	
	ke::AString name;
	ke::AString ip;
	ke::AString team;

	bool initialized;
	bool ingame;
	bool authorized;
	bool disconnecting;
	bool vgui;
	bool teamIdsInitialized;

	float time;
	float playtime;
	float menuexpire;
	
	struct
	{
		int ammo;
		int clip;
	} weapons[MAX_WEAPONS];
	
	int current;
	int teamId;
	int deaths;
	int aiming;
	int menu;
	int keys;
	int index;
	int flags[32];

	int death_headshot;
	int death_killer;
	int death_victim;
	bool death_tk;
	ke::AString death_weapon;
	int newmenu;
	int page;

	float channels[5];
	cell hudmap[5];
	
	Vector lastTrace;
	Vector lastHit;
	
	List<ClientCvarQuery_Info *> queries;

	void Init(edict_t* e, int i);
	void Disconnect();
	void PutInServer();
	
	bool Connect(const char* connectname, const char* ipaddress);

	inline bool IsBot()
	{
		if ((pEdict->v.flags & FL_FAKECLIENT) == FL_FAKECLIENT)
		{
			return true;
		}
		
		const char *auth = GETPLAYERAUTHID(pEdict); 	 
		if (auth && (strcmp(auth, "BOT") == 0)) 	 
		{
			return true;
		}
		
		return false;
	}

	inline bool IsAlive()
	{
		return ((pEdict->v.deadflag == DEAD_NO) && (pEdict->v.health > 0));
	}

	inline void Authorize() { authorized = true; }

	int NextHUDChannel();

};

// *****************************************************
// class Grenades
// *****************************************************

class Grenades
{
	struct Obj 
	{
		CPlayer* player;
		edict_t* grenade;
		float time;
		int type;
		Obj* next;
	} *head;

public:
	Grenades() { head = 0; }
	~Grenades() { clear(); }
	
	void put(edict_t* grenade, float time, int type, CPlayer* player);
	bool find(edict_t* enemy, CPlayer** p, int& type);
	void clear();
};

// *****************************************************
// class ForceObject
// *****************************************************

class ForceObject
{
	ke::AString filename;
	FORCE_TYPE type;
	Vector mins;
	Vector maxs;
	AMX* amx;
public:
	ForceObject(const char* n, FORCE_TYPE c, Vector& mi, Vector& ma, AMX* a) : filename(n), type(c), mins(mi), maxs(ma), amx(a) {}

	inline const char* getFilename() { return filename.chars(); }
	inline AMX* getAMX() { return amx; }
	
	Vector& getMin() { return mins; }
	Vector& getMax() { return maxs; }
	
	inline FORCE_TYPE getForceType() { return type; }
};

// *****************************************************
// class XVars
// *****************************************************

class XVars
{
	struct XVarEle
	{
		AMX* amx;
		cell* value;
	};

	XVarEle* head;
	
	int size;
	int num;
	int realloc_array(int nsize);

public:
	XVars() { num = 0; size = 0; head = 0; }
	~XVars() { clear(); }
	
	void clear();
	int put(AMX* a, cell* v);
	
	inline cell getValue(int a)
	{
		return (a >= 0 && a < num) ? *(head[a].value) : 0;
	}

	inline int setValue(int a, cell v)
	{ 
		if (a >= 0 && a < num)
		{
			*(head[a].value) = v;
			return 0;
		}

		return 1;
	}
};

// *****************************************************
// class CScript
// *****************************************************

class CScript
{
	ke::AString filename;
	AMX* amx;
	void* code;
public:
	CScript(AMX* aa, void* cc, const char* ff) : filename(ff), amx(aa), code(cc) {}
	
	inline AMX* getAMX() { return amx; }
	inline const char* getName() { return filename.chars(); }
	inline bool operator==(void* a) { return (amx == (AMX*)a); }
	inline void* getCode() { return code; }
};

// *****************************************************
// class TeamIds
// *****************************************************

class TeamIds
{
	struct TeamEle
	{
		ke::AString name;
		int id;
		char tid;
		static char uid;
		TeamEle* next;
		
		TeamEle(const char* n, int& i) : name(n), id(i), next(0)
		{
			tid = uid++;
		}
		
		~TeamEle() { --uid; }
	} *head;

	int newTeam;

public:
	TeamIds();
	~TeamIds();
	
	void registerTeam(const char* n, int s);
	int findTeamId(const char* n);
	int findTeamIdCase(const char* n);
	inline bool isNewTeam() { return newTeam ? true : false; }
};

class CAdminData
{
private:
	cell		m_AuthData[44];
	cell		m_Password[32];
	cell		m_Flags;
	cell		m_Access;
public:

	CAdminData()
	{
		m_AuthData[0]=0;
		m_Password[0]=0;
		m_Flags=0;
		m_Access=0;
	};

	void SetAccess(cell Access)
	{
		m_Access=Access;
	};
	cell GetAccess(void) const
	{
		return m_Access;
	};

	void SetFlags(cell Flags)
	{
		m_Flags=Flags;
	};
	cell GetFlags(void) const
	{
		return m_Flags;
	};

	void SetAuthID(const cell *Input)
	{
		unsigned int i=0;
		while (i<sizeof(m_AuthData)-1)
		{
			if ((m_AuthData[i++]=*Input++)==0)
			{
				return;
			}
		}

		m_AuthData[arraysize(m_AuthData)-1]=0;

	};
	const cell *GetAuthID(void) const
	{
		return &m_AuthData[0];
	};

	void SetPass(const cell *Input)
	{
		unsigned int i=0;
		while (i<sizeof(m_Password)-1)
		{
			if ((m_Password[i++]=*Input++)==0)
			{
				return;
			}
		}

		m_Password[arraysize(m_Password)-1]=0;

	};
	const cell *GetPass(void) const
	{
		return &m_Password[0];
	};

	CAdminData & operator = (const CAdminData &src)
	{
		this->SetAccess(src.GetAccess());
		this->SetFlags(src.GetFlags());
		this->SetAuthID(src.GetAuthID());
		this->SetPass(src.GetPass());

		return *this;
	}
};
#endif //CMISC_H
