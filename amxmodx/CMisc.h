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

#ifndef CMISC_H
#define CMISC_H

#include "CList.h"
#include "sh_list.h"

// *****************************************************
// class CCVar
// *****************************************************

class CCVar
{
	cvar_t cvar;
	String name;
	String plugin;

public:	
	CCVar(const char* pname, const char* pplugin, int pflags, float pvalue) : name(pname), plugin(pplugin)
	{
		cvar.name = (char*)name.c_str();
		cvar.flags = pflags;
		cvar.string = "";
		cvar.value = pvalue;
	}
	
	inline cvar_t* getCvar() { return &cvar; }
	inline const char* getPluginName() { return plugin.c_str(); }
	inline const char* getName() { return name.c_str(); }
	inline bool operator == (const char* string) { return (strcmp(name.c_str(), string) == 0); }
	int plugin_id;
};

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
	
	String name;
	String ip;
	String team;

	bool initialized;
	bool ingame;
	bool authorized;
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
	String death_weapon;
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
	String filename;
	FORCE_TYPE type;
	Vector mins;
	Vector maxs;
	AMX* amx;
public:
	ForceObject(const char* n, FORCE_TYPE c, Vector& mi, Vector& ma, AMX* a) : filename(n), type(c), mins(mi), maxs(ma), amx(a) {}

	inline const char* getFilename() { return filename.c_str(); }
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
	String filename;
	AMX* amx;
	void* code;
public:
	CScript(AMX* aa, void* cc, const char* ff) : filename(ff), amx(aa), code(cc) {}
	
	inline AMX* getAMX() { return amx; }
	inline const char* getName() { return filename.c_str(); }
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
		String name;
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
