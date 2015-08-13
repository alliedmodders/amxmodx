// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "newmenus.h"
// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Init(edict_t* e, int i)
{
	index = i;
	pEdict = e;
	initialized = false;
	ingame = false;
	authorized = false;
	disconnecting = false;
	teamIdsInitialized = false;

	current = 0;
	teamId = -1;
	deaths = 0;
	aiming = 0;
	menu = 0;
	keys = 0;
	menuexpire = 0.0;
	newmenu = -1;

	death_weapon = nullptr;
	name = nullptr;
	ip = nullptr;
	team = nullptr;
}

void CPlayer::Disconnect()
{
	ingame = false;
	initialized = false;
	authorized = false;
	disconnecting = false;
	teamIdsInitialized = false;

	if (Menu *pMenu = get_menu_by_id(newmenu))
		pMenu->Close(index);

	List<ClientCvarQuery_Info *>::iterator iter, end=queries.end();
	for (iter=queries.begin(); iter!=end; iter++)
	{
		unregisterSPForward((*iter)->resultFwd);
		delete [] (*iter)->params;
		delete (*iter);
	}
	queries.clear();

	menu = 0;
	newmenu = -1;
}

void CPlayer::PutInServer()
{
	playtime = gpGlobals->time;
	ingame = true;
}

int CPlayer::NextHUDChannel()
{
	int ilow = 1;

	for (int i=ilow+1; i<=4; i++)
	{
		if (channels[i] < channels[ilow])
			ilow = i;
	}

	return ilow;
}

bool CPlayer::Connect(const char* connectname, const char* ipaddress)
{
	name = connectname;
	ip = ipaddress;
	time = gpGlobals->time;
	death_killer = 0;
	menu = 0;
	newmenu = -1;
	
	memset(flags, 0, sizeof(flags));
	memset(weapons, 0, sizeof(weapons));
	
	initialized = true;
	authorized = false;

	for (int i=0; i<=4; i++)
	{
		channels[i] = 0.0f;
		hudmap[i] = 0;
	}

	List<ClientCvarQuery_Info *>::iterator iter, end=queries.end();
	for (iter=queries.begin(); iter!=end; iter++)
	{
		unregisterSPForward((*iter)->resultFwd);
		delete [] (*iter)->params;
		delete (*iter);
	}
	queries.clear();

	const char* authid = GETPLAYERAUTHID(pEdict);

	if ((authid == 0) || (*authid == 0) || (strcmp(authid, "STEAM_ID_PENDING") == 0))
		return true;

	return false;
}

// *****************************************************
// class Grenades
// *****************************************************

void Grenades::put(edict_t* grenade, float time, int type, CPlayer* player)
{
	Obj* a = new Obj;
	if (a == 0) return;
	a->player = player;
	a->grenade = grenade;
	a->time = gpGlobals->time + time;
	a->type = type;
	a->next = head;
	head = a;
}

bool Grenades::find(edict_t* enemy, CPlayer** p, int& type)
{
	bool found = false;
	Obj** a = &head;
	
	while (*a)
	{
		if ((*a)->time > gpGlobals->time)
		{
			if ((*a)->grenade == enemy)
			{
				found = true;
				(*p) = (*a)->player;
				type = (*a)->type;
			}
		} else {
			Obj* b = (*a)->next;
			delete *a;
			*a = b;
			continue;
		}
		a = &(*a)->next;
	}
	
	return found;
}

void Grenades::clear()
{
	while (head)
	{
		Obj* a = head->next;
		delete head;
		head = a;
	}
}

// *****************************************************
// class XVars
// *****************************************************

void XVars::clear()
{
	delete[] head;
	head = 0;
	num = 0;
	size = 0;
}

int XVars::put(AMX* p, cell* v)
{
	for (int a = 0; a < num; ++a)
	{
		if ((head[a].amx == p) && (head[a].value == v))
			return a;
	}

	if ((num >= size) && realloc_array(size ? (size * 2) : 8))
		return -1;

	head[num].value = v;
	head[num].amx = p;
	
	return num++;
}

int XVars::realloc_array(int nsize)
{
	XVarEle* me = new XVarEle[nsize];
	
	if (me)
	{
		for (int a = 0 ; a < num; ++a)
			me[a] = head[a];
		
		delete[] head;
		head = me;
		size = nsize;
		return 0;
	}
	
	return 1;
}

// *****************************************************
// class TeamIds
// *****************************************************

TeamIds::TeamIds() { head = 0; newTeam = 0; }

TeamIds::~TeamIds()
{
	while (head)
	{
		TeamEle* a = head->next;
		delete head;
		head = a;
	}
}

void TeamIds::registerTeam(const char* n, int s)
{
	TeamEle** a = &head;
	
	while (*a)
	{
		if (strcmp((*a)->name.chars(),n) == 0)
		{
			if (s != -1)
			{
				(*a)->id = s;
				newTeam &= ~(1<<(*a)->tid);				
			}
			
			return;
		}
		a = &(*a)->next;
	}

	*a = new TeamEle(n, s);
	
	if (*a == 0)
		return;
	
	newTeam |= (1<<(*a)->tid);
}

int TeamIds::findTeamId(const char* n)
{
	TeamEle* a = head;
	
	while (a)
	{
		if (!stricmp(a->name.chars(), n))
			return a->id;
		a = a->next;
	}
	
	return -1;
}

int TeamIds::findTeamIdCase(const char* n)
{
	TeamEle* a = head;
	
	while (a)
	{
		if (!strcmp(a->name.chars(), n))
			return a->id;
		a = a->next;
	}
	
	return -1;
}

char TeamIds::TeamEle::uid = 0;
