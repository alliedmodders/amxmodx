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
#include "amxmodx.h"
// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Init(edict_t* e, int i)
{
	index = i;
	pEdict = e;
	initialized = false;
	ingame = false;
	bot = false;
	authorized = false;

	current = 0;
	teamId = -1;
	deaths = 0;
	aiming = 0;
	menu = 0;
	keys = 0;

	death_weapon.clear();
	name.clear();
	ip.clear();
	team.clear();
}

void CPlayer::Disconnect()
{
	ingame = false;
	initialized = false;
	authorized = false;

	while (!cvarQueryQueue.empty())
	{
		ClientCvarQuery_Info *pQuery = cvarQueryQueue.front();
		unregisterSPForward(pQuery->resultFwd);
		
		if (pQuery->params)
			delete [] pQuery->params;
		
		delete pQuery;
		cvarQueryQueue.pop();
	}

	bot = 0;
}

void CPlayer::PutInServer()
{
	playtime = gpGlobals->time;
	ingame = true;
}

bool CPlayer::Connect(const char* connectname, const char* ipaddress)
{
	name.assign(connectname);
	ip.assign(ipaddress);
	time = gpGlobals->time;
	bot = IsBot();
	death_killer = 0;
	memset(flags, 0, sizeof(flags));
	memset(weapons, 0, sizeof(weapons));
	initialized = true;
	authorized = false;

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
		if (strcmp((*a)->name.c_str(),n) == 0)
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
	if (*a == 0) return;
	newTeam |= (1<<(*a)->tid);
}

int TeamIds::findTeamId(const char* n)
{
	TeamEle* a = head;
	
	while (a)
	{
		if (!stricmp(a->name.c_str(), n))
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
		if (!strcmp(a->name.c_str(), n))
			return a->id;
		a = a->next;
	}
	
	return -1;
}

char TeamIds::TeamEle::uid = 0;
