// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TFCX Module
//

#include "amxxmodule.h"
#include "CMisc.h"
#include "tfcx.h"


// *****************************************************
// class Grenades
// *****************************************************
void Grenades::put( edict_t* grenade, float time, int type, CPlayer* player  )
{
  Obj* a = new Obj;
  if ( a == 0 ) return;
  a->player = player;
  a->grenade = grenade;
  a->time = gpGlobals->time + time;
  a->type = type;
  a->prev = 0;
  a->next = head;
  if ( head ) head->prev = a;
  head = a;
}

bool Grenades::find( edict_t* enemy, CPlayer** p, int* type )
{
  bool found = false;
  Obj* a = head;
  while ( a ){
    if ( a->time > gpGlobals->time ) {
      if ( a->grenade == enemy && !found ) {
        found = true;
        *p = a->player;
        *type = a->type;
      }
    }
    else {
      Obj* next = a->next;
      if (a->prev)  a->prev->next = next;
      else  head = next;
      if (next) next->prev = a->prev;
      delete a;
      a = next;
      continue;
    }
    a = a->next;
  }
  return found;
}

void Grenades::clear()
{
  while(head){
    Obj* a = head->next;
    delete head;
    head = a;
  }
}

// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Disconnect(){
	
	ingame = false;
	bot = false;

	if ( ignoreBots(pEdict) || !isModuleActive() ) // ignore if he is bot and bots rank is disabled or module is paused
		return;

	rank->updatePosition( &life );
	rank = 0;
}

void CPlayer::PutInServer(){

	ingame = true;

	if ((int)tfcstats_rankbots->value == 0 && IsBot()) // ignoreBots() can't be used as bot's flags are not set yet.
		return;

	restartStats();

	const char* name = STRING(pEdict->v.netname);
	const char* unique = name;
	bool isip = false;
	switch((int)tfcstats_rank->value) {
	case 1: 
		if ( (unique = GETPLAYERAUTHID(pEdict)) == 0 )
			unique = name; // failed to get authid
		break;
	case 2:
		unique = ip;
		isip = true;
	}
	rank = g_rank.findEntryInRank( unique , name , isip);
}
void CPlayer::Connect(const char* address ){
	bot = IsBot();
	strcpy(ip,address);
	// Strip the port from the ip
	for (size_t i = 0; i < sizeof(ip); i++)
	{
		if (ip[i] == ':')
		{
			ip[i] = '\0';
			break;
		}
	}
	rank = 0;
	clearStats = 0.0f;
}

void CPlayer::restartStats(bool all)
{
	if ( all ) memset(&weapons,0,sizeof(weapons));
	memset(&weaponsRnd,0,sizeof(weaponsRnd));   //DEC-Weapon (Round) stats
	memset(&attackers,0,sizeof(attackers));
	memset(&victims,0,sizeof(victims));
	life = {};
}

void CPlayer::Init( int pi, edict_t* pe )
{
    pEdict = pe;
    index = pi;
	current = 0;
	clearStats = 0.0f;
	rank = 0;
	ingame = false;
	bot = false;
}

void CPlayer::saveKill(CPlayer* pVictim, int wweapon, int hhs, int ttk)
{
	if (!isModuleActive())
		return;

	if ( ignoreBots(pEdict,pVictim->pEdict) )
		return;

	//PRINT_CONSOLE("Death! Killer:%d Victim:%d weapon:%d hs:%d tk:%d\n",index,pVictim->index,wweapon,hhs,ttk);

	if ( pVictim->index == index ){ // killed self
		pVictim->weapons[0].deaths++;
		pVictim->life.deaths++;
		pVictim->weaponsRnd[0].deaths++;       // DEC-Weapon (round) stats
		return;
	}

	pVictim->attackers[index].name = weaponData[wweapon].name;
	pVictim->attackers[index].kills++;
	pVictim->attackers[index].hs += hhs;
	pVictim->attackers[index].tks += ttk;
	pVictim->attackers[0].kills++;
	pVictim->attackers[0].hs += hhs;
	pVictim->attackers[0].tks += ttk;
	pVictim->weapons[pVictim->current].deaths++;
	pVictim->weapons[0].deaths++;
	pVictim->life.deaths++;
	
	
	pVictim->weaponsRnd[pVictim->current].deaths++; // DEC-Weapon (round) stats
	pVictim->weaponsRnd[0].deaths++;                   // DEC-Weapon (round) stats
	
	int vi = pVictim->index;
	victims[vi].name = weaponData[wweapon].name;
	victims[vi].deaths++;
	victims[vi].hs += hhs;
	victims[vi].tks += ttk;
	victims[0].deaths++;
	victims[0].hs += hhs;
	victims[0].tks += ttk;
	
	
	weaponsRnd[wweapon].kills++;                // DEC-Weapon (round) stats
	weaponsRnd[wweapon].hs += hhs;         // DEC-Weapon (round) stats
	weaponsRnd[wweapon].tks += ttk;     // DEC-Weapon (round) stats
	weaponsRnd[0].kills++;                     // DEC-Weapon (round) stats
	weaponsRnd[0].hs += hhs;              // DEC-Weapon (round) stats
	weaponsRnd[0].tks += ttk;          // DEC-Weapon (round) stats
	
	weapons[wweapon].kills++;
	weapons[wweapon].hs += hhs;
	weapons[wweapon].tks += ttk;
	weapons[0].kills++;
	weapons[0].hs += hhs;
	weapons[0].tks += ttk;
	life.kills++;
	life.hs += hhs;
	life.tks += ttk;
}

void CPlayer::saveHit(CPlayer* pVictim, int wweapon, int ddamage, int bbody)
{
	if (!isModuleActive())
		return;

	if ( ignoreBots(pEdict,pVictim->pEdict) )
		return;

	if ( ignoreDamage )
		return;

	//PRINT_CONSOLE("Hit! Attacker:%d Victim:%d weapon:%d damage:%d body:%d\n",index,pVictim->index,wweapon,ddamage,bbody);

	if ( index == pVictim->index ) return;

	pVictim->attackers[index].hits++;
	pVictim->attackers[index].damage += ddamage;
	pVictim->attackers[index].bodyHits[bbody]++;
	pVictim->attackers[0].hits++;
	pVictim->attackers[0].damage += ddamage;
	pVictim->attackers[0].bodyHits[bbody]++;

	int vi = pVictim->index;
	victims[vi].hits++;
	victims[vi].damage += ddamage;
	victims[vi].bodyHits[bbody]++;
	victims[0].hits++;
	victims[0].damage += ddamage;
	victims[0].bodyHits[bbody]++;

	weaponsRnd[wweapon].hits++;              // DEC-Weapon (round) stats
	weaponsRnd[wweapon].damage += ddamage;    // DEC-Weapon (round) stats
	weaponsRnd[wweapon].bodyHits[bbody]++;   // DEC-Weapon (round) stats
	weaponsRnd[0].hits++;                   // DEC-Weapon (round) stats
	weaponsRnd[0].damage += ddamage;         // DEC-Weapon (round) stats
	weaponsRnd[0].bodyHits[bbody]++;        // DEC-Weapon (round) stats

	weapons[wweapon].hits++;
	weapons[wweapon].damage += ddamage;
	weapons[wweapon].bodyHits[bbody]++;
	weapons[0].hits++;
	weapons[0].damage += ddamage;
	weapons[0].bodyHits[bbody]++;

	life.hits++;
	life.damage += ddamage;
	life.bodyHits[bbody]++;
}

void CPlayer::saveShot(int weapon)
{
	if (!isModuleActive())
		return;

	//PRINT_CONSOLE("Shot! Weapon:%d\n",weapon);

	if ( ignoreBots(pEdict) )
		return;

	if ( ignoreDamage )
		return;

	victims[0].shots++;
	weapons[weapon].shots++;
	weapons[0].shots++;
	life.shots++;
	weaponsRnd[weapon].shots++;       // DEC-Weapon (round) stats
	weaponsRnd[0].shots++;            // DEC-Weapon (round) stats
}

void CPlayer::killPlayer(){
	pEdict->v.dmg_inflictor = NULL;
	pEdict->v.health = 0;
	pEdict->v.deadflag = DEAD_RESPAWNABLE;
	pEdict->v.weaponmodel = 0;
	pEdict->v.weapons = 0;
}
