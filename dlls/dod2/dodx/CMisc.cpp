/*
 * DoDx 
 * Copyright (c) 2004 Lukasz Wlasinski
 *
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#include "amxxmodule.h"
#include "CMisc.h"
#include "dodx.h"

// *****************************************************
// class Grenades
// *****************************************************
void Grenades::put( edict_t* grenade, float time, int type, CPlayer* player  )
{
  Obj* a = new Obj;
  a->player = player;
  a->grenade = grenade;
  a->time = gpGlobals->time + time;
  a->type = type;
  a->next = head;
  head = a;
}

bool Grenades::find( edict_t* enemy, CPlayer** p, int& type )
{
  bool found = false;
  float lastTime = 0.0;
  Obj** a = &head;
  while ( *a ){
    if ( (*a)->time > gpGlobals->time ) {
      if ( (*a)->grenade == enemy ) {
        found = true;
		if ( (*a)->time > lastTime ){ // we need this because of catched grenades
			(*p) = (*a)->player;      // two people can have the same nade in our list
			type = (*a)->type;
			lastTime = (*a)->time;
		}
      }
    }
    else {
      Obj* next = (*a)->next;
      delete *a;
      *a = next;
      continue;

    }
    a = &(*a)->next;
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
	savedScore = 0;

	if ( ignoreBots(pEdict) || !isModuleActive() ) // ignore if he is bot and bots rank is disabled or module is paused
		return;

	rank->updatePosition( &life );

}

void CPlayer::PutInServer(){

	ingame = true;

	if ( ignoreBots(pEdict) )
		return;

	restartStats();
	
	const char* unique;
	const char* name = STRING(pEdict->v.netname);
	switch((int)dodstats_rank->value) {
	case 1: 
		if ( (unique = GETPLAYERAUTHID(pEdict)) == 0 )
			unique = name; // failed to get authid
		break;
	case 2: 
		unique = ip; 
		break;
	default: 
		unique = name;
	}
	if ( ( rank = g_rank.findEntryInRank( unique , name ) ) == 0 )
		ingame = false;
}

void CPlayer::Connect(const char* nn,const char* ippp ){
	bot = IsBot();
	strcpy(ip,ippp);
}

void CPlayer::restartStats(bool all)
{
	if ( all ){
		memset(weapons,0,sizeof(weapons));
		memset(&round,0,sizeof(round));
		memset(weaponsRnd,0,sizeof(weaponsRnd));
	}
	memset(weaponsLife,0,sizeof(weaponsLife));   //DEC-Weapon (Round) stats
	memset(attackers,0,sizeof(attackers));
	memset(victims,0,sizeof(victims));
	memset(&life,0,sizeof(life));
}

void CPlayer::Init( int pi, edict_t* pe )
{
    pEdict = pe;
    index = pi;
	current = 0;
	clearStats = 0.0f;
	ingame =  false;
	savedScore = 0;
}

void CPlayer::saveKill(CPlayer* pVictim, int wweapon, int hhs, int ttk){

	if ( ignoreBots(pEdict,pVictim->pEdict) )
		return;

	if ( pVictim->index == index ){ // killed self
		pVictim->weapons[0].deaths++;
		pVictim->life.deaths++;
		pVictim->round.deaths++;
		pVictim->weaponsLife[0].deaths++;       // DEC-Weapon (life) stats
		pVictim->weaponsRnd[0].deaths++;       // DEC-Weapon (round) stats
		return;
	}

	int vw = get_weaponid(pVictim);

	pVictim->attackers[index].name = (char*)weaponData[wweapon].name;
	pVictim->attackers[index].kills++;
	pVictim->attackers[index].hs += hhs;
	pVictim->attackers[index].tks += ttk;
	pVictim->attackers[0].kills++;
	pVictim->attackers[0].hs += hhs;
	pVictim->attackers[0].tks += ttk;
	pVictim->weapons[vw].deaths++;
	pVictim->weapons[0].deaths++;
	pVictim->life.deaths++;
	pVictim->round.deaths++;
	
	
	pVictim->weaponsLife[vw].deaths++; // DEC-Weapon (life) stats
	pVictim->weaponsLife[0].deaths++;                // DEC-Weapon (life) stats
	pVictim->weaponsRnd[vw].deaths++; // DEC-Weapon (round) stats
	pVictim->weaponsRnd[0].deaths++;                // DEC-Weapon (round) stats
	
	int vi = pVictim->index;
	victims[vi].name = (char*)weaponData[wweapon].name;
	victims[vi].deaths++;
	victims[vi].hs += hhs;
	victims[vi].tks += ttk;
	victims[0].deaths++;
	victims[0].hs += hhs;
	victims[0].tks += ttk;
	
	
	weaponsLife[wweapon].kills++;                // DEC-Weapon (life) stats
	weaponsLife[wweapon].hs += hhs;         // DEC-Weapon (life) stats
	weaponsLife[wweapon].tks += ttk;     // DEC-Weapon (life) stats
	weaponsLife[0].kills++;                     // DEC-Weapon (life) stats
	weaponsLife[0].hs += hhs;              // DEC-Weapon (life) stats
	weaponsLife[0].tks += ttk;          // DEC-Weapon (life) stats

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
	round.kills++;
	round.hs += hhs;
	round.tks += ttk;
}

void CPlayer::saveHit(CPlayer* pVictim, int wweapon, int ddamage, int bbody){

	if ( index == pVictim->index ) return;

	if ( ignoreBots(pEdict,pVictim->pEdict) )
		return;

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

	weaponsLife[wweapon].hits++;              // DEC-Weapon (life) stats
	weaponsLife[wweapon].damage += ddamage;    // DEC-Weapon (life) stats
	weaponsLife[wweapon].bodyHits[bbody]++;   // DEC-Weapon (life) stats
	weaponsLife[0].hits++;                   // DEC-Weapon (life) stats
	weaponsLife[0].damage += ddamage;         // DEC-Weapon (life) stats
	weaponsLife[0].bodyHits[bbody]++;        // DEC-Weapon (life) stats

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

	round.hits++;
	round.damage += ddamage;
	round.bodyHits[bbody]++;
}

void CPlayer::saveShot(int weapon){

	if ( ignoreBots(pEdict) )
		return;

	victims[0].shots++;
	weapons[weapon].shots++;
	weapons[0].shots++;
	life.shots++;
	round.shots++;
	weaponsLife[weapon].shots++;       // DEC-Weapon (life) stats
	weaponsLife[0].shots++;            // DEC-Weapon (life) stats

	weaponsRnd[weapon].shots++;       // DEC-Weapon (round) stats
	weaponsRnd[0].shots++;            // DEC-Weapon (round) stats
}

void CPlayer::updateScore(int weapon, int score){

	if ( ignoreBots(pEdict) )
		return;

	life.points += score;
	round.points += score;
	weaponsLife[weapon].points += score;
	weaponsLife[0].points += score;
	weaponsRnd[weapon].points += score;
	weaponsRnd[0].points += score;
	weapons[weapon].points += score;
	weapons[0].points += score;
}

void CPlayer::killPlayer(){
	pEdict->v.dmg_inflictor = NULL;
	pEdict->v.health = 0;
	pEdict->v.deadflag = DEAD_RESPAWNABLE;
	pEdict->v.weaponmodel = 0;
	pEdict->v.weapons = 0;
}

void CPlayer::setTeamName( char *szName ){
	
	for (int i=0;i<16;i++){
		if ( bSteam )
			*( (char*)pEdict->pvPrivateData + STEAM_PDOFFSET_TEAMNAME + i ) = szName[i];
		else
			*( (char*)pEdict->pvPrivateData + WON_PDOFFSET_TEAMNAME + i ) = szName[i];
	}
}

// *****************************************************
// class CMapInfo
// *****************************************************

void CMapInfo::Init(){
	pEdict = 0;
	initialized = false;

	/* default values from dod.fgd */
	detect_axis_paras = 0;
	detect_allies_paras = 0;
	detect_allies_country = 0;

}

// *****************************************************
// class Forward
// *****************************************************

void Forward::put( AMX *a , int i ){
	head = new AmxCall( a, i , head );
}


void Forward::clear(){
	while ( head )  {
		AmxCall* a = head->next;
		delete head;
		head = a;
	}
}

void Forward::exec(int p1,int p2,int p3,int p4,int p5,int p6){
	AmxCall* a = head;
	while ( a ){
		MF_AmxExec(a->amx, NULL, a->iFunctionIdx, 6,p1, p2, p3, p4, p5, p6);
		a = a->next;
	}
}

void Forward::exec(int p1,int p2,int p3,int p4,int p5){
	AmxCall* a = head;
	while ( a ){
		MF_AmxExec(a->amx, NULL, a->iFunctionIdx, 5,p1, p2, p3, p4, p5);
		a = a->next;
	}
}

void Forward::exec(int p1,int p2){
	AmxCall* a = head;
	while ( a ){
		MF_AmxExec(a->amx, NULL, a->iFunctionIdx, 2,p1, p2);
		a = a->next;
	}
}