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
// DODX Module
//

#include "amxxmodule.h"
#include "CMisc.h"
#include "dodx.h"

// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Disconnect()
{
	ingame = false;
	bot = false;
	savedScore = 0;

	oldteam = 0;
	oldclass = 0;
	oldprone = 0;
	oldstamina = 0.0f;

	// Model Stuff
	sModel.is_model_set = false;
	sModel.body_num = 0;

	// Object stuff
	object.pEdict = NULL;
	object.type = 0;
	object.carrying = false;
	object.do_forward = false;

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
	bool isip = false;
	switch((int)dodstats_rank->value) {
	case 1:
		if ( (unique = GETPLAYERAUTHID(pEdict)) == 0 )
			unique = name; // failed to get authid
		break;
	case 2:
		unique = ip;
		isip = true;
		break;
	default:
		unique = name;
	}
	if ( ( rank = g_rank.findEntryInRank( unique , name , isip) ) == 0 )
		ingame = false;
}

void CPlayer::Connect(const char* nn,const char* ippp ){
	bot = IsBot();
	strcpy(ip,ippp);
	// Strip the port from the ip
	for (size_t i = 0; i < sizeof(ip); i++)
	{
		if (ip[i] == ':')
		{
			ip[i] = '\0';
			break;
		}
	}
}

void CPlayer::restartStats(bool all)
{
	if ( all )
	{
		memset(&weapons,0,sizeof(weapons));
		memset(static_cast<void *>(&round),0,sizeof(round));
		memset(&weaponsRnd,0,sizeof(weaponsRnd));
	}

	memset(&weaponsLife,0,sizeof(weaponsLife));   //DEC-Weapon (Round) stats
	memset(&attackers,0,sizeof(attackers));
	memset(&victims,0,sizeof(victims));
	life = {};
}

void CPlayer::Init( int pi, edict_t* pe )
{
	aiming = 0;
	wpnModel = 0;
	wpnscount = 0;
	lastScore = 0;
	sendScore = 0;
	clearRound = 0.0f;
    pEdict = pe;
    index = pi;
	current = 0;
	clearStats = 0.0f;
	ingame =  false;
	bot = false;
	savedScore = 0;
	oldteam = 0;
	oldclass = 0;
	oldprone = 0;
	oldstamina = 0.0f;

	do_scoped = false;
	is_scoped = false;
	
	// Model Stuff
	sModel.is_model_set = false;
	sModel.body_num = 0;

	// Object stuff
	object.pEdict = NULL;
	object.type = 0;
	object.carrying = false;
	object.do_forward = false;
}

void CPlayer::saveKill(CPlayer* pVictim, int wweapon, int hhs, int ttk){

	if ( ignoreBots(pEdict,pVictim->pEdict) )
		return;

	if ( pVictim->index == index )
	{ // killed self
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

void CPlayer::saveShot(int weapon)
{

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

void CPlayer::updateScore(int weapon, int score)
{

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

void CPlayer::killPlayer()
{
	pEdict->v.dmg_inflictor = NULL;
	pEdict->v.health = 0;
	pEdict->v.deadflag = DEAD_RESPAWNABLE;
	pEdict->v.weaponmodel = 0;
	pEdict->v.weapons = 0;
}

void CPlayer::initModel(char* model)
{
	strcpy(sModel.modelclass, (const char*)model);
	sModel.is_model_set = true;
}

void CPlayer::clearModel()
{
	sModel.is_model_set = false;
}

bool CPlayer::setModel()
{
	if(!ingame || ignoreBots(pEdict))
		return false;

	if(sModel.is_model_set)
	{
		ENTITY_SET_KEYVALUE(pEdict, "model", sModel.modelclass);
		pEdict->v.body = sModel.body_num;
		return true;
	}

	return false;
}

void CPlayer::setBody(int bn)
{
	if(!ingame || ignoreBots(pEdict))
		return;

	sModel.body_num = bn;

	return;
}

/*
	iuser3 = 0 standing up
	iuser3 = 1 going prone or mg tearing down
	iuser3 = 2 setting up mg while laying down
*/
void CPlayer::PreThink()
{
	if(!ingame || ignoreBots(pEdict))
		return;

	if(oldteam != pEdict->v.team && iFTeamForward != -1)
		MF_ExecuteForward(iFTeamForward, index, pEdict->v.team, oldteam);

	if(oldclass != pEdict->v.playerclass && iFClassForward != -1)
		MF_ExecuteForward(iFClassForward, index, pEdict->v.playerclass, oldclass);

	if(oldprone != pEdict->v.iuser3 && oldprone != 2 && pEdict->v.iuser3 != 2 && iFProneForward != -1)
		MF_ExecuteForward(iFProneForward, index, pEdict->v.iuser3);

	if(oldstamina > pEdict->v.fuser4 && iFStaminaForward != -1)
		MF_ExecuteForward(iFStaminaForward, index, ((int)pEdict->v.fuser4));

	if(wpns_bitfield != pEdict->v.weapons)
		WeaponsCheck(pEdict->v.weapons & ~(1<<31));

	// Set the old variables for 
	oldprone = pEdict->v.iuser3;
	oldteam = pEdict->v.team;
	oldclass = pEdict->v.playerclass;
	oldstamina = pEdict->v.fuser4;

	wpns_bitfield = pEdict->v.weapons & ~(1<<31);
}

void CPlayer::Scoping(int value)
{
	// Everyone gets a 0 then another call for 90, so I need to figure out
	// what weapon they have before I can then check if they are scoped or not

	do_scoped = false;

	switch(value)
	{
	// This is when the scope is dropped from the eye
	case 0:
		// Is this an initial call
		if(mPlayer->current == 0)
			return;

		//			SKar					Spring					SFG42						SEnfield
		if((mPlayer->current == 6 || mPlayer->current == 9 || mPlayer->current == 32 || mPlayer->current == 35) && is_scoped)
		{
			is_scoped = false;
			do_scoped = true;
		}

		break;

	// This is when the scope is put up to the eye
	case 20:
		//			SKar					Spring					SFG42						SEnfield
		if((mPlayer->current == 6 || mPlayer->current == 9 || mPlayer->current == 32 || mPlayer->current == 35) && !is_scoped)
		{
			is_scoped = true;
			do_scoped = true;
		}

		break;

	// This means the scope has been initialized
	case 90:
		is_scoped = false;
		return;
	};
}

void CPlayer::ScopingCheck()
{
	if(do_scoped)
		MF_ExecuteForward(iFScopeForward, index, (int)is_scoped);
}

void CPlayer::WeaponsCheck(int weapons)
{
	if(wpns_bitfield == 0)
		return;
	
	else if(pEdict->v.weapons == 0)
		return;

	int old;
	int cur;

	for(int i = 1; i < DODMAX_WEAPONS; ++i)
	{
		// Check to see we are not talking about a grenade and we have changed
		if(i != 13 && i != 14 && i != 15 && i != 16 && i != 36)
		{
			old = wpns_bitfield&(1<<i);
			cur = weapons&(1<<i);

			if(old != cur)
				MF_ExecuteForward(iFWpnPickupForward, index, i, ((weapons&(1<<i)) ? 1 : 0));
		}
	}
}

// *****************************************************
// class Grenades
// *****************************************************
void Grenades::put(edict_t* grenade, float time, int type, CPlayer* player )
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

	while(*a)
	{
		if((*a)->time > gpGlobals->time) 
		{
			if((*a)->grenade == enemy) 
			{
				found = true;

				// we need this because of catched grenades
				if((*a)->time > lastTime)
				{ 
					(*p) = (*a)->player;      // two people can have the same nade in our list
					type = (*a)->type;
					lastTime = (*a)->time;
				}
			}
		}

		else 
		{
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
	while(head)
	{
		Obj* a = head->next;
		delete head;
		head = a;
	}
}

// *****************************************************
// class CMapInfo
// *****************************************************

void CMapInfo::Init()
{
	pEdict = 0;
	initialized = false;

	/* default values from dod.fgd */
	detect_axis_paras = 0;
	detect_allies_paras = 0;
	detect_allies_country = 0;

}

