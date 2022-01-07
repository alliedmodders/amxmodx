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
// TSX Module
//

#include "amxxmodule.h"
#include "tsx.h"


void Client_ResetHUD_End(void* mValue)
{
	if ( mPlayer->IsAlive() ){ // ostatni przed spawn'em 
		mPlayer->clearStats = gpGlobals->time + 0.25f; // teraz czysc statystyki 
	}
	else { // dalej "dead" nie czysc statystyk!
		mPlayer->items = 0;
		mPlayer->is_specialist = 0;
		mPlayer->killingSpree = 0;
		mPlayer->killFlags = 0;
		mPlayer->frags = (int)mPlayer->pEdict->v.frags;
		/* 
		fix dla user_kill() z addfrag 
		oraz self kills
		*/
	}
}

void Client_ScoreInfo(void* mValue)
{
	static int iId;
	switch(mState++){
	case 0:
		iId = *(int*)mValue;
		break;
	case 4:
		if ( iId && (iId < 33) ){
			GET_PLAYER_POINTER_I(iId)->teamId = *(int*)mValue;
		}
		break;
	}
}

void Client_WeaponInfo(void* mValue)
{
	static int wpn;
	switch(mState++){
	case 0:
		wpn =  *(int*)mValue;
		if ( !wpn ) wpn = 36; // kung fu
		mPlayer->current = wpn;
		break;
	case 1:
		mPlayer->weapons[wpn].clip = *(int*)mValue;
		break;
	case 2:
		mPlayer->weapons[wpn].ammo = *(int*)mValue;
		break;
	case 3:
		mPlayer->weapons[wpn].mode = *(int*)mValue;
		break;
	case 4:
		mPlayer->weapons[wpn].attach = *(int*)mValue;
		break;
	}
}

void Client_ClipInfo(void* mValue)
{
	int iValue = *(int*)mValue;
	if ( iValue < mPlayer->weapons[mPlayer->current].clip ) {
		mPlayer->saveShot(mPlayer->current);
	}
	mPlayer->weapons[mPlayer->current].clip = iValue;
}

void Client_TSHealth_End(void* mValue){
	edict_t *enemy = mPlayer->pEdict->v.dmg_inflictor;
	int damage = (int)mPlayer->pEdict->v.dmg_take;

	if ( !damage || !enemy )
		return;

	int aim = 0;
	int weapon = 0;
	mPlayer->pEdict->v.dmg_take = 0.0; 

	CPlayer* pAttacker = NULL;
	if ( enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT) ){
		pAttacker = GET_PLAYER_POINTER(enemy);
		weapon = pAttacker->current;
		aim = pAttacker->aiming;
		pAttacker->saveHit( mPlayer , weapon , damage, aim );
	}
	else {
		char szCName[16];
		strcpy( szCName,STRING(enemy->v.classname) );

		if ( szCName[0] == 'g' ) { 
			if ( enemy->v.owner && enemy->v.owner->v.flags & (FL_CLIENT | FL_FAKECLIENT) ){ 
				pAttacker = GET_PLAYER_POINTER(enemy->v.owner);
				weapon = 24; // grenade
				if ( pAttacker != mPlayer )
					pAttacker->saveHit( mPlayer , weapon , damage, 0 );
			}
		}
		else if ( szCName[0] == 'k' ) {
			edict_t *pOwner =  (edict_t *)*( (int*)enemy->pvPrivateData + gKnifeOffset );

			if ( FNullEnt( (edict_t*)pOwner) )
				return;

			pAttacker = GET_PLAYER_POINTER( pOwner );
			
			weapon = 37; // throwing knife
			aim = pAttacker ? pAttacker->aiming : 0;
			if (pAttacker)
				pAttacker->saveHit( mPlayer , weapon , damage, aim );
		}
	}
	if ( !pAttacker ) pAttacker = mPlayer;

	int TA = 0;
	if ( mPlayer->teamId || is_theonemode  ){
		if ( (mPlayer->teamId == pAttacker->teamId ) && (mPlayer != pAttacker) )
			TA = 1;
	}

	if ( weaponData[weapon].melee ) 
		pAttacker->saveShot(weapon);
	
	MF_ExecuteForward(g_damage_info,
		(cell)pAttacker->index,
		(cell)mPlayer->index,
		(cell)damage,
		(cell)weapon,
		(cell)aim,
		(cell)TA
		);

	if ( mPlayer->IsAlive() )
		return;

	// death

    if ( (int)pAttacker->pEdict->v.frags - pAttacker->frags == 0 ) // nie bylo fraga ? jest tak dla bledu z granatem ..
		pAttacker = mPlayer;

	int killFlags = 0;

	if ( !TA && mPlayer!=pAttacker ) {
		int sflags = pAttacker->pEdict->v.iuser4;
	
		int stuntKill = 0;
		int slpos = 0;

		if ( weapon == 24 ) // dla granata nie liczy sie sflags
			; // nic nie rob..
		else if ( sflags == 20 || sflags == 1028 || sflags == 2052 )
			stuntKill = 1;
		else if ( sflags == 36)
			slpos = 1;

		int doubleKill = 0;
		
		if ( gpGlobals->time - pAttacker->lastKill < 1.0 )
			doubleKill = 1;
		
		if ( stuntKill )
			killFlags |= TSKF_STUNTKILL;
		
		pAttacker->lastKill = gpGlobals->time;
	
		pAttacker->killingSpree++;

		if ( pAttacker->killingSpree == 10 )
			pAttacker->is_specialist = 1;
	
		pAttacker->lastFrag = weaponData[weapon].bonus + 2*stuntKill;

		if ( doubleKill ){
			pAttacker->lastFrag *= 2;
			killFlags |= TSKF_DOUBLEKILL;
		}

		if ( pAttacker->is_specialist ){
			pAttacker->lastFrag *= 2;
			killFlags |= TSKF_ISSPEC;
		}

		if ( mPlayer->is_specialist ){
			pAttacker->lastFrag += 5; 
			killFlags |= TSKF_KILLEDSPEC;
		}

		pAttacker->frags += pAttacker->lastFrag; 
			if ( pAttacker->frags != pAttacker->pEdict->v.frags ){
				// moze to sliding kill ?
				if ( slpos )
					killFlags |= TSKF_SLIDINGKILL;	
				else  // moze to kung fu z bronia ?
					weapon = 36;
				pAttacker->lastFrag += (int)pAttacker->pEdict->v.frags - pAttacker->frags;
				pAttacker->frags = (int)pAttacker->pEdict->v.frags;
			}
	}

	pAttacker->killFlags = killFlags;
	pAttacker->saveKill(mPlayer,weapon,( aim == 1 ) ? 1:0 ,TA);
	MF_ExecuteForward(g_death_info,
		(cell)pAttacker->index,
		(cell)mPlayer->index,
		(cell)weapon,
		(cell)aim,
		(cell)TA);
}

void Client_TSState(void* mValue)
{
	mPlayer->oldstate = mPlayer->state;
	mPlayer->checkstate = 1;
	mPlayer->state =  *(int*)mValue;
}

void Client_WStatus(void* mValue)
{
	switch(mState++){
	case 1:
		if ( !*(int*)mValue ){
			mPlayer->current = 36; // fix dla wytraconej broni
		}
		break;
	}
}

void Client_TSCash(void* mValue)
{
	mPlayer->money = *(int*)mValue;
}

void Client_TSSpace(void* mValue)
{
	mPlayer->space = *(int*)mValue;
}

void Client_PwUp(void* mValue)
{
	static int iPwType;
	switch(mState++){
	case 0:
		iPwType = *(int*)mValue;
		switch(iPwType){
		case TSPWUP_KUNGFU :
			mPlayer->items |= TSITEM_KUNGFU;
			break;
		case TSPWUP_SJUMP:
			mPlayer->items |= TSITEM_SUPERJUMP;
			break;
		default: mPlayer->PwUp = iPwType;
		}
		break;
	case 1:
		if ( iPwType != TSPWUP_KUNGFU && iPwType != TSPWUP_SJUMP )
			mPlayer->PwUpValue = *(int*)mValue;
		break;
	}
}
