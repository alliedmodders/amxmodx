

#include "amxxmodule.h"
#include "rank.h"

weaponsVault weaponData[MAX_WEAPONS+MAX_CWEAPONS];

int damage;
int TA;
int weapon;
int aim;
CPlayer *pAttacker;

int g_Planter;
int g_Defuser;

void Client_ResetHUD(void* mValue){
	if ( mPlayer && mPlayer->IsAlive() ) 
		mPlayer->clearStats = gpGlobals->time + 0.25f;
}

void Client_WeaponList(void* mValue){
  static int wpnList;
  static int iSlot;
  static const char* wpnName;

  switch (mState++) {
  case 0:
    wpnName = (const char*)mValue;
    break;
  case 1:
    iSlot = *(int*)mValue;
    break;
  case 7:
    int iId = *(int*)mValue;
    if ( (iId < 0 || iId >= MAX_WEAPONS ) || ( wpnList & (1<<iId) ) )
      break;

    wpnList |= (1<<iId);
    weaponData[iId].ammoSlot = iSlot;

	char* wpnPrefix = strstr( wpnName,"weapon_");

	if ( wpnPrefix )
	{
		weaponData[iId].name = wpnPrefix + 7;
		if ( strcmp( weaponData[iId].name, "hegrenade" ) == 0 )
			weaponData[iId].name += 2;
	}
  } 
}

void Client_Damage(void* mValue){
  static int bits;
  switch (mState++) {
  case 1: 
    damage = *(int*)mValue;
    break;
  case 2:
    bits = *(int*)mValue;
    break;
  case 3:
    if (!mPlayer || !damage || !*(float*)mValue || bits)  break;
    edict_t *enemy;
	enemy = mPlayer->pEdict->v.dmg_inflictor;
    
	if ( FNullEnt( enemy ) )
		break;
	aim = 0;
	weapon = 0;
	pAttacker = NULL;
    
	if (enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT) ) {
		pAttacker = GET_PLAYER_POINTER(enemy);
		aim = pAttacker->aiming;
		weapon = pAttacker->current;
		pAttacker->saveHit( mPlayer , weapon , damage, aim);
		break;
	}
    if( g_grenades.find(enemy , &pAttacker , &weapon ) )
        pAttacker->saveHit( mPlayer , weapon , damage, aim );
  }
}

void Client_Damage_End(void* mValue){
	if ( !mPlayer || !damage )
		return;

	if ( !pAttacker ) pAttacker = mPlayer;
		TA = 0;
	if ( (mPlayer->teamId == pAttacker->teamId) && (mPlayer != pAttacker) )
		TA = 1;
	MF_ExecuteForward( iFDamage, pAttacker->index, mPlayer->index, damage, weapon, aim, TA );
	 
	if ( !mPlayer->IsAlive() ){
		pAttacker->saveKill(mPlayer,weapon,( aim == 1 ) ? 1:0 ,TA);
		MF_ExecuteForward( iFDeath, pAttacker->index, mPlayer->index, weapon, aim, TA );
	}
}

void Client_CurWeapon(void* mValue){
  static int iState;
  static int iId;
  switch (mState++){
  case 0: 
    iState = *(int*)mValue;
    break;
  case 1:
    if (!iState) break; 
    iId = *(int*)mValue;
    break;
  case 2:
	if (!mPlayer || !iState ) break;
    int iClip = *(int*)mValue;
    if ((iClip > -1) && (iClip < mPlayer->weapons[iId].clip)) 
      mPlayer->saveShot(iId);
    mPlayer->weapons[iId].clip = iClip;
	mPlayer->current = iId;
  }
}

void Client_AmmoX(void* mValue){
  static int iAmmo;
  switch (mState++){
  case 0:
    iAmmo = *(int*)mValue;
    break;
  case 1:
	if (!mPlayer ) break;
    for(int i = 1; i < MAX_WEAPONS ; ++i) 
      if (iAmmo == weaponData[i].ammoSlot)
        mPlayer->weapons[i].ammo = *(int*)mValue;
  }
}


void Client_AmmoPickup(void* mValue){
  static int iSlot;
  switch (mState++){
  case 0:
    iSlot = *(int*)mValue;
    break;
  case 1:
	if (!mPlayer ) break;
    for(int i = 1; i < MAX_WEAPONS ; ++i)
      if (weaponData[i].ammoSlot == iSlot)
        mPlayer->weapons[i].ammo += *(int*)mValue;
  }
}


void Client_ScoreInfo(void* mValue){
  static int index;
  switch (mState++){
  case 0:
    index = *(int*)mValue;
    break;
  case 4:
	if ( index > 0 && index <= gpGlobals->maxClients )
		GET_PLAYER_POINTER_I( index )->teamId = *(int*)mValue;
  }
}

void Client_SendAudio(void* mValue){
	static const char* szText;
	if ( mState == 1 ){
		szText = (const char*)mValue;
		if ( !mPlayer && szText[7]=='B' ) {
			if ( szText[11]=='P' && g_Planter )
				GET_PLAYER_POINTER_I(g_Planter)->saveBPlant();
			else if ( szText[11]=='D' && g_Defuser )
				GET_PLAYER_POINTER_I(g_Defuser)->saveBDefused();

		}
	}
	mState++;
}

void Client_TextMsg(void* mValue){
	static const char* szText;
	if ( !mPlayer && mState==1 ){
		szText = (const char*)mValue;
		if ( szText[1]=='T' && szText[8]=='B' && g_Planter )
			GET_PLAYER_POINTER_I(g_Planter)->saveBExplode();
	}
	mState++;
}

void Client_BarTime(void* mValue){
	int iTime = *(int*)mValue;
	if ( !iTime || !mPlayer->IsAlive() ) return;
	if ( iTime == 3 ){
		g_Planter = mPlayerIndex;
		g_Defuser = 0;
	}
	else {
		mPlayer->saveBDefusing();
		g_Defuser = mPlayerIndex;
	}
}