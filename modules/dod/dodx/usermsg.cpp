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
#include "dodx.h"

void Client_ResetHUD_End(void* mValue)
{
	mPlayer->clearStats = gpGlobals->time + 0.25f;
}

void Client_RoundState(void* mValue)
{
	if ( mPlayer ) return;
	int result = *(int*)mValue;
	if ( result == 1 )
	{
		for (int i=1;i<=gpGlobals->maxClients;i++)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
			if (pPlayer->ingame) 
			{
				pPlayer->clearRound = gpGlobals->time + 0.25f;
			}
		}
	}
}

void Client_TeamScore(void* mValue)
{
	static int index;

	switch(mState++)
	{
	case 0:
		index = *(int*)mValue;
		break;
	case 1:
		switch (index)
		{
		case 1:
			AlliesScore = *(int*)mValue;
			break;
		case 2:
			AxisScore = *(int*)mValue;
			break;
		}
		break;
	}
}

void Client_ObjScore(void* mValue)
{
	static CPlayer *pPlayer;
	static int score;

	switch(mState++)
	{
	case 0:
		pPlayer = GET_PLAYER_POINTER_I(*(int*)mValue);
		break;
	case 1:
		score = *(int*)mValue;
		if ( (pPlayer->lastScore = score - (int)(pPlayer->savedScore)) && isModuleActive() )
		{
			pPlayer->updateScore(pPlayer->current,pPlayer->lastScore);
			pPlayer->sendScore = (int)(gpGlobals->time + 0.25f);
		}
		pPlayer->savedScore = score;
		break;
	}
}

void Client_CurWeapon(void* mValue)
{
	static int iState;
	static int iId;

	switch (mState++)
	{
		case 0: 
			iState = *(int*)mValue;
			break;

		case 1:
			if (!iState) 
				break; 

			iId = *(int*)mValue;
			break;

		case 2:
			if(!iState || !isModuleActive())
				break;

			int iClip = *(int*)mValue;
			mPlayer->old = mPlayer->current;
			mPlayer->current = iId;

			if(weaponData[iId].needcheck)
			{
				iId = get_weaponid(mPlayer);
				mPlayer->current = iId;
			}

			if(iClip > -1) 
			{
				if(mPlayer->current == 17)
				{
					if(iClip+2 == mPlayer->weapons[iId].clip)
					mPlayer->saveShot(iId);
				}

				else 
				{
					if ( iClip+1 == mPlayer->weapons[iId].clip)
						mPlayer->saveShot(iId);
				}
			}

			mPlayer->weapons[iId].clip = iClip;
			mCurWpnEnd = 1;
			break;
	};
}

void Client_CurWeapon_End(void*)
{
	if(mCurWpnEnd == 1 && mPlayer->index && mPlayer->current && mPlayer->old && (mPlayer->current != mPlayer->old))
		MF_ExecuteForward(iFCurWpnForward, mPlayer->index, mPlayer->current, mPlayer->old);

	mCurWpnEnd = 0;
}


/*
Nie ma damage event ...
*/
void Client_Health_End(void* mValue)
{
    if ( !isModuleActive() )
		return;

	if (!mPlayer)
		return;

	edict_t *enemy = mPlayer->pEdict->v.dmg_inflictor;
	int damage = (int)mPlayer->pEdict->v.dmg_take;

	if (!damage || !enemy)
		return;
	
	int weapon = 0;
	int aim = 0;
		
	mPlayer->pEdict->v.dmg_take = 0.0; 
	
	CPlayer* pAttacker = NULL;

	if(enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT))
	{
		pAttacker = GET_PLAYER_POINTER(enemy);
		weapon = pAttacker->current;

		if ( weaponData[weapon].needcheck )
			weapon = get_weaponid(pAttacker);

		aim = pAttacker->aiming;

		if ( weaponData[weapon].melee )
			pAttacker->saveShot(weapon);
	}
	else 
	{
		g_grenades.find(enemy , &pAttacker , weapon);
	}

	int TA = 0;
	
	if ( !pAttacker )
	{
		pAttacker = mPlayer;
	}

	if ( pAttacker->index != mPlayer->index )
	{ 
		pAttacker->saveHit( mPlayer , weapon , damage, aim );

		if ( mPlayer->pEdict->v.team == pAttacker->pEdict->v.team )
			TA = 1;
	}

	MF_ExecuteForward( iFDamage, pAttacker->index, mPlayer->index, damage, weapon, aim, TA );

	if ( !mPlayer->IsAlive() )
	{
		pAttacker->saveKill(mPlayer,weapon,( aim == 1 ) ? 1:0 ,TA);
		MF_ExecuteForward( iFDeath, pAttacker->index, mPlayer->index, weapon, aim, TA );
	}
}

void Client_AmmoX(void* mValue)
{
  static int iAmmo;

  switch (mState++)
  {
  case 0:
    iAmmo = *(int*)mValue;
    break;
  case 1:
	if (!mPlayer ) 
		break;
    for(int i = 1; i < DODMAX_WEAPONS ; ++i)
	{
      if (iAmmo == weaponData[i].ammoSlot)
        mPlayer->weapons[i].ammo = *(int*)mValue;
	}
  }
}

void Client_AmmoShort(void* mValue)
{
  static int iAmmo;

  switch (mState++)
  {
  case 0:
    iAmmo = *(int*)mValue;
    break;

  case 1:
	if(!mPlayer ) 
		break;

    for(int i = 1; i < DODMAX_WEAPONS ; ++i) 
	{
      if (iAmmo == weaponData[i].ammoSlot)
		  mPlayer->weapons[i].ammo = *(int*)mValue;
	}
  }
}

// Called with a value of 90 at start 20 when someone scopes in and 0 when they scope out
void Client_SetFOV(void* mValue)
{
	if(!mPlayer)
		return;

	mPlayer->Scoping(*(int*)mValue);
}

void Client_SetFOV_End(void* mValue)
{
	if(!mPlayer)
		return;

	mPlayer->ScopingCheck();
}

void Client_Object(void* mValue)
{
	if(!mPlayer) 
		return;

	// First need to find out what was picked up
	const char *classname;
	edict_t* pObject = NULL;

	//const char* value;

	//if(mValue)
	//{
	//	value = (char*)mValue;
	//}

	if(!mPlayer->object.carrying)
	{
		// We grab the first object within the sphere of our player
		pObject = FindEntityInSphere(mPlayer->pEdict, mPlayer->pEdict, 50.0);

		// The loop through all the objects within the sphere
		while(pObject && !FNullEnt(pObject))
		{
			classname = STRING(pObject->v.classname);

			if(strcmp(classname, "dod_object") == 0)
			{
				mPlayer->object.pEdict = pObject;
				mPlayer->object.do_forward = true;
				return;
			}

			pObject = FindEntityInSphere(pObject, mPlayer->pEdict, 50.0);
		}
	}

	else
	{
		mPlayer->object.do_forward = true;
	}
}

void Client_Object_End(void* mValue)
{
	if(!mPlayer) 
		return;

	float fposition[3];

	if(mPlayer->object.do_forward)
	{
		mPlayer->object.do_forward = (mPlayer->object.do_forward) ? false : true;
		mPlayer->object.carrying = (mPlayer->object.carrying) ? false : true;

		mPlayer->object.pEdict->v.origin.CopyToArray(fposition);
		cell position[3]; 
		position[0] = amx_ftoc(fposition[0]);
		position[1] = amx_ftoc(fposition[1]);
		position[2] = amx_ftoc(fposition[2]);
		cell pos = MF_PrepareCellArray(position, 3);
		MF_ExecuteForward(iFObjectTouched, mPlayer->index, ENTINDEX(mPlayer->object.pEdict), pos, mPlayer->object.carrying);

		if(!mPlayer->object.carrying)
			mPlayer->object.pEdict = NULL;
	}
}

// This seems to be only called when the player spawns
void Client_PStatus(void* mValue)
{
	switch(mState++)
	{
		case 0:
			MF_ExecuteForward(iFSpawnForward, *(int*)mValue);
		break;
	}
}

