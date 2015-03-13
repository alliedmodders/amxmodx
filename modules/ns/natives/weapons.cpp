// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#include "amxxmodule.h"

#include "ns.h" 

#include "utilfunctions.h"
#include "NEW_Util.h"

#include "GameManager.h"
#include "CPlayer.h"

// ns_has_weapon(idPlayer,NsWeapon,set=0)
static cell AMX_NATIVE_CALL ns_has_weapon(AMX *amx,cell *params)
{
	CreatePlayerPointer(amx,params[1]);
	
	if (!player->IsConnected())
	{
		return 0;
	}

	if (params[3] == -1)
	{
		if ((player->GetPev()->weapons & (1<<params[2])) > 0)
		{
			return 1;
		}
	}
	else
	{
		if ((player->GetPev()->weapons & (1<<params[2])) > 0)
		{
			if (params[3] == 0)
			{
				player->GetPev()->weapons &= ~(1<<params[2]);
				return 1;
			}
			return 0;
		}
		else
		{
			if (params[3] == 1)
			{
				player->GetPev()->weapons |= (1<<params[2]);
				return 1;
			}
		}
		return 0;
	}
	return 0;
}
// ns_set_weap_dmg(WeaponID,Float:damage)
static cell AMX_NATIVE_CALL ns_set_weapon_dmg(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	set_private_f(Entity,MAKE_OFFSET(WEAPDMG),amx_ctof2(params[2]));

	return 1;
}

// Float:ns_get_weap_dmg(WeaponID)
static cell AMX_NATIVE_CALL ns_get_weapon_dmg(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	REAL ret=get_private_f(Entity,MAKE_OFFSET(WEAPDMG));
	return amx_ftoc2(ret);
}

// ns_set_weap_range(WeaponID,Float:range)
static cell AMX_NATIVE_CALL ns_set_weapon_range(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	set_private_f(Entity,MAKE_OFFSET(WEAPRANGE),amx_ctof2(params[2]));

	return 1;
}
// Float:ns_get_weap_range(WeaponID)
static cell AMX_NATIVE_CALL ns_get_weapon_range(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	REAL ret=get_private_f(Entity,MAKE_OFFSET(WEAPRANGE));
	return amx_ftoc2(ret);
}
// ns_get_weap_ammo(WeaponID)
static cell AMX_NATIVE_CALL ns_get_weapon_clip(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	return get_private(Entity,MAKE_OFFSET(WEAPCLIP));
}
// ns_set_weap_ammo(WeaponID,ammo)
static cell AMX_NATIVE_CALL ns_set_weapon_clip(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	set_private(Entity,MAKE_OFFSET(WEAPCLIP),params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_add_weapon_clip(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL || Entity->free)
	{
		return 0;
	}

	return inc_private(Entity,MAKE_OFFSET(WEAPCLIP),static_cast<int>(params[2]),0);
}
// ns_get_weap_reserve(PlayerID,WeaponType)
static cell AMX_NATIVE_CALL ns_get_weap_reserve(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	switch (params[2])
	{
	case WEAPON_PISTOL:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_PISTOL));
	case WEAPON_LMG:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_LMG));
	case WEAPON_SHOTGUN:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_SHOTGUN));
	case WEAPON_HMG:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_HMG));
	case WEAPON_GRENADE_GUN:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_GL));
	case WEAPON_GRENADE:
		return get_private(player->GetEdict(),MAKE_OFFSET(AMMO_HG));
	default:
		return 0;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_set_weap_reserve(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	switch (params[2])
	{
	case WEAPON_PISTOL:
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_PISTOL),params[3]);
		return 1;
	case WEAPON_LMG:
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_LMG),(int)params[3]);
		return 1;
	case WEAPON_SHOTGUN: 
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_SHOTGUN),(int)params[3]); 
		return 1;
	case WEAPON_HMG:
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_HMG),(int)params[3]);
		return 1;
	case WEAPON_GRENADE_GUN:
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_GL),(int)params[3]);
		return 1;
	case WEAPON_GRENADE:
		set_private(player->GetEdict(),MAKE_OFFSET(AMMO_HG),(int)params[3]);
		return 1;
	default:
		return 0;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_add_weap_reserve(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	switch (params[2])
	{
	case WEAPON_PISTOL:
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_PISTOL),params[3],0);
	case WEAPON_LMG:
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_LMG),(int)params[3],0);
	case WEAPON_SHOTGUN: 
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_SHOTGUN),(int)params[3],0); 
	case WEAPON_HMG:
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_HMG),(int)params[3],0);
	case WEAPON_GRENADE_GUN:
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_GL),(int)params[3],0);
	case WEAPON_GRENADE:
		return inc_private(player->GetEdict(),MAKE_OFFSET(AMMO_HG),(int)params[3],0);
	default:
		return 0;
	}
	return 0;
}
// ns_get_weapon(idPlayer,weaponid,&weapontype=0)
static cell AMX_NATIVE_CALL ns_get_weapon(AMX *amx, cell *params)
{
	// Peachy did it like this:
	// if weapontype is 0, return the primary weapon index of the player
	// if weapontype is < 0, return the last inventory weapon index of the player
	// otherwise, scan the player's inventory and look for a weapon of the given type
	// such as WEAPON_KNIFE, etc, etc
	// I added the last parameter, which will byref the weapontype of the weapon found
	// returns 0 on failure
	// last param default value added to not conflict with his version

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	if (params[2]<0) // find lastinv weapon
	{
		edict_t *Weapon=private_to_edict(get_private_p<void *>(player->GetEdict(),MAKE_OFFSET(LAST_WEAPON)));

		if (Weapon==NULL) // no weapon
		{
			return 0;
		}

		if ((params[0] / sizeof(cell))>2) // If this plugin was compiled with peachy's .inc then don't byref
		{
			*MF_GetAmxAddr_NEW(amx,params[3])=get_private(Weapon,MAKE_OFFSET(WEAPID));
		}

		return ENTINDEX_NEW(Weapon);

	}
	if (params[2]==0) // find current weapon
	{
		edict_t *Weapon=private_to_edict(get_private_p<void *>(player->GetEdict(),MAKE_OFFSET(CURRENT_WEAPON)));

		if (Weapon==NULL) // no weapon
		{
			return 0;
		}

		if ((params[0] / sizeof(cell))>2) // If this plugin was compiled with peachy's .inc then don't byref
		{
			*MF_GetAmxAddr_NEW(amx,params[3])=get_private(Weapon,MAKE_OFFSET(WEAPID));
		}

		return ENTINDEX_NEW(Weapon);
	}

	// Finding weapon by ID

	char **pPlayerItems = reinterpret_cast<char**>(static_cast<char*>(player->GetEdict()->pvPrivateData) + MAKE_OFFSET(PLAYER_ITEMS));
	char *pItem;
	int weapon=params[2];

	for (int i = 0; i < 6; i++)
	{
		pItem = pPlayerItems[i];
		while (pItem)
		{
			if (*(int *)(pItem + MAKE_OFFSET(WEAPID)) == weapon)
			{
				return ENTINDEX_NEW(private_to_edict(pItem));
			}
			else
			{
				pItem = *(char **)(pItem + MAKE_OFFSET(WEAP_NEXT));
			}
		}
	}
	return 0;
}
#ifdef DEVELOPER_BUILD
// ns_find_weapon_offset(idPlayer,"primweapon","lastinvweapon")
static cell AMX_NATIVE_CALL ns_find_weapon_offset(AMX *amx, cell *params)
{
	char *SPrimWeapon=MF_GetAmxString(amx,params[2],0,NULL);
	char *SLastInv=MF_GetAmxString(amx,params[3],1,NULL);
	edict_t *ePlayer=INDEXENT_NEW(params[1]);

	// Locate entities by name
	edict_t *PrimWeapon=NULL;
	edict_t *LastInv=NULL;

	edict_t *Temp=NULL;

	while ((Temp=UTIL_FindEntityByString(Temp,"classname",SPrimWeapon))!=NULL)
	{
		if (Temp->v.owner==ePlayer)
		{
			PrimWeapon=Temp;
			break;
		}
	}
	Temp=NULL;
	while ((Temp=UTIL_FindEntityByString(Temp,"classname",SLastInv))!=NULL)
	{
		if (Temp->v.owner==ePlayer)
		{
			LastInv=Temp;
			break;
		}
	}

	if (LastInv == NULL || PrimWeapon == NULL)
	{
		if (LastInv==NULL)
		{
			MF_Log("LastInv==NULL");
		}
		if (PrimWeapon==NULL)
		{
			MF_Log("PrimWeapon=NULL");
		}
		return 0;
	}

	// now iterate through the client's private data until we find the pointer to PrimWeapon/LastInv's offset
	unsigned int *Ptr=(unsigned int*)ePlayer->pvPrivateData;

	int FoundLastInv=0;
	int FoundPrim=0;
	size_t count=0;
	unsigned int iPrim;
	unsigned int iLast;

	// so nasty D: this is basically horrible_cast
	union bleh
	{
		void *ptr;
		unsigned int ival;
	}blah;
	
	blah.ptr=PrimWeapon->pvPrivateData;
	iPrim=blah.ival;

	blah.ptr=LastInv->pvPrivateData;
	iLast=blah.ival;

	while (count<4000)
	{
		if (*Ptr==iLast)
		{
			MF_Log("Found LastInv: %d",count);
			FoundLastInv=1;
		}
		if (*Ptr==iPrim)
		{
			MF_Log("Found Primary: %d",count);
			FoundPrim=1;
		}

		if (FoundLastInv && FoundPrim)
		{
			//break;
		}
		count+=4;
		Ptr++;
	}
	return 1;
}
#endif


AMX_NATIVE_INFO weapon_natives[] = {

	{ "ns_has_weapon",			ns_has_weapon },

	{ "ns_set_weap_dmg",		ns_set_weapon_dmg },
	{ "ns_get_weap_dmg",		ns_get_weapon_dmg },

	{ "ns_set_weap_range",		ns_set_weapon_range },
	{ "ns_get_weap_range",		ns_get_weapon_range },

	{ "ns_set_weap_clip",		ns_set_weapon_clip },
	{ "ns_get_weap_clip",		ns_get_weapon_clip },
	{ "ns_add_weap_clip",		ns_add_weapon_clip },

	{ "ns_set_weap_reserve",	ns_set_weap_reserve },
	{ "ns_get_weap_reserve",	ns_get_weap_reserve },
	{ "ns_add_weap_reserve",	ns_add_weap_reserve },

	{ "ns_get_weapon",			ns_get_weapon},

#ifdef DEVELOPER_BUILD
	{ "ns_find_weapon_offset",	ns_find_weapon_offset},
#endif

	{ NULL,						NULL }
};
void AddNatives_Weapons()
{
	MF_AddNatives(weapon_natives);
}
