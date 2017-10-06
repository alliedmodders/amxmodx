// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017 SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxxmodule.h"
#include "msghooking.h"
#include "CPlayer.h"

CPlayer PlayersArray[33];

void CPlayer::UpdateHUD()
{
	MESSAGE_BEGIN(MSG_ONE, GetMsgIDByName("WeaponInfo"), 0, this->PlayerEdict);
		WRITE_BYTE(this->CurrentWeapon);
		WRITE_BYTE(this->Weapons[this->CurrentWeapon].clip);
		WRITE_SHORT(this->Weapons[this->CurrentWeapon].ammo);
		WRITE_BYTE(this->Weapons[this->CurrentWeapon].mode);
		WRITE_BYTE(this->Weapons[this->CurrentWeapon].attachments);
	MESSAGE_END();
}

void CPlayer::GivePowerup(uint16_t type, byte duration)
{
	this->SetPDataInt(453, type);
	this->SetPDataInt(455, duration);
}

uint16_t CPlayer::GetPowerup(int *duration)
{
	if (duration)
		*duration = this->GetPDataInt(455);
	return (uint16_t)this->GetPDataInt(453);
}

uint16_t CPlayer::GetPowerup()
{
	return (uint16_t)this->GetPDataInt(453);
}

edict_t *CPlayer::GetWeaponEdict()
{
	edict_t *weapon = NULL;
	while ((weapon = g_engfuncs.pfnFindEntityByString(weapon, "classname", "weapon_tsgun")) != 0)
		if (weapon->v.owner == this->PlayerEdict)
			return weapon;
	return nullptr;
}

edict_t *CreateWeapon(int id, Vector coord, short ttl, uint16_t clips, byte atcments) {
	edict_t *ent = CREATE_NAMED_ENTITY((string_t)MAKE_STRING("ts_groundweapon"));
	if (FNullEnt(ent)) return nullptr;

	KeyValueData params;
	char buffer[24];

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "tsweaponid";
	sprintf(buffer, "%d", id);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "wduration";
	sprintf(buffer, "%d", ttl);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "wextraclip";
	sprintf(buffer, "%d", clips);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "spawnflags";
	sprintf(buffer, "%d", atcments);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	MDLL_Spawn(ent);
	ent->v.origin = coord;
	return ent;
}

edict_t *CreatePowerup(uint16_t type, Vector coord, short ttl) {
	edict_t *ent = CREATE_NAMED_ENTITY((string_t)MAKE_STRING("ts_powerup"));
	if (FNullEnt(ent)) return nullptr;

	KeyValueData params;
	char buffer[24];

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "pwuptype";
	sprintf(buffer, "%d", type);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	params.szClassName = (char *)STRING(ent->v.classname);
	params.szKeyName = "pwupduration";
	sprintf(buffer, "%d", ttl);
	params.szValue = buffer;
	params.fHandled = false;
	MDLL_KeyValue(ent, &params);

	MDLL_Spawn(ent);
	ent->v.origin = coord;
	return ent;
}

void CPlayer::Init(edict_t* ple, int plindex)
{
	this->PlayerEdict = ple;
	this->PlayerIndex = plindex;
}

long CPlayer::GetPDataInt(long idx)
{	
	#ifndef WIN32
		idx += 5;
	#endif
	return *((int *)this->PlayerEdict->pvPrivateData + idx);
}		

float CPlayer::GetPDataFloat(long idx)
{
	#ifndef WIN32
		idx += 5;
	#endif
	return *((float *)this->PlayerEdict->pvPrivateData + idx);
}

void CPlayer::SetPDataInt(long idx, long val)
{
	#ifndef WIN32
		idx += 5;
	#endif
	*((int *)this->PlayerEdict->pvPrivateData + idx) = val;
}

void CPlayer::SetPDataFloat(long idx, float val)
{
	#ifndef WIN32
		idx += 5;
	#endif
	*((float *)this->PlayerEdict->pvPrivateData + idx) = val;
}

long CPlayer::GetWeapPDataInt(long idx)
{
	#ifndef WIN32
		idx += 4;
	#endif
	return *((int *)this->GetWeaponEdict()->pvPrivateData + idx);
}

float CPlayer::GetWeapPDataFloat(long idx)
{
	#ifndef WIN32
		idx += 4;
	#endif
	return *((float *)this->GetWeaponEdict()->pvPrivateData + idx);
}

void CPlayer::SetWeapPDataInt(long idx, long val)
{
	#ifndef WIN32
		idx += 4;
	#endif
	*((int *)this->GetWeaponEdict()->pvPrivateData + idx) = val;
}

void CPlayer::SetWeapPDataFloat(long idx, float val)
{
	#ifndef WIN32
		idx += 4;
	#endif
	*((int *)this->GetWeaponEdict()->pvPrivateData + idx) = val;
}

