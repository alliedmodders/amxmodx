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
#include "functions.h"
#include "msghooking.h"
#include "weapinfo.h"
#include "CPlayer.h"
#include "misc.h"


static cell AMX_NATIVE_CALL tse_getuserslots(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetFreeSlots();
}

static cell AMX_NATIVE_CALL tse_setuserslots(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	Player(pid)->SetFreeSlots(params[2]);
	return 1;
}

static cell AMX_NATIVE_CALL tse_getusercash(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetCash();
}

static cell AMX_NATIVE_CALL tse_setusercash(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	Player(pid)->SetCash(params[2]);
	return 1;
}

static cell AMX_NATIVE_CALL tse_getuserkevlar(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return (bool)(Player(pid)->GetPDataInt(628) != 0);
}

static cell AMX_NATIVE_CALL tse_getuserstamina(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetMeleeStamina();
}

static cell AMX_NATIVE_CALL tse_setuserstamina(AMX *amx, cell *params) 
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	Player(pid)->SetMeleeStamina(amx_ctof(params[2]));
	MESSAGE_BEGIN(MSG_ONE, GetMsgIDByName("KFuPower"), 0, Player(pid)->PlayerEdict);
		WRITE_BYTE((byte)round(amx_ctof(params[2])));
	MESSAGE_END();
	return 1;
}

static cell AMX_NATIVE_CALL tse_geteffectsphysspd(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetEffectsPhysicsSpeed();
}

static cell AMX_NATIVE_CALL tse_seteffectsphysspd(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	Player(pid)->SetEffectsPhysicsSpeed(amx_ctof(params[2]));
	return 1;
}

static cell AMX_NATIVE_CALL tse_getuserphysspd(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetPlayerPhysicsSpeed();
}

static cell AMX_NATIVE_CALL tse_setuserphysspd(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	Player(pid)->SetPlayerPhysicsSpeed(amx_ctof(params[2]));
	return 1;
}

static cell AMX_NATIVE_CALL tse_getusercurritems(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	switch ((uint32_t)Player(pid)->GetPDataInt(462))
	{
		case 65536:
			return 1;
		case 16777216:
			return 2;
		case 16842752:
			return 3;
	}
	return 0;
}

static cell AMX_NATIVE_CALL tse_setusercurritems(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	switch (params[2])
	{
		case 0: {
			Player(pid)->SetPDataInt(462, 0);
			break;
		}
		case 1: {
			Player(pid)->SetPDataInt(462, 65536);
			break;
		}
		case 2: {
			Player(pid)->SetPDataInt(462, 16777216);
			break;
		}
		case 3: {
			Player(pid)->SetPDataInt(462, 16842752);
			break;
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL tse_getuserstate(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	if (!Player(pid)->HooksInfo.State) return -1;
	return Player(pid)->HooksInfo.State;
}

static cell AMX_NATIVE_CALL tse_getuserstatus(AMX *amx, cell *params)
{
	byte pid = params[1];
	if (!IsPlayerValid(amx, pid)) return -1;
	return (Player(pid)->GetPDataInt(462) & 15);
}

static cell AMX_NATIVE_CALL tse_setuserstatus(AMX *amx, cell *params)
{
	byte pid = params[1];
	byte status = params[2];
	if (!IsPlayerValid(amx, pid)) return 0;
	Player(pid)->SetPDataInt(462, (Player(pid)->GetPDataInt(462) & ~15) ^ status);
	return 1;
}

static cell AMX_NATIVE_CALL tse_createpwup(AMX *amx, cell *params)
{
	uint16_t type = params[1];
	if (type < 1 || type > 256) return -1;
	cell *coordptr = MF_GetAmxAddr(amx, params[2]);
	short ttl = params[3];
	return ENTINDEX(CreatePowerup(type, Vector(*(float *)&coordptr[0], *(float *)&coordptr[1], *(float *)&coordptr[2]), ttl));
}

static cell AMX_NATIVE_CALL tse_giveuserpwup(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	uint16_t type = params[2];
	if (type < 1 || type > 256) return 0;
	byte duration = params[3];
	if (duration < 0 || duration > 255) return 0;
	Player(pid)->HooksInfo.PwupFlag = true;
	Player(pid)->GivePowerup(type, duration);
	return 1;
}

static cell AMX_NATIVE_CALL tse_getuserpwup(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	if (params[2]) {
		cell *durationptr = MF_GetAmxAddr(amx, params[2]);
		*durationptr = Player(pid)->GetPDataInt(455);
	}
	return Player(pid)->GetPDataInt(453);
}

static cell AMX_NATIVE_CALL tse_getuseractivepwup(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return Player(pid)->GetPDataInt(452);
}

static cell AMX_NATIVE_CALL tse_setfakepwup(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte pwup = params[2];
	float duration = amx_ctof(params[3]);
	Player(pid)->SetPDataInt(453, pwup);
	Player(pid)->SetPDataInt(452, pwup);
	Player(pid)->SetPDataFloat(454, Player(pid)->GetTime() + duration);
	if (pwup == TSE_PWUP_SLOWMO || pwup == TSE_PWUP_SLOWPAUSE) 
	{
		float speed = amx_ctof(params[4]);
		float auradist = amx_ctof(params[5]);
		if (speed != -1.0 && auradist != -1.0) 
		{
			Player(pid)->SetPDataFloat(90, speed);
			Player(pid)->SetPDataFloat(91, auradist);
			Player(pid)->SetPDataFloat(86, auradist);
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL tse_setuserpwupduration(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte duration = params[2];
	Player(pid)->SetPDataInt(455, duration);
	return 1;
}

static cell AMX_NATIVE_CALL tse_configmeleeatk(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	float damage = amx_ctof(params[2]);
	float duration = amx_ctof(params[3]);
	Player(pid)->SetPDataFloat(483, damage);
	Player(pid)->SetPDataFloat(482, duration);
	return 1;
}

AMX_NATIVE_INFO pl_funcs[] = {
	{ "tse_getuserslots", tse_getuserslots },
	{ "tse_setuserslots", tse_setuserslots },
	{ "tse_getusercash", tse_getusercash },
	{ "tse_setusercash", tse_setusercash },
	{ "tse_getuserkevlar", tse_getuserkevlar },
	{ "tse_getuserstamina", tse_getuserstamina },
	{ "tse_setuserstamina", tse_setuserstamina },
	{ "tse_getuserstamina", tse_getuserstamina },
	{ "tse_setuserstamina", tse_setuserstamina },
	{ "tse_geteffectsphysspd", tse_geteffectsphysspd },
	{ "tse_seteffectsphysspd", tse_seteffectsphysspd },
	{ "tse_getuserphysspd", tse_getuserphysspd },
	{ "tse_setuserphysspd", tse_setuserphysspd },
	{ "tse_getusercurritems", tse_getusercurritems },
	{ "tse_setusercurritems", tse_setusercurritems },
	{ "tse_createpwup", tse_createpwup },
	{ "tse_giveuserpwup", tse_giveuserpwup },
	{ "tse_getuserpwup", tse_getuserpwup },
	{ "tse_getuserstate", tse_getuserstate },
	{ "tse_getuserstatus", tse_getuserstatus },
	{ "tse_setuserstatus", tse_setuserstatus },
	{ "tse_getuseractivepwup", tse_getuseractivepwup },
	{ "tse_setfakepwup", tse_setfakepwup },
	{ "tse_setuserpwupduration", tse_setuserpwupduration },
	{ "tse_configmeleeatk", tse_configmeleeatk },
	{ NULL, NULL }
};

static cell AMX_NATIVE_CALL tse_sendweapparams(AMX *amx, cell *params)
{
	byte pid = params[1];
	if (!IsPlayerValid(amx, pid)) return 0;
	char paramsstr[128];
	snprintf(paramsstr, 128, "%i %f %f %f %f %i", params[2], amx_ctof(params[3]), amx_ctof(params[4]), amx_ctof(params[5]), amx_ctof(params[6]), params[7]);
	MESSAGE_BEGIN(MSG_ONE, GetMsgIDByName("CustomWP"), 0, Player(pid)->PlayerEdict);
		WRITE_STRING(paramsstr);
	MESSAGE_END();
	return 1;
}

static cell AMX_NATIVE_CALL tse_getuserweapent(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	return ENTINDEX(Player(pid)->GetWeaponEdict());
}

static cell AMX_NATIVE_CALL tse_getusercurrweap(AMX *amx, cell *params)
{ 
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	int weapon = Player(pid)->CurrentWeapon;
	return weapon;
}

static cell AMX_NATIVE_CALL tse_setweapfiremode(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	byte mode = params[3];
	if (weapon < 1 || weapon > MAXWEAPS) return 0;
	if (mode < 0 || mode > 5) return 0;
	if (!WeaponsList[weapon].offsets.clip) return 0;
	uint32_t ofstval = 1 + WeaponsList[weapon].offsets.fmbase + FireModes[mode];
	if (Player(pid)->CurrentWeapon == weapon)
	{
		Player(pid)->SetWeapPDataInt(603, ofstval);
		Player(pid)->Weapons[weapon].mode = mode;
		Player(pid)->UpdateHUD();
	}
	Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip - 1, ofstval);
	return 1;
}

static cell AMX_NATIVE_CALL tse_getweapfiremode(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	byte weapon = params[2];
	if (weapon < 1 || weapon > MAXWEAPS) return -1;
	if (!WeaponsList[weapon].offsets.clip) return -1;
	return GetFiremodeByMask(Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.clip - 1) - WeaponsList[weapon].offsets.fmbase - 1);
}

static cell AMX_NATIVE_CALL tse_setweapatcments(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	byte attachments = params[3];
	byte isactive = params[4];
	if (weapon < 1 || weapon > MAXWEAPS) return 0;
	if (attachments < 0 || attachments > 15) return 0;
	if (!WeaponsList[weapon].offsets.clip) return 0;
	if (Player(pid)->CurrentWeapon == weapon)
	{
		Player(pid)->Weapons[weapon].attachments = attachments;
		Player(pid)->UpdateHUD();
		if (isactive) {
			MESSAGE_BEGIN(MSG_ONE, GetMsgIDByName("ActItems"), 0, Player(pid)->PlayerEdict);
				WRITE_BYTE(1);
				WRITE_BYTE(attachments);
			MESSAGE_END();
		}
	}
	if (isactive) {
		Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip + 3, attachments);
		Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip + 4, attachments);
	}
	else
		Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip + 3, attachments);
	return 1;
}

static cell AMX_NATIVE_CALL tse_setweapclip(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	if (!WeaponsList[weapon].offsets.clip) return 0;
	byte clip = params[3];
	Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip, clip);
	return 1;
}

static cell AMX_NATIVE_CALL tse_setweapammo(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	if (!WeaponsList[weapon].offsets.clip) return 0;
	byte ammo = params[3];
	Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.ammo, ammo);
	return 1;
}

static cell AMX_NATIVE_CALL tse_setweapready(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	if (!WeaponsList[weapon].offsets.clip) return 0;
	int isready = params[3];
	Player(pid)->SetWeapPDataInt(WeaponsList[weapon].offsets.clip + 8, (int)!(bool)(isready != 0));
	return 1;
}

static cell AMX_NATIVE_CALL tse_getweapinfo(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	byte weapon = params[2];
	if (weapon < 1 || weapon > MAXWEAPS) return 0;
	if (!WeaponsList[weapon].offsets.clip) return 0;
	cell *clip = MF_GetAmxAddr(amx, params[3]);
	cell *ammo = MF_GetAmxAddr(amx, params[4]);
	cell *attachs = MF_GetAmxAddr(amx, params[5]);
	cell *active_attachs = MF_GetAmxAddr(amx, params[6]);
	*clip = Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.clip);
	*ammo = Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.ammo);
	*attachs = Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.clip + 3);
	*active_attachs = Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.clip + 4);
	return 1;
}

static cell AMX_NATIVE_CALL tse_isuserhasweap(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_NGTV(pid);
	byte weapon = params[2];
	if (!WeaponsList[weapon].offsets.clip) return -1;
	byte offset = WeaponsList[weapon].offsets.fmbase != 0 ? 1 : 0;
	return Player(pid)->GetWeapPDataInt(WeaponsList[weapon].offsets.clip - offset) > 0;
}

static cell AMX_NATIVE_CALL tse_createweap(AMX *amx, cell *params)
{
	byte weapid = params[1];
	cell *coordptr = MF_GetAmxAddr(amx, params[2]);
	uint16_t clips = params[3];
	byte atcments = params[4];
	short ttl = params[5];
	return ENTINDEX(CreateWeapon(weapid, Vector(*(float *)((void *)&coordptr[0]), *(float *)((void *)&coordptr[1]), *(float *)((void *)&coordptr[2])), ttl, clips, atcments));
}

static cell AMX_NATIVE_CALL tse_giveuserweap(AMX *amx, cell *params)
{
	byte pid = params[1];
	CHECK_PLAYER_ZERO(pid);
	int weapid = params[2];
	if (weapid < 1 || weapid > MAXWEAPS) return 0;
	uint16_t clips = params[3];
	byte atcments = params[4];
	edict_t *weapent = CreateWeapon(weapid, Player(pid)->PlayerEdict->v.origin, 120, clips, atcments);
	MDLL_Use(weapent, Player(pid)->PlayerEdict);
	REMOVE_ENTITY(weapent);
	return 1;
}

AMX_NATIVE_INFO weap_funcs[] = {
	{ "tse_sendweapparams", tse_sendweapparams },
	{ "tse_getuserweapent", tse_getuserweapent },
	{ "tse_getusercurrweap", tse_getusercurrweap },
	{ "tse_setweapfiremode", tse_setweapfiremode },
	{ "tse_getweapfiremode", tse_getweapfiremode }, 
	{ "tse_setweapatcments", tse_setweapatcments },
	{ "tse_setweapclip", tse_setweapclip },
	{ "tse_setweapammo", tse_setweapammo },
	{ "tse_setweapready", tse_setweapready },
	{ "tse_getweapinfo", tse_getweapinfo },
	{ "tse_isuserhasweap", tse_isuserhasweap },
	{ "tse_createweap", tse_createweap },
	{ "tse_giveuserweap", tse_giveuserweap },
	{ NULL, NULL }
};