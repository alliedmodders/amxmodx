// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx.h>
#include <messages.h>

static cell AMX_NATIVE_CALL draw_ammo_pickup_icon(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[4] != 0), gmsgAmmoPickup, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(params[2]);
		WRITE_BYTE(params[3]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_weapon_pickup_icon(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[3] != 0), gmsgWeapPickup, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(params[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_status_icon(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	int len;
	int status = params[3];
	char* sprite = get_amxstring(amx, params[2], 0, len);

	MESSAGE_BEGIN(get_msg_destination(index, params[7] != 0), gmsgStatusIcon, NULL, index ? TypeConversion.id_to_edict(index) : NULL);

		WRITE_BYTE(status);

		if(status)
		{
			WRITE_STRING(sprite);
			WRITE_BYTE(params[4]);
			WRITE_BYTE(params[5]);
			WRITE_BYTE(params[6]);
		}

	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_train_controls(AMX *amx, cell *params)
{
	if(g_bmod_dod)
		return 0;

	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[3] != 0), gmsgTrain, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(params[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL send_geiger_signal(AMX *amx, cell *params)
{
	if(g_bmod_dod)
		return 0;
	
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[3] != 0), gmsgGeiger, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(params[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL hide_hud_elements(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	int destination = get_msg_destination(index, params[4] != 0);
	edict_t *receiver = index ? TypeConversion.id_to_edict(index) : NULL;

	MESSAGE_BEGIN(destination, gmsgHideWeapon, NULL, receiver);
		WRITE_BYTE(params[2]);
	MESSAGE_END();

	if(params[3])
	{
		MESSAGE_BEGIN(destination, gmsgCrosshair, NULL, receiver);
			WRITE_BYTE(0);
		MESSAGE_END();
	}

	return 1;
}

static cell AMX_NATIVE_CALL fade_user_screen(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[9] != 0), gmsgScreenFade, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[3]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[2]), (1<<12)));
		WRITE_SHORT(params[4]);
		WRITE_BYTE(params[5]);
		WRITE_BYTE(params[6]);
		WRITE_BYTE(params[7]);
		WRITE_BYTE(params[8]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL shake_user_screen(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[5] != 0), gmsgScreenShake, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[2]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[3]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[4]), (1<<12)));
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL set_user_fov(AMX *amx, cell *params)
{
	int index = params[1];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[3] != 0), gmsgSetFOV, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(params[2]);
	MESSAGE_END();

	return 1;
}

AMX_NATIVE_INFO msgnat_Natives[] =
{
	{"draw_ammo_pickup_icon",					draw_ammo_pickup_icon},
	{"draw_weapon_pickup_icon",					draw_weapon_pickup_icon},
	{"draw_status_icon",						draw_status_icon},
	{"draw_train_controls",						draw_train_controls},
	{"send_geiger_signal",						send_geiger_signal},
	{"hide_hud_elements",						hide_hud_elements},
	{"fade_user_screen",						fade_user_screen},
	{"shake_user_screen",						shake_user_screen},
	{"set_user_fov",							set_user_fov},
	{NULL,										NULL},
};