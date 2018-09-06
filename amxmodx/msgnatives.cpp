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
	enum args { arg_numargs, arg_id, arg_ammoid, arg_amount, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgAmmoPickup, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_BYTE(params[arg_ammoid]);
		WRITE_BYTE(params[arg_amount]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_weapon_pickup_icon(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_weaponid, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgWeapPickup, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_BYTE(params[arg_weaponid]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_status_icon(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_sprite, arg_status, arg_r, arg_g, arg_b, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	int len;
	int status = params[arg_status];
	char* sprite = get_amxstring(amx, params[arg_sprite], 0, len);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgStatusIcon, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);

		WRITE_BYTE(status);

		if(status)
		{
			WRITE_STRING(sprite);
			WRITE_BYTE(params[arg_r]);
			WRITE_BYTE(params[arg_g]);
			WRITE_BYTE(params[arg_b]);
		}

	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL draw_train_controls(AMX *amx, cell *params)
{
	if(g_bmod_dod)
		return 0;

	enum args { arg_numargs, arg_id, arg_speed, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgTrain, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL send_geiger_signal(AMX *amx, cell *params)
{
	if(g_bmod_dod)
		return 0;
	
	enum args { arg_numargs, arg_id, arg_distance, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgGeiger, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_BYTE(params[arg_distance]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL hide_hud_elements(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_elements, arg_noadd, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	int destination = get_msg_destination(index, params[arg_reliable] != 0);
	edict_t *receiver = index ? TypeConversion.id_to_edict(index) : nullptr;

	MESSAGE_BEGIN(destination, gmsgHideWeapon, nullptr, receiver);
		WRITE_BYTE(params[arg_elements]);
	MESSAGE_END();

	if(params[arg_noadd])
	{
		MESSAGE_BEGIN(destination, gmsgCrosshair, nullptr, receiver);
			WRITE_BYTE(0);
		MESSAGE_END();
	}

	return 1;
}

static cell AMX_NATIVE_CALL fade_user_screen(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_duration, arg_fadetime, arg_flags, arg_r, arg_g, arg_b, arg_a, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgScreenFade, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[arg_fadetime]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[arg_duration]), (1<<12)));
		WRITE_SHORT(params[arg_flags]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL shake_user_screen(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_amplitude, arg_duration, arg_frequency, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgScreenShake, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[arg_amplitude]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[arg_duration]), (1<<12)));
		WRITE_SHORT(FixedUnsigned16(amx_ctof(params[arg_frequency]), (1<<12)));
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL set_user_fov(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_id, arg_fov, arg_reliable };

	int index = params[arg_id];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), gmsgSetFOV, nullptr, index ? TypeConversion.id_to_edict(index) : nullptr);
		WRITE_BYTE(params[arg_fov]);
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
	{nullptr,										nullptr},
};