#include "bb.h"

static cell AMX_NATIVE_CALL get_user_exp(AMX *amx,cell *params)
{
	return amx_ftoc(GetUserExp(params[1]));
}

static cell AMX_NATIVE_CALL set_user_exp(AMX *amx,cell *params)
{
	float Exp = amx_ctof(params[2]);
	SetUserExp(params[1], Exp );
	return 1;
}

static cell AMX_NATIVE_CALL get_user_points(AMX *amx,cell *params)
{
	return amx_ftoc(GetUserPoints(params[1]));
}

static cell AMX_NATIVE_CALL set_user_points(AMX *amx,cell *params)
{
	float Exp = amx_ctof(params[2]);
	SetUserPoints(params[1], Exp );
	return 1;
}

static cell AMX_NATIVE_CALL get_user_level(AMX *amx,cell *params)
{
	return GetUserLevel(params[1]);
}

static cell AMX_NATIVE_CALL set_user_level(AMX *amx,cell *params)
{
	if(GetUserLevel(params[0]) > params[2])
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Must set to a level higher than current one!");
		return 0;
	}
	SetUserLevel(params[1], params[2] );
	return 1;
}

static cell AMX_NATIVE_CALL get_user_speed(AMX *amx,cell *params)
{
	return GetUserSpeed(params[1]);
}

static cell AMX_NATIVE_CALL set_user_speed(AMX *amx,cell *params)
{
	SetUserSpeed(params[1], params[2] );
	return 1;
}

static cell AMX_NATIVE_CALL get_user_hitpoints(AMX *amx,cell *params)
{
	return GetUserHitPoints(params[1]);
}

static cell AMX_NATIVE_CALL set_user_hitpoints(AMX *amx,cell *params)
{
	SetUserHitPoints(params[1], params[2] );
	return 1;
}

static cell AMX_NATIVE_CALL get_user_skill(AMX *amx,cell *params)
{
	return GetUserSkill(params[1]);
}

static cell AMX_NATIVE_CALL set_user_skill(AMX *amx,cell *params)
{
	SetUserSkill(params[1], params[2] );
	return 1;
}

static cell AMX_NATIVE_CALL send_progress_bar(AMX *amx,cell *params)
{
	int len = 0;
	float time = amx_ctof(params[3]);
	SendProgressBar(params[1], MF_GetAmxString( amx, params[2], 0, &len ), time );
	return 1;
}

static cell AMX_NATIVE_CALL send_show_objective(AMX *amx,cell *params)
{
	int len = 0;
	SendShowObjective(params[1], MF_GetAmxString( amx, params[2], 0, &len ) );
	return 1;
}

static cell AMX_NATIVE_CALL send_show_message(AMX *amx,cell *params)
{
	int len = 0;
	float time = amx_ctof(params[2]);
	SendShowMessage(params[1], time, MF_GetAmxString( amx, params[2], 0, &len ), MF_GetAmxString( amx, params[3], 0, &len ) );
	return 1;
}

static cell AMX_NATIVE_CALL reset_user_hud(AMX *amx,cell *params)
{
	UpdateBBHud( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL is_user_zombie(AMX *amx,cell *params)
{
	return IsUserZombie(params[1]);
}


AMX_NATIVE_INFO bb_Exports[] = 
{
	{"bb_is_user_zombie",is_user_zombie},
	{"bb_reset_user_hud", reset_user_hud},

	{"bb_show_message",send_show_message},
	{"bb_show_objective", send_show_objective},
	{"bb_show_progress_bar", send_progress_bar},

	{"bb_get_user_skill",get_user_skill},
	{"bb_set_user_skill", set_user_skill},

	{"bb_get_user_hitpoints",get_user_hitpoints},
	{"bb_set_user_hitpoints", set_user_hitpoints},

	{"bb_get_user_speed",get_user_speed},
	{"bb_set_user_speed", set_user_speed},

	{"bb_get_user_exp",get_user_exp},
	{"bb_set_user_exp", set_user_exp},

	{"bb_get_user_points",get_user_points},
	{"bb_set_user_points", set_user_points},

	{"bb_get_user_level",get_user_level},
	{"bb_set_user_level", set_user_level},

	{ NULL, NULL }
};

void OnAmxxAttach()
{
	MF_AddNatives(bb_Exports);
}