
#pragma once

#include <interface_helpers.h>
#include <reapi/cstrike/regamedll_api.h>

extern IReGameApi*          ReGameApi;
extern const ReGameFuncs_t* ReGameFuncs;
extern IReGameHookchains*   ReGameHookchains;

extern bool RegamedllApi_Init();
