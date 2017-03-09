#pragma once

#include <interface_helpers.h>
#include "engine/rehlds_api.h"

extern IRehldsApi* RehldsApi;
extern const RehldsFuncs_t* RehldsFuncs;
extern IRehldsServerData* RehldsData;
extern IRehldsHookchains* RehldsHookchains;
extern IRehldsServerStatic* RehldsSvs;

extern bool RehldsApi_Init();