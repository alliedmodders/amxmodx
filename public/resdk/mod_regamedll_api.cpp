
#include "mod_regamedll_api.h"

IReGameApi*          ReGameApi;
const ReGameFuncs_t* ReGameFuncs;
IReGameHookchains *  ReGameHookchains;

bool RegamedllApi_Init()
{
	const auto library = GET_GAME_INFO(PLID, GINFO_DLL_FULLPATH);

	if (!library || !GET_IFACE<IReGameApi>(library, ReGameApi, VRE_GAMEDLL_API_VERSION, false) || !ReGameApi)
	{
		return false;
	}

	const auto majorVersion = ReGameApi->GetMajorVersion();
	const auto minorVersion = ReGameApi->GetMinorVersion();

	if (majorVersion != REGAMEDLL_API_VERSION_MAJOR || minorVersion < REGAMEDLL_API_VERSION_MINOR)
	{
		return false;
	}

	ReGameFuncs      = ReGameApi->GetFuncs();
	ReGameHookchains = ReGameApi->GetHookchains();

	return true;
}