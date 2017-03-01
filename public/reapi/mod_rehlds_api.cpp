
#include "mod_rehlds_api.h"

IRehldsApi*          RehldsApi;
const RehldsFuncs_t* RehldsFuncs;
IRehldsServerData*   RehldsData;
IRehldsHookchains*   RehldsHookchains;
IRehldsServerStatic* RehldsSvs;

bool RehldsApi_Init()
{
#if defined(PLATFORM_WINDOWS)
	auto library = "swds";
#elif defined(PLATFORM_UNIX)
	auto library = "engine_i486";
#endif

	if (!GET_IFACE<IRehldsApi>(library, RehldsApi, VREHLDS_HLDS_API_VERSION))
	{
		return false;
	}

	auto majorVersion = RehldsApi->GetMajorVersion();
	auto minorVersion = RehldsApi->GetMinorVersion();

	if (majorVersion != REHLDS_API_VERSION_MAJOR || minorVersion < REHLDS_API_VERSION_MINOR)
	{
		return false;
	}

	RehldsFuncs      = RehldsApi->GetFuncs();
	RehldsData       = RehldsApi->GetServerData();
	RehldsHookchains = RehldsApi->GetHookchains();
	RehldsSvs        = RehldsApi->GetServerStatic();

	return true;
}
