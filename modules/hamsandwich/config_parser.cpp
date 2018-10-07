// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include <amxxmodule.h>
#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"

IGameConfig *CommonConfig;
IGameConfigManager *ConfigManager;

int ReadConfig(void)
{
	ConfigManager = MF_GetConfigManager();

	char error[256] = "";

	if (!ConfigManager->LoadGameConfigFile("common.games", &CommonConfig, error, sizeof error))
	{
		MF_Log("common.games gamedata could not be read: %s", error);
		return -1;
	}

	TypeDescription value;

	if (CommonConfig->GetOffset("pev", &value))
	{
		Offsets.SetPev(value.fieldOffset);
	}

	if (CommonConfig->GetOffset("base", &value))
	{
		Offsets.SetBase(value.fieldOffset);
	}

	for (auto index = 0; index < HAM_LAST_ENTRY_DONT_USE_ME_LOL; ++index)
	{
		if (CommonConfig->GetOffset(hooklist[index].name, &value))
		{
			hooklist[index].isset = 1;
			hooklist[index].vtid = value.fieldOffset;
		}
	}

	return 1;
}
