// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "fakemeta.h"

int LoadMetamodPlugin(const char *path, void **handle, PLUG_LOADTIME now)
{
	int err = 0;
	if ( (err = LOAD_PLUGIN(PLID, path, now, handle)) || !*handle)
	{
		LOG_MESSAGE(PLID, "Can't Attach Module \"%s\".", path);
		return 0;
	}

	return 1;
}

int UnloadMetamodPlugin(void *handle)
{
	if (UNLOAD_PLUGIN_BY_HANDLE(PLID, (void *)handle, PT_ANYTIME, PNL_PLUGIN))
	{
		return 0;
	}

	return 1;
}
