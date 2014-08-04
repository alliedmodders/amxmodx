// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Fun Module
//

#include "amxxmodule.h"
#include "dodfun.h"

edict_t *FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue)
{
	edict_t *pentEntity;
	pentEntity=FIND_ENTITY_BY_STRING(pentStart, szKeyword, szValue);
	if(!FNullEnt(pentEntity))
		return pentEntity;
	return NULL;
}

edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName)
{
	return FindEntityByString(pentStart, "classname", szName);
}

