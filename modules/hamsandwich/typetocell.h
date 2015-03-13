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

#ifndef TYPETOCELL_H
#define TYPETOCELL_H

#include <extdll.h>
#include "amxxmodule.h"

#include "hook.h"
#include "forward.h"

#include "ham_const.h"	
#include "ham_utils.h"



inline cell TypeToCell(const float& value)
{
	return amx_ftoc2(value);
}
inline cell TypeToCell(const float*& value)
{
	return amx_ftoc2(*value);
}

inline cell TypeToCell(const Vector*& value)
{
	return reinterpret_cast<cell>(value);
}
inline cell TypeToCell(const int& value)
{
	return value;
}

inline cell TypeToCell(const edict_t*& value)
{
	if (value == NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(value);
}

inline cell TypeToCell(const entvars_t*& value)
{
	if (value == NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(value->pContainingEntity);
}

inline cell TypeToCell(const HLBaseEntity*& value)
{
	return PrivateToIndex(reinterpret_cast<const void *>(value));
}


#endif
