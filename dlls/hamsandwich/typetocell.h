/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#ifndef TYPETOCELL_H
#define TYPETOCELL_H

#include <extdll.h>
#include "sdk/amxxmodule.h"

#include "CVector.h"

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
