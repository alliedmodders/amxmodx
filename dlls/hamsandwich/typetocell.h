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
