// prevent double include
#ifndef __PDATA_H__
#define __PDATA_H__

#include <extdll.h>
#include "amxxmodule.h"

#if defined __linux__
	#define EXTRAOFFSET					5 // offsets 5 higher in Linux builds
#else
	#define EXTRAOFFSET					0 // no change in Windows builds
#endif // defined __linux__

template <typename ValueType>
inline void SetPData( long& targetid, long offset, ValueType value, bool reset = false )
{
	edict_t* target = MF_GetPlayerEdict( targetid );
	*((ValueType *)target->pvPrivateData + offset + EXTRAOFFSET) = value;
	if(reset) UpdateBBHud( targetid );
};

template <typename ValueType>
inline ValueType GetPData( long& targetid, long offset, ValueType value )
{
	edict_t* target = MF_GetPlayerEdict( targetid );
	return *((ValueType *)target->pvPrivateData + offset + EXTRAOFFSET);
}

#endif