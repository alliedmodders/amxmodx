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

inline edict_t* MF_GetEntityEdict( long& EntID )
{
	if( (EntID > 0) && (EntID <= (gpGlobals->maxClients) ) ) 
		return MF_GetPlayerEdict( EntID );

	return INDEXENT( EntID );
}

template <typename ValueType>
inline void SetPData( long& targetid, long offset, ValueType value)
{
	edict_t* target = MF_GetEntityEdict( targetid );
	if(target == NULL) return;

	*((ValueType *)target->pvPrivateData + offset + EXTRAOFFSET) = value;
};

template <typename ValueType>
inline ValueType GetPData( long& targetid, long offset, ValueType value )
{
	edict_t* target = MF_GetEntityEdict( targetid );
	if(target == NULL) return NULL;
	return *((ValueType *)target->pvPrivateData + offset + EXTRAOFFSET);
}

#endif