// prevent double include
#ifndef __SVEN_H__
#define __SVEN_H__

#include "amxxmodule.h"
#include "pdata.h"

#include "svencoop_const.h"

inline void SetFrags( long& targetid, long val)
{
	SetPData(targetid, PDATA_FRAGS, static_cast<float>(val) );
};

inline long GetFrags( long& targetid)
{
	return static_cast<long>( GetPData(targetid, PDATA_FRAGS, float(NULL) ) );
};

inline long GetDeaths( long& targetid)
{
	return GetPData(targetid, PDATA_DEATHS, int(NULL) );
};

inline void SetDeaths( long& targetid, long val)
{
	SetPData(targetid, PDATA_DEATHS, static_cast<int>(val) );
};

inline long IsEntAlly( long& targetid)
{
	return GetPData(targetid, PDATA_ALLY, unsigned char( NULL ) );
};

inline long SetSvenWeapon(long& targetid, long val, const long sven_wep[2])
{
	SetPData(targetid, sven_wep[0], static_cast<int>(val) );
	SetPData(targetid, sven_wep[1], static_cast<int>(val) );

	return 1;
}

inline long GetSvenWeapon(long& targetid, const long sven_wep[2])
{
	return GetPData(targetid, sven_wep[0], int(NULL) );
}

#endif
