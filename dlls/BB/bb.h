// prevent double include
#ifndef __BB_H__
#define __BB_H__

#include "pdata.h"
#include "bb_const.h"

void UpdateBBHud( long& target);

inline float GetUserExp( long& target)
	{ return GetPData(target, BB_PDATA_EXP, 100.0); }

inline void SetUserExp( long& target, float& exp)
	{ SetPData(target, BB_PDATA_EXP, exp); }

inline float GetUserPoints( long& target)
	{ return GetPData(target, BB_PDATA_POINT, 100.0); }

inline void SetUserPoints( long& target, float& points)
	{SetPData(target, BB_PDATA_POINT, points, true);}

inline long GetUserLevel(long& target)
	{ return GetPData(target,BB_PDATA_LEVEL,100); }

inline void SetUserLevel(long& target, long& level)
{
	long i;
	float totalxp = 0.0;

	for(i=1;i<=level;i++) {
		totalxp += 150.0 + ((i-1) * 300.0);
	}

	SetUserExp( target, totalxp );

	MESSAGE_BEGIN(MSG_ONE,120, NULL, MF_GetPlayerEdict( target) );
	WRITE_COORD(0);
	WRITE_BYTE(level);
	WRITE_BYTE( GetUserPoints(target) );
	MESSAGE_END();

	MESSAGE_BEGIN(MSG_ALL,81, NULL, MF_GetPlayerEdict( target ));
	WRITE_BYTE( target );
	WRITE_SHORT( MF_GetPlayerFrags( target ) );
	WRITE_SHORT( MF_GetPlayerDeaths( target ) );
	WRITE_BYTE(level);
	MESSAGE_END();

	SetPData(target,BB_PDATA_LEVEL,level);
	SetPData(target,BB_PDATA_LEVEL - 1,level);

}

inline long GetUserSpeed(long& target)
	{ return GetPData(target,BB_PDATA_SPEED,100); }

inline void SetUserSpeed(long& target, long& speed)	
	{ SetPData(target,BB_PDATA_SPEED,speed, true);}

inline long GetUserHitPoints(long& target)
	{ return GetPData(target,BB_PDATA_HITPOINTS,100); }

inline void SetUserHitPoints(long& target, long& hitpoints)
	{ SetPData(target,BB_PDATA_HITPOINTS,hitpoints, true); }

inline long GetUserSkill(long& target)
	{ return GetPData(target,BB_PDATA_SKILL,100); }

inline void SetUserSkill(long& target, long& skill )
	{ SetPData(target,BB_PDATA_SKILL,skill,true); }

inline bool IsUserZombie(long& target)
{
	return ( (MF_GetPlayerEdict( target ))->v.team == 2);
}

inline void SendProgressBar( long& target, char* message, float& time)
{
	MESSAGE_BEGIN(MSG_ONE, 122, NULL, MF_GetPlayerEdict( target));
	WRITE_STRING(message);
	WRITE_COORD(time);
	MESSAGE_END();
}

inline void SendShowObjective( long& target, char* message)
{
	MESSAGE_BEGIN(MSG_ONE, 122, NULL, MF_GetPlayerEdict( target));
	WRITE_COORD(-1);
	WRITE_BYTE(144);
	WRITE_STRING(message);
	MESSAGE_END();
}

inline void SendShowMessage( long& target, float& duration, char* message, char* message2)
{
	MESSAGE_BEGIN(MSG_ONE, 122, NULL, MF_GetPlayerEdict( target));
	WRITE_COORD(duration);
	WRITE_BYTE(32);
	WRITE_STRING(message);
	WRITE_STRING(message2);
	MESSAGE_END();
}

void UpdateBBHud( long& target)
{
	MESSAGE_BEGIN( MSG_ONE, 113, NULL, MF_GetPlayerEdict( target) );
	WRITE_BYTE( GetUserHitPoints(target) );
	WRITE_BYTE( GetUserSpeed(target) );
	WRITE_BYTE( GetUserSkill(target) );
	WRITE_BYTE( GetUserPoints(target) );
	MESSAGE_END();
}

#endif

