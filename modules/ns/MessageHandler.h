// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

/* Class with virtual members for easy message handling
 * Don't forget to add new messages to "Initialize_MessageHandler()"
 */
#ifndef MESSAGEHANDLER_H
#define MESSAGEHANDLER_H

#include "utilfunctions.h"

class MessageHandler
{
public:
	unsigned int	 m_Count;
	int				 m_Target;
	float			 m_Origin[3];
	edict_t			*m_Entity;
	int				 m_Msg;


	/**
	 * Return 1 to hook the rest of this message, 0 otherwise
	 */
	virtual int Begin(int Target, int Msg, const float *Origin, edict_t *Entity) 
	{ 
		return 0; 
	};

	virtual void End(void)
	{
		return;
	};

	virtual void WriteByte(int Data)
	{
		++m_Count;
	};

	virtual void WriteChar(int Data)
	{
		++m_Count;
	};

	virtual void WriteShort(int Data)
	{
		++m_Count;
	};

	virtual void WriteLong(int Data)
	{
		++m_Count;
	};

	virtual void WriteAngle(REAL Data)
	{
		++m_Count;
	};

	virtual void WriteCoord(REAL Data)
	{
		++m_Count;
	};

	virtual void WriteString(const char *Data)
	{
		++m_Count;
	};

	virtual void WriteEntity(int Data)
	{
		++m_Count;
	};

};


class MessageCountDown : public MessageHandler
{
public:
	int			m_CountDownTime;

	virtual int Begin(int Target, int Msg, const float *Origin, edict_t *Entity)
	{
		m_Count=0;
		return 1;
	};

	virtual void End(void)
	{
		if (m_Count!=1) // invalid message?
		{
			MF_Log("[NS] Invalid Countdown message received! Got %d args, expected 1.",m_Count);
			return;
		}

		GameMan.HandleCountdown(m_CountDownTime);
	};

	virtual void WriteByte(int Data)
	{
		++m_Count;
		m_CountDownTime=Data;
	};
};

class MessageGameStatus : public MessageHandler
{
public:
	int			FirstByte;

	virtual int Begin(int Target, int Msg, const float *Origin, edict_t *Entity)
	{
		m_Count=0;
		return 1;
	};

	virtual void End(void)
	{
		GameMan.HandleGameStatus(FirstByte);
	};

	virtual void WriteByte(int iValue)
	{
		if (m_Count==0)
		{
			FirstByte=iValue;
		};
		++m_Count;
	};
};


void Initialize_MessageHandler(void);

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
void MessageEnd_Post(void);
void WriteByte_Post(int iValue);
void WriteChar_Post(int iValue);
void WriteShort_Post(int iValue);
void WriteLong_Post(int iValue);
void WriteAngle_Post(float flValue);
void WriteCoord_Post(float flValue);
void WriteString_Post(const char *sz);
void WriteEntity_Post(int iValue);


#endif // MESSAGEHANDLER_H
