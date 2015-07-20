// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef __CEVENTS_H__
#define __CEVENTS_H__

#include "natives_handles.h"

#define MAX_AMX_REG_MSG MAX_REG_MSGS + 16
 
enum
{
	CS_DEATHMSG = MAX_REG_MSGS,
//	CS_ROUNDEND,
//	CS_ROUNDSTART,
//	CS_RESTART,
};

// *****************************************************
// class EventsMngr
// *****************************************************

enum ForwardState 
{
	FSTATE_ACTIVE, 
	FSTATE_STOP
};

class EventsMngr
{
	enum MsgParamType
	{
		MSG_INTEGER,
		MSG_FLOAT,
		MSG_STRING,
	};

	enum CS_EventsIds
	{
		CS_Null = 0, 
		CS_DeathMsg = MAX_REG_MSGS,	// start from last element
//		CS_RoundEnd,
//		CS_RoundStart,
//		CS_Restart,
	};

public:

	class ClEvent
	{
		friend class EventsMngr;				// events manager may access our private members

		int m_Func;								// function to be executed
		CPluginMngr::CPlugin *m_Plugin;			// the plugin this ClEvent class is assigned to

		// flags
		bool m_FlagClient;
		bool m_FlagWorld;
		bool m_FlagOnce;
		bool m_FlagDead;
		bool m_FlagAlive;
		bool m_FlagPlayer;
		bool m_FlagBot;

		float m_Stamp;	// for 'once' flag

		bool m_Done;
		ForwardState m_State;
		
		// conditions
		struct cond_t
		{
			int paramId;				// the message parameter id

			ke::AString sValue;				// value (string)
			float fValue;				// value (float)
			int iValue;					// value (int)
			int type;					// type (can be int, float, string)

			cond_t *next;
		};

		cond_t *m_Conditions;

	public:
		// constructors & destructors
		ClEvent(CPluginMngr::CPlugin* plugin, int func, int flags);
		~ClEvent();

		inline CPluginMngr::CPlugin* getPlugin();
		inline int getFunction();
		void registerFilter(char* filter);			// add a condition
		void setForwardState(ForwardState value);
	};

private:
	struct MsgDataEntry
	{
		float fValue;
		int iValue;
		const char* sValue;
		MsgParamType type;
	};

	MsgDataEntry *m_ParseVault;
	MsgDataEntry *m_ReadVault;
	int m_ParseVaultSize;
	int m_ReadVaultSize;
	void NextParam();			// make sure a new parameter can be added

	typedef CList<ClEvent> ClEventVec;
	typedef ClEventVec::iterator ClEventVecIter;

	ClEventVec m_Events[MAX_AMX_REG_MSG];
	ClEventVec *m_ParseFun;		// current Event vector

	bool m_ParseNotDone;
	int m_ParsePos;				// is args. num. - 1
	int m_ReadPos;
	float* m_Timer;
	
	ClEvent* getValidEvent(ClEvent* a);

	int m_ParseMsgType;
	int m_ReadMsgType;
public:
	EventsMngr();
	~EventsMngr();

	// Interface

	int registerEvent(CPluginMngr::CPlugin* plugin, int func, int flags, int msgid);

	void parserInit(int msg_type, float* timer, CPlayer* pPlayer, int index);
	void parseValue(int iValue);
	void parseValue(float fValue);
	void parseValue(const char *sz);
	void executeEvents();
	
	int getArgNum() const;		//{ return (parsePos + 1); }
	const char* getArgString(int a) const;
	int getArgInteger(int a) const;
	float getArgFloat(int a) const;
	void clearEvents(void);
	static int getEventId(const char* msg);
	int getCurrentMsgType();
};

struct EventHook
{
	EventHook(EventsMngr::ClEvent *event) : m_event(event) {}
	EventsMngr::ClEvent *m_event;
};

extern NativeHandle<EventHook> EventHandles;

#endif //__CEVENTS_H__
