/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#ifndef __CEVENTS_H__
#define __CEVENTS_H__

#define MAX_AMX_REG_MSG MAX_REG_MSGS+16
 
enum {
	CS_DEATHMSG = MAX_REG_MSGS,
//	CS_ROUNDEND,
//	CS_ROUNDSTART,
//	CS_RESTART,
};

// *****************************************************
// class EventsMngr
// *****************************************************

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
		friend class EventsMngr;		// events manager may access our private members

		int m_Func;								// function to be executed
		CPluginMngr::CPlugin *m_Plugin;			// the plugin this ClEvent class is assigned to

		// flags
		bool m_FlagPlayer;
		bool m_FlagWorld;
		bool m_FlagOnce;
		bool m_FlagDead;
		bool m_FlagAlive;

		float m_Stamp;	// for 'once' flag

		bool m_Done;
		
		// conditions
		struct cond_t
		{
			int paramId;				// the message parameter id

			String sValue;				// value (string)
			float fValue;				// value (float)
			int iValue;					// value (int)
			int type;					// type (can be int, float, string)

			cond_t *next;
		};

		cond_t *m_Conditions;

	public:
		// constructors & destructors
		ClEvent(CPluginMngr::CPlugin* plugin,  int func, int flags);
		~ClEvent();

		inline CPluginMngr::CPlugin* getPlugin();
		inline int getFunction();
		void registerFilter(char* filter);			// add a condition
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
	int m_ParseVaultSize;
	void NextParam();				// make sure a new parameter can be added

	typedef CList<ClEvent> ClEventVec;
	typedef ClEventVec::iterator ClEventVecIter;

	ClEventVec m_Events[MAX_AMX_REG_MSG];
	ClEventVec *m_ParseFun;	// current Event vector

	bool m_ParseNotDone;
	int m_ParsePos;				// is args. num. - 1
	float* m_Timer;
	
	ClEvent* getValidEvent(ClEvent* a );

public:
	EventsMngr();
	~EventsMngr();

	// Interface

	ClEvent* registerEvent(CPluginMngr::CPlugin* plugin, int func, int flags, int msgid);
	void parserInit(int msg_type, float* timer, CPlayer* pPlayer, int index);
	void parseValue(int iValue);
	void parseValue(float fValue);
	void parseValue(const char *sz);
	void executeEvents();
	int getArgNum() const; //{ return (parsePos+1); }
	const char* getArgString(int a) const;
	int getArgInteger(int a) const;
	float getArgFloat(int a) const;
	void clearEvents(void);
	static int getEventId( const char* msg );
};

#endif // #ifdef __CEVENTS_H__


