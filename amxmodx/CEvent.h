/*
 * Copyright (c) 2002-2003 Aleksander Naszko
 *
 *    This file is part of AMX Mod.
 *
 *    AMX Mod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    AMX Mod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with AMX Mod; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#ifndef EVENTS_H
#define EVENTS_H

#define MAX_PARSE_VALUES  32
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


class EventsMngr {

	enum MsgValueType{
		MSG_INTEGER,
		MSG_FLOAT,
		MSG_STRING,
	};

public:

	enum CS_EventsIds {
		CS_Null = 0, 
		CS_DeathMsg = MAX_REG_MSGS,	// start from last element
//		CS_RoundEnd,
//		CS_RoundStart,
//		CS_Restart,
	};

	class iterator;
	friend class iterator;

	class ClEvent {
		friend class EventsMngr;
		friend class iterator;
		CPluginMngr::CPlugin* plugin;
		int func;
		bool player;
		bool world;
		bool once;
		bool done;
		bool dead;
		bool alive;
		float stamp;
		struct cond_t {
			String sValue;
			float fValue;
			int iValue;
			int type;
			cond_t* next;
		} *cond[MAX_PARSE_VALUES];
		ClEvent* next;
		ClEvent( CPluginMngr::CPlugin* p,  int f, int flags  );
		~ClEvent();
	public:
		inline CPluginMngr::CPlugin* getPlugin() { return plugin; }
		inline int getFunction() { return func; }
		void registerFilter( char* filter );
	};

private:
	struct MsgDataVault {
		float fValue;
		int iValue;
		const char* sValue;
		MsgValueType type;
	} parseVault[MAX_PARSE_VALUES];


	ClEvent* modMsgsFunCall[MAX_AMX_REG_MSG];
	ClEvent* parseFun;
	bool parseNotDone;
	int parsePos; // is -1 less then args. num.
	float* timer;
	ClEvent* getValidEvent(ClEvent* a );

public:
	EventsMngr();
	~EventsMngr();

	// Interface

	ClEvent* registerEvent( CPluginMngr::CPlugin* p,  int f, int flags, int pos );
	void parserInit(int msg_type, float* timer , CPlayer* target = 0, int index = 0);
	void parseValue(int iValue);
	void parseValue(float fValue);
	void parseValue(const char *sz);
	void executeEvents();
	inline int getArgNum() { return (parsePos+1); }
	const char* getArgString(int a);
	int getArgInteger(int a);
	float getArgFloat(int a);
	void clearEvents(void);
	static int getEventId( const char* msg );


	class iterator {
		EventsMngr* b;
		ClEvent* a;
	public:
		inline iterator(ClEvent*aa,EventsMngr* bb) : a(aa), b(bb) {}
		inline iterator& operator++() {
			a = b->getValidEvent( a->next );
			return *this;
		}
		inline bool operator==(const iterator& c) const { return a == c.a; }
		inline bool operator!=(const iterator& c) const  { return !operator==(c); }
		ClEvent& operator*() { return *a; }
		operator bool ( ) const { return a ? true : false; }
	};
	inline iterator begin() { return iterator(getValidEvent(parseFun),this); }
	inline iterator end() { return iterator(0,this); }
};

#endif

