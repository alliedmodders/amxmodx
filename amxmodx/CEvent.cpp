/*
* Copyright (c) 2002-2003 Aleksander Naszko, Pavol Marko
*
*    This file is part of AMX Mod X.
*
*    AMX Mod X is free software; you can redistribute it and/or modify it
*    under the terms of the GNU General Public License as published by the
*    Free Software Foundation; either version 2 of the License, or (at
*    your option) any later version.
*
*    AMX Mod X is distributed in the hope that it will be useful, but
*    WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with AMX Mod Xod; if not, write to the Free Software Foundation,
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

#include <extdll.h>
#include <meta_api.h>
#include "amxmod.h"
#include "CEvent.h"

// *****************************************************
// class ClEvent
// *****************************************************

EventsMngr::ClEvent::ClEvent(CPluginMngr::CPlugin* plugin,  int func, int flags)
{
	m_Plugin = plugin;
	m_Func = func;

	// flags
	m_FlagWorld = (flags & 1) ? true : false;						// flag a
	m_FlagPlayer = (flags & 2) ? true : false;						// flag b
	m_FlagOnce = (flags & 4) ? true : false;						// flag c
	if (flags & 24)
	{
		m_FlagAlive = (flags & 16) ? true : false;				// flag e
		m_FlagDead = (flags & 8) ? true : false;				// flag d
	}

	m_Stamp = 0.0f;
	m_Done = false;
}

EventsMngr::ClEvent::~ClEvent()
{

}

int EventsMngr::ClEvent::getFunction()
{
	return m_Func;
}

EventsMngr::EventsMngr()
{
	clearEvents();
}

EventsMngr::~EventsMngr()
{
	clearEvents();
}


CPluginMngr::CPlugin * EventsMngr::ClEvent::getPlugin()
{
	return m_Plugin;
}

// *****************************************************
// class EventsMngr
// *****************************************************

void EventsMngr::ClEvent::registerFilter(char *filter)
{
	// filters (conditions) have the form x&y
	// x is the param number
	// & may also be other characters
	// y is a string or a number
	if (!filter)
		return;

	char* value = filter;

	// get the first numbr
	while (isdigit(*value))
		++value;

	// end of string => ignore
	if (!*value)
		return;

	CondMapPair pair;

	// type character
	pair.second.type = *value;

	// set a null here so param id can be recognized later
	*value++ = 0;

	// rest of line
	pair.second.sValue = value;
	pair.second.fValue = atof(value);
	pair.second.iValue = atoi(value);

	// param id
	pair.first = atoi(filter);

	m_Conditions.insert(pair);
}

EventsMngr::ClEvent* EventsMngr::registerEvent(CPluginMngr::CPlugin* plugin, int func, int flags, int msgid)
{
	// validate parameter
	if (msgid < 0 || msgid >= MAX_AMX_REG_MSG)
		return NULL;

	ClEvent *event = new ClEvent(plugin, func, flags);
	if (!event)
		return NULL;

	m_Events[msgid].push_back(event);

	return event;
}

void EventsMngr::parserInit(int msg_type, float* timer, CPlayer* pPlayer, int index)
{
	if (msg_type < 0 || msg_type > MAX_AMX_REG_MSG)
		return;

	m_ParseNotDone = false;
	m_Timer = timer;

	// don't parse if nothing to do
	if (m_Events[msg_type].empty())
		return;

	for(ClEventVecIter iter = m_Events[msg_type].begin(); iter != m_Events[msg_type].end(); ++iter)
	{
		if ((*iter)->m_Done)
			continue;

		if (!(*iter)->m_Plugin->isExecutable((*iter)->m_Func))
		{
			(*iter)->m_Done = true;
			continue;
		}

		if (pPlayer)
		{
			if (!(*iter)->m_FlagPlayer || (pPlayer->IsAlive() ? !(*iter)->m_FlagAlive : !(*iter)->m_FlagDead ) )
			{
				(*iter)->m_Done = true;
				continue;
			}
		}
		else if (!(*iter)->m_FlagWorld)
		{
			(*iter)->m_Done = true;
			continue;
		}

		if ((*iter)->m_FlagOnce && (*iter)->m_Stamp == (float)(*timer))
		{
			(*iter)->m_Done = true;
			continue;
		}
		m_ParseNotDone = true;
	}

	if (m_ParseNotDone)
	{
		// we don't clear it (performance)
		if (m_ParseVault.size() < 1)
		{
			m_ParseVault.reserve(32);				// 32 as default
			m_ParseVault.push_back(MsgDataVault());
		}
		m_ParsePos = 0;
		m_ParseVault[m_ParsePos].type = MSG_INTEGER;
		m_ParseVault[m_ParsePos].iValue = index;
	}
	m_ParseFun = &m_Events[msg_type];
}

void EventsMngr::parseValue(int iValue)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;

	// grow if needed
	if (m_ParseVault.size() <= static_cast<size_t>(++m_ParsePos))
	{
		MsgDataVault tmp;
		m_ParseVault.push_back(tmp);
	}

	m_ParseVault[m_ParsePos].type = MSG_INTEGER;
	m_ParseVault[m_ParsePos].iValue = iValue;

	// loop through the registered funcs, and decide whether they have to be called
	bool skip;
	for (ClEventVecIter iter = m_ParseFun->begin(); iter != m_ParseFun->end(); ++iter)
	{
		if ((*iter)->m_Done)
			continue;

		skip = false;
		ClEvent::CondMapIter condIter = (*iter)->m_Conditions.find(m_ParsePos);
		if (condIter == (*iter)->m_Conditions.end())
			continue;

		do
		{
			switch(condIter->second.type)
			{
			case '=': if (condIter->second.iValue == iValue) skip=true; break;
			case '!': if (condIter->second.iValue != iValue) skip=true; break;
			case '&': if (iValue & condIter->second.iValue) skip=true; break;
			case '<': if (iValue < condIter->second.iValue) skip=true; break;
			case '>': if (iValue > condIter->second.iValue) skip=true; break;
			}
			if (skip)
				break;
		} while ( ++condIter != (*iter)->m_Conditions.end() );

		if (skip)
			continue;
		(*iter)->m_Done = true;
	}
}

void EventsMngr::parseValue(float fValue)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;

	// grow if needed
	if (m_ParseVault.size() <= static_cast<size_t>(++m_ParsePos))
	{
		MsgDataVault tmp;
		m_ParseVault.push_back(tmp);
	}

	m_ParseVault[m_ParsePos].type = MSG_FLOAT;
	m_ParseVault[m_ParsePos].fValue = fValue;

	// loop through the registered funcs, and decide whether they have to be called
	bool skip;
	for (ClEventVecIter iter = m_ParseFun->begin(); iter != m_ParseFun->end(); ++iter)
	{
		if ((*iter)->m_Done)
			continue;

		skip = false;
		ClEvent::CondMapIter condIter = (*iter)->m_Conditions.find(m_ParsePos);
		if (condIter == (*iter)->m_Conditions.end())
			continue;

		do
		{
			switch(condIter->second.type)
			{
			case '=': if (condIter->second.fValue == fValue) skip=true; break;
			case '!': if (condIter->second.fValue != fValue) skip=true; break;
			case '<': if (fValue < condIter->second.fValue) skip=true; break;
			case '>': if (fValue > condIter->second.fValue) skip=true; break;
			}
			if (skip)
				break;
		} while ( ++condIter != (*iter)->m_Conditions.end() );

		if (skip)
			continue;
		(*iter)->m_Done = true;
	}
}

void EventsMngr::parseValue(const char *sz)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;

	// grow if needed
	if (m_ParseVault.size() <= static_cast<size_t>(++m_ParsePos))
	{
		MsgDataVault tmp;
		m_ParseVault.push_back(tmp);
	}

	m_ParseVault[m_ParsePos].type = MSG_STRING;
	m_ParseVault[m_ParsePos].sValue = sz;

	// loop through the registered funcs, and decide whether they have to be called
	bool skip;
	for (ClEventVecIter iter = m_ParseFun->begin(); iter != m_ParseFun->end(); ++iter)
	{
		if ((*iter)->m_Done)
			continue;

		skip = false;
		ClEvent::CondMapIter condIter = (*iter)->m_Conditions.find(m_ParsePos);
		if (condIter == (*iter)->m_Conditions.end())
			continue;

		do
		{
			switch(condIter->second.type)
			{
			case '=': if (!strcmp(sz, condIter->second.sValue.c_str())) skip=true; break;
			case '!': if (strcmp(sz, condIter->second.sValue.c_str())) skip=true; break;
			case '&': if (strstr(sz, condIter->second.sValue.c_str())) skip=true; break;
			}
			if (skip)
				break;
		} while ( ++condIter != (*iter)->m_Conditions.end() );

		if (skip)
			continue;
		(*iter)->m_Done = true;
	}
}

void EventsMngr::executeEvents()
{
	int err;

#ifdef ENABLEEXEPTIONS
	try
	{
#endif		// #ifdef ENABLEEXEPTIONS
		for (ClEventVecIter iter = m_ParseFun->begin(); iter != m_ParseFun->end(); ++iter)
		{
			if ( (*iter)->m_Done ) 
			{
				(*iter)->m_Done = false;
				continue;
			}

			(*iter)->m_Stamp = *m_Timer;

			if ((err = amx_Exec((*iter)->m_Plugin->getAMX(), NULL, (*iter)->m_Func, 1, m_ParseVault.size() ? m_ParseVault[0].iValue : 0)) != AMX_ERR_NONE)
			{
				print_srvconsole("[AMX] Run time error %d on line %ld (plugin \"%s\")\n", err, 
					(*iter)->m_Plugin->getAMX()->curline, (*iter)->m_Plugin->getName());
			}
		}

#ifdef ENABLEEXEPTIONS
	}
	catch( ... )
	{
		print_srvconsole( "[AMX] fatal error at event execution\n");
	}
#endif		// #ifdef ENABLEEXEPTIONS
}

int EventsMngr::getArgNum()
{
	return m_ParsePos + 1;
}

const char* EventsMngr::getArgString(int a)
{
	if ( a < 0 || a > m_ParsePos )
		return "";

	static char var[32];

	switch(m_ParseVault[a].type)
	{
	case MSG_INTEGER: 
		sprintf( var, "%d", m_ParseVault[a].iValue );
		return var;
	case MSG_STRING: 
		return m_ParseVault[a].sValue;
	default:
		sprintf( var, "%g", m_ParseVault[a].fValue );
		return var;
	}
}

int EventsMngr::getArgInteger(int a)
{
	if ( a < 0 || a > m_ParsePos )
		return 0;

	switch(m_ParseVault[a].type)
	{
	case MSG_INTEGER:
		return m_ParseVault[a].iValue;
	case MSG_STRING:
		return atoi(m_ParseVault[a].sValue);
	default:
		return (int)m_ParseVault[a].fValue; 
	}
}

float EventsMngr::getArgFloat(int a)
{
	if ( a < 0 || a > m_ParsePos )
		return 0.0f;

	switch(m_ParseVault[a].type)
	{
	case MSG_INTEGER:
		return m_ParseVault[a].iValue;
	case MSG_STRING:
		return atof(m_ParseVault[a].sValue);
	default:
		return m_ParseVault[a].fValue; 
	}
}

void EventsMngr::clearEvents(void)
{
	for (int i = 0; i < MAX_AMX_REG_MSG; ++i)
	{
		for (ClEventVecIter iter = m_Events[i].begin(); iter != m_Events[i].end(); ++iter)
		{
			if (*iter)
				delete *iter;
		}
		m_Events[i].clear();
	}
}

int EventsMngr::getEventId(const char* msg)
{
	const struct CS_Events
	{
		const char* name;
		CS_EventsIds id;
	} table[] =
	{
		{ "CS_DeathMsg" ,	CS_DeathMsg  },
			//		{ "CS_RoundEnd" ,	CS_RoundEnd  },
			//		{ "CS_RoundStart" , CS_RoundStart  },
			//		{ "CS_Restart" ,	CS_Restart  },
		{ "" ,				CS_Null  }
	};
	// if msg is a number, return it
	int pos = atoi(msg);
	if (pos != 0)
		return pos;

	// try to find in table first
	for (pos = 0; table[ pos ].id != CS_Null; ++pos )
		if ( !strcmp( table[ pos ].name , msg ) )
			return table[ pos ].id;

	// not found
	return pos = GET_USER_MSG_ID(PLID, msg , 0 );
}
