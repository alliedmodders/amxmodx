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

#include <extdll.h>
#include <meta_api.h>
#include "amxmodx.h"
#include "CEvent.h"

// *****************************************************
// class ClEvent
// *****************************************************

EventsMngr::ClEvent::ClEvent(CPluginMngr::CPlugin* plugin,  int func, int flags)
{
	m_Plugin = plugin;
	m_Func = func;

	// flags
	m_FlagAlive = true;
	m_FlagDead = true;

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

	m_Conditions = NULL;
}

EventsMngr::ClEvent::~ClEvent()
{
	cond_t *tmp1 = m_Conditions;
	cond_t *tmp2 = NULL;
	while (tmp1)
	{
		tmp2 = tmp1->next;
		delete tmp1;
		tmp1 = tmp2;
	}
	m_Conditions = NULL;
}

void EventsMngr::NextParam()
{
	const int INITIAL_PARSEVAULT_SIZE = 32;

	if (m_ParsePos < m_ParseVaultSize)
		return;

	MsgDataEntry *tmp = NULL;
	int tmpSize = 0;
	if (m_ParseVault)
	{
		// copy to tmp
		tmp = new MsgDataEntry[m_ParseVaultSize];
		memcpy(tmp, m_ParseVault, m_ParseVaultSize * sizeof(MsgDataEntry));
		tmpSize = m_ParseVaultSize;
		delete [] m_ParseVault;
		m_ParseVault = NULL;
	}

	if (m_ParseVaultSize > 0)
		m_ParseVaultSize *= 2;
	else
		m_ParseVaultSize = INITIAL_PARSEVAULT_SIZE;

	m_ParseVault = new MsgDataEntry[m_ParseVaultSize];
	if (tmp)
	{
		memcpy(m_ParseVault, tmp, tmpSize * sizeof(MsgDataEntry));
		delete [] tmp;
		tmp = NULL;
	}
}

int EventsMngr::ClEvent::getFunction()
{
	return m_Func;
}

EventsMngr::EventsMngr()
{
	m_ParseVault = NULL;
	m_ParseVaultSize = 0;
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

	cond_t *tmpCond = new cond_t;
	if (!tmpCond)
		return;

	// type character
	tmpCond->type = *value;

	// set a null here so param id can be recognized, and save it
	*value++ = 0;
	tmpCond->paramId = atoi(filter);

	// rest of line
	tmpCond->sValue.set(value);
	tmpCond->fValue = atof(value);
	tmpCond->iValue = atoi(value);
	
	tmpCond->next = NULL;

	if (m_Conditions)
	{
		cond_t *tmp = m_Conditions;
		while (tmp->next)
			tmp = tmp->next;
		tmp->next = tmpCond;

	}
	else
		m_Conditions = tmpCond;
}

EventsMngr::ClEvent* EventsMngr::registerEvent(CPluginMngr::CPlugin* plugin, int func, int flags, int msgid)
{
	// validate parameter
	if (msgid < 0 || msgid >= MAX_AMX_REG_MSG)
		return NULL;

	ClEvent *event = new ClEvent(plugin, func, flags);
	if (!event)
		return NULL;

	m_Events[msgid].put(event);

	return event;
}

void EventsMngr::parserInit(int msg_type, float* timer, CPlayer* pPlayer, int index)
{
	if (msg_type < 0 || msg_type > MAX_AMX_REG_MSG)
		return;

	m_ParseNotDone = false;
	m_Timer = timer;

	// don't parse if nothing to do
	if (!m_Events[msg_type].size())
		return;

	for(ClEventVecIter iter = m_Events[msg_type].begin(); iter; ++iter)
	{
		if ((*iter).m_Done)
			continue;


		if (!(*iter).m_Plugin->isExecutable((*iter).m_Func))
		{
			(*iter).m_Done = true;
			continue;
		}

		if (pPlayer)
		{
			if (!(*iter).m_FlagPlayer || (pPlayer->IsAlive() ? !(*iter).m_FlagAlive : !(*iter).m_FlagDead ) )
			{
				(*iter).m_Done = true;
				continue;
			}
		}
		else if (!(*iter).m_FlagWorld)
		{
			(*iter).m_Done = true;
			continue;
		}

		if ((*iter).m_FlagOnce && (*iter).m_Stamp == (float)(*timer))
		{
			(*iter).m_Done = true;
			continue;
		}
		m_ParseNotDone = true;
	}

	if (m_ParseNotDone)
	{
		m_ParsePos = 0;
		NextParam();
		m_ParseVault[0].type = MSG_INTEGER;
		m_ParseVault[0].iValue = index;
	}
	m_ParseFun = &m_Events[msg_type];
}

void EventsMngr::parseValue(int iValue)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;


	// grow if needed
	++m_ParsePos;
	NextParam();

	m_ParseVault[m_ParsePos].type = MSG_INTEGER;
	m_ParseVault[m_ParsePos].iValue = iValue;

	// loop through the registered funcs, and decide whether they have to be called or not
	// if they shouldnt, their m_Done is set to true
	for (ClEventVecIter iter = m_ParseFun->begin(); iter; ++iter)
	{
		if ((*iter).m_Done)
			continue;		// already skipped; don't bother with parsing

		// loop through conditions
		bool execute = false;
		bool anyConditions = false;
		for (ClEvent::cond_t *condIter = (*iter).m_Conditions; condIter; condIter = condIter->next)
		{
			if (condIter->paramId == m_ParsePos)
			{
				anyConditions = true;
				switch(condIter->type)
				{
				case '=': if (condIter->iValue == iValue) execute=true; break;
				case '!': if (condIter->iValue != iValue) execute=true; break;
				case '&': if (iValue & condIter->iValue) execute=true; break;
				case '<': if (iValue < condIter->iValue) execute=true; break;
				case '>': if (iValue > condIter->iValue) execute=true; break;
				}
				if (execute)
					break;
			}
		}
		if (anyConditions && !execute)
			(*iter).m_Done = true;		// don't execute
	}
}

void EventsMngr::parseValue(float fValue)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;


	// grow if needed
	++m_ParsePos;
	NextParam();

	m_ParseVault[m_ParsePos].type = MSG_FLOAT;
	m_ParseVault[m_ParsePos].fValue = fValue;

	// loop through the registered funcs, and decide whether they have to be called or not
	// if they shouldnt, their m_Done is set to true
	for (ClEventVecIter iter = m_ParseFun->begin(); iter; ++iter)
	{
		if ((*iter).m_Done)
			continue;		// already skipped; don't bother with parsing

		// loop through conditions
		bool execute = false;
		bool anyConditions = false;
		for (ClEvent::cond_t *condIter = (*iter).m_Conditions; condIter; condIter = condIter->next)
		{
			if (condIter->paramId == m_ParsePos)
			{
				anyConditions = true;
				switch(condIter->type)
				{
				case '=': if (condIter->fValue == fValue) execute=true; break;
				case '!': if (condIter->fValue != fValue) execute=true; break;
				case '<': if (fValue < condIter->fValue) execute=true; break;
				case '>': if (fValue > condIter->fValue) execute=true; break;
				}
				if (execute)
					break;
			}
		}
		if (anyConditions && !execute)
			(*iter).m_Done = true;		// don't execute
	}
}

void EventsMngr::parseValue(const char *sz)
{
	// not parsing
	if (!m_ParseNotDone || !m_ParseFun)
		return;

	// grow if needed
	++m_ParsePos;
	NextParam();

	m_ParseVault[m_ParsePos].type = MSG_STRING;
	m_ParseVault[m_ParsePos].sValue = sz;

	// loop through the registered funcs, and decide whether they have to be called or not
	// if they shouldnt, their m_Done is set to true
	for (ClEventVecIter iter = m_ParseFun->begin(); iter; ++iter)
	{
		if ((*iter).m_Done)
			continue;		// already skipped; don't bother with parsing

		// loop through conditions
		bool execute = false;
		bool anyConditions = false;
		for (ClEvent::cond_t *condIter = (*iter).m_Conditions; condIter; condIter = condIter->next)
		{
			if (condIter->paramId == m_ParsePos)
			{
				anyConditions = true;
				switch(condIter->type)
				{
				case '=': if (!strcmp(sz, condIter->sValue.str())) execute=true; break;
				case '!': if (strcmp(sz, condIter->sValue.str())) execute=true; break;
				case '&': if (strstr(sz, condIter->sValue.str())) execute=true; break;
				}
				if (execute)
					break;
			}
		}
		if (anyConditions && !execute)
			(*iter).m_Done = true;		// don't execute
	}
}

void EventsMngr::executeEvents()
{
	int err;

	if (!m_ParseFun)
	{
		return;
	}

#ifdef ENABLEEXEPTIONS
	try
	{
#endif		// #ifdef ENABLEEXEPTIONS
		for (ClEventVecIter iter = m_ParseFun->begin(); iter; ++iter)
		{
			if ( (*iter).m_Done ) 
			{
				(*iter).m_Done = false;
				continue;
			}

			(*iter).m_Stamp = (float)*m_Timer;

			if ((err = amx_Exec((*iter).m_Plugin->getAMX(), NULL, (*iter).m_Func, 1, m_ParseVault ? m_ParseVault[0].iValue : 0)) != AMX_ERR_NONE)
			{
				AMXXLOG_Log("[AMXX] Run time error %d on line %ld (plugin \"%s\")", err, 
					(*iter).m_Plugin->getAMX()->curline, (*iter).m_Plugin->getName());
			}
		}

#ifdef ENABLEEXEPTIONS
	}
	catch( ... )
	{
		AMXXLOG_Log( "[AMXX] fatal error at event execution");
	}
#endif		// #ifdef ENABLEEXEPTIONS

	m_ParseFun = NULL;
}

int EventsMngr::getArgNum() const
{
	return m_ParsePos + 1;
}

const char* EventsMngr::getArgString(int a) const
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

int EventsMngr::getArgInteger(int a) const
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

float EventsMngr::getArgFloat(int a) const
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
		m_Events[i].clear();
	}
	// delete parsevault
	if (m_ParseVault)
	{
		delete [] m_ParseVault;
		m_ParseVault = NULL;
		m_ParseVaultSize = 0;
	}
}

int EventsMngr::getEventId(const char* msg)
{
	// :TODO: Remove this somehow!!! :)
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

	// find the id of the message
	return pos = GET_USER_MSG_ID(PLID, msg , 0 );
}
