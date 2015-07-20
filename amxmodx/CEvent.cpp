// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "CEvent.h"

// *****************************************************
// class ClEvent
// *****************************************************

NativeHandle<EventHook> EventHandles;

EventsMngr::ClEvent::ClEvent(CPluginMngr::CPlugin* plugin, int func, int flags)
{
	m_Plugin = plugin;
	m_Func = func;

	// flags
	m_FlagAlive = true;
	m_FlagDead = true;

	m_FlagWorld = (flags & 1) ? true : false;			// flag a
	m_FlagClient = (flags & 2) ? true : false;			// flag b
	m_FlagOnce = (flags & 4) ? true : false;			// flag c
	
	if (flags & 24)
	{
		m_FlagAlive = (flags & 16) ? true : false;		// flag e
		m_FlagDead = (flags & 8) ? true : false;		// flag d
	}

	if (m_FlagClient)
	{
		m_FlagPlayer = true;
		m_FlagBot = true;

		if (flags & 96)
		{
			m_FlagPlayer = (flags & 32) ? true : false;	 // flag f
			m_FlagBot = (flags & 64) ? true : false;	 // flag g
		}
	}

	m_Stamp = 0.0f;
	m_Done = false;
	m_State = FSTATE_ACTIVE;

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
		
		if (!tmp)
		{
			return;		// :TODO: Error report !!
		}
		
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
	m_ParseMsgType = -1;
	m_ReadVault = NULL;
	m_ReadVaultSize = 0;
	m_ReadPos = -1;
	m_ReadMsgType = -1;
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
	tmpCond->sValue = value;
	tmpCond->fValue = static_cast<float>(atof(value));
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

void EventsMngr::ClEvent::setForwardState(ForwardState state)
{
	m_State = state;
}


int EventsMngr::registerEvent(CPluginMngr::CPlugin* plugin, int func, int flags, int msgid)
{
	if (msgid < 0 || msgid >= MAX_AMX_REG_MSG)
	{
		return 0;
	}

	auto event = new ClEvent(plugin, func, flags);

	int handle = EventHandles.create(event);
	
	if (!handle)
	{
		return 0;
	}

	m_Events[msgid].put(event);

	return handle;
}

void EventsMngr::parserInit(int msg_type, float* timer, CPlayer* pPlayer, int index)
{
	if (msg_type < 0 || msg_type > MAX_AMX_REG_MSG)
		return;

	m_ParseNotDone = false;

	// don't parse if nothing to do
	if (!m_Events[msg_type].size())
		return;

	m_ParseMsgType = msg_type;
	m_Timer = timer;

	for (ClEventVecIter iter = m_Events[msg_type].begin(); iter; ++iter)
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
			if (!(*iter).m_FlagClient || (pPlayer->IsBot() ? !(*iter).m_FlagBot : !(*iter).m_FlagPlayer) || (pPlayer->IsAlive() ? !(*iter).m_FlagAlive : !(*iter).m_FlagDead))
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
				switch (condIter->type)
				{
					case '=': if (condIter->iValue == iValue) execute = true; break;
					case '!': if (condIter->iValue != iValue) execute = true; break;
					case '&': if (iValue & condIter->iValue) execute = true; break;
					case '<': if (iValue < condIter->iValue) execute = true; break;
					case '>': if (iValue > condIter->iValue) execute = true; break;
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
				switch (condIter->type)
				{
					case '=': if (condIter->fValue == fValue) execute = true; break;
					case '!': if (condIter->fValue != fValue) execute = true; break;
					case '<': if (fValue < condIter->fValue) execute = true; break;
					case '>': if (fValue > condIter->fValue) execute = true; break;
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
				switch (condIter->type)
				{
					case '=': if (!strcmp(sz, condIter->sValue.chars())) execute = true; break;
					case '!': if (strcmp(sz, condIter->sValue.chars())) execute = true; break;
					case '&': if (strstr(sz, condIter->sValue.chars())) execute = true; break;
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
	static unsigned int reentrant = 0;
	if (!m_ParseFun)
	{
		return;
	}

	// Store old read data, which are either default values or previous event data
	int oldMsgType = m_ReadMsgType, oldReadPos = m_ReadPos;
	MsgDataEntry *oldReadVault = m_ReadVault, *readVault = NULL;
	
	// We have a re-entrant call
	if (reentrant++)
	{
		// Create temporary read vault
		readVault = new MsgDataEntry[m_ParsePos + 1];
		m_ReadVault = readVault;
	} else if (m_ReadVaultSize != m_ParseVaultSize) {
		// Extend read vault size if necessary
		delete [] m_ReadVault;
		m_ReadVault = new MsgDataEntry[m_ParseVaultSize];
		m_ReadVaultSize = m_ParseVaultSize;
		
		// Update old read vault so we don't restore to a wrong pointer
		oldReadVault = m_ReadVault;
	}

	// Copy data over to readvault
	m_ReadPos = m_ParsePos;
	m_ReadMsgType = m_ParseMsgType;

	if (m_ParseVault)
	{
		memcpy(m_ReadVault, m_ParseVault, (m_ParsePos + 1) * sizeof(MsgDataEntry));
	}

	// Reset this here so we don't trigger re-entrancy for unregistered messages
	ClEventVec *parseFun = m_ParseFun;
	m_ParseFun = NULL;

	for (ClEventVecIter iter = parseFun->begin(); iter; ++iter)
	{
		if ((*iter).m_Done) 
		{
			(*iter).m_Done = false;
			continue;
		}
		
		(*iter).m_Stamp = (float)*m_Timer;

		if ((*iter).m_State == FSTATE_ACTIVE)
		{
			executeForwards((*iter).m_Func, static_cast<cell>(m_ReadVault ? m_ReadVault[0].iValue : 0));
		}
	}
	
	// Restore old read data, either resetting to default or to previous event data
	m_ReadMsgType = oldMsgType;
	m_ReadPos = oldReadPos;
	m_ReadVault = oldReadVault;
	
	delete [] readVault;

	--reentrant;
}

int EventsMngr::getArgNum() const
{
	return m_ReadPos + 1;
}

const char* EventsMngr::getArgString(int a) const
{
	if (a < 0 || a > m_ReadPos)
		return "";

	static char var[32];

	switch (m_ReadVault[a].type)
	{
		case MSG_INTEGER: 
			sprintf(var, "%d", m_ReadVault[a].iValue);
			return var;
		case MSG_STRING: 
			return m_ReadVault[a].sValue;
		default:
			sprintf(var, "%g", m_ReadVault[a].fValue);
			return var;
	}
}

int EventsMngr::getArgInteger(int a) const
{
	if (a < 0 || a > m_ReadPos)
		return 0;

	switch (m_ReadVault[a].type)
	{
		case MSG_INTEGER:
			return m_ReadVault[a].iValue;
		case MSG_STRING:
			return atoi(m_ReadVault[a].sValue);
		default:
			return (int)m_ReadVault[a].fValue; 
	}
}

float EventsMngr::getArgFloat(int a) const
{
	if (a < 0 || a > m_ReadPos)
		return 0.0f;

	switch (m_ReadVault[a].type)
	{
		case MSG_INTEGER:
			return static_cast<float>(m_ReadVault[a].iValue);
		case MSG_STRING:
			return static_cast<float>(atof(m_ReadVault[a].sValue));
		default:
			return m_ReadVault[a].fValue; 
	}
}

void EventsMngr::clearEvents(void)
{
	for (int i = 0; i < MAX_AMX_REG_MSG; ++i)
	{
		m_Events[i].clear();
	}

	EventHandles.clear();

	// delete parsevault
	if (m_ParseVault)
	{
		delete [] m_ParseVault;
		m_ParseVault = NULL;
		m_ParseVaultSize = 0;
	}

	if (m_ReadVault)
	{
		delete [] m_ReadVault;
		m_ReadVault = NULL;
		m_ReadVaultSize = 0;
		m_ReadPos = -1;
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
		{"CS_DeathMsg",		CS_DeathMsg},
//		{"CS_RoundEnd",		CS_RoundEnd},
//		{"CS_RoundStart",	CS_RoundStart},
//		{"CS_Restart",		CS_Restart},
		{"",				CS_Null}
	};

	// if msg is a number, return it
	int pos = atoi(msg);
	
	if (pos != 0)
		return pos;

	// try to find in table first
	for (pos = 0; table[pos].id != CS_Null; ++pos)
		if (!strcmp(table[pos].name, msg))
			return table[pos].id;

	// find the id of the message
	return pos = GET_USER_MSG_ID(PLID, msg, 0);
}

int EventsMngr::getCurrentMsgType()
{
	return m_ReadMsgType;
}
