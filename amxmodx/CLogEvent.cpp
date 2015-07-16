// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "CLogEvent.h"

// *****************************************************
// class LogEventsMngr
// *****************************************************

LogEventsMngr::LogEventsMngr()
{
	logCurrent = logCounter = 0;
	logcmplist = 0;
	arelogevents = false;
	memset(logevents, 0, sizeof(logevents));
}

LogEventsMngr::~LogEventsMngr()
{
	clearLogEvents();
}

int LogEventsMngr::CLogCmp::compareCondition(const char* string)
{
	if (logid == parent->logCounter)
		return result;
	
	logid = parent->logCounter;
	
	if (in)
		return result = strstr(string, text.chars()) ? 0 : 1;
	
	return result = strcmp(string,text.chars());
}

LogEventsMngr::CLogCmp* LogEventsMngr::registerCondition(char* filter)
{
	char* temp = filter;
	// expand "1=message"

	while (isdigit(*filter))
		++filter;

	bool in = (*filter=='&');
	*filter++ = 0;
	int pos = atoi(temp);
	
	if (pos < 0 || pos >= MAX_LOGARGS)
		pos = 0;
	
	CLogCmp* c = logcmplist;
	
	while (c)
	{
		if ((c->pos == pos) && (c->in == in) && !strcmp(c->text.chars(), filter))
			return c;
		c = c->next;
	}
	
	return logcmplist = new CLogCmp(filter, in, pos, logcmplist, this);
}

void LogEventsMngr::CLogEvent::registerFilter(char* filter)
{
	CLogCmp *cmp = parent->registerCondition(filter);
	if (cmp == 0) return;
	
	for (LogCond* c = filters; c; c = c->next)
	{
		if (c->argnum == cmp->pos)
		{
			c->list = new LogCondEle(cmp, c->list);
			return;
		}
	}
	
	LogCondEle* aa = new LogCondEle(cmp, 0);
	
	if (aa == 0)
		return;

	filters = new LogCond(cmp->pos, aa, filters);
}

void LogEventsMngr::setLogString(const char* frmt, va_list& vaptr)
{
	++logCounter;
	int len = vsnprintf(logString, 255, frmt, vaptr);
	
	if (len == - 1)
	{
		len = 255;
		logString[len] = 0;
	}
	
	if (len)
		logString[--len] = 0;
	
	logArgc = 0;
}

void LogEventsMngr::setLogString(const char* frmt, ...)
{
	++logCounter;
	va_list logArgPtr;
	va_start(logArgPtr, frmt);
	int len = vsnprintf(logString, 255, frmt, logArgPtr);
	
	if (len == - 1)
	{
		len = 255;
		logString[len] = 0;
	}
	
	va_end(logArgPtr);
	
	if (len)
		logString[--len] = 0;
	
	logArgc = 0;
}

void LogEventsMngr::parseLogString()
{
	register const char* b = logString;
	register int a;
	
	while (*b && logArgc < MAX_LOGARGS)
	{
		a = 0;
		
		if (*b == '"')
		{
			++b;
			
			while (*b && *b != '"' && a < 127) 
				logArgs[logArgc][a++] = *b++;
			
			logArgs[logArgc++][a] = 0;
			if (*b) b+=2; // thanks to double terminator
		}
		else if (*b == '(')
		{
			++b;
			
			while (*b && *b != ')' && a < 127) 
				logArgs[logArgc][a++] = *b++;
			
			logArgs[logArgc++][a] = 0;
			if (*b) b+=2;
		} else {
			while (*b && *b != '(' && *b != '"' && a < 127) 
			logArgs[logArgc][a++] = *b++;
			if (*b) --a;
			logArgs[logArgc++][a] = 0;
		}
	}
}

LogEventsMngr::CLogEvent* LogEventsMngr::registerLogEvent(CPluginMngr::CPlugin* plugin, int func, int pos)
{
	if (pos < 1 || pos > MAX_LOGARGS)
		return 0;

	arelogevents = true;
	CLogEvent** d = &logevents[pos];
	
	while (*d)
		d = &(*d)->next;
	
	return *d = new CLogEvent(plugin, func, this);
}

void LogEventsMngr::executeLogEvents()
{
	bool valid;

	for (CLogEvent* a = logevents[logArgc]; a; a = a->next)
	{
		valid = true;
		
		for (CLogEvent::LogCond* b = a->filters; b; b = b->next)
		{
			valid = false;

			for (CLogEvent::LogCondEle* c = b->list; c; c = c->next)
			{
				if (c->cmp->compareCondition(logArgs[b->argnum]) == 0)
				{
					valid = true;
					break;
				}
			}
			
			if (!valid) 
				break;
		}
		
		if (valid)
		{
			executeForwards(a->func);
		}
	}
}

void LogEventsMngr::clearLogEvents()
{
	logCurrent = logCounter = 0;
	arelogevents = false;
	
	for (int i = 0; i < MAX_LOGARGS + 1; ++i)
	{
		CLogEvent **a = &logevents[i];
		while (*a)
		{
			CLogEvent* bb = (*a)->next;
			delete *a;
			*a = bb;
		}
	}
	
	clearConditions();
}

void LogEventsMngr::clearConditions()
{
	while (logcmplist)
	{
		CLogCmp* a = logcmplist->next;
		delete logcmplist;
		logcmplist = a;
	}
}

LogEventsMngr::CLogEvent::LogCond::~LogCond()
{
	while (list)
	{
		LogCondEle* cc = list->next;
		delete list;
		list = cc;
	}
}

LogEventsMngr::CLogEvent::~CLogEvent()
{
	while (filters)
	{
		LogCond* cc = filters->next;
		delete filters;
		filters = cc;
	}
}

LogEventsMngr::CLogEvent *LogEventsMngr::getValidLogEvent(CLogEvent * a)
{
	bool valid;
	
	while (a)
	{
		valid = true;
		
		for (CLogEvent::LogCond* b = a->filters; b; b = b->next)
		{
			valid = false;

			for (CLogEvent::LogCondEle* c = b->list; c; c = c->next)
			{
				if (c->cmp->compareCondition(logArgs[b->argnum]) == 0)
				{
					valid = true;
					break;
				}
			}
			
			if (!valid) break;
		}
		
		if (!valid)
		{
			a = a->next;
			continue;
		}
		
		return a;
	}
	
	return 0;
}
