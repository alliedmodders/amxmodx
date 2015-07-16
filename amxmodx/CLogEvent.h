// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef LOGEVENTS_H
#define LOGEVENTS_H

#define MAX_LOGARGS 12

#include <stdarg.h>

// *****************************************************
// class LogEventsMngr
// *****************************************************

class LogEventsMngr
{
	char logString[256];
	char logArgs[MAX_LOGARGS][128];
	int logArgc;
	int logCounter;
	int logCurrent;
	bool arelogevents;

public:
	class CLogCmp;
	class iterator;
	class CLogEvent;
	friend class CLogEvent;
	friend class CLogCmp;
	friend class iterator;

	class CLogCmp 
	{
		friend class LogEventsMngr;
		friend class CLogEvent;
		
		LogEventsMngr* parent;
		ke::AString text;
		
		int logid;
		int pos;
		int result;
		bool in;
		
		CLogCmp *next;
		
		CLogCmp(const char* s, bool r, int p, CLogCmp *n, LogEventsMngr* mg) : text(s)
		{
			logid = result = 0;
			pos = p;
			parent = mg;
			in = r;
			next = n;
		}
	
	public:
		int compareCondition(const char* string);
	};

private:
	CLogCmp *logcmplist;
public:

	class CLogEvent
	{
		friend class LogEventsMngr;
		friend class iterator;
		
		struct LogCondEle
		{
			CLogCmp *cmp;
			LogCondEle *next;
			LogCondEle(CLogCmp *c, LogCondEle *n): cmp(c), next(n) {}
		};
	
		struct LogCond
		{
			int argnum;
			
			LogCondEle *list;
			LogCond *next;
			LogCond(int a, LogCondEle* ee, LogCond* n) : argnum(a), list(ee), next(n) {}
			~LogCond();
		};
		
		CPluginMngr::CPlugin *plugin;
		
		int func;
		
		LogCond *filters;
		LogEventsMngr* parent;
		
		CLogEvent *next;
		CLogEvent(CPluginMngr::CPlugin *p, int f, LogEventsMngr* ppp) : plugin(p), func(f), filters(0), parent(ppp), next(0) {}
		~CLogEvent();
	public:
		inline CPluginMngr::CPlugin *getPlugin() { return plugin; }
		void registerFilter(char* filter);
		inline int getFunction() { return func; }
	};

private:
	CLogEvent *logevents[MAX_LOGARGS + 1];
	CLogEvent *getValidLogEvent(CLogEvent * a);
	CLogCmp* registerCondition(char* filter);
	
	void clearConditions();
public:
	LogEventsMngr();
	~LogEventsMngr();

	// Interface
	CLogEvent* registerLogEvent(CPluginMngr::CPlugin* plugin, int func, int pos);
	inline bool logEventsExist() { return arelogevents; } 
	
	void setLogString(const char* frmt, va_list& vaptr);
	void setLogString(const char* frmt, ...);
	void parseLogString();
	void executeLogEvents();
	
	inline const char* getLogString() { return logString; }
	inline int getLogArgNum() { return logArgc; }
	inline const char* getLogArg(int i) { return (i < 0 || i >= logArgc) ? "" : logArgs[i]; }
	void clearLogEvents();

	class iterator
	{
		CLogEvent* a;
		LogEventsMngr* b;
	
	public:
		inline iterator(CLogEvent*aa, LogEventsMngr* bb) : a(aa), b(bb) {}
			
		inline iterator& operator++()
		{
			a = b->getValidLogEvent(a->next);
			return *this;
		}

		inline bool operator==(const iterator& c) const { return a == c.a; }
		inline bool operator!=(const iterator& c) const { return !operator == (c); }
		CLogEvent& operator*() { return *a; }
		operator bool () const { return a ? true : false; }
	};
	
	inline iterator begin() { return iterator(getValidLogEvent(logevents[logArgc]), this); }
	inline iterator end() { return iterator(0, this); }
};

#endif //LOGEVENTS_H
