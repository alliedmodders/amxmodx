// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_DEBUGGER_H_
#define _INCLUDE_DEBUGGER_H_

#include "amxdbg.h"

/**
 * Third revision of the AMX Mod X Plugin Debugger.
 * This final, object oriented version is safe for multiple calls and lets you 
 *  fine-tune error handling.
 *  -BAILOPAN
 */
class Debugger
{
public:
	class Tracer
	{
	public:
		
		struct trace_info
		{
			trace_info() : cip(0), frm(0), next(NULL), prev(NULL), used(false) {};
			
			cell cip;
			cell frm;
			
			trace_info *next;
			trace_info *prev;
			
			bool used;
		};
	
	public:
		Tracer() : m_Error(0), m_pStart(NULL), m_pEnd(NULL), m_Reset(true) {};
		~Tracer();
	public:
		void StepI(cell frm, cell cip);
		void Reset();
		void Clear();
		
		Debugger::Tracer::trace_info *GetStart() const;
		Debugger::Tracer::trace_info *GetEnd() const;
	public:
		int m_Error;
	private:
		trace_info *m_pStart;
		trace_info *m_pEnd;
		
		bool m_Reset;
	};

public:
	Debugger(AMX *pAmx, AMX_DBG *pAmxDbg) : m_pAmx(pAmx), m_pAmxDbg(pAmxDbg), m_Top(-1)
	{
		_CacheAmxOpcodeList();
	};
	~Debugger();
public:
	//Begin a trace for a function
	void BeginExec();

	//Step through one instruction
	void StepI();

	//Get/set the last traced error
	int GetTracedError();
	void SetTracedError(int error);

	//Get the first trace info of the call stack
	Debugger::Tracer::trace_info *GetTraceStart() const;

	//Get extra info about the call stack
	bool GetTraceInfo(Debugger::Tracer::trace_info *pTraceInfo, long &line, const char *&function, const char *&file);

	//Get the next trace in the call stack, NULL if none
	Debugger::Tracer::trace_info *GetNextTrace(Debugger::Tracer::trace_info *pTraceInfo);

	//Returns true if an error exists
	bool ErrorExists();

	//Formats the error message into a buffer.
	//returns length of data copied, or -1 if there is no error.
	int FormatError(char *buffer, size_t maxLength);

	//End a trace
	void EndExec();

	//Reset the internal states as if the debugger was inactive
	void Reset();

	//Destroy internal states for shutdown
	void Clear();

	void DisplayTrace(const char *message);

	AMX *GetAMX() const { return m_pAmx; }
public:
	//generic static opcode breaker
	static int AMXAPI DebugHook(AMX *amx);
	
	static void FmtGenericMsg(AMX *amx, int error, char buffer[], size_t maxLength);
	static void GenericMessage(AMX *amx, int error);
private:
	void _CacheAmxOpcodeList();
	
	int _GetOpcodeFromCip(cell cip, cell *&addr);
	cell _CipAsVa(cell cip);
	
	const char *_GetFilename();
	const char *_GetVersion();
public:
	AMX *m_pAmx;
	AMX_DBG *m_pAmxDbg;
	
	int m_Top;
	cell *m_pOpcodeList;
	ke::AString m_FileName;
	ke::AString m_Version;

	ke::Vector<Tracer *> m_pCalls;
};

typedef Debugger::Tracer::trace_info trace_info_t;

/**
 * Error handler for plugins
 */

class Handler
{
public:
	Handler(AMX *pAmx) : m_pAmx(pAmx), m_iErrFunc(-1), m_iModFunc(-1), m_iNatFunc(-1), m_Handling(false), m_InNativeFilter(false) {};
	~Handler() {};
public:
	int SetErrorHandler(const char *function);
	int SetNativeFilter(const char *function);
	int SetModuleFilter(const char *function);
public:
	int HandleError(const char *msg);
	int HandleNative(const char *native, int index, int trap);
	int HandleModule(const char *module, bool isClass=false);
public:
	bool IsHandling() const { return m_Handling; }
	void SetErrorMsg(const char *msg);
	
	const char *GetLastMsg();
	trace_info_t *GetTrace() const { return m_pTrace; }
	const char *GetFmtCache() { return m_FmtCache.chars(); }
	
	bool IsNativeFiltering() { return (m_iNatFunc > -1); }
	bool InNativeFilter() { return m_InNativeFilter; }
private:
	AMX *m_pAmx;
	
	int m_iErrFunc;
	int m_iModFunc;
	int m_iNatFunc;
	
	bool m_Handling;
	
	//in the future, make this a stack!
	bool m_InNativeFilter;
	
	ke::AString m_MsgCache;
	ke::AString m_FmtCache;
	
	trace_info_t *m_pTrace;
};

extern AMX_NATIVE_INFO g_DebugNatives[];

#endif //_INCLUDE_DEBUGGER_H_
