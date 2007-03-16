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

#ifndef _INCLUDE_DEBUGGER_H_
#define _INCLUDE_DEBUGGER_H_

#include "CVector.h"
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
public:
	AMX *m_pAmx;
	AMX_DBG *m_pAmxDbg;
	
	int m_Top;
	cell *m_pOpcodeList;
	String m_FileName;
	
	CVector<Tracer *> m_pCalls;
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
	const char *GetFmtCache() { return m_FmtCache.c_str(); }
	
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
	
	String m_MsgCache;
	String m_FmtCache;
	
	trace_info_t *m_pTrace;
};

extern AMX_NATIVE_INFO g_DebugNatives[];

#endif //_INCLUDE_DEBUGGER_H_
