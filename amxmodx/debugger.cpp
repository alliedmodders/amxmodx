// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "debugger.h"
#include "binlog.h"

/**
 * AMX Mod X Debugging Engine
 *  Written by David "BAILOPAN" Anderson
 */

enum AmxOpcodes
{
  OP_NONE,              /* invalid opcode */
  OP_LOAD_PRI,
  OP_LOAD_ALT,
  OP_LOAD_S_PRI,
  OP_LOAD_S_ALT,
  OP_LREF_PRI,
  OP_LREF_ALT,
  OP_LREF_S_PRI,
  OP_LREF_S_ALT,
  OP_LOAD_I,
  OP_LODB_I,
  OP_CONST_PRI,
  OP_CONST_ALT,
  OP_ADDR_PRI,
  OP_ADDR_ALT,
  OP_STOR_PRI,
  OP_STOR_ALT,
  OP_STOR_S_PRI,
  OP_STOR_S_ALT,
  OP_SREF_PRI,
  OP_SREF_ALT,
  OP_SREF_S_PRI,
  OP_SREF_S_ALT,
  OP_STOR_I,
  OP_STRB_I,
  OP_LIDX,
  OP_LIDX_B,
  OP_IDXADDR,
  OP_IDXADDR_B,
  OP_ALIGN_PRI,
  OP_ALIGN_ALT,
  OP_LCTRL,
  OP_SCTRL,
  OP_MOVE_PRI,
  OP_MOVE_ALT,
  OP_XCHG,
  OP_PUSH_PRI,
  OP_PUSH_ALT,
  OP_PUSH_R,
  OP_PUSH_C,
  OP_PUSH,
  OP_PUSH_S,
  OP_POP_PRI,
  OP_POP_ALT,
  OP_STACK,
  OP_HEAP,
  OP_PROC,
  OP_RET,
  OP_RETN,
  OP_CALL,
  OP_CALL_PRI,
  OP_JUMP,
  OP_JREL,
  OP_JZER,
  OP_JNZ,
  OP_JEQ,
  OP_JNEQ,
  OP_JLESS,
  OP_JLEQ,
  OP_JGRTR,
  OP_JGEQ,
  OP_JSLESS,
  OP_JSLEQ,
  OP_JSGRTR,
  OP_JSGEQ,
  OP_SHL,
  OP_SHR,
  OP_SSHR,
  OP_SHL_C_PRI,
  OP_SHL_C_ALT,
  OP_SHR_C_PRI,
  OP_SHR_C_ALT,
  OP_SMUL,
  OP_SDIV,
  OP_SDIV_ALT,
  OP_UMUL,
  OP_UDIV,
  OP_UDIV_ALT,
  OP_ADD,
  OP_SUB,
  OP_SUB_ALT,
  OP_AND,
  OP_OR,
  OP_XOR,
  OP_NOT,
  OP_NEG,
  OP_INVERT,
  OP_ADD_C,
  OP_SMUL_C,
  OP_ZERO_PRI,
  OP_ZERO_ALT,
  OP_ZERO,
  OP_ZERO_S,
  OP_SIGN_PRI,
  OP_SIGN_ALT,
  OP_EQ,
  OP_NEQ,
  OP_LESS,
  OP_LEQ,
  OP_GRTR,
  OP_GEQ,
  OP_SLESS,
  OP_SLEQ,
  OP_SGRTR,
  OP_SGEQ,
  OP_EQ_C_PRI,
  OP_EQ_C_ALT,
  OP_INC_PRI,
  OP_INC_ALT,
  OP_INC,
  OP_INC_S,
  OP_INC_I,
  OP_DEC_PRI,
  OP_DEC_ALT,
  OP_DEC,
  OP_DEC_S,
  OP_DEC_I,
  OP_MOVS,
  OP_CMPS,
  OP_FILL,
  OP_HALT,
  OP_BOUNDS,
  OP_SYSREQ_PRI,
  OP_SYSREQ_C,
  OP_FILE,    /* obsolete */
  OP_LINE,    /* obsolete */
  OP_SYMBOL,  /* obsolete */
  OP_SRANGE,  /* obsolete */
  OP_JUMP_PRI,
  OP_SWITCH,
  OP_CASETBL,
  OP_SWAP_PRI,
  OP_SWAP_ALT,
  OP_PUSHADDR,
  OP_NOP,
  OP_SYSREQ_D,
  OP_SYMTAG,  /* obsolete */
  OP_BREAK,
  /* ----- */
  OP_NUM_OPCODES
} OPCODE;


const char *GenericError(int err);

Debugger::Tracer::~Tracer()
{
	Clear();
}

void Debugger::Tracer::StepI(cell frm, cell cip)
{
	if (m_pEnd == NULL)
	{
		assert(m_Reset);

		if (m_pStart == NULL)
			m_pStart = new trace_info();

		m_pEnd = m_pStart;
		m_Reset = true;
		m_pEnd->cip = cip;
		m_pEnd->frm = frm;
		m_pEnd->used = true;
	} else {
		if (m_pEnd->frm > frm)
		{
			//the last frame has moved down the stack.
			//push a new call onto our list
			if (m_pEnd->next)
			{
				m_pEnd = m_pEnd->next;
				m_pEnd->used = true;
			} else {
				trace_info *pInfo = new trace_info();
				m_pEnd->next = pInfo;
				pInfo->prev = m_pEnd;
				pInfo->used = true;
				m_pEnd = pInfo;
			}
			//if we're pushing a new call, save the initial frame
			m_pEnd->frm = frm;
		} else if (m_pEnd->frm < frm) {
			//the last frame has moved up the stack.
			//pop a call from our list
			m_pEnd->used = false;
			m_pEnd = m_pEnd->prev;
		}
		//no matter where we are, save the current cip
		m_pEnd->cip = cip;
	}
}

void Debugger::Tracer::Clear()
{
	trace_info *pInfo, *pNext;

	pInfo = m_pStart;
	while (pInfo)
	{
		pNext = pInfo->next;
		delete pInfo;
		pInfo = pNext;
	}

	m_pStart = NULL;
	m_pEnd = NULL;
	m_Error = AMX_ERR_NONE;
	m_Reset = true;
}

void Debugger::Tracer::Reset()
{
	trace_info *pInfo = m_pStart;

	while (pInfo && pInfo->used)
	{
		pInfo->used = false;
		pInfo = pInfo->next;
	}

	m_pEnd = NULL;
	m_Error = AMX_ERR_NONE;
	m_Reset = true;
}

trace_info_t *Debugger::Tracer::GetStart() const
{
	return m_pStart;
}

trace_info_t *Debugger::Tracer::GetEnd() const
{
	return m_pEnd;
}

void Debugger::BeginExec()
{
	m_Top++;
	assert(m_Top >= 0);

	if (m_Top >= (int)m_pCalls.length())
	{
		Tracer *pTracer = new Tracer();
		m_pCalls.append(pTracer);
		assert(m_Top == static_cast<int>(m_pCalls.length() - 1));
	}

	m_pCalls[m_Top]->Reset();
}

void Debugger::EndExec()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	m_pCalls[m_Top]->Reset();

	m_Top--;
}

void Debugger::StepI()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

#if defined BINLOG_ENABLED
	if (g_binlog_level & 32)
	{
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(m_pAmx);
		if (pl)
		{
			long line;
			dbg_LookupLine(m_pAmxDbg, m_pAmx->cip, &line);
			g_BinLog.WriteOp(BinLog_SetLine, pl->getId(), (int)(line + 1));
		}
	}
#endif

	m_pCalls[m_Top]->StepI(m_pAmx->frm, m_pAmx->cip);
}

void Debugger::Reset()
{
	//no call state
	m_Top = -1;
}

int Debugger::GetTracedError()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	return m_pCalls[m_Top]->m_Error;
}

void Debugger::SetTracedError(int error)
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	m_pCalls[m_Top]->m_Error = error;
}

trace_info_t *Debugger::GetTraceStart() const
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	return m_pCalls[m_Top]->GetEnd();
}

bool Debugger::GetTraceInfo(trace_info_t *pTraceInfo, long &line, const char *&function, const char *&file)
{
	cell addr = pTraceInfo->cip;

	dbg_LookupFunction(m_pAmxDbg, addr, &function);
	dbg_LookupLine(m_pAmxDbg, addr, &line);
	dbg_LookupFile(m_pAmxDbg, addr, &file);

	return true;
}

trace_info_t *Debugger::GetNextTrace(trace_info_t *pTraceInfo)
{
	if (!pTraceInfo->prev || !pTraceInfo->prev->used)
		return NULL;

	return pTraceInfo->prev;
}

bool Debugger::ErrorExists()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	return (m_pCalls[m_Top]->m_Error != AMX_ERR_NONE);
}

int Debugger::FormatError(char *buffer, size_t maxLength)
{
	if (!ErrorExists())
		return -1;

	assert(m_Top >= 0 && m_Top < (int)m_pCalls.length());

	Tracer *pTracer = m_pCalls[m_Top];
	int error = pTracer->m_Error;
	const char *gen_err = GenericError(error);
	int size = 0;
	//trace_info_t *pTrace = pTracer->GetEnd();
	//cell cip = _CipAsVa(m_pAmx->cip);
	//cell *p_cip = NULL;
	//int amx_err = AMX_ERR_NONE;

	size += ke::SafeSprintf(buffer, maxLength, "Run time error %d: %s ", error, gen_err);
	buffer += size;
	maxLength -= size;

	if (error == AMX_ERR_NATIVE || error == AMX_ERR_INVNATIVE)
	{
		char native_name[sNAMEMAX+1];
		int num = 0;
		/*//go two instructions back
		cip -= (sizeof(cell) * 2);
		int instr = _GetOpcodeFromCip(cip, p_cip);
		if (instr == OP_SYSREQ_C)
		{
			num = (int)*p_cip;
		}*/
		//New code only requires this...
		num = (int)(_INT_PTR)m_pAmx->usertags[UT_NATIVE];
		/*amx_err = */amx_GetNative(m_pAmx, num, native_name);
		/*if (num)
			amx_err = amx_GetNative(m_pAmx, (int)*p_cip, native_name);
		else 
			amx_err = AMX_ERR_NOTFOUND;*/
		//if (!amx_err)
			size += ke::SafeSprintf(buffer, maxLength, "(native \"%s\")", native_name);
	}

	return size;
}

cell Debugger::_CipAsVa(cell cip)
{
	AMX_HEADER *hdr = (AMX_HEADER*)(m_pAmx->base);
	unsigned char *code = m_pAmx->base + (int)hdr->cod;

	if (cip >= (cell)code && cip < (cell)(m_pAmx->base + (int)hdr->dat))
	{
		return (cell)(cip-(cell)code);
	} else {
		return (cell)(code + cip);
	}
}

int Debugger::_GetOpcodeFromCip(cell cip, cell *&addr)
{
	AMX_HEADER *hdr = (AMX_HEADER*)(m_pAmx->base);
	unsigned char *code = m_pAmx->base + (int)hdr->cod;

	cell *p_cip = NULL;
	//test if cip is between these 
	if (cip >= (cell)code && cip < (cell)(m_pAmx->base + (int)hdr->dat))
	{
		p_cip = (cell *)(cip);
	} else {
		p_cip = (cell *)(code + cip);
	}

	//move forward one entry
	addr = p_cip + 1;

	//p_cip should be aligned to an instruction!
	cell instr = *p_cip;

	if (instr < 1 || instr >= OP_NUM_OPCODES)
	{
		if (!m_pOpcodeList)
			return 0;

		//we have an invalid opcode, so try searching for it
		for (cell i=1; i<OP_NUM_OPCODES; i++)
		{
			if ((cell)m_pOpcodeList[i] == instr)
			{
				instr = i;
				break;
			}
		}

		if (instr < 1 || instr >= OP_NUM_OPCODES)
			instr = 0;		//nothing found
	}

	return (int)instr;
}

void Debugger::_CacheAmxOpcodeList()
{
    m_pOpcodeList = (cell *)m_pAmx->userdata[UD_OPCODELIST];
}

//by BAILOPAN
//  generic error printing routine
//  for pawn 3.0 this is just a wrapper
const char *GenericError(int err)
{
	static const char *amx_errs[] =
	{
		NULL,
		"forced exit",
		"assertion failed",
		"stack error",
		"index out of bounds",
		"memory access",
		"invalid instruction",
		"stack low",
		"heap low",
		"callback",
		"native error",
		"divide",
		"sleep",
		"invalid access state",
		"native not found",
		NULL,
		"out of memory", //16
		"bad file format",
		"bad file version",
		"function not found",
		"invalid entry point",
		"debugger cannot run",
		"plugin un or re-initialized",
		"userdata table full",
		"JIT failed to initialize",
		"parameter error",
		"domain error",
	};
	//does this plugin have line ops?
	const char *geterr = NULL;
	if (err <= 26 && err > 0)
		geterr = amx_errs[err];

	return geterr ? geterr : "unknown error";
}

int AMXAPI Debugger::DebugHook(AMX *amx)
{
	Debugger *pDebugger = NULL;

	if (!amx || !(amx->flags & AMX_FLAG_DEBUG))
		return AMX_ERR_NONE;

	if (amx->flags & AMX_FLAG_PRENIT)
		return AMX_ERR_NONE;

	pDebugger = (Debugger *)amx->userdata[UD_DEBUGGER];

	if (!pDebugger)
		return AMX_ERR_NONE;

	pDebugger->StepI();

	return AMX_ERR_NONE;
}

void Debugger::Clear()
{
	for (size_t i = 0; i < m_pCalls.length(); i++)
	{
		delete m_pCalls[i];
	}

	m_pCalls.clear();
}

void Debugger::DisplayTrace(const char *message)
{
	if (message != NULL)
		AMXXLOG_Error("%s", message);

	char buffer[512];
	int length = FormatError(buffer, sizeof(buffer)-1);

	const char *filename = _GetFilename();
	const char *version = _GetVersion();

	AMXXLOG_Error("[AMXX] Displaying debug trace (plugin \"%s\", version \"%s\")", filename, version);
	
	if (length != -1) // Don't show blank line if AMX_ERR_NONE is set since there is no error message.
	{
		AMXXLOG_Error("[AMXX] %s", buffer);
	}

	int count = 0;
	long lLine;
	const char *file, *function;
	trace_info_t *pTrace = GetTraceStart();
	while (pTrace)
	{
		GetTraceInfo(pTrace, lLine, function, file);
		AMXXLOG_Error(
			"[AMXX]    [%d] %s::%s (line %d)",
			count,
			file,
			function,
			(int)(lLine + 1)
			);
		count++;
		pTrace = GetNextTrace(pTrace);
	}
}

const char *Debugger::_GetFilename()
{
    if (m_FileName.length() < 1)
    {
        CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(m_pAmx);
        if (pl)
        {
            m_FileName = pl->getName();
        }
        else
        {
            for (auto script : g_loadedscripts)
            {
                if (script->getAMX() == m_pAmx)
                {
                    m_FileName = script->getName();
                    break;
                }
            }
        }
    }
    return m_FileName.chars();
}

const char *Debugger::_GetVersion()
{
    if (m_Version.length() < 1)
    {
        const char *version = "";
        CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(m_pAmx);
        if (pl)
        {
            version = pl->getVersion();
        }

        m_Version = version;
    }

    return m_Version.chars();
}

void Debugger::FmtGenericMsg(AMX *amx, int error, char buffer[], size_t maxLength)
{
    const char *filename = "";
    char native[sNAMEMAX+1];
    for (auto script : g_loadedscripts)
    {
        if (script->getAMX() == amx)
        {
            filename = script->getName();
            break;
        }
    }
    size_t len = strlen(filename);
    for (size_t i=len-1; i<len; i--)
    {
        if ((filename[i] == '/' || filename[i] == '\\') && i != len - 1)
        {
            filename = &(filename[i+1]);
            break;
        }
    }

    if (error == AMX_ERR_EXIT)
    {
        ke::SafeSprintf(buffer, maxLength, "Run time error %d (plugin \"%s\") - %s", error, filename, GenericError(AMX_ERR_EXIT));
    } else if (error == AMX_ERR_NATIVE) {
        amx_GetNative(amx, reinterpret_cast<long>(amx->usertags[UT_NATIVE]), native);
        ke::SafeSprintf(buffer, maxLength, "Run time error %d (plugin \"%s\") (native \"%s\") - debug not enabled!", error, filename, native);
    } else {
        ke::SafeSprintf(buffer, maxLength, "Run time error %d (plugin \"%s\") - debug not enabled!", error, filename);
    }
}

void Debugger::GenericMessage(AMX *amx, int err)
{
	static char buffer[512];

	buffer[0] = '\0';
	Debugger::FmtGenericMsg(amx, err, buffer, sizeof(buffer)-1);

	if (buffer[0] != '\0')
		AMXXLOG_Error("[AMXX] %s", buffer);
}

Debugger::~Debugger()
{
	Clear();
	dbg_FreeInfo(m_pAmxDbg);
	delete m_pAmxDbg;
}

int Handler::SetErrorHandler(const char *function)
{
	int error;
	
	error = amx_FindPublic(m_pAmx, function, &m_iErrFunc);

	if (error != AMX_ERR_NONE && m_iErrFunc < 0)
		m_iErrFunc = -1;

	return error;
}

int Handler::SetModuleFilter(const char *function)
{
	int error;
	
	error = amx_FindPublic(m_pAmx, function, &m_iModFunc);

	if (error != AMX_ERR_NONE && m_iModFunc < 0)
		m_iModFunc = -1;

	return error;
}

int Handler::SetNativeFilter(const char *function)
{
	int error;
	
	error = amx_FindPublic(m_pAmx, function, &m_iNatFunc);

	if (error != AMX_ERR_NONE && !IsNativeFiltering())
		m_iNatFunc = -1;

	return error;
}

void Handler::SetErrorMsg(const char *msg)
{
	if (!msg)
		m_MsgCache = nullptr;
	else
		m_MsgCache = msg;
}

const char *Handler::GetLastMsg()
{
	if (m_MsgCache.length() < 1)
		return NULL;

	return m_MsgCache.chars();
}

int Handler::HandleModule(const char *module, bool isClass)
{
	if (m_iModFunc < 0)
		return 0;

	/**
	 * This is the most minimalistic handler of them all
	 */

	cell hea_addr, *phys_addr, retval;
	Debugger *pd;

	pd = DisableDebugHandler(m_pAmx);

	//temporarily set prenit
	m_pAmx->flags |= AMX_FLAG_PRENIT;
	amx_Push(m_pAmx, isClass ? 1 : 0);
	amx_PushString(m_pAmx, &hea_addr, &phys_addr, module, 0, 0);
	int err = amx_ExecPerf(m_pAmx, &retval, m_iModFunc);
	amx_Release(m_pAmx, hea_addr);
	m_pAmx->flags &= ~AMX_FLAG_PRENIT;

	EnableDebugHandler(m_pAmx, pd);

	if (err != AMX_ERR_NONE)
		return 0;

	return (int)retval;
}

int Handler::HandleNative(const char *native, int index, int trap)
{
	if (!IsNativeFiltering())
		return 0;

	/**
	 * Our handler here is a bit different from HandleError().
	 * For one, there is no current error in pDebugger, so we 
	 *  don't have to save some states.
	 */

	m_InNativeFilter = true;

	Debugger *pDebugger = (Debugger *)m_pAmx->userdata[UD_DEBUGGER];

	if (pDebugger && trap)
		pDebugger->BeginExec();
	else if (pDebugger && !trap)
		DisableDebugHandler(m_pAmx);

	cell hea_addr, *phys_addr, retval;

	if (!trap)
		m_pAmx->flags |= AMX_FLAG_PRENIT;

	amx_Push(m_pAmx, trap);
	amx_Push(m_pAmx, index);
	amx_PushString(m_pAmx, &hea_addr, &phys_addr, native, 0, 0);
	int err = amx_ExecPerf(m_pAmx, &retval, m_iNatFunc);
	if (err != AMX_ERR_NONE)
	{
		//LogError() took care of something for us.
		if (err == -1)
		{
			m_InNativeFilter = false;
			amx_Release(m_pAmx, hea_addr);
			return 1;
		}
		if (!trap)
		{
			AMXXLOG_Error("[AMXX] Runtime failure %d occurred in native filter.  Aborting plugin load.", err);
			return 0;
		}
		//handle this manually.
		//we need to push this through an error filter, same as executeForwards!
		if (pDebugger && pDebugger->ErrorExists())
		{
			//don't display, it was already handled.
		} else if (err != -1) {
			LogError(m_pAmx, err, NULL);
		}
		AMXXLOG_Error("[AMXX] NOTE: Runtime failures in native filters are not good!");
		retval = 0;
	}
	if (!trap)
		m_pAmx->flags &= ~AMX_FLAG_PRENIT;
	if (pDebugger && trap)
		pDebugger->EndExec();
	else if (pDebugger && !trap)
		EnableDebugHandler(m_pAmx, pDebugger);

	amx_Release(m_pAmx, hea_addr);

	m_InNativeFilter = false;

	return (int)retval;
}

int Handler::HandleError(const char *msg)
{
	if (m_iErrFunc <= 0)
		return 0;

	m_Handling = true;
	m_pTrace = nullptr;
	m_FmtCache = nullptr;

	Debugger *pDebugger = (Debugger *)m_pAmx->userdata[UD_DEBUGGER];

	int error = m_pAmx->error;

	static char _buffer[512];
	if (pDebugger)
	{
		pDebugger->SetTracedError(error);
		m_pTrace = pDebugger->GetTraceStart();
		pDebugger->FormatError(_buffer, sizeof(_buffer)-1);
		m_FmtCache = _buffer;
		pDebugger->BeginExec();
	} else {
		Debugger::FmtGenericMsg(m_pAmx, error, _buffer, sizeof(_buffer)-1);
		m_FmtCache = _buffer;
	}
	
	SetErrorMsg(msg);

	cell hea_addr, *phys_addr, result;

	amx_PushString(m_pAmx, &hea_addr, &phys_addr, msg, 0, 0);
	amx_Push(m_pAmx, pDebugger ? 1 : 0);
	amx_Push(m_pAmx, error);
	int err = amx_ExecPerf(m_pAmx, &result, m_iErrFunc);
	if (err != AMX_ERR_NONE)
	{
		//handle this manually.
		if (pDebugger)
		{
			pDebugger->SetTracedError(err);
			pDebugger->DisplayTrace(msg);
		} else {
			if (GetLastMsg())
				AMXXLOG_Error("%s", GetLastMsg());
			Debugger::GenericMessage(m_pAmx, err);
		}
		AMXXLOG_Error("[AMXX] NOTE: Runtime failures in an error filter are not good!");
	}

	if (pDebugger)
		pDebugger->EndExec();

	amx_Release(m_pAmx, hea_addr);

	m_Handling = false;
	m_pTrace = nullptr;
	m_FmtCache = nullptr;

	if (err != AMX_ERR_NONE || !result)
		return 0;

	return result;
}

static cell AMX_NATIVE_CALL set_error_filter(AMX *amx, cell *params)
{
	int len;
	char *function = get_amxstring(amx, params[1], 0, len);

	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (!pHandler)
	{
		Debugger::GenericMessage(amx, AMX_ERR_NOTFOUND);
		AMXXLOG_Error("[AMXX] Plugin not initialized correctly.");
		return 0;
	}

	int err = pHandler->SetErrorHandler(function);
	if (err != AMX_ERR_NONE)
	{
		Debugger::GenericMessage(amx, AMX_ERR_NOTFOUND);
		AMXXLOG_Error("[AMXX] Function not found: %s", function);
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL dbg_trace_begin(AMX *amx, cell *params)
{
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (!pHandler)
		return 0;		//should never happen

	trace_info_t *pTrace = pHandler->GetTrace();

	return (cell)(pTrace);
}

static cell AMX_NATIVE_CALL dbg_trace_next(AMX *amx, cell *params)
{
	Debugger *pDebugger = (Debugger *)amx->userdata[UD_DEBUGGER];

	if (!pDebugger)
		return 0;

	trace_info_t *pTrace = (trace_info_t *)(params[1]);

	if (pTrace)
		return (cell)(pDebugger->GetNextTrace(pTrace));

	return 0;
}

static cell AMX_NATIVE_CALL dbg_trace_info(AMX *amx, cell *params)
{
	Debugger *pDebugger = (Debugger *)amx->userdata[UD_DEBUGGER];

	if (!pDebugger)
		return 0;

	trace_info_t *pTrace = (trace_info_t *)(params[1]);

	if (!pTrace)
		return 0;

	cell *line_addr = get_amxaddr(amx, params[2]);
	long lLine=-1;
	const char *function=NULL, *file=NULL;

	pDebugger->GetTraceInfo(pTrace, lLine, function, file);

	set_amxstring(amx, params[3], function ? function : "", params[4]);
	set_amxstring(amx, params[5], file ? file : "", params[5]);
	*line_addr = (cell)lLine + 1;

	return 1;
}

static cell AMX_NATIVE_CALL dbg_fmt_error(AMX *amx, cell *params)
{
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (!pHandler)
		return 0;

	const char *str = pHandler->GetFmtCache();

	set_amxstring(amx, params[1], str, params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL set_native_filter(AMX *amx, cell *params)
{
	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (!pHandler)
	{
		Debugger::GenericMessage(amx, AMX_ERR_NOTFOUND);
		AMXXLOG_Error("[AMXX] Plugin not initialized correctly.");
		return 0;
	}

	if (!pHandler->IsNativeFiltering())
	{
		//we can only initialize this during PRENIT
		if (! (amx->flags & AMX_FLAG_PRENIT) )
			return 0;
	}

	int len;
	char *func = get_amxstring(amx, params[1], 0, len);

	int err = pHandler->SetNativeFilter(func);

	if (err != AMX_ERR_NONE)
	{
		Debugger::GenericMessage(amx, AMX_ERR_NOTFOUND);
		AMXXLOG_Error("[AMXX] Function not found: %s", function);
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL set_module_filter(AMX *amx, cell *params)
{
	if ( !(amx->flags & AMX_FLAG_PRENIT) )
		return -1;

	Handler *pHandler = (Handler *)amx->userdata[UD_HANDLER];

	if (!pHandler)
		return -2;

	int len;
	char *function = get_amxstring(amx, params[1], 0, len);

	return pHandler->SetModuleFilter(function);
}


AMX_NATIVE_INFO g_DebugNatives[] = {
	{"set_error_filter",	set_error_filter},
	{"dbg_trace_begin",		dbg_trace_begin},
	{"dbg_trace_next",		dbg_trace_next},
	{"dbg_trace_info",		dbg_trace_info},
	{"dbg_fmt_error",		dbg_fmt_error},
	{"set_native_filter",	set_native_filter},
	{"set_module_filter",	set_module_filter},
	{NULL,					NULL},
};
