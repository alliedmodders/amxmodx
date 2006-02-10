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

#include "amxmodx.h"
#include "debugger.h"

#if !defined WIN32 && !defined _WIN32
#define _snprintf snprintf
#endif

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

	if (m_Top >= (int)m_pCalls.size())
	{
		Tracer *pTracer = new Tracer();
		m_pCalls.push_back(pTracer);
		assert(m_Top == (m_pCalls.size() - 1));
	}

	m_pCalls[m_Top]->Reset();
}

void Debugger::EndExec()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	m_pCalls[m_Top]->Reset();

	m_Top--;
}

void Debugger::StepI()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	m_pCalls[m_Top]->StepI(m_pAmx->frm, m_pAmx->cip);
}

void Debugger::Reset()
{
	//no call state
	m_Top = -1;
}

int Debugger::GetTracedError()
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	return m_pCalls[m_Top]->m_Error;
}

void Debugger::SetTracedError(int error)
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	m_pCalls[m_Top]->m_Error = error;
}

trace_info_t *Debugger::GetTraceStart() const
{
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

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
	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	return (m_pCalls[m_Top]->m_Error != AMX_ERR_NONE);
}

#define	FLAG_INDIRECT	(1<<0)

//vaddr - the address of our current index vector
//base - the base address of which the array is offset to
//dim - the current dimension to search
//dimNum - the number of dimensions total
//sizes[] - an array containing the dimension sizes
//Indexes[] - an output array to contain each dimension's index
int WalkArray(cell *vaddr, unsigned char *base, cell *addr, int dim, int dimNum, int &flags, int sizes[], int Indexes[])
{
	cell *my_addr;
	int idx = 0;

	//if we are the second to last walker, we only need to check the ranges of our vector.
	if (dim == dimNum - 2)
	{
		my_addr = vaddr;
		//first check the actual vectors themselves
		for (int i=0; i<sizes[dim]; i++)
		{
			if (addr == my_addr)
				return i;
			my_addr++;
		}

		return -1;
	}

	//otherwise, search lower vectors!
	//vaddr is the address where we can start reading vectors
	flags |= FLAG_INDIRECT;
	for (int i=0; i<sizes[dim]; i++)
	{
		//the next vector is offset from the last address!
		//this is funky but that's the internal implementation
		my_addr = (cell *)((char *)vaddr + i*sizeof(cell) + vaddr[i]);
		idx = WalkArray(my_addr, base, addr, dim+1, dimNum, flags, sizes, Indexes);
		if (idx != -1)
		{
			Indexes[dim+1] = idx;
			return i;
		}
	}

	return -1;
}

int Debugger::FormatError(char *buffer, size_t maxLength)
{
	if (!ErrorExists())
		return -1;

	assert(m_Top >= 0 && m_Top < (int)m_pCalls.size());

	Tracer *pTracer = m_pCalls[m_Top];
	int error = pTracer->m_Error;
	const char *gen_err = GenericError(error);
	int size = 0;
	trace_info_t *pTrace = pTracer->GetEnd();
	cell cip = _CipAsVa(m_pAmx->cip);
	cell *p_cip = NULL;
	int amx_err = AMX_ERR_NONE;

	size += _snprintf(buffer, maxLength, "Run time error %d: %s ", error, gen_err);
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
		num = (int)m_pAmx->usertags[UT_NATIVE];
		amx_err = amx_GetNative(m_pAmx, num, native_name);
		/*if (num)
			amx_err = amx_GetNative(m_pAmx, (int)*p_cip, native_name);
		else 
			amx_err = AMX_ERR_NOTFOUND;*/
		//if (!amx_err)
			size += _snprintf(buffer, maxLength, "(native \"%s\")", native_name);
#if 0
	} else if (error == AMX_ERR_BOUNDS) {
		tagAMX_DBG *pDbg = m_pAmxDbg;
		int symbols = pDbg->hdr->symbols;
		int index = 0;
		tagAMX_DBG_SYMBOL **pSymbols = pDbg->symboltbl;
		tagAMX_DBG_SYMBOL *pSymbol, *pLastSymbol=NULL;
		const tagAMX_DBG_SYMDIM *pDims;
		ucell addr = 0;
		int flags = 0;
		char v_class, i_dent;
		cell *arr_addr=NULL, *p_addr=NULL;
		unsigned char *data = m_pAmx->base + ((AMX_HEADER *)m_pAmx->base)->dat;
		bool valid=false;
		//we can't really browse the assembly because
		// we've no idea what the peephole optimizer did.
		// so we're gonna try to go out on a limb and guess.
		if (m_pAmx->alt < 0)
		{
			//take a guess that it's local
			addr = m_pAmx->alt - pTrace->frm;
			v_class = 1;
		} else {
			//take a guess that it's a global
			//it won't be global if it's passed in from the stack frame, however
			// doing this with a hardcoded array size is quite rare, and is probably passed
			// as iREFARRAY not iARRAY!
			addr = m_pAmx->alt;
			v_class = 0;
		}
		bool found = false;
		bool _found = true;
		static char _msgbuf[255];
		size_t _size = 0;
		//take a pre-emptive guess at the v_class!
		//are we GLOBAL (0) or LOCAL (1) ?
		if (m_pAmx->alt < 0)
		{
			v_class = 1;
			i_dent = iARRAY;
			arr_addr = (cell *)(data + pTrace->frm + m_pAmx->alt);
		} else {
			//it's greater than 0, check other things!
			if (m_pAmx->alt >= m_pAmx->hlw &&
				m_pAmx->alt <= m_pAmx->stp)
			{
				//it's in the stack somewhere... guess that it's a local!
				v_class = 1;
				//relocate it
				m_pAmx->alt -= pTrace->frm;
				//alt cannot be zero
				if (m_pAmx->alt < 0)
					i_dent = iARRAY;
				else
					i_dent = iREFARRAY;
				arr_addr = (cell *)(data + pTrace->frm + m_pAmx->alt);
			} else {
				//guess that it's DAT
				v_class = 0;
				i_dent = iARRAY;
				arr_addr = (cell *)(data + m_pAmx->alt);
			}
		}
		for (index = 0; index < symbols; index++)
		{
			pSymbol = pSymbols[index];
			if (pSymbol->codestart <= (ucell)cip && 
				pSymbol->codeend >= (ucell)cip &&
				(pSymbol->ident == iARRAY || pSymbol->ident == iREFARRAY))
			{
				amx_err = dbg_GetArrayDim(pDbg, pSymbol, &pDims);
				if (amx_err != AMX_ERR_NONE)
					continue;
				//calculate the size of the array.  this is important!
				ucell size = pDims[0].size;
				ucell aggre = pDims[0].size;
				valid = false;
				for (int16_t i=1; i<pSymbol->dim; i++)
				{
					aggre *= pDims[i].size;
					size += aggre;
				}
				if (pSymbol->vclass != v_class)
					continue;
				if (pSymbol->ident != i_dent)
					continue;
				if (v_class == 1)
				{
					if (i_dent == iARRAY)
					{
						p_addr = (cell *)(data + pTrace->frm + pSymbol->address);
					} else if (i_dent == iREFARRAY) {
						//get the variable off the stack, by reference
						ucell _addr = (ucell)*((cell *)(data + pTrace->frm + pSymbol->address));
						p_addr = (cell *)(data + _addr);
					}
				} else if (v_class == 0) {
					p_addr = (cell *)(data + pSymbol->address);
				}
				//make sure our address is in bounds!
				if (arr_addr < p_addr || arr_addr > (p_addr + size))
					continue;
				int *sizes = new int[pSymbol->dim];
				int *indexes = new int[pSymbol->dim];
				for (int i=0; i<pSymbol->dim; i++)
				{
					sizes[i] = pDims[i].size;
					indexes[i] = -1;
				}
				flags = 0;
				if (pSymbol->dim >= 2)
				{
					int dims = pSymbol->dim;
					indexes[0] = WalkArray(p_addr, data, arr_addr, 0, pSymbol->dim, flags, sizes, indexes);
					if (indexes[0] == -1)
					{
						while (indexes[0] == -1 && --dims > 0)
						{
							flags = 0;
							indexes[0] = WalkArray(p_addr, data, arr_addr, 0, dims, flags, sizes, indexes);
						}
					}
					//find the last known good dimension
					for (dims=pSymbol->dim-1; dims>=0; dims--)
					{
						if (indexes[dims] != -1)
							break;
					}
					//check for the "impossible" case.
					//if we have [X][-1], and X is zero, the array did not walk properly.
					if (dims >= 0 
						&& indexes[dims] == 0 
						&& !(flags & FLAG_INDIRECT) 
						&& dims < pSymbol->dim - 1)
					{
						//here we have the dreaded MIXED CASE.  we don't know whether 
						//[-][X] or [0][-] (where - is a bad input) was intended.
						//first, we take a guess by checking the bounds.
						cell *_cip = (cell *)_CipAsVa(cip);
						_cip -= 1;
						cell bounds = *_cip;
						if (sizes[dims] != sizes[dims+1])
						{
							//we were checking initial bounds
							if (bounds == sizes[dims] - 1)
							{
								indexes[dims] = m_pAmx->pri;
							} else if (bounds == sizes[dims+1] - 1) {
								indexes[dims + 1] = m_pAmx->pri;
								indexes[dims] = 0;
							} else {
								//this should really never happen...
								_found = false;
							}
						} else {
							_found = false;
						}
						if (!_found)
						{
							//we still don't have a good approximation.
							//the user did something like:
							//new X[40][40]
							//we could do some really complicated and random guesswork
							// but fact is, we have no way of deterministically knowing
							// what the user intended.
						}
					} else {
						//set the last know index to our culprit
						indexes[dims + 1] = m_pAmx->pri;
					}
				} else {
					indexes[0] = m_pAmx->pri;
				}
				_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "(array \"%s", pSymbol->name);
				for (int i=0; i<pSymbol->dim; i++)
					_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "[%d]", pDims[i].size);
				if (_found)
				{
					_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "\") (indexed \"");
					for (int i=0; i<pSymbol->dim; i++)
					{
						if (indexes[i] == -1)
							_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "[]");
						else
							_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "[%d]", indexes[i]);
					}
					_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "\")");
				} else {
					_size += _snprintf(&(_msgbuf[_size]), sizeof(_msgbuf)-_size, "\") (unknown index \"%d\")", m_pAmx->pri);
				}
				found = true;
				delete [] indexes;
				delete [] sizes;
				break;
			} /* symbol validation */
		} /* is in valid ranges */
		if (!found)
			_msgbuf[0] = '\0';

		size += _snprintf(buffer, maxLength, "%s", _msgbuf);
#endif	//0 - NOT USED!
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
	for (size_t i=0; i<m_pCalls.size(); i++)
			delete m_pCalls[i];

	m_pCalls.clear();
}

void Debugger::DisplayTrace(const char *message)
{
	if (message != NULL)
		AMXXLOG_Log("%s", message);

	char buffer[512];
	FormatError(buffer, sizeof(buffer)-1);

	const char *filename = _GetFilename();

	AMXXLOG_Log("[AMXX] Displaying debug trace (plugin \"%s\")", filename);
	AMXXLOG_Log("[AMXX] %s", buffer);
	
	int count = 0;
	long lLine;
	const char *file, *function;
	trace_info_t *pTrace = GetTraceStart();
	while (pTrace)
	{
		GetTraceInfo(pTrace, lLine, function, file);
		AMXXLOG_Log(
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
	if (m_FileName.size() < 1)
	{
		const char *filename = "";
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(m_pAmx);
		if (pl)
		{
			filename = pl->getName();
		} else {
			CList<CScript,AMX*>::iterator a = g_loadedscripts.find(m_pAmx);
			if (a)
				filename = (*a).getName();
		}
		m_FileName.assign(filename);
	}

	return m_FileName.c_str();
}

void Debugger::FmtGenericMsg(AMX *amx, int error, char buffer[], size_t maxLength)
{
	const char *filename = "";

	CList<CScript,AMX*>::iterator a = g_loadedscripts.find(amx);
	if (a)
		filename = (*a).getName();
	size_t len = strlen(filename);
	for (size_t i=len-1; i>=0; i--)
	{
		if (filename[i] == '/' || filename[i] == '\\' && i != len - 1)
		{
			filename = &(filename[i+1]);
			break;
		}
	}

	_snprintf(buffer, maxLength, "Run time error %d (plugin \"%s\") - debug not enabled!", error, filename);
}

void Debugger::GenericMessage(AMX *amx, int err)
{
	static char buffer[512];

	buffer[0] = '\0';
	Debugger::FmtGenericMsg(amx, err, buffer, sizeof(buffer)-1);

	if (buffer[0] != '\0')
		AMXXLOG_Log("[AMXX] %s", buffer);
}

Debugger::~Debugger()
{
	Clear();
}

int Handler::SetErrorHandler(const char *function)
{
	int error;
	
	error = amx_FindPublic(m_pAmx, function, &m_iErrFunc);

	if (error != AMX_ERR_NONE && m_iErrFunc < 1)
		m_iErrFunc = -1;

	return error;
}

int Handler::SetModuleFilter(const char *function)
{
	int error;
	
	error = amx_FindPublic(m_pAmx, function, &m_iModFunc);

	if (error != AMX_ERR_NONE && m_iModFunc < 1)
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
		m_MsgCache.clear();
	else
		m_MsgCache.assign(msg);
}

const char *Handler::GetLastMsg()
{
	if (m_MsgCache.size() < 1)
		return NULL;

	return m_MsgCache.c_str();
}

int Handler::HandleModule(const char *module)
{
	if (m_iModFunc < 1)
		return 0;

	/**
	 * This is the most minimalistic handler of them all
	 */

	cell hea_addr, *phys_addr, retval;
	Debugger *pd;

	pd = DisableDebugHandler(m_pAmx);

	//temporarily set prenit
	m_pAmx->flags |= AMX_FLAG_PRENIT;
	amx_PushString(m_pAmx, &hea_addr, &phys_addr, module, 0, 0);
	int err = amx_Exec(m_pAmx, &retval, m_iModFunc);
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
	int err = amx_Exec(m_pAmx, &retval, m_iNatFunc);
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
			AMXXLOG_Log("[AMXX] Runtime failure %d occurred in native filter.  Aborting plugin load.", err);
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
		AMXXLOG_Log("[AMXX] NOTE: Runtime failures in native filters are not good!");
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
	m_pTrace = NULL;
	m_FmtCache.clear();

	Debugger *pDebugger = (Debugger *)m_pAmx->userdata[UD_DEBUGGER];

	int error = m_pAmx->error;

	static char _buffer[512];
	if (pDebugger)
	{
		pDebugger->SetTracedError(error);
		m_pTrace = pDebugger->GetTraceStart();
		pDebugger->FormatError(_buffer, sizeof(_buffer)-1);
		m_FmtCache.assign(_buffer);
		pDebugger->BeginExec();
	} else {
		Debugger::FmtGenericMsg(m_pAmx, error, _buffer, sizeof(_buffer)-1);
		m_FmtCache.assign(_buffer);
	}
	
	SetErrorMsg(msg);

	cell hea_addr, *phys_addr, result;

	amx_PushString(m_pAmx, &hea_addr, &phys_addr, msg, 0, 0);
	amx_Push(m_pAmx, pDebugger ? 1 : 0);
	amx_Push(m_pAmx, error);
	int err = amx_Exec(m_pAmx, &result, m_iErrFunc);
	if (err != AMX_ERR_NONE)
	{
		//handle this manually.
		if (pDebugger)
		{
			pDebugger->SetTracedError(err);
			pDebugger->DisplayTrace(msg);
		} else {
			if (GetLastMsg())
				AMXXLOG_Log("%s", GetLastMsg());
			Debugger::GenericMessage(m_pAmx, err);
		}
		AMXXLOG_Log("[AMXX] NOTE: Runtime failures in an error filter are not good!");
	}

	if (pDebugger)
		pDebugger->EndExec();

	amx_Release(m_pAmx, hea_addr);

	m_Handling = false;
	m_pTrace = NULL;
	m_FmtCache.clear();

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
		AMXXLOG_Log("[AMXX] Plugin not initialized correctly.");
		return 0;
	}

	int err = pHandler->SetErrorHandler(function);
	if (err != AMX_ERR_NONE)
	{
		Debugger::GenericMessage(amx, AMX_ERR_NOTFOUND);
		AMXXLOG_Log("[AMXX] Function not found: %s", function);
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
		AMXXLOG_Log("[AMXX] Plugin not initialized correctly.");
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
		AMXXLOG_Log("[AMXX] Function not found: %s", function);
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
