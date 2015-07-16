// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

/*
	CForward.h
	forwards
	1) normal forwards: called in all plugins
	2) single plugin (sp) forwards: called in one plugin

	The SP Forwards are handled differently because they are expected to be created / deleted
	often, but the "normal" forwards are expected to be initialized at start up.

	Note about forward ids:
		for normal forwards:	<index to vector> << 1
		for sp forwards:		(<index to vector> << 1) | 1
*/

#ifndef FORWARD_H
#define FORWARD_H

#include <stdarg.h>
#include "sh_stack.h"

const int FORWARD_MAX_PARAMS = 32;

enum ForwardExecType
{
	ET_IGNORE = 0,					// Ignore return vaue
	ET_STOP,						// Stop on PLUGIN_HANDLED
	ET_STOP2,						// Stop on PLUGIN_HANDLED, continue on other values, return biggest return value
	ET_CONTINUE,					// Continue; return biggest return value
};

enum ForwardParam
{
	FP_DONE = -1,					// specify this as the last argument
									// only tells the function that there are no more arguments
	FP_CELL,						// normal cell
	FP_FLOAT,						// float; used as normal cell though
	FP_STRING,						// string
	FP_STRINGEX,					// string; will be updated to the last function's value
	FP_ARRAY,						// array; use the return value of prepareArray.
};

// for prepareArray
enum ForwardArrayElemType
{
	Type_Cell = 0,
	Type_Char
};

struct ForwardPreparedArray
{
	void *ptr;
	
	ForwardArrayElemType type;
	
	unsigned int size;
	bool copyBack;
};

// Normal forward
class CForward
{
	const char *m_FuncName;
	ForwardExecType m_ExecType;
	int m_NumParams;
	ke::AString m_Name;
	
	struct AMXForward
	{
		CPluginMngr::CPlugin *pPlugin;
		int func;
	};
	
	typedef ke::Vector<AMXForward> AMXForwardList;
	
	AMXForwardList m_Funcs;
	ForwardParam m_ParamTypes[FORWARD_MAX_PARAMS];

public:
	CForward(const char *name, ForwardExecType et, int numParams, const ForwardParam * paramTypes);
	CForward() {}		// leaves everything unitialized'
	
	cell execute(cell *params, ForwardPreparedArray *preparedArrays);
	
	int getParamsNum() const
	{
		return m_NumParams;
	}
	
	int getFuncsNum() const
	{
		return m_Funcs.length();
	}

	const char *getFuncName() const
	{
		return m_Name.chars();
	}
	
	ForwardParam getParamType(int paramId) const
	{
		if (paramId < 0 || paramId >= m_NumParams)
			return FP_DONE;
		
		return m_ParamTypes[paramId];
	}
};

// Single plugin forward
class CSPForward
{
	friend class CForwardMngr;
	int m_NumParams;
	
	ForwardParam m_ParamTypes[FORWARD_MAX_PARAMS];
	AMX *m_Amx;
	
	int m_Func;
	bool m_HasFunc;
	ke::AString m_Name;
	bool m_InExec;
	bool m_ToDelete;

public:
	bool isFree;
public:
	CSPForward() { m_HasFunc = false; }
	void Set(const char *funcName, AMX *amx, int numParams, const ForwardParam * paramTypes);
	void Set(int func, AMX *amx, int numParams, const ForwardParam * paramTypes);

	cell execute(cell *params, ForwardPreparedArray *preparedArrays);
	
	int getParamsNum() const
	{
		return m_NumParams;
	}
	
	int getFuncsNum() const
	{
		return (m_HasFunc) ? 1 : 0;
	}

	const char *getFuncName() const
	{
		return m_Name.chars();
	}
	
	ForwardParam getParamType(int paramId) const
	{
		if (paramId < 0 || paramId >= m_NumParams)
			return FP_DONE;
		
		return m_ParamTypes[paramId];
	}
};

class CForwardMngr
{
	typedef ke::Vector<CForward*> ForwardVec;
	typedef ke::Vector<CSPForward*> SPForwardVec;
	typedef CStack<int> FreeSPVec;							// Free SP Forwards

	ForwardVec m_Forwards;

	SPForwardVec m_SPForwards;
	FreeSPVec m_FreeSPForwards;								// so we don't have to free memory

	ForwardPreparedArray m_TmpArrays[FORWARD_MAX_PARAMS];	// used by prepareArray
	int m_TmpArraysNum;
public:

	CForwardMngr()
	{ m_TmpArraysNum = 0; }
	~CForwardMngr() {}

	// Interface
	// Register normal forward
	int registerForward(const char *funcName, ForwardExecType et, int numParams, const ForwardParam *paramTypes);
	// Register single plugin forward
	int registerSPForward(const char *funcName, AMX *amx, int numParams, const ForwardParam * paramTypes);
	int registerSPForward(int func, AMX *amx, int numParams, const ForwardParam * paramTypes);
	
	// Unregister single plugin forward
	void unregisterSPForward(int id);
	int duplicateSPForward(int id);
	int isSameSPForward(int id1, int id2);
	
	// execute forward
	cell executeForwards(int id, cell *params);
	void clear();							// delete all forwards
	
	bool isIdValid(int id) const;			// check whether forward id is valid
	bool isSPForward(int id) const;			// check whether forward is single plugin
	int getParamsNum(int id) const;			// get num of params of a forward
	int getFuncsNum(int id) const;			// get num of found functions of a forward
	const char *getFuncName(int id) const;	// get the function name
	
	ForwardParam getParamType(int id, int paramId) const;
	cell prepareArray(void *ptr, unsigned int size, ForwardArrayElemType type, bool copyBack);		// prepare array
};

// (un)register forward
int registerForward(const char *funcName, ForwardExecType et, ...);
int registerForwardC(const char *funcName, ForwardExecType et, cell *list, size_t num);
int registerSPForwardByName(AMX *amx, const char *funcName, ...);
int registerSPForwardByNameC(AMX *amx, const char *funcName, cell *list, size_t num);
int registerSPForward(AMX *amx, int func, ...);
void unregisterSPForward(int id);

// execute forwards
cell executeForwards(int id, ...);
// prepare array
cell prepareCellArray(cell *ptr, unsigned int size, bool copyBack = false);
cell prepareCharArray(char *ptr, unsigned int size, bool copyBack = false);

#endif //FORWARD_H
