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

#ifndef FORWARD_H
#define FORWARD_H

// *****************************************************
// class CmdMngr
// *****************************************************

const int FORWARD_MAX_PARAMS = 16;

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
};

class CForward
{
	const char *m_FuncName;
	ForwardExecType m_ExecType;
	int m_NumParams;
	struct AMXForward
	{
		CPluginMngr::CPlugin *pPlugin;
		int func;
	};
	typedef CList<AMXForward> AMXForwardList;
	AMXForwardList m_Funcs;
	ForwardParam m_ParamTypes[FORWARD_MAX_PARAMS];
public:
	CForward(const char *name, ForwardExecType et, int numParams, const ForwardParam * paramTypes);
	CForward()
	{ }			// leaves everything unitialized
	int execute(cell *params, ForwardPreparedArray *preparedArrays);
	int getParamsNum() const
	{
		return m_NumParams;
	}
};

class CForwardMngr
{
	typedef CVector<CForward*> ForwardVec;
	ForwardVec m_Forwards;
	ForwardPreparedArray m_TmpArrays[FORWARD_MAX_PARAMS];		// used by prepareArray
	int m_TmpArraysNum;
public:

	CForwardMngr()
	{ m_TmpArraysNum = 0; }
	~CForwardMngr()
	{ }

	// Interface
	int registerForward(const char *funcName, ForwardExecType et, int numParams, const ForwardParam *paramTypes);
	int executeForwards(int id, cell *params);
	void clear();		// delete all forwards
	bool isIdValid(int id) const;
	int getParamsNum(int id) const;
	cell prepareArray(void *ptr, unsigned int size, ForwardArrayElemType type);
};

// register forward
int registerForward(const char *funcName, ForwardExecType et, ...);
// execute forwards
int executeForwards(int id, ...);
// prepare array
cell prepareCellArray(cell *ptr, unsigned int size);
cell prepareCharArray(char *ptr, unsigned int size);

#endif

