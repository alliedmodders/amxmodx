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


CForward::CForward(const char *name, ForwardExecType et, int numParams, const ForwardParam *paramTypes)
{
	m_FuncName = name;
	m_ExecType = et;
	m_NumParams = numParams;
	memcpy((void *)m_ParamTypes, paramTypes, numParams * sizeof(ForwardParam));
	// find funcs
	int func;
	AMXForward *tmp = NULL;
	for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
	{
		if (amx_FindPublic((*iter).getAMX(), name, &func) == AMX_ERR_NONE)
		{
			tmp = new AMXForward;
			tmp->pPlugin = &(*iter);
			tmp->func = func;
			m_Funcs.put(tmp);
		}
	}
}

int CForward::execute(cell *params, ForwardPreparedArray *preparedArrays)
{
	cell realParams[FORWARD_MAX_PARAMS];
	cell *physAddrs[FORWARD_MAX_PARAMS];

	const STRINGEX_MAXLENGTH = 128;

	int globRetVal = 0;

	for (CList<AMXForward>::iterator iter = m_Funcs.begin(); iter; ++iter)
	{
		if ((*iter).pPlugin->isExecutable((*iter).func))
		{
			// handle strings & arrays
			int i;
			for (i = 0; i < m_NumParams; ++i)
			{
				if (m_ParamTypes[i] == FP_STRING || m_ParamTypes[i] == FP_STRINGEX)
				{
					cell *tmp;
					amx_Allot((*iter).pPlugin->getAMX(),
						(m_ParamTypes[i] == FP_STRING) ? strlen(reinterpret_cast<const char*>(params[i]))+1 : STRINGEX_MAXLENGTH,
						&realParams[i], &tmp);
					amx_SetString(tmp, (const char *)(params[i]), 0, 0);
					physAddrs[i] = tmp;
				}
				else if (m_ParamTypes[i] == FP_ARRAY)
				{
					cell *tmp;
					amx_Allot((*iter).pPlugin->getAMX(), preparedArrays[params[i]].size,
						&realParams[i], &tmp);
					physAddrs[i] = tmp;
					if (preparedArrays[params[i]].type == Type_Cell)
					{
						memcpy(tmp, preparedArrays[params[i]].ptr, preparedArrays[params[i]].size * sizeof(cell));
					}
					else
					{
						char *data = (char*)preparedArrays[params[i]].ptr;
						for (unsigned int j = 0; j < preparedArrays[params[i]].size; ++j)
							*tmp++ = (static_cast<cell>(*data++)) & 0xFF;
					}
				}
				else
				{
					realParams[i] = params[i];
				}
			}
			// exec
			cell retVal;
			amx_Execv((*iter).pPlugin->getAMX(), &retVal, (*iter).func, m_NumParams, realParams);
			// cleanup strings & arrays
			for (i = 0; i < m_NumParams; ++i)
			{
				if (m_ParamTypes[i] == FP_STRING)
				{
					amx_Release((*iter).pPlugin->getAMX(), realParams[i]);
				}
				else if (m_ParamTypes[i] == FP_STRINGEX)
				{
					// copy back
					amx_GetString(reinterpret_cast<char*>(params[i]), physAddrs[i], 0);
					amx_Release((*iter).pPlugin->getAMX(), realParams[i]);
				}
				else if (m_ParamTypes[i] == FP_ARRAY)
				{
					// copy back
					cell *tmp = physAddrs[i];
					if (preparedArrays[params[i]].type == Type_Cell)
					{
						memcpy(preparedArrays[params[i]].ptr, tmp, preparedArrays[params[i]].size * sizeof(cell));
					}
					else
					{
						char *data = (char*)preparedArrays[params[i]].ptr;
						for (unsigned int j = 0; j < preparedArrays[params[i]].size; ++j)
							*data++ = static_cast<char>(*tmp++ & 0xFF);
					}
					amx_Release((*iter).pPlugin->getAMX(), realParams[i]);
				}
			}

			// decide what to do (based on exectype and retval)
			switch (m_ExecType)
			{
			case ET_IGNORE:
				break;
			case ET_STOP:
				if (retVal > 0)
					return retVal;
			case ET_STOP2:
				if (retVal == 1)
					return 1;
				else if (retVal > globRetVal)
					globRetVal = retVal;
				break;
			case ET_CONTINUE:
				if (retVal > globRetVal)
					globRetVal = retVal;
				break;
			}
		}
	}
	return globRetVal;
}

int CForwardMngr::registerForward(const char *funcName, ForwardExecType et, int numParams, const ForwardParam * paramTypes)
{
	int retVal = m_Forwards.size();
	m_Forwards.push_back(new CForward(funcName, et, numParams, paramTypes));
	return retVal;
}

bool CForwardMngr::isIdValid(int id) const
{
	return (id >= 0) && (static_cast<size_t>(id) < m_Forwards.size());
}

int CForwardMngr::executeForwards(int id, cell *params)
{
	int retVal = m_Forwards[id]->execute(params, m_TmpArrays);
	m_TmpArraysNum = 0;
	return retVal;
}

int CForwardMngr::getParamsNum(int id) const
{
	return m_Forwards[id]->getParamsNum();
}

void CForwardMngr::clear()
{
	for (ForwardVec::iterator iter = m_Forwards.begin(); iter != m_Forwards.end(); ++iter)
	{
		delete (*iter);
	}
	m_Forwards.clear();
	m_TmpArraysNum = 0;
}

int registerForward(const char *funcName, ForwardExecType et, ...)
{
	int curParam = 0;
	va_list argptr;
	va_start(argptr, et);
	ForwardParam params[FORWARD_MAX_PARAMS];
	ForwardParam tmp;
	while (true)
	{
		if (curParam == FORWARD_MAX_PARAMS)
			break;
		tmp = va_arg(argptr, ForwardParam);
		if (tmp == FP_DONE)
			break;
		params[curParam] = tmp;
		++curParam;
	}
	va_end(argptr);
	return g_forwards.registerForward(funcName, et, curParam, params);
}

int executeForwards(int id, ...)
{
	if (!g_forwards.isIdValid(id))
		return -1;

	cell params[FORWARD_MAX_PARAMS];
	int paramsNum = g_forwards.getParamsNum(id);
	va_list argptr;
	va_start(argptr, id);
	for (int i = 0; i < paramsNum && i < FORWARD_MAX_PARAMS; ++i)
	{
		params[i] = va_arg(argptr, cell);
	}
	va_end(argptr);
	return g_forwards.executeForwards(id, params);
}

cell CForwardMngr::prepareArray(void *ptr, unsigned int size, ForwardArrayElemType type)
{
	m_TmpArrays[m_TmpArraysNum].ptr = ptr;
	m_TmpArrays[m_TmpArraysNum].size = size;
	m_TmpArrays[m_TmpArraysNum].type = type;
	return m_TmpArraysNum++;
}

cell prepareCellArray(cell *ptr, unsigned int size)
{
	return g_forwards.prepareArray((void*)ptr, size, Type_Cell);
}

cell prepareCharArray(char *ptr, unsigned int size)
{
	return g_forwards.prepareArray((void*)ptr, size, Type_Char);
}