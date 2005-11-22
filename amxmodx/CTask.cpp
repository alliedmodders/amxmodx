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
#include "CTask.h"
#include "sh_stack.h"

CStack<CTaskMngr::CTask *> *g_FreeTasks;

/*********************** CTask ***********************/

int CTaskMngr::CTask::getTaskId() const
{
	return m_iId;
}

CPluginMngr::CPlugin *CTaskMngr::CTask::getPlugin() const
{
	return m_pPlugin;
}

void CTaskMngr::CTask::set(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime)
{
	clear();
	m_bFree = false;

	m_pPlugin = pPlugin;
	m_iFunc = iFunc;
	m_iId = iId;
	m_fBase = fBase;

	if (iFlags & 2)
	{
		m_bLoop = true;
		m_iRepeat = -1;
	}
	else if (iFlags & 1)
	{
		m_bLoop = true;
		m_iRepeat = iRepeat;
	}
	
	type = 0;
	if (iFlags & 4)
		type = 1;
	if (iFlags & 8)
		type = 2;

	m_fNextExecTime = fCurrentTime + m_fBase;

	if (iParamsLen)
	{
		m_iParamLen = iParamsLen + 1;
		if (m_ParamSize < m_iParamLen)
		{
			m_ParamSize = m_iParamLen;
			cell *temp = new cell[m_ParamSize];
			if (m_pParams != NULL)
				delete [] m_pParams;
			m_pParams = temp;
		}
		cell *dest = m_pParams;
		__asm
		{
			push esi;
			push edi;
			push ecx;
			mov esi, pParams;
			mov edi, dest;
			mov ecx, iParamsLen;
			rep movsd;
			pop esi;
			pop edi;
			pop ecx;
		};
		//memcpy(m_pParams, pParams, sizeof(cell) * iParamsLen);
		m_pParams[iParamsLen] = 0;
	} else {
		m_iParamLen = 0;
	}
}

void CTaskMngr::CTask::clear()
{
	m_bFree = true;

	if (m_iFunc >= 0)
	{
		unregisterSPForward(m_iFunc);
		m_iFunc = -1;
	}

	m_iId = 0;
	m_fNextExecTime = 0.0f;
}

bool CTaskMngr::CTask::isFree() const
{
	return m_bFree;
}

void CTaskMngr::CTask::changeBase(float fNewBase)
{
	m_fBase = fNewBase;
}

void CTaskMngr::CTask::resetNextExecTime(float fCurrentTime)
{
	m_fNextExecTime = fCurrentTime + m_fBase;
}

bool CTaskMngr::CTask::executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft)
{
	bool execute = false;
	bool done = false;

	switch (type)
	{
	case 1:
		{
			if (fCurrentTime - fTimeLeft + 1.0f >= m_fBase)
				execute = true;
			break;
		}
	case 2:
		{
			if (fTimeLimit != 0.0f && (fTimeLeft + fTimeLimit * 60.0f) - fCurrentTime - 1.0f <= m_fBase)
				execute = true;
			break;
		}
	default:
		{
			execute = (m_fNextExecTime <= fCurrentTime) ? true : false;
		}
	}

	if (execute)
	{
		//only bother calling if we have something to call
		if (!(m_bLoop && !m_iRepeat))
		{
			if (m_iParamLen)	// call with parameters
			{
				cell arr = prepareCellArray(m_pParams, m_iParamLen);
				executeForwards(m_iFunc, arr, m_iId);
			} else {
				executeForwards(m_iFunc, m_iId);
			}
		}

		// set new exec time OR remove the task if needed
		if (m_bLoop)
		{
			if (m_iRepeat != -1 && --m_iRepeat <= 0)
				done = true;
		} else {
			done = true;
		}

		if (!done)
			m_fNextExecTime += m_fBase;
	}

	return done;
}

CTaskMngr::CTask::CTask()
{
	m_bFree = true;

	m_pPlugin = NULL;
	m_iFunc = -1;
	m_iId = 0;
	m_fBase = 0.0f;

	m_iRepeat =	0;
	m_bLoop = false;
	type = 0;

	m_fNextExecTime = 0.0f;

	m_iParamLen = 0;
	m_ParamSize = 0;
	m_pParams = NULL;
}

CTaskMngr::CTask::~CTask()
{
	clear();

	if (m_pParams)
	{
		delete [] m_pParams;
		m_pParams = NULL;
	}
}

/*********************** CTaskMngr ***********************/

CTaskMngr::CTaskMngr()
{
	m_pTmr_CurrentTime = NULL;
	m_pTmr_TimeLimit = NULL;
	m_pTmr_TimeLeft = NULL;

	g_FreeTasks = new CStack<CTaskMngr::CTask *>();
}

CTaskMngr::~CTaskMngr()
{
	clear();
	delete g_FreeTasks;
	g_FreeTasks = NULL;
}

void CTaskMngr::registerTimers(float *pCurrentTime, float *pTimeLimit, float *pTimeLeft)
{
	m_pTmr_CurrentTime = pCurrentTime;
	m_pTmr_TimeLimit = pTimeLimit;
	m_pTmr_TimeLeft = pTimeLeft;
}

void CTaskMngr::registerTask(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat)
{
	// first, search for free tasks
	if (!g_FreeTasks->empty())
	{
		// found: reuse it
		CTask *pTmp = g_FreeTasks->front();
		g_FreeTasks->pop();
		pTmp->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
		m_Tasks.unshift(pTmp);
	} else {
		// not found: make a new one
		CTask *pTmp = new CTask;
		
		pTmp->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
		m_Tasks.unshift(pTmp);
	}
}

int CTaskMngr::removeTasks(int iId, AMX *pAmx)
{
	CTaskDescriptor descriptor(iId, pAmx);
	List<CTask *>::iterator iter, end=m_Tasks.end();
	int i = 0;

	for (iter=m_Tasks.begin(); iter!=end; )
	{
		if ( descriptor == (*iter) )
		{
			g_FreeTasks->push( (*iter) );
			iter = m_Tasks.erase(iter);
			++i;
		} else {
			iter++;
		}
	}
	
	return i;
}

int CTaskMngr::changeTasks(int iId, AMX *pAmx, float fNewBase)
{
	CTaskDescriptor descriptor(iId, pAmx);
	List<CTask *>::iterator iter, end=m_Tasks.end();
	int i = 0;
	
	for (iter=m_Tasks.begin(); iter!=end; iter++)
	{
		if ( descriptor == (*iter) )
		{
			(*iter)->changeBase(fNewBase);
			(*iter)->resetNextExecTime(*m_pTmr_CurrentTime);
			++i;
		}
	}
	
	return i;
}

bool CTaskMngr::taskExists(int iId, AMX *pAmx)
{
	CTaskDescriptor descriptor(iId, pAmx);
	List<CTask *>::iterator iter, end=m_Tasks.end();

	for (iter=m_Tasks.begin(); iter!=end; iter++)
	{
		if ( descriptor == (*iter) )
			return true;
	}

	return false;
}

void CTaskMngr::startFrame()
{
	List<CTask *>::iterator iter, end=m_Tasks.end();
	CTask *pTask;
	int ignored=0, used=0;
	for (TaskListIter iter = m_Tasks.begin(); iter!=end;)
	{
		pTask = (*iter);
		if (pTask->executeIfRequired(*m_pTmr_CurrentTime, *m_pTmr_TimeLimit, *m_pTmr_TimeLeft))
		{
			pTask->clear();
			iter = m_Tasks.erase(iter);
			g_FreeTasks->push(pTask);
			used++;
		} else {
			iter++;
			ignored++;
		}
	}
}

void CTaskMngr::clear()
{
	while (!g_FreeTasks->empty())
	{
		delete g_FreeTasks->front();
		g_FreeTasks->pop();
	}

	List<CTask *>::iterator iter, end=m_Tasks.end();
	for (iter=m_Tasks.begin(); iter!=end; iter++)
		delete (*iter);
	m_Tasks.clear();
}
