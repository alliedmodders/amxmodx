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

/*********************** CTask ***********************/

void CTaskMngr::CTask::set(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime)
{
	clear();
	m_bFree = false;

	m_pPlugin = pPlugin;
	m_iFunc = iFunc;
	m_iId = iId;
	m_fBase = fBase;
	m_bInExecute = false;

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
	
	m_bAfterStart =	(iFlags & 4) ? true : false;
	m_bBeforeEnd = (iFlags & 8) ? true : false;

	m_fNextExecTime = fCurrentTime + m_fBase;

	if (iParamsLen)
	{
		m_iParamLen = iParamsLen + 1;
		m_pParams = new cell[m_iParamLen];
		memcpy(m_pParams, pParams, sizeof(cell)*iParamsLen);
		m_pParams[iParamsLen] = 0;
	} else {
		m_iParamLen = 0;
		m_pParams = NULL;
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

	if (m_pParams)
	{
		delete [] m_pParams;
		m_pParams = NULL;
	}

	m_pPlugin = NULL;
	m_iId = 0;
	m_fBase = 0.0f;

	m_iRepeat =	0;
	m_bLoop = false;
	m_bAfterStart =	false;
	m_bBeforeEnd = false;

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

void CTaskMngr::CTask::executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft)
{
	bool execute = false;
	bool done = false;
	
	if (m_bAfterStart)
	{
		if (fCurrentTime - fTimeLeft + 1.0f >= m_fBase)
			execute = true;
	}
	else if (m_bBeforeEnd)
	{
		if (fTimeLimit != 0.0f && (fTimeLeft + fTimeLimit * 60.0f) - fCurrentTime - 1.0f <= m_fBase)
			execute = true;
	}
	else if (m_fNextExecTime <= fCurrentTime)
	{
		execute = true;
	}

	if (execute)
	{
		//only bother calling if we have something to call
		if (!(m_bLoop && !m_iRepeat))
		{
			m_bInExecute = true;
			if (m_iParamLen)	// call with parameters
			{
				cell arr = prepareCellArray(m_pParams, m_iParamLen);
				executeForwards(m_iFunc, arr, m_iId);
			} else {
				executeForwards(m_iFunc, m_iId);
			}
			m_bInExecute = false;
		}
	
		if (isFree())
			return;

		// set new exec time OR remove the task if needed
		if (m_bLoop)
		{
			if (m_iRepeat != -1 && --m_iRepeat <= 0)
				done = true;
		} else {
			done = true;
		}

		if (done)
		{
			clear();
		} else {
			m_fNextExecTime = fCurrentTime + m_fBase;
		}
	}
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
	m_bAfterStart =	false;
	m_bBeforeEnd = false;
	m_bInExecute = false;

	m_fNextExecTime = 0.0f;

	m_iParamLen = 0;
	m_pParams = NULL;
}

CTaskMngr::CTask::~CTask()
{
	clear();
}

/*********************** CTaskMngr ***********************/

CTaskMngr::CTaskMngr()
{
	m_pTmr_CurrentTime = NULL;
	m_pTmr_TimeLimit = NULL;
	m_pTmr_TimeLeft = NULL;
}

CTaskMngr::~CTaskMngr()
{
	clear();
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
	TaskListIter iter = m_Tasks.find(CTaskDescriptor(0, NULL, true));
	
	if (iter)
	{
		// found: reuse it
		iter->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
	} else {
		// not found: make a new one
		CTask *pTmp = new CTask;
		
		if (!pTmp)
			return;
		
		pTmp->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
		m_Tasks.put(pTmp);
	}
}

int CTaskMngr::removeTasks(int iId, AMX *pAmx)
{
	CTaskDescriptor descriptor(iId, pAmx);
	TaskListIter iter = m_Tasks.find(descriptor);
	int i = 0;
	
	while (iter)
	{
		iter->clear();
		++i;
		iter = m_Tasks.find(++iter, descriptor);
	}
	
	return i;
}

int CTaskMngr::changeTasks(int iId, AMX *pAmx, float fNewBase)
{
	CTaskDescriptor descriptor(iId, pAmx);
	TaskListIter iter = m_Tasks.find(descriptor);
	int i = 0;
	
	while (iter)
	{
		iter->changeBase(fNewBase);
		iter->resetNextExecTime(*m_pTmr_CurrentTime);
		++i;
		iter = m_Tasks.find(++iter, descriptor);
	}
	
	return i;
}

bool CTaskMngr::taskExists(int iId, AMX *pAmx)
{
	return m_Tasks.find(CTaskDescriptor(iId, pAmx));
}

void CTaskMngr::startFrame()
{
	for (TaskListIter iter = m_Tasks.begin(); iter; ++iter)
	{
		if (iter->isFree())
			continue;
		iter->executeIfRequired(*m_pTmr_CurrentTime, *m_pTmr_TimeLimit, *m_pTmr_TimeLeft);
	}
}

void CTaskMngr::clear()
{
	m_Tasks.clear();
}
