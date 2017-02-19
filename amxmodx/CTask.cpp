// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include <algorithm>
#include <vector>
#include <memory>
#include "CTask.h"


/********************** CForward *********************/

CTaskMngr::CTask::CForward::CForward(CPluginMngr::CPlugin *pPlugin, char * sFunc, int iParamsLen)
{
	auto amx = pPlugin->getAMX();
	if (iParamsLen)
	{
		m_iFunc = registerSPForwardByName(amx, sFunc, FP_ARRAY, FP_CELL, FP_DONE);
	}
	else
	{
		m_iFunc = registerSPForwardByName(amx, sFunc, FP_CELL, FP_DONE);
	}

	if (m_iFunc == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", sFunc, pPlugin->getName());
		return;
	}
}

CTaskMngr::CTask::CForward::~CForward()
{
	if (m_iFunc >= 0)
		unregisterSPForward(m_iFunc);
}

/*********************** CTask ***********************/

CTaskMngr::CTask::CTask(CPluginMngr::CPlugin *pPlugin, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime, char * sFunc)
{
	m_pPlugin = pPlugin;
	auto amx = getAMX();

	m_Forward = std::make_shared<CForward>(pPlugin, sFunc, iParamsLen);

	m_iId = iId;
	m_fBase = fBase;
	m_bInExecute = false;
	m_iRepeat = 0;
	m_bLoop = false;
	m_bKillMe = false;
	m_iParamLen = iParamsLen;

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

	m_bAfterStart = (iFlags & 4) ? true : false;
	m_bBeforeEnd = (iFlags & 8) ? true : false;

	m_fNextExecTime = fCurrentTime + m_fBase;

	while (iParamsLen)
	{
		m_Params.push_back(*pParams);
		pParams++;
		iParamsLen--;
	}

	m_Params.push_back(0);

}

void CTaskMngr::CTask::changeBase(float fNewBase)
{
	m_fBase = fNewBase;
}

void CTaskMngr::CTask::resetNextExecTime(float fCurrentTime)
{
	// If we're here while we're executing we would add m_fBase twice
	if (!m_bInExecute)
		m_fNextExecTime = fCurrentTime + m_fBase;
}

/**
 *@return   1  task should be executed
 *          0  task should not be executed
 *         -1  this or any following task should not be executed
 */
int CTaskMngr::CTask::executionRequiredStatus(float fCurrentTime, float fTimeLimit, float fTimeLeft)
{
	if (m_bAfterStart)
	{
		if (fCurrentTime - fTimeLeft + 1.0f >= m_fBase)
			return 1;
		return 0;
	}
	else if (m_bBeforeEnd)
	{
		if (fTimeLimit != 0.0f && (fTimeLeft + fTimeLimit * 60.0f) - fCurrentTime - 1.0f <= m_fBase)
			return 1;
		return 0;
	}
	else if (m_fNextExecTime <= fCurrentTime)
	{
		return 1;
	}
	return -1;
}

/**
 * Executes task
 * @return   true  task should be removed from tasklist
 *           false  task should be kept
 */
bool CTaskMngr::CTask::execute()
{
	m_bInExecute = true;
	if (m_iParamLen)	// call with parameters
	{
		cell arr = prepareCellArray(m_Params.data(), m_Params.size());
		executeForwards(m_Forward->m_iFunc, arr, m_iId);
	}
	else
	{
		executeForwards(m_Forward->m_iFunc, m_iId);
	}
	m_bInExecute = false;
	
	if (m_bLoop)
	{
		if (m_iRepeat == -1 || --m_iRepeat > 0)
		{
			m_fNextExecTime += m_fBase;
			return false;
		}
	}

	return true;
}

void CTaskMngr::CTask::killMe()
{
	m_bKillMe = true;
	m_fNextExecTime = 0.0f;
}

/*********************** CTaskMngr ***********************/

CTaskMngr::CTaskMngr()
{
	m_pTmr_CurrentTime = NULL;
	m_pTmr_TimeLimit = NULL;
	m_pTmr_TimeLeft = NULL;
	m_bRemoveNeeded = false;
	m_bSortNeeded = false;
}

CTaskMngr::~CTaskMngr()
{
	clear();
}

void CTaskMngr::clear()
{
	m_Tasks.clear();
}

void CTaskMngr::registerTimers(float *pCurrentTime, float *pTimeLimit, float *pTimeLeft)
{
	m_pTmr_CurrentTime = pCurrentTime;
	m_pTmr_TimeLimit = pTimeLimit;
	m_pTmr_TimeLeft = pTimeLeft;
}

int CTaskMngr::registerTask(CPluginMngr::CPlugin *pPlugin, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, char * sFunc)
{
	// Avoid unnecesary copies
	m_Tasks.emplace_back(CTask(pPlugin, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime, sFunc));

	m_bSortNeeded = true;

	auto &task = m_Tasks.back();
	if (task.isFailed())
	{
		task.killMe();
		return 0;
	}
	return 1;
}

int CTaskMngr::removeTasks(int iId, AMX *pAmx)
{
	CTaskDescriptor descriptor(iId, pAmx);

	int i = 0;
	for (auto & task : m_Tasks)
	{
		if (task == descriptor)
		{
			task.killMe();
			i++;
		}
	}

	if (i > 0)
		m_bRemoveNeeded = true;

	return i;
}

int CTaskMngr::changeTasks(int iId, AMX *pAmx, float fNewBase)
{
	CTaskDescriptor descriptor(iId, pAmx);

	int i = 0;
	for (auto & task : m_Tasks)
	{
		if (task == descriptor)
		{
			task.changeBase(fNewBase);
			task.resetNextExecTime(*m_pTmr_CurrentTime);
			i++;
		}
	}
	
	if (i > 0)
		m_bSortNeeded = true;

	return i;
}

bool CTaskMngr::taskExists(int iId, AMX *pAmx)
{
	return std::find(m_Tasks.begin(), m_Tasks.end(), CTaskDescriptor(iId, pAmx)) != m_Tasks.end();
}

void CTaskMngr::startFrame()
{
	int status;
	bool shouldKill;

	if (m_bRemoveNeeded)
	{
		m_Tasks.erase(std::remove_if(m_Tasks.begin(), m_Tasks.end(), [](const CTask &t) { return t.shouldKillMe(); }), std::end(m_Tasks));
		m_bRemoveNeeded = false;
	}

	if (m_bSortNeeded)
	{
		std::sort(m_Tasks.begin(), m_Tasks.end());
		m_bSortNeeded = false;
	}

	// Can't use iterators because pushing new tasks invalidates them
	for (std::vector<CTask>::size_type i = 0; i < m_Tasks.size(); i++)
	{
		auto &task = m_Tasks[i];
		status = task.executionRequiredStatus(*m_pTmr_CurrentTime, *m_pTmr_TimeLimit, *m_pTmr_TimeLeft);

		// Execute task
		if (status > 0)
		{
			shouldKill = task.execute();

			if (shouldKill)
			{
				// Our reference may no longer be valid due to reallocation, so reaccess vector
				m_Tasks[i].killMe(); 
				m_bRemoveNeeded = true;
			}
			else
			{
				m_bSortNeeded = true;
			}
		}
		else if (status == -1) // Following tasks needn't be executed
		{
			break;
		}
	}

}
