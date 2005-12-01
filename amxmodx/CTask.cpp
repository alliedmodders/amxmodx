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
#if defined WIN32 && !defined __GNUC__
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
#else
		memcpy(m_pParams, pParams, sizeof(cell) * iParamsLen);
#endif
		m_pParams[iParamsLen] = 0;
	} else {
		m_iParamLen = 0;
	}
}

void CTaskMngr::CTask::clear()
{
	if (m_iFunc >= 0)
	{
		unregisterSPForward(m_iFunc);
		m_iFunc = -1;
	}

	m_iId = 0;
	m_fNextExecTime = 0.0f;
}

void CTaskMngr::CTask::changeBase(float fNewBase)
{
	m_fBase = fNewBase;
}

void CTaskMngr::CTask::resetNextExecTime(float fCurrentTime)
{
	m_fNextExecTime = fCurrentTime + m_fBase;
}

int CTaskMngr::CTask::executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft)
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

	return (execute ? (done ? Task_Done : Task_Rel) : Task_Nothing);
}

CTaskMngr::CTask::CTask()
{
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
	if (g_FreeTasks)
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
	CTask *pTmp = NULL;
	if (!g_FreeTasks->empty())
	{
		// found: reuse it
		pTmp = g_FreeTasks->front();
		g_FreeTasks->pop();
		pTmp->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
	} else {
		// not found: make a new one
		pTmp = new CTask;
		pTmp->set(pPlugin, iFunc, iFlags, iId, fBase, iParamsLen, pParams, iRepeat, *m_pTmr_CurrentTime);
	}

	insertTask(pTmp);
}

float CTaskMngr::CTask::nextThink()
{
	float _next = 0.0f;
	/*switch (type)
	{
	case 0:
		{*/
			_next = m_fNextExecTime - g_tasksMngr.CurrentTime();
			if (_next < 0.0f)
				_next = 0.0f;
			/*break;
		}
	case 1:
		{
			_next = m_fBase - (g_tasksMngr.CurrentTime() - g_tasksMngr.TimeLeft() + 1.0f);
			if (_next < 0.0f)
				_next = 0.0f;
			break;
		}
	case 2:
		{
			if (g_tasksMngr.TimeLimit() == 0.0f)
			{
				_next = INFINITE;
			} else {
				_next = ((g_tasksMngr.TimeLeft() + g_tasksMngr.TimeLimit() * 60.0f) - g_tasksMngr.CurrentTime() - 1.0f) - m_fBase;
				if (_next < 0.0f)
					_next = 0.0f;
			}
			break;
		}
	}*/
	return _next;
}

int CTaskMngr::removeTasks(int iId, AMX *pAmx)
{
	CTask *pTask = first;
	CTask *pTemp;
	int num = 0;
	while (pTask)
	{
		pTemp = pTask->next;
		if ((!pAmx || (pTask->m_pPlugin->getAMX() == pAmx))
			&& (pTask->m_iId == iId))
		{
			removeTask(pTask);
			pTask->clear();
			delete pTask;
			num++;
		}
		pTask = pTemp;
	}

	return num;
}

int CTaskMngr::changeTasks(int iId, AMX *pAmx, float fNewBase)
{
	CTask *pTask = first;
	int num = 0;
	while (pTask)
	{
		if ((!pAmx || (pTask->m_pPlugin->getAMX() == pAmx))
			&& (pTask->m_iId == iId))
		{
			pTask->changeBase(fNewBase);
			pTask->resetNextExecTime(*m_pTmr_CurrentTime);
			num++;
		}
		pTask = pTask->next;
	}

	return num;
}

bool CTaskMngr::taskExists(int iId, AMX *pAmx)
{
	CTask *pTask = first;
	while (pTask)
	{
		if ((!pAmx || (pTask->m_pPlugin->getAMX() == pAmx))
			&& (pTask->m_iId == iId))
		{
			return true;
		}
		pTask = pTask->next;
	}

	return false;
}

static int run_tasks = 0;

void CTaskMngr::startFrame()
{
	CTask *pTask = first;
	CTask *pNext;
	float time_cur = *m_pTmr_CurrentTime;
	float time_left = *m_pTmr_TimeLeft;
	float time_limit = *m_pTmr_TimeLimit;
	int val;
	while (pTask)
	{
		pNext = pTask->next;
		if ( (val=pTask->executeIfRequired(time_cur, time_limit, time_left)) == Task_Nothing )
		{
			//we're DONE
			break;
		} else {
			run_tasks++;
			removeTask(pTask);
			if (val == Task_Rel)
			{
				//relocate task
				insertTask(pTask);
			} else {
				//do this for now to catch list/tree errors
				delete pTask;
			}
		}
		pTask = pNext;
	}
}

static int removed_tasks = 0;

void CTaskMngr::removeTask(CTask *pTask)
{
	removed_tasks++;
	//first, remove us from the linked list.
	if (pTask->prev)
		pTask->prev->next = pTask->next;
	if (pTask->next)
		pTask->next->prev = pTask->prev;
	if (pTask == first)
		first = pTask->next;

	//case 1: no right child
	if (pTask->child[AVL_RIGHT] == NULL)
	{
		if (pTask->parent)
		{
			//link our parent's child to our child, eliminating us
			if (pTask->parent->child[AVL_LEFT] == pTask)
				pTask->parent->child[AVL_LEFT] = pTask->child[AVL_LEFT];
			else
				pTask->parent->child[AVL_RIGHT] = pTask->child[AVL_LEFT];
		} else {
			//the only node with no parent is root
			root = pTask->child[AVL_LEFT];
		}
		//if we have a left child, correct its parent
		if (pTask->child[AVL_LEFT])
			pTask->child[AVL_LEFT]->parent = pTask->parent;
	} else {
		CTask *pSwap = NULL;
		if (pTask->child[AVL_LEFT] == NULL)
		{
			//case 2: we have no left child, it's safe to just rebind
			if (pTask->parent)
			{
				if (pTask->parent->child[AVL_LEFT] == pTask)
					pTask->parent->child[AVL_LEFT] = pTask->child[AVL_RIGHT];
				else
					pTask->parent->child[AVL_RIGHT] = pTask->child[AVL_RIGHT];
			} else {
				//the only node with no parent is root
				root = pTask->child[AVL_RIGHT];
			}
			//if we have a left child, correct its parent
			//we have a right channel because of the case we're in.
			pTask->child[AVL_RIGHT]->parent = pTask->parent;
		} else {
			//case 3: we have a right child, but it has no left child.
			if (pTask->child[AVL_RIGHT]->child[AVL_LEFT] == NULL)
			{
				pSwap = pTask->child[AVL_RIGHT];
				if (pTask->parent)
				{
					//link our parent's child to our child, eliminating us
					if (pTask->parent->child[AVL_LEFT] == pTask)
						pTask->parent->child[AVL_LEFT] = pSwap;
					else
						pTask->parent->child[AVL_RIGHT] = pSwap;
				} else {
					root = pSwap;
				}
				//set our child's parent to our parent
				pSwap->parent = pTask->parent;
				//if we have a left child, link it up to its new parent
				if (pTask->child[AVL_LEFT])
					pTask->child[AVL_LEFT]->parent = pSwap;
				//Set our child's left to whatever our left is
				pSwap->child[AVL_LEFT] = pTask->child[AVL_LEFT];
			} else {
				//case 4: we have a right child with at least a left child.
				//climb down the list to find the BIGGEST value that is
				// SMALLER than our node.
				pSwap = pTask->child[AVL_LEFT];
				while (pSwap->child[AVL_RIGHT])
					pSwap = pSwap->child[AVL_RIGHT];
				if (pTask->parent)
				{
					if (pTask->parent->child[AVL_LEFT] == pTask)
						pTask->parent->child[AVL_LEFT] = pSwap;
					else
						pTask->parent->child[AVL_RIGHT] = pSwap;
				} else {
					root = pSwap;
				}
				//the left child is guaranteed to not have another left child.
				//for this reason we can perform a swap like above ones.
				//however, there are more precautions.
				//unlink our swapped node.  it's gone from that position.
				if (pSwap->parent)
					pSwap->parent->child[AVL_RIGHT] = NULL;
				//link swap to new parent
				pSwap->parent = pTask->parent;
				//set our children to have the swap as the parent
				pTask->child[AVL_RIGHT]->parent = pSwap;
				if (pTask->child[AVL_LEFT])
					pTask->child[AVL_LEFT]->parent = pSwap;
				//link our children in
				pSwap->child[AVL_LEFT] = pTask->child[AVL_LEFT];
				pSwap->child[AVL_RIGHT] = pTask->child[AVL_RIGHT];
			}
		}
	}

	//pTask->clear();
	//g_FreeTasks->push(pTask);
}

static int inserted_tasks = 0;

void CTaskMngr::insertTask(CTask *pTask, float time, CTask *pRoot)
{
	inserted_tasks++;
	int num_ran = 0;
	while (true)
	{
		num_ran++;
		float parent = pRoot->nextThink();
		if (time <= parent)
		{
			if (pRoot->child[AVL_LEFT])
			{
				pRoot = pRoot->child[AVL_LEFT];
				continue;
			} else {
				pTask->balance = 0;
				pTask->dir = AVL_LEFT;
				pTask->child[AVL_LEFT] = NULL;
				pTask->child[AVL_RIGHT] = NULL;
				pTask->parent = pRoot;
				pRoot->child[AVL_LEFT] = pTask;
				//insert us into the linked list.
				//we want to be between pRoot and pRoot->prev
				pTask->prev = pRoot->prev;		//set our previous node
				pTask->next = pRoot;			//set our next node
				if (pRoot->prev)
				{
					pRoot->prev->next = pTask;	//link previous node forward to us
					pRoot->prev = pTask;			//link next node back to us
				} else {
					assert(pRoot == first);
					pRoot->prev = pTask;
					first = pTask;
				}
				break;
			}
		} else if (time > parent) {
			if (pRoot->child[AVL_RIGHT])
			{
				pRoot = pRoot->child[AVL_RIGHT];
				continue;
			} else {
				pTask->balance = 0;
				pTask->dir = AVL_RIGHT;
				pTask->child[AVL_LEFT] = NULL;
				pTask->child[AVL_RIGHT] = NULL;
				pTask->parent = pRoot;
				pRoot->child[AVL_RIGHT] = pTask;
				//insert us into the linked list.
				//we want to be between pRoot and pRoot->next
				pTask->next = pRoot->next;		//set our next node
				pTask->prev = pRoot;			//set our previous node
				if (pRoot->next)
					pRoot->next->prev = pTask;	//link next node back to us
				pRoot->next = pTask;			//link previous node forward to us
				break;
			}
		}
	}
	//:TODO: rebalance the tree!
}

void CTaskMngr::insertTask(CTask *pTask)
{
	if (root == NULL)
	{
		pTask->parent = NULL;
		pTask->child[AVL_LEFT] = NULL;
		pTask->child[AVL_RIGHT] = NULL;
		pTask->next = NULL;
		pTask->prev = NULL;
		pTask->balance = 0;
		pTask->dir = AVL_PARENT;
		root = pTask;
		first = pTask;
	} else {
		float num = pTask->nextThink();
		insertTask(pTask, num, root);
	}
}

void CTaskMngr::clear()
{
	if (g_FreeTasks)
	{
		while (!g_FreeTasks->empty())
		{
			delete g_FreeTasks->front();
			g_FreeTasks->pop();
		}
	}

	//:TODO: Free the linked list
}
