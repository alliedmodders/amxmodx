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

#ifndef CTASK_H
#define CTASK_H

#define AVL_LEFT	0
#define	AVL_RIGHT	1
#define	AVL_PARENT	-1

#include "sh_list.h"

class CTaskMngr
{
public:
	enum
	{
		Task_Nothing = 0,
		Task_Done,
		Task_Rel
	};
	/*** class CTask ***/
	class CTask
	{
		friend class CTaskMngr;
		//plugin
		CPluginMngr::CPlugin *m_pPlugin;
		//task ID
		cell m_iId;
		//registered forward
		int m_iFunc;
		//number of times to repeat
		int m_iRepeat;
		//type of task (a|b, c, d)
		int type;
		// for normal tasks, stores the interval, for the others, stores the amount of time before start / after end
		float m_fBase;
		//Number of parameters
		int m_iParamLen;
		//Parameter array
		cell *m_pParams;
		//Size of parameter array
		cell m_ParamSize;
		//next execution 
		float m_fNextExecTime;
		//will we repeat?
		bool m_bLoop;
	private:
		//Tasks are both a binary tree and a doubly-linked list...
		//The path of execution is stored in the linked list.
		//The search tree is for fast insertion.
		CTask *next;
		CTask *prev;
		CTask *child[2];
		CTask *parent;
		//balance factor for AVL trees
		char balance;
		int dir;
	public:
		void set(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime);
		void clear();
		bool isFree() const;
		CPluginMngr::CPlugin *getPlugin() const;
		int getTaskId() const;
		int executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft);
		void changeBase(float fNewBase);
		void resetNextExecTime(float fCurrentTime);
		bool shouldRepeat();
		float nextThink();
		CTask();
		~CTask();
	};

	/*** CTaskMngr priv members ***/
	CTask *root;
	CTask *first;
	float *m_pTmr_CurrentTime;
	float *m_pTmr_TimeLimit;
	float *m_pTmr_TimeLeft;
public:
	CTaskMngr();
	~CTaskMngr();

	inline float CurrentTime() { return *m_pTmr_CurrentTime; }
	inline float TimeLimit() { return *m_pTmr_TimeLimit; }
	inline float TimeLeft() { return *m_pTmr_TimeLeft; }
	void registerTimers(float *pCurrentTime, float *pTimeLimit, float *pTimeLeft);	// The timers will always point to the right value
	void registerTask(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat);
	
	void insertTask(CTask *pTask);
	void removeTask(CTask *pTask);
	int removeTasks(int iId, AMX *pAmx);											// remove all tasks that match the id and amx
	int changeTasks(int iId, AMX *pAmx, float fNewBase);							// change all tasks that match the id and amx
	bool taskExists(int iId, AMX *pAmx);
	
	void startFrame();
	void clear();
private:
	void insertTask(CTask *pTask, float time, CTask *pRoot);
};

#endif //CTASK_H
