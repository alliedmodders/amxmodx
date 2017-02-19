// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef CTASK_H
#define CTASK_H

#include <vector>
#include <memory>

class CTaskMngr
{
private:
	/*** class CTask ***/
	class CTask
	{
		struct CForward
		{
			int m_iFunc;
			CForward(CPluginMngr::CPlugin *pPlugin, char * sFunc, int iParamsLen);
			~CForward();
		};

		CPluginMngr::CPlugin *m_pPlugin;
		std::vector<cell> m_Params;
		std::shared_ptr<CForward> m_Forward;
		cell m_iId;

		bool m_bKillMe;
		bool m_bInExecute;
		bool m_bLoop;
		bool m_bAfterStart;
		bool m_bBeforeEnd;

		int m_iRepeat;
		int m_iParamLen;

		float m_fNextExecTime;
		float m_fBase; // for normal tasks, stores the interval, for the others, stores the amount of time before start / after end

	public:

		CTask(CPluginMngr::CPlugin *pPlugin, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime, char * sFunc);

		inline CPluginMngr::CPlugin *getPlugin() const { return m_pPlugin; }
		inline AMX *getAMX() const { return m_pPlugin->getAMX(); }
		inline int getTaskId() const { return m_iId; }
		inline bool shouldKillMe() const { return m_bKillMe; }
		inline bool inExecute() const { return m_bInExecute; }
		inline bool isFailed() const { return m_Forward->m_iFunc == -1; }

		void killMe();
		void changeBase(float fNewBase);
		void resetNextExecTime(float fCurrentTime);

		bool execute();

		int executionRequiredStatus(float fCurrentTime, float fTimeLimit, float fTimeLeft);

		friend bool operator < (const CTask &left, const CTask &right)
		{
			if (left.m_bAfterStart || left.m_bAfterStart)
				return true;

			return left.m_fNextExecTime < right.m_fNextExecTime;
		}
	};

	class CTaskDescriptor
	{
	public:
		cell m_iId;
		AMX *m_pAmx;

		CTaskDescriptor(int iId, AMX *pAmx)
		{
			m_iId = iId;
			m_pAmx = pAmx;
		}

		friend bool operator == (const CTask &left, const CTaskDescriptor &right)
		{
			// Dead tasks are invalid and may not be compared
			if (left.shouldKillMe())
				return false;

			if (right.m_pAmx == 0 || right.m_pAmx == left.getAMX())
			{
				if (right.m_iId == left.getTaskId())
				{
					return true;
				}
			}
			return false;
		}
	};

	/*** CTaskMngr priv members ***/
	std::vector<CTask> m_Tasks;

	bool m_bSortNeeded;
	bool m_bRemoveNeeded;
	
	float *m_pTmr_CurrentTime;
	float *m_pTmr_TimeLimit;
	float *m_pTmr_TimeLeft;

public:

	CTaskMngr();
	~CTaskMngr();

	void CTaskMngr::clear();
	void startFrame();
	void registerTimers(float *pCurrentTime, float *pTimeLimit, float *pTimeLeft);	// The timers will always point to the right value

	bool taskExists(int iId, AMX *pAmx);

	int registerTask(CPluginMngr::CPlugin *pPlugin, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, char * sFunc);
	int removeTasks(int iId, AMX *pAmx);											// remove all tasks that match the id and amx
	int changeTasks(int iId, AMX *pAmx, float fNewBase);							// change all tasks that match the id and amx
	
};

#endif //CTASK_H
