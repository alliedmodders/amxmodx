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

class CTaskMngr
{
private:
	/*** class CTask ***/
	class CTask
	{
		// task settings
		
		CPluginMngr::CPlugin *m_pPlugin;
		cell m_iId;
		int m_iFunc;
		int m_iRepeat;
		
		bool m_bInExecute;
		bool m_bLoop;
		bool m_bAfterStart;
		bool m_bBeforeEnd;
		float m_fBase;		// for normal tasks, stores the interval, for the others, stores the amount of time before start / after end
		int m_iParamLen;
		
		cell *m_pParams;
		bool m_bFree;

		// execution
		float m_fNextExecTime;
	public:
		void set(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime);
		void clear();
		bool isFree() const;

		inline CPluginMngr::CPlugin *getPlugin() const { return m_pPlugin; }
		inline AMX *getAMX() const { return m_pPlugin->getAMX(); }
		inline int getTaskId() const { return m_iId; }

		void executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft);	// also removes the task if needed

		void changeBase(float fNewBase);
		void resetNextExecTime(float fCurrentTime);
		inline bool inExecute() const { return m_bInExecute; }

		bool shouldRepeat();

		CTask();
		~CTask();
	};

	class CTaskDescriptor
	{
	public:
		cell m_iId;
		AMX *m_pAmx;
		bool m_bFree;

		CTaskDescriptor(int iId, AMX *pAmx, bool bFree = false)
		{
			m_iId = iId;
			m_pAmx = pAmx;
			m_bFree = bFree;
		}

		friend bool operator == (const CTask &left, const CTaskDescriptor &right)
		{
			if (right.m_bFree)
				return (left.isFree() && !left.inExecute());
			
			return (!left.isFree()) && 
					(right.m_pAmx ? left.getAMX() == right.m_pAmx : true) && 
					(left.getTaskId() == right.m_iId);
		}
	};

	/*** CTaskMngr priv members ***/
	typedef CList<CTask, CTaskDescriptor> TaskList;
	typedef TaskList::iterator TaskListIter;
	
	TaskList m_Tasks;
	
	float *m_pTmr_CurrentTime;
	float *m_pTmr_TimeLimit;
	float *m_pTmr_TimeLeft;
public:
	CTaskMngr();
	~CTaskMngr();

	void registerTimers(float *pCurrentTime, float *pTimeLimit, float *pTimeLeft);	// The timers will always point to the right value
	void registerTask(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, cell iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat);
	
	int removeTasks(int iId, AMX *pAmx);											// remove all tasks that match the id and amx
	int changeTasks(int iId, AMX *pAmx, float fNewBase);							// change all tasks that match the id and amx
	bool taskExists(int iId, AMX *pAmx);
	
	void startFrame();
	void clear();
};

#endif //CTASK_H
