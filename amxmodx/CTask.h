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

class CTaskMngr
{
private:
	/*** class CTask ***/
	class CTask
	{
		// task settings
		CPluginMngr::CPlugin *m_pPlugin;
		int m_iId;
		int m_iFunc;
		int m_iRepeat;
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
		 void set(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, int iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat, float fCurrentTime);
		 void clear();
		 bool isFree() const;

		 CPluginMngr::CPlugin *getPlugin() const;
		 int getTaskId() const;

		 void executeIfRequired(float fCurrentTime, float fTimeLimit, float fTimeLeft);	// also removes the task if needed

		 void changeBase(float fNewBase);
		 void resetNextExecTime(float fCurrentTime);

		 bool shouldRepeat();

		CTask();
		~CTask();
	};

	class CTaskDescriptor
	{
	public:
		int m_iId;
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
				return left.isFree();
			
			return !left.isFree() &&
				(right.m_pAmx ? left.getPlugin()->getAMX() == right.m_pAmx : true) &&
				left.getTaskId() == right.m_iId;
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
	void registerTask(CPluginMngr::CPlugin *pPlugin, int iFunc, int iFlags, int iId, float fBase, int iParamsLen, const cell *pParams, int iRepeat);
	int removeTasks(int iId, AMX *pAmx);	// remove all tasks that match the id and amx
	int changeTasks(int iId, AMX *pAmx, float fNewBase);	// change all tasks that match the id and amx
	bool taskExists(int iId, AMX *pAmx);
	void startFrame();
	void clear();
};

#endif


