// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_SOURCEMOD_THREADWORKER_H
#define _INCLUDE_SOURCEMOD_THREADWORKER_H

#include "BaseWorker.h"

#define DEFAULT_THINK_TIME_MS	25

class ThreadWorker : public BaseWorker, public IThread
{
public:
	ThreadWorker();
	ThreadWorker(IThreader *pThreader, unsigned int thinktime=DEFAULT_THINK_TIME_MS);
	virtual ~ThreadWorker();
public:	//IThread
	virtual void OnTerminate(IThreadHandle *pHandle, bool cancel);
	virtual void RunThread(IThreadHandle *pHandle);
public:	//IWorker
	//Controls the worker
	virtual bool Pause();
	virtual bool Unpause();
	virtual bool Start();
	virtual bool Stop(bool flush_cancel);
	//returns status and number of threads in queue
	virtual WorkerState GetStatus(unsigned int *numThreads);
public:	//BaseWorker
	virtual void AddThreadToQueue(SWThreadHandle *pHandle);
	virtual SWThreadHandle *PopThreadFromQueue();
protected:
	IThreader *m_Threader;
	IMutex *m_QueueLock;
	IMutex *m_StateLock;
	IEventSignal *m_PauseSignal;
	IEventSignal *m_AddSignal;
	IThreadHandle *me;
	unsigned int m_think_time;
	volatile bool m_Waiting;
	volatile bool m_FlushType;
};

#endif //_INCLUDE_SOURCEMOD_THREADWORKER_H
