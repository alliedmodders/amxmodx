// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_SOURCEMOD_BASEWORKER_H
#define _INCLUDE_SOURCEMOD_BASEWORKER_H

#include <sh_list.h>
#include "ThreadSupport.h"

#define SM_DEFAULT_THREADS_PER_FRAME	1

class BaseWorker;

//SW = Simple Wrapper
class SWThreadHandle : public IThreadHandle
{
	friend class BaseWorker;
public:
	SWThreadHandle(IThreadCreator *parent, const ThreadParams *p, IThread *thread);
	IThread *GetThread();
public:
	//NOTE: We don't support this by default.
	//It's specific usage that'd require many mutexes
	virtual bool WaitForThread();
public:
	virtual void DestroyThis();
	virtual IThreadCreator *Parent();
	virtual void GetParams(ThreadParams *ptparams);
public:
	//Priorities not supported by default.
	virtual ThreadPriority GetPriority();
	virtual bool SetPriority(ThreadPriority prio);
public:
	virtual ThreadState GetState();
	virtual bool Unpause();
private:
	ThreadState m_state;
	ThreadParams m_params;
	IThreadCreator *m_parent;
	IThread *pThread;
};

class BaseWorker : public IWorker
{
public:
	BaseWorker();
	virtual ~BaseWorker();
public:	//IWorker
	virtual unsigned int RunFrame();
	//Controls the worker
	virtual bool Pause();
	virtual bool Unpause();
	virtual bool Start();
	virtual bool Stop(bool flush_cancel);
	//Flushes out any remaining threads
	virtual unsigned int Flush(bool flush_cancel);
	//returns status and number of threads in queue
	virtual WorkerState GetStatus(unsigned int *numThreads);
public:	//IThreadCreator
	virtual void MakeThread(IThread *pThread);
	virtual IThreadHandle *MakeThread(IThread *pThread, ThreadFlags flags);
	virtual IThreadHandle *MakeThread(IThread *pThread, const ThreadParams *params);
	virtual void GetPriorityBounds(ThreadPriority &max, ThreadPriority &min);
public:	//BaseWorker
	virtual void AddThreadToQueue(SWThreadHandle *pHandle);
	virtual SWThreadHandle *PopThreadFromQueue();
	virtual void SetMaxThreadsPerFrame(unsigned int threads);
	virtual unsigned int GetMaxThreadsPerFrame();
protected:
	List<SWThreadHandle *> m_ThreadQueue;
	unsigned int m_perFrame;
	volatile WorkerState m_state;
};

#endif //_INCLUDE_SOURCEMOD_BASEWORKER_H
