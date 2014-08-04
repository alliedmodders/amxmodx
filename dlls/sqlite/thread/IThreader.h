// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_SOURCEMOD_THREADER_H
#define _INCLUDE_SOURCEMOD_THREADER_H

namespace SourceMod
{
	enum ThreadFlags
	{
		Thread_Default = 0,
		//auto release handle on finish
		//you are not guaranteed the handle for this is valid after
		// calling MakeThread(), so never use it until OnTerminate is called.
		Thread_AutoRelease = 1,
		//Thread is created "suspended", meaning
		// it is inactive until unpaused.
		Thread_CreateSuspended = 2,
	};

	enum ThreadPriority
	{
		ThreadPrio_Minimum = -8,
		ThreadPrio_Low = -3,
		ThreadPrio_Normal = 0,
		ThreadPrio_High = 3,
		ThreadPrio_Maximum = 8,
	};

	enum ThreadState
	{
		Thread_Running = 0,
		Thread_Paused = 1,
		Thread_Done = 2,
	};

	struct ThreadParams
	{
		ThreadParams() : 
			flags(Thread_Default), 
			prio(ThreadPrio_Normal)
		{
		};
		ThreadFlags flags;
		ThreadPriority prio;
	};

	class IThreadCreator;

	/**
	 * Describes a handle to a thread
	 */
	class IThreadHandle
	{
	public:
		virtual ~IThreadHandle() { };
	public:
		/**
		 * Pauses parent thread until this thread completes.
		 */
		virtual bool WaitForThread() =0;
		/**
		 * Destroys the thread handle.
		 * This will not necessarily cancel the thread.
		 */
		virtual void DestroyThis() =0;
		/**
		 * Returns the parent threader.
		 */
		virtual IThreadCreator *Parent() =0;
		/**
		 * Returns the thread states.
		 */
		virtual void GetParams(ThreadParams *ptparams) =0;
		/**
		 * Returns priority
		 */
		virtual ThreadPriority GetPriority() =0;
		/**
		 * Sets thread priority
		 */
		virtual bool SetPriority(ThreadPriority prio) =0;
		/**
		 * Gets thread state
		 */
		virtual ThreadState GetState() =0;
		/**
		 * Attempts to unpause a paused thread.
		 */
		virtual bool Unpause() =0;
	};

	/**
	 * Describes a single unit of execution/context flow
	 */
	class IThread
	{
	public:
		virtual ~IThread() { };
	public:
		//Called when the thread runs
		virtual void RunThread(IThreadHandle *pHandle) =0;
		//Called when the thread terminates.
		//"Cancel" is true if the thread did not finish
		//(this could mean suspended or terminated abruptly)
		virtual void OnTerminate(IThreadHandle *pHandle, bool cancel) =0;
	};


	/**
	 * Describes a thread creator
	 */
	class IThreadCreator
	{
	public:
		virtual ~IThreadCreator() { };
	public:
		//Makes a thread and cleans up the handle for you
		virtual void MakeThread(IThread *pThread) =0;
		//Makes a thread with flag specified
		virtual IThreadHandle *MakeThread(IThread *pThread, ThreadFlags flags) =0;
		//Makes a thread, full options can be specified
		virtual IThreadHandle *MakeThread(IThread *pThread, const ThreadParams *params) =0;
		//Return priority bounds
		virtual void GetPriorityBounds(ThreadPriority &max, ThreadPriority &min) =0;
	};

	/**
	 * Basic Mutex
	 */
	class IMutex
	{
	public:
		virtual ~IMutex() { };
	public:
		/**
		 * Attempts to lock, but returns instantly.
		 */
		virtual bool TryLock() =0;
		/**
 		 * Attempts to lock by waiting for release.
		 */
		virtual void Lock() =0;
		/**
		 * Unlocks mutex.
		 */
		virtual void Unlock() =0;
		/**
		 * Frees the mutex handle.
		 */
		virtual void DestroyThis() =0;
	};


	class IEventSignal
	{
	public:
		virtual ~IEventSignal() { };
	public:
		/**
		 * Waits for the signal.
		 */
		virtual void Wait() =0;
		/** 
		 * Triggers the signal.
		 * Resets the signals after triggering.
		 */
		virtual void Signal() =0;
		/**
		 * Frees the signal handle.
		 */
		virtual void DestroyThis() =0;
	};

	/**
	 * Describes a threading system
	 */
	class IThreader : public IThreadCreator
	{
	public:
		virtual IMutex *MakeMutex() =0;
		virtual void MakeThread(IThread *pThread) =0;
		virtual IThreadHandle *MakeThread(IThread *pThread, ThreadFlags flags) =0;
		virtual IThreadHandle *MakeThread(IThread *pThread, const ThreadParams *params) =0;
		virtual void GetPriorityBounds(ThreadPriority &max, ThreadPriority &min) =0;
		virtual void ThreadSleep(unsigned int ms) =0;
		/**
		 * Creates a non-signalled event.
		 */
		virtual IEventSignal *MakeEventSignal() =0;
	};

	enum WorkerState
	{
		Worker_Invalid = -3,
		Worker_Stopped = -2,
		Worker_Paused = -1,
		Worker_Running,
	};

	/**
	 * This is an extension of the threader that is implemented.
	 * It "simulates" threading in a queue, and processes the queue whenever
	 *  RunFrame is called (leaving it up to the implementation).
	 * Worker may or may not be started upon instantiation.
	 */
	class IWorker : public IThreadCreator
	{
	public:
		virtual unsigned int RunFrame() =0;
		virtual void MakeThread(IThread *pThread) =0;
		virtual IThreadHandle *MakeThread(IThread *pThread, ThreadFlags flags) =0;
		virtual IThreadHandle *MakeThread(IThread *pThread, const ThreadParams *params) =0;
		virtual void GetPriorityBounds(ThreadPriority &max, ThreadPriority &min) =0;
	public:
		//Controls the worker
		virtual bool Pause() =0;
		virtual bool Unpause() =0;
		virtual bool Start() =0;
		//If flush is true, all remaining tasks will be cancelled.
		//Otherwise, it will wait until the tasks have been depleted, then
		// end.
		virtual bool Stop(bool flush_cancel) =0;
		//Flushes out any remaining threads
		virtual unsigned int Flush(bool flush_cancel) =0;
		//returns status and number of threads in queue
		virtual WorkerState GetStatus(unsigned int *numThreads) =0;
	};
};

#endif //_INCLUDE_SOURCEMOD_THREADER_H
