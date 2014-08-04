// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//by David "BAILOPAN" Anderson
#ifndef _INCLUDE_CQUEUE_H
#define _INCLUDE_CQUEUE_H

template <class T>
class CQueue
{
public:
	class CQueueItem
	{
	public:
		CQueueItem(const T &i, CQueueItem *n)
		{
			item = i;
			next = n;
		}
		
		CQueueItem *GetNext()
		{
			return next;
		}
		
		T & GetItem()
		{
			return item;
		}
		
		void SetNext(CQueueItem *n)
		{
			next = n;
		}
	private:
		T item;
		CQueueItem *next;
	};
public:
	CQueue()
	{
		mSize = 0;
		mFirst = NULL;
		mLast = NULL;
	}

	bool empty()
	{
		return ((mSize == 0) ? true : false);
	}

	void push(const T &v)
	{
		CQueueItem *p = new CQueueItem(v, NULL);
		if (empty())
		{
			mFirst = p;
		} else {
			mLast->SetNext(p);
		}
		mLast = p;
		mSize++;
	}

	void pop()
	{
		if (mFirst == mLast)
		{
			delete mFirst;
			mFirst = NULL;
			mLast = NULL;
		} else {
			CQueueItem *p = mFirst->GetNext();
			delete mFirst;
			mFirst = p;
		}
		mSize--;
	}

	T & front()
	{
		return mFirst->GetItem();
	}

	T & back()
	{
		return mLast->GetItem();
	}

	unsigned int size()
	{
		return mSize;
	}
private:
	CQueueItem *mFirst;
	CQueueItem *mLast;
	
	unsigned int mSize;
};

#endif //_INCLUDE_CQUEUE_H
