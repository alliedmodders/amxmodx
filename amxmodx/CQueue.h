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
		return ((mSize==0)?true:false);
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

