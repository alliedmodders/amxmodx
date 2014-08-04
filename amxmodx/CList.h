// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef CLIST_H
#define CLIST_H

// *****************************************************
// class CList
// *****************************************************

// Linked list
template <typename T, typename F = char* >
class CList
{
private:
	// One list element
	class CElement
	{
		T *m_pObject;						// pointer to the object
		CElement *m_pNext;					// pointer to the next element
		CElement *m_pPrev;					// pointer to the previous element
	public:
		// dereference operator
		T& operator* ()
		{
			return *m_pObject;
		}

		// constructor
		CElement(T *pObj)
		{
			m_pObject = pObj;
			m_pNext = NULL;
			m_pPrev = NULL;
		}

		// destructor
		~CElement()
		{
			delete m_pObject;
			
			if (m_pNext)
				m_pNext->m_pPrev = m_pPrev;
			
			if (m_pPrev)
				m_pPrev->m_pNext = m_pNext;
		}

		// returns object pointer
		T *GetObj()
		{
			return m_pObject;
		}

		// returns next element pointer
		CElement *GetNext()
		{
			return m_pNext;
		}

		// sets next element
		void SetNext(CElement *newNext)
		{
			m_pNext = newNext;
		}

		// returns previous element pointer
		CElement *GetPrev()
		{
			return m_pPrev;
		}

		// sets previous element
		void SetPrev(CElement *newPrev)
		{
			m_pPrev = newPrev;
		}
	};

	// CList<T, F> class
	CElement *m_pHead;					// head of the linked list
	CElement *m_pTail;					// tail of the linked list
public:
	// iterator class
	class iterator
	{
		friend class CList<T, F>;
		
		CList<T, F> *m_pList;			// The list that created this iterator
		CElement *m_CurPos;				// Current position in the list
	public:
		iterator()
		{
			m_pList = NULL;
			m_CurPos = NULL;
		}

		// constructor based on list, element
		iterator(CList<T, F> *pList, CElement *startPos)
		{
			m_pList = pList;
			m_CurPos = startPos;
		}

		// constructor based on other iterator
		iterator(const iterator &other)
		{
			m_pList = other.m_pList;
			m_CurPos = other.m_CurPos;
		}

		// dereference operator
		T & operator* () const
		{
			return *m_CurPos->GetObj();
		}

		T * operator-> () const
		{
			return m_CurPos->GetObj();
		}

		// validity check operator
		inline operator bool () const
		{
			return m_pList != NULL && m_CurPos != NULL && m_CurPos->GetObj() != NULL;
		}

		// pre increment operator
		inline iterator& operator ++ ()
		{
			m_CurPos = m_CurPos->GetNext();
			return *this;
		}

		// post increment operator
		inline iterator operator++(int)
		{
			iterator tmp(*this);
			m_CurPos = m_CurPos->next;
			
			return tmp;
		}

		// returns iterator that points to next element
		iterator GetNext()
		{
			iterator tmp(*this);
			return ++tmp;
		}

		iterator remove()
		{
			return m_pList->remove(*this);
		}
		
		iterator put(T *obj)
		{
			return m_pList->put(obj, *this);
		}
	};

	CList<T, F>()
	{
		m_pHead = NULL;
		m_pTail = NULL;
	}
	
	~CList<T, F>()
	{
		clear();
	}

	// removes the object referenced by where
	// sets where to the next object
	// returns an iterator pointing to the next object
	iterator remove(iterator &where)
	{
		iterator tmp(where.GetNext());
		
		if (where.m_CurPos == m_pHead)
			m_pHead = where.m_CurPos->GetNext();
		
		if (where.m_CurPos == m_pTail)
			m_pTail = where.m_CurPos->GetPrev();
		
		delete where.m_CurPos;
		where = tmp;
		
		return tmp;
	}

	// puts an element to the end of the list
	// returns an iterator pointing to it
	iterator put_back(T *pObj)
	{
		CElement *pTmp = new CElement(pObj);
		
		if (!m_pHead)
		{
			m_pHead = pTmp;
			m_pTail = pTmp;
		} else {
			pTmp->SetNext(NULL);
			pTmp->SetPrev(m_pTail);
			m_pTail->SetNext(pTmp);
			m_pTail = pTmp;
		}
		
		return iterator(this, pTmp);
	}

	iterator put_front(T *pObj)
	{
		CElement *pTmp = new CElement(pObj);
		
		if (!m_pHead)
		{
			m_pHead = pTmp;
			m_pTail = pTmp;
		} else {
			pTmp->SetNext(m_pHead);
			pTmp->SetPrev(NULL);
			m_pHead->SetPrev(pTmp);
			m_pHead = pTmp;
		}
		
		return iterator(this, pTmp);
	}

	// alias for put_back
	iterator put(T *pObj)
	{
		return put_back(pObj);
	}

	// puts an element after where
	// alters where to point to the new element
	// returns an iterator pointing to the new element
	iterator put(T *pObj, iterator &where)
	{
		CElement *pTmp = new CElement(pObj);
		
		if (where.m_CurPos->GetNext())
			where.m_CurPos->GetNext()->SetPrev(pTmp);
		else		// where = tail
			m_pTail = pTmp;

		pTmp->SetPrev(where.m_CurPos);
		pTmp->SetNext(where.m_CurPos->GetNext());

		where.m_CurPos->SetNext(pTmp);
		
		return ++where;
	}

	iterator begin()
	{
		return iterator(this, m_pHead);
	}

	void clear()
	{
		iterator iter = begin();
		while (iter) iter.remove();
	}

	iterator find(iterator startOn, const F &desc)
	{
		iterator iter = startOn;
		while (iter)
		{
			if (*iter == desc)
				break;
			++iter;
		}
		
		return iter;
	}

	iterator find(const F &desc)
	{
		return find(begin(), desc);
	}

	int size()
	{
		iterator iter = begin();
		int i = 0;
		
		while (iter)
		{
			++i;
			++iter;
		}
		
		return i;
	}
};

#endif //CLIST_H
