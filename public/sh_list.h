/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_SMM_LIST_H
#define _INCLUDE_SMM_LIST_H

// MSVC8 fix for offsetof macro redefition warnings
#ifdef _MSC_VER 
	#if _MSC_VER >= 1400
		#undef offsetof
	#endif
#endif

#include <new>
#include <stdlib.h>

//namespace SourceHook
//{

	//This class is from CSDM for AMX Mod X
	/*
		A circular, doubly-linked list with one sentinel node

		Empty:
			m_Head = sentinel
			m_Head->next = m_Head;
			m_Head->prev = m_Head;
		One element:
			m_Head = sentinel
			m_Head->next = node1
			m_Head->prev = node1
			node1->next = m_Head
			node1->prev = m_Head
		Two elements:
			m_Head = sentinel
			m_Head->next = node1
			m_Head->prev = node2
			node1->next = node2
			node1->prev = m_Head
			node2->next = m_Head
			node2->prev = node1
	*/
	template <class T>
	class List
	{
	public:
		class iterator;
		friend class iterator;
		class ListNode
		{
		public:
			ListNode(const T & o) : obj(o) { };
			ListNode() { };
			T obj;
			ListNode *next;
			ListNode *prev;
		};
	private:
		// Initializes the sentinel node.
		// BAIL used malloc instead of new in order to bypass the need for a constructor.
		ListNode *_Initialize()
		{
			ListNode *n = (ListNode *)malloc(sizeof(ListNode));
			n->next = n;
			n->prev = n;
			return n;
		}
	public:
		List() : m_Head(_Initialize()), m_Size(0)
		{
		}
		List(const List &src) : m_Head(_Initialize()), m_Size(0)
		{
			iterator iter;
			for (iter=src.begin(); iter!=src.end(); iter++)
				push_back( (*iter) );
		}
		~List()
		{
			clear();

			// Don't forget to free the sentinel
			if (m_Head)
			{
				free(m_Head);
				m_Head = NULL;
			}
		}
		void push_back(const T &obj)
		{
			ListNode *node = new ListNode(obj);

			node->prev = m_Head->prev;
			node->next = m_Head;
			m_Head->prev->next = node;
			m_Head->prev = node;

			m_Size++;
		}
		size_t size()
		{
			return m_Size;
		}

		void clear()
		{
			ListNode *node = m_Head->next;
			ListNode *temp;
			m_Head->next = m_Head;
			m_Head->prev = m_Head;

			// Iterate through the nodes until we find g_Head (the sentinel) again
			while (node != m_Head)
			{
				temp = node->next;
				delete node;
				node = temp;
			}
			m_Size = 0;
		}
		bool empty()
		{
			return (m_Size == 0);
		}
		T & back()
		{
			return m_Head->prev->obj;
		}
	private:
		ListNode *m_Head;
		size_t m_Size;
	public:
		class iterator
		{
		friend class List;
		public:
			iterator()
			{
				m_This = NULL;
			}
			iterator(const List &src)
			{
				m_This = src.m_Head;
			}
			iterator(ListNode *n) : m_This(n)
			{
			}
			iterator(const iterator &where)
			{
				m_This = where.m_This;
			}
			//pre decrement
			iterator & operator--()
			{
				if (m_This)
					m_This = m_This->prev;
				return *this;
			}
			//post decrement
			iterator operator--(int)
			{
				iterator old(*this);
				if (m_This)
					m_This = m_This->prev;
				return old;
			}	
			
			//pre increment
			iterator & operator++()
			{
				if (m_This)
					m_This = m_This->next;
				return *this;
			}
			//post increment
			iterator operator++(int)
			{
				iterator old(*this);
				if (m_This)
					m_This = m_This->next;
				return old;
			}
			
			const T & operator * () const
			{
				return m_This->obj;
			}
			T & operator * ()
			{
				return m_This->obj;
			}
			
			T * operator -> ()
			{
				return &(m_This->obj);
			}
			const T * operator -> () const
			{
				return &(m_This->obj);
			}
			
			bool operator != (const iterator &where) const
			{
				return (m_This != where.m_This);
			}
			bool operator ==(const iterator &where) const
			{
				return (m_This == where.m_This);
			}
		private:
			ListNode *m_This;
		};
	public:
		iterator begin() const
		{
			return iterator(m_Head->next);
		}
		iterator end() const
		{
			return iterator(m_Head);
		}
		iterator erase(iterator &where)
		{
			ListNode *pNode = where.m_This;
			iterator iter(where);
			iter++;


			// Works for all cases: empty list, erasing first element, erasing tail, erasing in the middle...
			pNode->prev->next = pNode->next;
			pNode->next->prev = pNode->prev;

			delete pNode;
			m_Size--;

			return iter;
		}

		iterator insert(iterator where, const T &obj)
		{
			// Insert obj right before where

			ListNode *node = new ListNode(obj);
			ListNode *pWhereNode = where.m_This;
			
			pWhereNode->prev->next = node;
			node->prev = pWhereNode->prev;
			pWhereNode->prev = node;
			node->next = pWhereNode;

			m_Size++;

			return iterator(node);
		}

	public:
		void remove(const T & obj)
		{
			iterator b;
			for (b=begin(); b!=end(); b++)
			{
				if ( (*b) == obj )
				{
					erase( b );
					break;
				}
			}
		}
		template <typename U>
		iterator find(const U & equ)
		{
			iterator iter;
			for (iter=begin(); iter!=end(); iter++)
			{
				if ( (*iter) == equ )
					return iter;
			}
			return end();
		}
		List & operator =(const List &src)
		{
			clear();
			iterator iter;
			for (iter=src.begin(); iter!=src.end(); iter++)
				push_back( (*iter) );
			return *this;
		}
	};
//};	//NAMESPACE

#endif //_INCLUDE_CSDM_LIST_H
