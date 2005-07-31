#ifndef _INCLUDE_NHASH_H
#define _INCLUDE_NHASH_H

#include <time.h>
#include "compat.h"

/**
 * This is a primitive, typical hash class.
 * Design goals were: modular, easy to use, compact
 * The table size is fixed by a constant, 2K gives about ~8-16K in immediate memory usage.
 * Each entry in the table uses about 20-28 bytes, depending on the data being stored.
 * In theory we could optimize this further by storing a linked list of the hash items.
 *  (this would sacrifice ~8 bytes per node!)
 * --- by David "BAILOPAN" Anderson
 */

#define	TABLE_SIZE		2048

template <class K>
int HashFunction(const K & k);

template <class K>
bool Compare(const K & k1, const K & k2);

template <class K, class V>
class NHash
{
private:
	struct hashnode
	{
		K key;
		V val;
		time_t stamp;
		hashnode *next;
		hashnode *prev;
	};
	struct bucket
	{
		hashnode *head;
		hashnode *tail;
	};
public:
	NHash()
	{
		memset(&m_Buckets, 0, sizeof(m_Buckets));
		m_Size = 0;
	}
	~NHash()
	{
		Clear();
	}
	void Clear()
	{
		hashnode *n, *t;
		for (size_t i=0; i<TABLE_SIZE; i++)
		{
			n = m_Buckets[i].head;
			while (n)
			{
				t = n->next;
				delete n;
				n = t;
			}
			m_Buckets[i].head = NULL;
			m_Buckets[i].tail = NULL;
		}
	}
	void Insert(const K & key, const V & val)
	{
		Insert(key, val, time(NULL));
	}
	void Insert(const K & key, const V & val, time_t stamp)
	{
		bucket *b;
		hashnode *n;
		if (!_Find(key, &b, &n))
		{
			n = new hashnode;
			n->key = key;
			_Insert(b, n);
		}
		n->val = val;
		n->stamp = stamp;
	}
	bool Exists(const K & k)
	{
		uint16_t h = HashFunction(k);
		if (h >= TABLE_SIZE)
			h = h % TABLE_SIZE;
		bucket *b = &(m_Buckets[h]);
		hashnode *n = b->head;
		while (n)
		{
			if (Compare(n->key,k))
				return true;
			n = n->next;
		}
		return false;
	}
	V & Retrieve(const K & k, time_t & stamp)
	{
		hashnode *n;
		bucket *b;
		if (!_Find(k, &b, &n))
		{
			n = new hashnode;
			n->key = k;
			n->stamp = time(NULL);
			_Insert(b, n);
		}
		stamp = n->stamp;
		return n->val;
	}
	V & Retrieve(const K & k)
	{
		time_t stamp;
		return Retrieve(k, stamp);
	}
	size_t Size()
	{
		return m_Size;
	}
	void Remove(const K & key)
	{
		bucket *b;
		hashnode *n;
		if (_Find(key, &b, &n))
		{
			_Remove(b, n);
		}
	}
	size_t Prune(time_t start=0, time_t end=0)
	{
		size_t num = m_Size;
		hashnode *n, *t;
		bucket *b;
		for (size_t i=0; i<TABLE_SIZE; i++)
		{
			b = &(m_Buckets[i]);
			n = b->head;
			while (n)
			{
				t = n->next;
				if (start == 0 && end == 0)
					_Remove(b, n);
				else if (start == 0 && n->stamp < end)
					_Remove(b, n);
				else if (end == 0 && n->stamp > start)
					_Remove(b, n);
				else if (n->stamp > start && n->stamp < end)
					_Remove(b, n);
				n = t;
			}
			if (!m_Size)
				return num;
		}
		return (num - m_Size);
	}
private:
	bucket m_Buckets[TABLE_SIZE];
	size_t m_Size;
public:
	friend class iterator;
	class iterator
	{
	public:
		iterator()
		{
		}
		iterator(NHash *hash) : m_Hash(hash), 
								m_CurPos(0), 
								m_CurNode(0)
		{
			Next();
		}
		void Next()
		{
			if (!m_CurNode || !m_CurNode->next)
			{
				bucket *b;
				int i;
				for (i=m_CurPos+1; i<TABLE_SIZE; i++)
				{
					b = &(m_Hash->m_Buckets[i]);
					if (b->head)
					{
						m_CurNode = b->head;
						break;
					}
				}
				//m_LastPos = m_CurPos;
				m_CurPos = i;
			} else {
				m_CurNode = m_CurNode->next;
			}
		}
		bool Done()
		{
			if (!m_CurNode)
				return true;
			if (!m_CurNode->next && m_CurPos >= TABLE_SIZE)
				return true;
			if (!m_CurNode->next)
			{
				bucket *b;
				for (int i=m_CurPos+1; i<TABLE_SIZE; i++)
				{
					b = &(m_Hash->m_Buckets[i]);
					if (b->head)
					{
						//trick next into moving to this one quickly :)
						m_CurPos = i - 1;
						return false;
					}
				}
			}
			return false;
		}
		K & GetKey()
		{
			return m_CurNode->key;
		}
		V & GetVal()
		{
			return m_CurNode->val;
		}
		time_t GetStamp()
		{
			return m_CurNode->stamp;
		}
	private:
		NHash *m_Hash;
		int m_CurPos;
		//int m_LastPos;
		hashnode *m_CurNode;
		//hashnode *m_LastNode;
	};
public:
	iterator GetIter()
	{
		return iterator(this);
	}
private:
	bool _Find(const K & k, bucket **b, hashnode **n)
	{
		uint16_t h = HashFunction(k);
		if (h >= TABLE_SIZE)
			h = h % TABLE_SIZE;
		bucket *bb = &(m_Buckets[h]);
		if (b)
			*b = bb;
		hashnode *nn = bb->head;
		while (nn)
		{
			if (Compare(nn->key,k))
			{
				if (n)
					*n = nn;
				return true;
			}
			nn = nn->next;
		}
		return false;
	}
	void _Insert(hashnode *n)
	{
		uint16_t h = HashFunction(n->key);
		if (h >= TABLE_SIZE)
			h = h % TABLE_SIZE;
		bucket *b = &(m_Buckets[h]);
		_Insert(b, n);
	}
	//Lowest call for insertion
	void _Insert(bucket *b, hashnode *n)
	{
		n->next = NULL;
		if (b->head == NULL)
		{
			b->head = n;
			b->tail = n;
			n->prev = NULL;
		} else {
			b->tail->next = n;
			n->prev = b->tail;
			b->tail = n;
		}
		m_Size++;
	}
	//Lowest call for deletion, returns next node if any
	hashnode *_Remove(bucket *b, hashnode *n)
	{
		hashnode *n2 = n->next;
		if (b->head == n && b->tail == n)
		{
			b->head = NULL;
			b->tail = NULL;
		} else if (b->head == n) {
			n->next->prev = NULL;
			b->head = n->next;
			if (b->head->next == NULL)
				b->tail = b->head;
		} else if (b->tail == n) {
			n->prev->next = NULL;
			b->tail = n->prev;
			if (b->tail->prev == NULL)
				b->head = b->tail;
		} else {
			n->prev->next = n->next;
			n->next->prev = n->prev;
		}
		delete n;
		m_Size--;
		return n2;
	}
};

#endif //_INCLUDE_NHASH_H
