/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_SH_TINYHASH_H_
#define _INCLUDE_SH_TINYHASH_H_

#include <time.h>
#include "sh_list.h"

#define _T_INIT_HASH_SIZE	32

//namespace SourceHook
//{
	template <class K>
	int HashFunction(const K & k);

	template <class K>
	int Compare(const K & k1, const K & k2);

	/**
	 * This is a tiny, growable hash class.
	 * Meant for quick and dirty dictionaries only!
	 */
	template <class K, class V> 
	class THash
	{
	public:
		struct THashNode
		{
			THashNode(const K & k, const V & v) : 
				key(k), val(v)
				{
				};
			K key;
			V val;
			time_t stamp;
		};
		typedef List<THashNode *> *	NodePtr;
	public:
		THash() : m_numBuckets(0), m_Buckets(NULL), m_Items(0)
		{
			_Refactor();
		}
		~THash()
		{
			_Clear();
		}
		void Clear()
		{
			_Clear();
			_Refactor();
		}
		void Insert(const K & key, const V & val)
		{
			Insert(key, val, time(NULL));
		}
		void Insert(const K & key, const V & val, time_t stamp)
		{
			THashNode *pNode = _FindOrInsert(key);
			pNode->val = val;
			pNode->stamp = stamp;
		}
		bool Exists(const K & key)
		{
			size_t place = HashFunction(key) % m_numBuckets;

			if (!m_Buckets[place])
				return false;

			typename List<THashNode *>::iterator iter;
			for (iter = m_Buckets[place]->begin(); iter != m_Buckets[place]->end(); iter++)
			{
				if (Compare(key, (*iter)->key) == 0)
					return true;
			}
			return false;
		}
		V & Retrieve(const K & k, time_t & stamp)
		{
			THashNode *pNode = _FindOrInsert(k);
			stamp = pNode->stamp;
			return pNode->val;
		}
		V & Retrieve(const K & k)
		{
			time_t stamp;
			return Retrieve(k, stamp);
		}
		void Remove(const K & key)
		{
			size_t place = HashFunction(key) % m_numBuckets;

			if (!m_Buckets[place])
				return;

			typename List<THashNode *>::iterator iter;
			for (iter = m_Buckets[place]->begin(); iter != m_Buckets[place]->end(); iter++)
			{
				if (Compare(key, (*iter)->key) == 0)
				{
					iter = m_Buckets[place]->erase(iter);
					return;
				}
			}
		}
		size_t Prune(time_t start=0, time_t end=0)
		{
			typename List<THashNode *>::iterator iter;
			time_t stamp;
			size_t removed = 0;
			for (size_t i=0; i<m_numBuckets; i++)
			{
				iter = m_Buckets[i]->begin();
				bool remove;
				while (iter != m_Buckets[i]->end())
				{
					stamp = (*iter)->stamp;
					remove = false;
					if (stamp != 0)
					{
						if (start == 0 && end == 0)
							remove = true;
						else if (start == 0 && stamp < end)
							remove = true;
						else if (end == 0 && stamp > start)
							remove = true;
						else if (stamp > start && stamp < end)
							remove = true;
						if (remove)
						{
							iter = m_Buckets[i]->erase(iter);
							removed++;
						} else {
							iter++;
						}
					}
				}
			}
			return removed;
		}
		size_t Size()
		{
			return m_Items;
		}
		size_t GetBuckets()
		{
			return m_numBuckets;
		}
		float PercentUsed()
		{
			return m_percentUsed;
		}
		V & operator [](const K & key)
		{
			THashNode *pNode = _FindOrInsert(key);
			return pNode->val;
		}
	private:
		void _Clear()
		{
			for (size_t i=0; i<m_numBuckets; i++)
			{
				if (m_Buckets[i])
				{
					m_Buckets[i]->clear();
					delete m_Buckets[i];
				}
			}
			delete [] m_Buckets;
			m_numBuckets = 0;
			m_Items = 0;
			m_Buckets = NULL;
		}
		THashNode *_FindOrInsert(const K & key)
		{
			size_t place = HashFunction(key) % m_numBuckets;
			THashNode *pNode = NULL;
			if (!m_Buckets[place])
			{
				m_Buckets[place] = new List<THashNode *>;
				pNode = new THashNode(key, V());
				m_Buckets[place]->push_back(pNode);
				m_percentUsed += (1.0f / (float)m_numBuckets);
			} else {
				typename List<THashNode *>::iterator iter;
				for (iter=m_Buckets[place]->begin(); iter!=m_Buckets[place]->end(); iter++)
				{
					if (Compare((*iter)->key, key) == 0)
						return (*iter);
				}
				//node does not exist
				pNode = new THashNode(key, V());
				m_Buckets[place]->push_back(pNode);
			}
			if (PercentUsed() > 0.75f)
				_Refactor();
			m_Items++;
			return pNode;
		}
		void _Refactor()
		{
			m_percentUsed = 0.0f;
			if (!m_numBuckets)
			{
				m_numBuckets = _T_INIT_HASH_SIZE;
				m_Buckets = new NodePtr[m_numBuckets];
				for (size_t i=0; i<m_numBuckets; i++)
					m_Buckets[i] = NULL;
			} else {
				size_t oldSize = m_numBuckets;
				m_numBuckets *= 2;
				typename List<THashNode *>::iterator iter;
				size_t place;
				THashNode *pHashNode;
                NodePtr *temp = new NodePtr[m_numBuckets];
				for (size_t i=0; i<m_numBuckets; i++)
					temp[i] = NULL;
				//look in old hash table
				for (size_t i=0; i<oldSize; i++)
				{
					//does a bucket have anything?
					if (m_Buckets[i])
					{
						//go through the list of items
						for (iter = m_Buckets[i]->begin(); iter != m_Buckets[i]->end(); iter++)
						{
							pHashNode = (*iter);
							//rehash it with the new bucket filter
							place = HashFunction(pHashNode->key) % m_numBuckets;
							//add it to the new hash table
							if (!temp[place])
							{
								temp[place] = new List<THashNode *>;
								m_percentUsed += (1.0f / (float)m_numBuckets);
							}
							temp[place]->push_back(pHashNode);
						}
						//delete that bucket!
						delete m_Buckets[i];
						m_Buckets[i] = NULL;
					}
				}
				//reassign bucket table
				delete [] m_Buckets;
				m_Buckets = temp;
			}
		}
	public:
		friend class iterator;
		class iterator
		{
			friend class THash;
		public:
			iterator() : hash(NULL), end(true)
			{
			};
			iterator(THash *h) : hash(h), end(false)
			{
				if (!h->m_Buckets)
					end = true;
				else
					_Inc();
			};
			//pre increment
			iterator & operator++()
			{
				_Inc();
				return *this;
			}
			//post increment
			iterator operator++(int)
			{
				iterator old(*this);
				_Inc();
				return old;
			}
			THashNode & operator * () const
			{
				return *(*iter);
			}
			THashNode & operator * ()
			{
				return *(*iter);
			}
			THashNode * operator ->() const
			{
				return (*iter);
			}
			THashNode * operator ->()
			{
				return (*iter);
			}
			bool operator ==(const iterator &where) const
			{
				if (where.hash == this->hash
					&& where.end == this->end
					&& 
					 (this->end || 
					   ((where.curbucket == this->curbucket) 
					     && (where.iter == iter))
						 ))
					return true;
				return false;
			}
			bool operator !=(const iterator &where) const
			{
				return !( (*this) == where );
			}
		private:
			void _Inc()
			{
				if (end || !hash || curbucket >= (int)hash->m_numBuckets)
					return;
				if (curbucket < 0)
				{
					for (int i=0; i<(int)hash->m_numBuckets; i++)
					{
						if (hash->m_Buckets[i])
						{
							iter = hash->m_Buckets[i]->begin();
							if (iter == hash->m_Buckets[i]->end())
								continue;
							curbucket = i;
							break;
						}
					}
					if (curbucket < 0)
						end = true;
				} else {
					if (iter != hash->m_Buckets[curbucket]->end())
						iter++;
					if (iter == hash->m_Buckets[curbucket]->end())
					{
						int oldbucket = curbucket;
						for (int i=curbucket+1; i<(int)hash->m_numBuckets; i++)
						{
							if (hash->m_Buckets[i])
							{
								iter = hash->m_Buckets[i]->begin();
								if (iter == hash->m_Buckets[i]->end())
									continue;
								curbucket = i;
								break;
							}
						}
						if (curbucket == oldbucket)
							end = true;
					}
				}
			}
		private:
			int curbucket;
			typename List<THashNode *>::iterator iter;
			THash *hash;
			bool end;
		};
	public:
		iterator begin()
		{
			return iterator(this);
		}
		iterator end()
		{
			iterator iter;
			iter.hash = this;
			return iter;
		}
		template <typename U>
		iterator find(const U & u) const
		{
			iterator b = begin();
			iterator e = end();
			for (iterator iter = b; iter != e; iter++)
			{
				if ( (*iter).key == u )
					return iter;
			}
			return end();
		}
		template <typename U>
		iterator find(const U & u)
		{
			iterator b = begin();
			iterator e = end();
			for (iterator iter = b; iter != e; iter++)
			{
				if ( (*iter).key == u )
					return iter;
			}
			return end();
		}
	private:
		NodePtr	*m_Buckets;
		size_t m_numBuckets;
		float m_percentUsed;
		size_t m_Items;
	};
//};

#endif //_INCLUDE_SH_TINYHASH_H_
