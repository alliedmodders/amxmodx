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

#include "sh_list.h"

#define _T_INIT_HASH_SIZE	512

//namespace SourceHook
//{
	template <class K>
	int HashFunction(const K & k);

	template <class K>
	int Compare(const K & k1, const K & k2);

	template <class U, class K>
	int CompareAlt(const U &k1, const K &k2);

	template <class U>
	int HashAlt(const U &u);

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
			THashNode & operator =(const THashNode &other)
			{
				key = other.key;
				val = other.val;
			}
			K key;
			V val;
		};
		typedef List<THashNode *> *	NodePtr;
	public:
		class const_iterator;
		THash() : m_Buckets(NULL), m_numBuckets(0), m_percentUsed(0.0f)
		{
			_Refactor();
		}
		THash(const THash &other) : m_Buckets(new NodePtr[other.m_numBuckets]),
			m_numBuckets(other.m_numBuckets), m_percentUsed(other.m_percentUsed)
		{
			for (size_t i=0; i<m_numBuckets; i++)
					m_Buckets[i] = NULL;
			for (const_iterator iter = other.begin(); iter != other.end(); ++iter)
				_FindOrInsert(iter->key)->val = iter->val;
		}
		void operator=(const THash &other)
		{
			clear();
			for (const_iterator iter = other.begin(); iter != other.end(); ++iter)
				_FindOrInsert(iter->key)->val = iter->val;
		}

		~THash()
		{
			_Clear();
		}
		void clear()
		{
			_Clear();
			_Refactor();
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
			typename List<THashNode *>::iterator iter, end;
			for (size_t i=0; i<m_numBuckets; i++)
			{
				if (m_Buckets[i])
				{
					end = m_Buckets[i]->end();
					iter = m_Buckets[i]->begin();
					while (iter != end)
					{
						delete (*iter);
						iter++;
					}
					delete m_Buckets[i];
					m_Buckets[i] = NULL;
				}
			}
			if (m_Buckets)
				delete [] m_Buckets;
			m_Buckets = NULL;
			m_numBuckets = 0;
		}
	public:
		template <typename U>
		V & AltFindOrInsert(const U & ukey)
		{
			size_t place = HashAlt(ukey) % m_numBuckets;
			THashNode *pNode = NULL;
			if (!m_Buckets[place])
			{
				m_Buckets[place] = new List<THashNode *>;
				pNode = new THashNode(ukey, V());
				m_Buckets[place]->push_back(pNode);
				m_percentUsed += (1.0f / (float)m_numBuckets);
				m_items++;
			} else {
				typename List<THashNode *>::iterator iter;
				for (iter=m_Buckets[place]->begin(); iter!=m_Buckets[place]->end(); iter++)
				{
					if (CompareAlt(ukey, (*iter)->key) == 0)
						return (*iter)->val;
				}
				pNode = new THashNode(ukey, V());
				m_Buckets[place]->push_back(pNode);
				m_items++;
			}
			if (PercentUsed() > 0.75f)
				_Refactor();
			return pNode->val;
		}
	private:
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
				m_items++;
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
				m_items++;
			}
			if (PercentUsed() > 0.75f)
				_Refactor();
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
		class iterator
		{
			friend class THash;
		public:
			iterator() : curbucket(-1), hash(NULL), end(true)
			{
			};
			iterator(THash *h) : curbucket(-1), hash(h), end(false)
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
			const THashNode & operator * () const
			{
				return *(*iter);
			}
			THashNode & operator * ()
			{
				return *(*iter);
			}
			const THashNode * operator ->() const
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

			void erase()
			{
				if (end || !hash || curbucket < 0 || curbucket >= static_cast<int>(hash->m_numBuckets))
					return;

				// Remove this element and move to the next one
				iterator tmp = *this;
				++tmp;
				delete (*iter);
				hash->m_Buckets[curbucket]->erase(iter);
				*this = tmp;

				// :TODO: Maybe refactor to a lower size if required
			}
		private:
			void _Inc()
			{
				if (end || !hash || curbucket >= static_cast<int>(hash->m_numBuckets))
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
		class const_iterator
		{
			friend class THash;
		public:
			const_iterator() : curbucket(-1), hash(NULL), end(true)
			{
			};
			const_iterator(const THash *h) : curbucket(-1), hash(h), end(false)
			{
				if (!h->m_Buckets)
					end = true;
				else
					_Inc();
			};
			//pre increment
			const_iterator & operator++()
			{
				_Inc();
				return *this;
			}
			//post increment
			const_iterator operator++(int)
			{
				iterator old(*this);
				_Inc();
				return old;
			}
			const THashNode & operator * () const
			{
				return *(*iter);
			}
			const THashNode * operator ->() const
			{
				return (*iter);
			}
			bool operator ==(const const_iterator &where) const
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
			bool operator !=(const const_iterator &where) const
			{
				return !( (*this) == where );
			}
		private:
			void _Inc()
			{
				if (end || !hash || curbucket >= static_cast<int>(hash->m_numBuckets))
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
			const THash *hash;
			bool end;
		};
		friend class iterator;
		friend class const_iterator;
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

		const_iterator begin() const
		{
			return const_iterator(this);
		}
		const_iterator end() const
		{
			const_iterator iter;
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

		iterator erase(iterator where)
		{
			where.erase();
			return where;
		}
		template <typename U>
		void erase(const U & u)
		{
			iterator iter = find(u);
			if (iter == end())
				return;
			iter.erase();
		}
	private:
		NodePtr	*m_Buckets;
		size_t m_numBuckets;
		float m_percentUsed;
		size_t m_items;
	};
//};

#endif //_INCLUDE_SH_TINYHASH_H_
