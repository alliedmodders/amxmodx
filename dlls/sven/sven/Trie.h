/* ======== Simple Trie ========
* Copyright (C) 2006-2007 Kuchiki Rukia
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Radical Edward
* Notes: Generic simple trie
* ============================
*/

// Rukia: Digital trees, or tries, are a combination of vector and tree structures.
//	They have garanteed O(1) worst case (literally O(m), constant for key length).
//	However, unless optimized (such as in Judy Arrays), they have terrible memory performance.
//	We will use a naive approach, due to time constraints. 
//	Judy Arrays would be a better approach, but would destroy the point of the assignment.

#ifndef __TRIE_CLASS__
#define __TRIE_CLASS__

// Rukia: HACK: Usage of assert to force metatemplates to work right.
#include <cassert>
#include <string.h>

// Rukia: Metaprogramming to aid in compile time constants and such.
template<size_t base, size_t N>
struct Exponential
{
   enum { value = base * Exponential<base,N - 1>::value };
};

template <size_t base>
struct Exponential<base,0> 
{
   enum { value = 1 };
};

// Rukia: NOTE: This is extremely ugly for these reasons:
// 1. It relies on template metaprogramming
// 2. It is unoptimized
// 3. It was written in exactly 1 hour and 7 minutes.
// However, preliminary tests show it is faster than the STL hashmap, in current form.
// HACK: Optimize further into a patricia tree and partial specialization digital tree (Judy Array). 

// Rukia: HACK: To optimize:
// 1. Add two bitvectors (vector<bool>) to each node.
//		* 0 0 = nothing at all
//		* 1 0 = compressed nodes 1
//		* 0 1 = compressed nodes 2
//		* 1 1 = uncompressed node
// 2. Add compressed node 1; a simple holder for one value
// 3. Add compressed node 2; a vector with a bitlookup table for up to 2^sizeof(C) values
// 4. Allow for hytersis in deletion for until 1 insert (will increase speed on multiple in row insert/deletes

// Rukia: Templates <Key, Value, Compare by>
template <typename K, typename V, typename C = unsigned char>
class Trie
{
public:

	// Rukia: HACK: Remove this from Trie class eventually; it looks ugly and is slow.
	class TrieNode
	{
	friend class Trie;
	public:
		TrieNode() 
		{ 
			// Rukia: Render all pointers NULL.
			// Rukia: HACK: Reformat this somehow, it is ugly.
			// Rukia: Use 0, not NULL. GCC dislikes usage of NULL.
			memset(reinterpret_cast<void*>(Children),0,Exponential<2,8*sizeof(C)>::value * sizeof(TrieNode*));
			Value = NULL;
		}
		// Rukia: We can garantee this will be an OK delete; either value, or NULL.
		~TrieNode()	
		{
			if( Value != NULL) { delete Value; }
			for(register long i = 0; i < Exponential<2,8*sizeof(C)>::value; i++)
			{
				delete Children[i];
			}
		}

		void Clear()
		{
			if( Value != NULL) { delete Value; }
			for(register long i = 0; i < Exponential<2,8*sizeof(C)>::value; i++)
			{
				delete Children[i];
				Children[i] = NULL;
			}
		}

		// Rukia: Little syntatical sugar for you. Hope you like it.
		TrieNode* operator[](C size)
		{
			return Children[size];
		}

		void InOrderAlt(void(*func)(V&) )
		{
			if( Value != NULL) { func(*Value); }
			for(register long i = 0; i < Exponential<2,8*sizeof(C)>::value; i++)
			{
				if(Children[i] != NULL) { (Children[i])->InOrderAlt(func); }
			}
		}

		void Insert(V& newval)
		{
			if(Value == NULL) { Value = new V; }
			
			*Value = newval;
		}

		// Rukia: This will be inlined out, and it is never good to expose too much.
		V* Retrieve()
		{
			return Value;
		}

		// Rukia: Return true if node is redundant, so we can remove it.
		// Rukia: HACK: Perhaps optimize for inserts by analyzing usage?
		void Delete()
		{
			delete Value;
			Value = NULL;
		}

	// Rukia: GCC doesn't like redundant friend declarations.
	//friend class Trie;
	private:
		TrieNode* Children[Exponential<2,8*sizeof(C)>::value];
		V* Value;
	};

	friend class TrieNode;

	// Rukia: Root/stem node.
	TrieNode Stem;

	// Simply calls the destructor on any and all children, until everything is dead.
	void Clear()
	{
		Stem.Clear();
	}

	bool IsValid(const K* key, size_t keylen)
	{
		return (Retrieve(key,keylen) != NULL);
	}

	void InOrderAlt(void(*func)(V&) )
	{
		Stem.InOrderAlt(func);
	}
	
	// Rukia: We use const for the key, even though we completely subvert the system.
	// Rukia: Why? Because we don't CHANGE it, even if we subvert the system.
	V* Retrieve(const  K* key, size_t keylen)
	{
		// Rukia: Convert to comparison types
		register C* realkey = (C*)(key);
		C CurrKey = *realkey;

		// Rukia: HACK: Convert to use bitwise shift operators
		register size_t reallen = keylen * (sizeof(K) / sizeof(C) );

		if(key == NULL) { return Stem.Retrieve(); }

		// Rukia: Iterate through the nodes till we find a NULL one, or run out of key.
		register TrieNode* CurrNode = Stem[CurrKey];

		// Rukia: HACK: Return NULL, don't use exceptions, they are slow.
		if(CurrNode == NULL) { return NULL; }
		
		// Rukia: initialize one lower because we've already decoded one from the key.
		for(reallen--;reallen != 0;reallen--)
		{
			realkey++;
			CurrKey = *realkey;

			CurrNode = (*CurrNode)[CurrKey];
			if(CurrNode == NULL) { return NULL; }
		}
		return CurrNode->Retrieve();
	};

	void Insert( const K* key, size_t keylen, V& value)
	{
		// Rukia: Convert to comparison types
		register C* realkey = (C*)(key);
		C CurrKey = *realkey;

		// Rukia: HACK: Convert to use bitwise shift operators
		register size_t reallen = keylen * (sizeof(K) / sizeof(C) );

		if(key == NULL) { Stem.Retrieve(); }

		// Rukia: Iterate through the nodes till we find a NULL one, or run out of key.
		register TrieNode* CurrNode = Stem[CurrKey];
		register TrieNode* TmpNode = NULL;

		// Rukia: HACK: Maybe an internal memory allocator?
		// Rukia: HACK: Quickly resort to 'friend'; reduces encapsulation, but worth the cost.
		if(CurrNode == NULL) { CurrNode = new TrieNode(); Stem.Children[CurrKey] = CurrNode; }

		// Rukia: initialize one lower because we've already decoded one from the key.
		for(reallen--;reallen != 0;reallen--)
		{
			realkey++;
			CurrKey = *realkey;

			TmpNode = (*CurrNode)[CurrKey];
			if(TmpNode == NULL) { TmpNode = new TrieNode; CurrNode->Children[CurrKey] = TmpNode; }

			CurrNode = TmpNode;
		}
		CurrNode->Insert(value);
	}

	// Rukia: HACK HACK HACK: Fix this SOON. Delete will NOT delete nodes, and has no hystersis operandi.	
	void Delete( const K* key, size_t keylen)
	{
		// Rukia: Convert to comparison types
		register C* realkey = (C*)(key);
		C CurrKey = *realkey;

		// Rukia: HACK: Convert to use bitwise shift operators
		register size_t reallen = keylen * (sizeof(K) / sizeof(C) );

		if(key == NULL) { Stem.Delete(); return; }

		// Rukia: Iterate through the nodes till we find a NULL one, or run out of key.
		register TrieNode* CurrNode = Stem[CurrKey];

		// Rukia: HACK: Return NULL, don't use exceptions, they are slow.
		if(CurrNode == NULL) { return; }
		
		// Rukia: initialize one lower because we've already decoded one from the key.
		for(reallen--;reallen != 0;reallen--)
		{
			realkey++;
			CurrKey = *realkey;

			CurrNode = (*CurrNode)[CurrKey];
			if(CurrNode == NULL) { return; }
		}
		CurrNode->Delete();
	}
	
};

#endif
