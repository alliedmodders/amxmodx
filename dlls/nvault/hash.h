#ifndef _INCLUDE_AMXX_HASH_H
#define _INCLUDE_AMXX_HASH_H

#include <time.h>
#include "sdk/amxxmodule.h"
#include "sdk/CString.h"

#if defined WIN32 || defined _WIN32
	typedef unsigned __int8	uint8_t;
	typedef __int8 int8_t;
#else
#include <stdint.h>
#endif

/** 
 * Hash Table implementation (by David "BAILOPAN" Anderson)
 *  This is not designed to be flexible for size/type, it's hardcoded.
 *  The table size is 2^11, which should be good enough for fast small lookups.
 *  A hash is computed by two chopped up 8bit versions of CRC32, which is then combined to 
 *   form an 11bit key.  That key is then an index into the appropriate bucket.
 */

extern const uint8_t Hash_CRCTable1[];
extern const uint8_t Hash_CRCTable2[];

//You should not change this without understanding the hash algorithm
#define HT_SIZE		2048

class HashTable
{
public:		//STRUCTORS
	HashTable();
	~HashTable();
public:		//PRE-DEF
	class iterator;
	struct htNode;
	friend class Vault;
private:	//PRE-DEF
	struct htNodeSet;
public:		//PUBLIC FUNCTIONS
	void Store(const char *key, const char *value, bool temporary=true);
	void Store(const char *key, const char *value, time_t stamp);
	htNode *Retrieve(const char *key);
	iterator Enumerate();
	size_t Prune(time_t begin, time_t end, bool all=false);
	void Clear();
	bool KeyExists(const char *key);
	size_t UsedHashes();
	void EraseKey(const char *key);
public:		//PUBLIC CLASSES
	class iterator
	{
	public:
		iterator(htNodeSet **table, uint32_t tableSize);
		const char *key();
		const char *val();
		time_t stamp();
		bool done();
		void next();
		uint32_t hash();
		htNode *operator *();
	private:
		htNodeSet **t;	//table
		uint32_t s;		//size
		uint32_t r;		//current
		htNode *d;		//delta
	};
private:	//PRIVATE API
	void _Insert(const char *key, const char *val, time_t stamp);
	htNode *_FindNode(const char *key, bool autoMake=true);
	htNodeSet *_FindNodeSet(const char *key);
	htNode *_InsertIntoNodeSet(htNodeSet *set, const char *key, bool skip=false);
	void _Unlink(htNodeSet *set, htNode *node);
public:		//PUBLIC STATIC API
	static uint16_t HashString(const char *str);
private:	//PRIVATE STATIC API
	static uint8_t _HashString1(const char *str);
	static uint8_t _HashString2(const char *str);
public:		//PUBLIC STRUCTURES
	struct htNode
	{
		String key;
		String val;
		time_t stamp;
		htNode *next;
	};
private:	//PRIVATE STRUCTURES
	struct htNodeSet
	{
		htNode *head;
		htNode *tail;
	};
private:	//INTERNAL VARIABLES
	htNodeSet *m_Table[HT_SIZE];
};

#endif //_INCLUDE_AMXX_HASH_H
