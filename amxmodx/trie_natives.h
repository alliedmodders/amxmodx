#ifndef _TRIE_NATIVES_H_
#define _TRIE_NATIVES_H_

#include "amxmodx.h"
#include "sm_stringhashmap.h"
#include "CVector.h"

using namespace SourceMod;

enum EntryType
{
	EntryType_Cell,
	EntryType_CellArray,
	EntryType_String,
};

class Entry
{
	struct ArrayInfo
	{
		size_t length;
		size_t maxbytes;

		void *base() {
			return this + 1;
		}
	};

public:
	Entry()
		: control_(0)
	{
	}
	Entry(ke::Moveable<Entry> other)
	{
		control_ = other->control_;
		data_ = other->data_;
		other->control_ = 0;
	}
	~Entry()
	{
		free(raw());
	}

	void setCell(cell value) {
		setType(EntryType_Cell);
		data_ = value;
	}
	void setArray(cell *cells, size_t length) {
		ArrayInfo *array = ensureArray(length * sizeof(cell));
		array->length = length;
		memcpy(array->base(), cells, length * sizeof(cell));
		setTypeAndPointer(EntryType_CellArray, array);
	}
	void setString(const char *str) {
		size_t length = strlen(str);
		ArrayInfo *array = ensureArray(length + 1);
		array->length = length;
		strcpy((char *)array->base(), str);
		setTypeAndPointer(EntryType_String, array);
	}

	size_t arrayLength() const {
		//assert(isArray());
		return raw()->length;
	}
	cell *array() const {
		//assert(isArray());
		return reinterpret_cast<cell *>(raw()->base());
	}
	char *chars() const {
		//assert(isString());
		return reinterpret_cast<char *>(raw()->base());
	}
	cell cell_() const {
		//assert(isCell());
		return data_;
	}

	bool isCell() const {
		return type() == EntryType_Cell;
	}
	bool isArray() const {
		return type() == EntryType_CellArray;
	}
	bool isString() const {
		return type() == EntryType_String;
	}

private:
	Entry(const Entry &other) KE_DELETE;

	ArrayInfo *ensureArray(size_t bytes) {
		ArrayInfo *array = raw();
		if (array && array->maxbytes >= bytes)
			return array;
		array = (ArrayInfo *)realloc(array, bytes + sizeof(ArrayInfo));
		if (!array)
		{
			fprintf(stderr, "Out of memory!\n");
			abort();
		}
		array->maxbytes = bytes;
		return array;
	}

	// Pointer and type are overlaid, so we have some accessors.
	ArrayInfo *raw() const {
		return reinterpret_cast<ArrayInfo *>(control_ & ~uintptr_t(0x3));
	}
	void setType(EntryType aType) {
		control_ = uintptr_t(raw()) | uintptr_t(aType);
		//assert(type() == aType);
	}
	void setTypeAndPointer(EntryType aType, ArrayInfo *ptr) {
		// malloc() should guarantee 8-byte alignment at worst
		//assert((uintptr_t(ptr) & 0x3) == 0);
		control_ = uintptr_t(ptr) | uintptr_t(aType);
		//assert(type() == aType);
	}
	EntryType type() const {
		return (EntryType)(control_ & 0x3);
	}

private:
	// Contains the bits for the type, and an array pointer, if one is set.
	uintptr_t control_;

	// Contains data for cell-only entries.
	cell data_;
};

struct CellTrie
{
	StringHashMap<Entry> map;
};

template <typename T>
class TrieHandles
{
private:
	CVector<T *> m_tries;

public:
	TrieHandles() { }
	~TrieHandles()
	{
		this->clear();
	}

	void clear()
	{
		for (size_t i = 0; i < m_tries.size(); i++)
		{
			if (m_tries[i] != NULL)
			{
				delete m_tries[i];
			}
		}

		m_tries.clear();
	}
	T *lookup(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_tries.size()))
		{
			return NULL;
		}

		return m_tries[handle];
	}
	int create()
	{
		for (size_t i = 0; i < m_tries.size(); i++)
		{
			if (m_tries[i] == NULL)
			{
				// reuse handle
				m_tries[i] = new T;

				return static_cast<int>(i) + 1;
			}
		}
		m_tries.push_back(new T);
		return m_tries.size();
	}
	bool destroy(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_tries.size()))
		{
			return false;
		}

		if (m_tries[handle] == NULL)
		{
			return false;
		}
		delete m_tries[handle];
		m_tries[handle] = NULL;

		return true;
	}
};


extern TrieHandles<CellTrie> g_TrieHandles;
extern AMX_NATIVE_INFO trie_Natives[];

#endif

