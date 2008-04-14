#ifndef _TRIE_NATIVES_H_
#define _TRIE_NATIVES_H_

#include "amxmodx.h"
#include "sm_trie_tpl.h"
#include "CVector.h"

#define TRIE_DATA_UNSET 	0
#define TRIE_DATA_CELL		1
#define TRIE_DATA_STRING	2
#define TRIE_DATA_ARRAY		3

#ifndef NDEBUG
extern size_t trie_malloc_count;
extern size_t trie_free_count;
#endif

class TrieData
{
private:
	cell *m_data;
	cell m_cell;
	cell m_cellcount;
	int m_type;

	void needCells(cell cellcount)
	{
		if (m_cellcount < cellcount)
		{
			if (m_data != NULL)
			{
				free(m_data);
#ifndef NDEBUG
				trie_free_count++;
#endif
			}
			size_t neededbytes = cellcount * sizeof(cell);
			m_data = static_cast<cell *>(malloc(neededbytes));

#ifndef NDEBUG
			trie_malloc_count++;
#endif
			m_cellcount = cellcount;
		}
	}
public:
	void freeCells()
	{
		if (m_data)
		{
#ifndef NDEBUG
			trie_free_count++;
#endif
			free(m_data);
			m_data = NULL;
		}
		m_cellcount = 0;
	}
	TrieData() : m_data(NULL), m_cell(0), m_cellcount(0), m_type(TRIE_DATA_UNSET) { }
	TrieData(const TrieData &src) : m_data(src.m_data),
					m_cell(src.m_cell),
					m_cellcount(src.m_cellcount),
					m_type(src.m_type) { }
	~TrieData() { }

	int getType() { return m_type; }

	void setCell(cell value)
	{
		freeCells();

		m_cell = value;
		m_type = TRIE_DATA_CELL;
	}
	void setString(cell *value)
	{
		cell len = 0;

		cell *p = value;

		while (*p++ != 0)
		{
			len++;
		}
		len += 1; // zero terminator
		needCells(len);
		memcpy(m_data, value, sizeof(cell) * len);

		m_type = TRIE_DATA_STRING;
	}
	void setArray(cell *value, cell size)
	{
		if (size <= 0)
			return;

		needCells(size);
		memcpy(m_data, value, sizeof(cell) * size);

		m_type = TRIE_DATA_ARRAY;
	}
	bool getCell(cell *out)
	{
		if (m_type == TRIE_DATA_CELL)
		{
			*out = m_cell;
			return true;
		}

		return false;
	}
	bool getString(cell *out, cell max)
	{
		if (m_type == TRIE_DATA_STRING && max >= 0)
		{
			memcpy(out, m_data, (max > m_cellcount ? m_cellcount : max) * sizeof(cell));
			return true;
		}
		return false;
	}
	bool getArray(cell *out, cell max)
	{
		if (m_type == TRIE_DATA_ARRAY && max >= 0)
		{
			memcpy(out, m_data, (max > m_cellcount ? m_cellcount : max) * sizeof(cell));
			return true;
		}
		return false;
	}
	void clear()
	{
		freeCells();
		m_type = TRIE_DATA_UNSET;
	}
};

class TrieHandles
{
private:
	CVector< KTrie< TrieData > *> m_tries;

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
	KTrie<TrieData> *lookup(int handle)
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
				m_tries[i] = new KTrie<TrieData>;

				return static_cast<int>(i) + 1;
			}
		}
		m_tries.push_back(new KTrie<TrieData>);
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


extern TrieHandles g_TrieHandles;
extern AMX_NATIVE_INFO trie_Natives[];

#endif

