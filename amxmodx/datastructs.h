// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef DATASTRUCTS_H
#define DATASTRUCTS_H

#include "natives_handles.h"

class CellArray
{
public:
	CellArray(size_t blocksize, size_t basesize = 0) : m_Data(nullptr), m_BlockSize(blocksize), m_AllocSize(0), m_BaseSize(basesize > 0 ? basesize : 8), m_Size(0)
	{
	}

	~CellArray()
	{
		free(m_Data);
	}

	size_t size() const
	{
		return m_Size;
	}

	cell *push()
	{
		if (!GrowIfNeeded(1))
		{
			return NULL;
		}
		cell *arr = &m_Data[m_Size * m_BlockSize];
		m_Size++;
		return arr;
	}

	cell *at(size_t b) const
	{
		return &m_Data[b * m_BlockSize];
	}

	size_t blocksize() const
	{
		return m_BlockSize;
	}

	void clear()
	{
		m_Size = 0;
	}

	bool swap(size_t item1, size_t item2)
	{
		/* Make sure there is extra space available */
		if (!GrowIfNeeded(1))
		{
			return false;
		}

		cell *pri = at(item1);
		cell *alt = at(item2);

		/* Get our temporary array 1 after the limit */
		cell *temp = &m_Data[m_Size * m_BlockSize];

		memcpy(temp, pri, sizeof(cell)* m_BlockSize);
		memcpy(pri, alt, sizeof(cell)* m_BlockSize);
		memcpy(alt, temp, sizeof(cell)* m_BlockSize);

		return true;
	}

	void remove(size_t index)
	{
		/* If we're at the end, take the easy way out */
		if (index == m_Size - 1)
		{
			m_Size--;
			return;
		}

		/* Otherwise, it's time to move stuff! */
		size_t remaining_indexes = (m_Size - 1) - index;
		cell *src = at(index + 1);
		cell *dest = at(index);
		memmove(dest, src, sizeof(cell)* m_BlockSize * remaining_indexes);

		m_Size--;
	}

	cell *insert_at(size_t index)
	{
		/* Make sure it'll fit */
		if (!GrowIfNeeded(1))
		{
			return nullptr;
		}

		/* move everything up */
		cell *src = at(index);
		cell *dst = at(index + 1);
		memmove(dst, src, sizeof(cell)* m_BlockSize * (m_Size - index));

		m_Size++;

		return src;
	}

	bool resize(size_t count)
	{
		if (count <= m_Size)
		{
			m_Size = count;
			return true;
		}

		if (!GrowIfNeeded(count - m_Size))
		{
			return false;
		}

		m_Size = count;
		return true;
	}

	CellArray *clone()
	{
		CellArray *array = new CellArray(m_BlockSize);
		array->m_AllocSize = m_AllocSize;
		array->m_Size = m_Size;
		array->m_Data = (cell *)malloc(sizeof(cell)* m_BlockSize * m_AllocSize);
		memcpy(array->m_Data, m_Data, sizeof(cell)* m_BlockSize * m_Size);
		return array;
	}

	cell *base()
	{
		return m_Data;
	}

	size_t mem_usage()
	{
		return m_AllocSize * m_BlockSize * sizeof(cell);
	}

private:
	bool GrowIfNeeded(size_t count)
	{
		/* Shortcut out if we can store this */
		if (m_Size + count <= m_AllocSize)
		{
			return true;
		}
		/* Set a base allocation size of 8 items */
		if (!m_AllocSize)
		{
			m_AllocSize = m_BaseSize;
		}
		/* If it's not enough, keep doubling */
		while (m_Size + count > m_AllocSize)
		{
			m_AllocSize *= 2;
		}
		/* finally, allocate the new block */
		if (m_Data)
		{
			m_Data = (cell *)realloc(m_Data, sizeof(cell)* m_BlockSize * m_AllocSize);
		}
		else {
			m_Data = (cell *)malloc(sizeof(cell)* m_BlockSize * m_AllocSize);
		}
		return (m_Data != NULL);
	}
private:
	cell  *m_Data;
	size_t m_BlockSize;
	size_t m_AllocSize;
	size_t m_BaseSize;
	size_t m_Size;
};

extern NativeHandle<CellArray> ArrayHandles;

#endif
