/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#ifndef DATASTRUCTS_H
#define DATASTRUCTS_H

#include <am-vector.h>

class CellArray
{
public:
	CellArray(size_t blocksize) : m_Data(NULL), m_BlockSize(blocksize), m_AllocSize(0), m_Size(0)
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
			return NULL;
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
			m_AllocSize = 8;
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
	size_t m_Size;
};

extern ke::Vector<CellArray*> VectorHolder;


inline CellArray* HandleToVector(AMX* amx, int handle)
{
	if (handle <= 0 || handle > (int)VectorHolder.length())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", handle);

		return NULL;
	}

	CellArray* ret = VectorHolder[handle - 1];

	if (ret == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", handle);

		return NULL;
	}

	return ret;
}


#endif
