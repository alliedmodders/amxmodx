/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
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

#ifndef __CVECTOR_H__
#define __CVECTOR_H__

#include <assert.h>

// Vector
template <class T> class CVector
{
	bool Grow()
	{
		// automatic grow
		size_t newSize = m_Size * 2;
		if (newSize == 0)
			newSize = 8;					// a good init value
		T *newData = new T[newSize];
		if (!newData)
			return false;
		if (m_Data)
		{
			memcpy(newData, m_Data, m_Size * sizeof(T));
			delete [] m_Data;
		}
		m_Data = newData;
		m_Size = newSize;
		return true;
	}

	bool GrowIfNeeded()
	{
		if (m_CurrentUsedSize >= m_Size)
			return Grow();
		else
			return true;
	}

	bool ChangeSize(size_t size)
	{
		// change size
		if (size == m_Size)
			return true;
		T *newData = new T[size];
		if (!newData)
			return false;
		if (m_Data)
		{
			memcpy(newData, m_Data, (m_Size < size) ? (m_Size * sizeof(T)) : (size * sizeof(T)));
			delete [] m_Data;
		}
		if (m_Size < size)
			m_CurrentSize = size;
		m_Data = newData;
		m_Size = size;
		return true;
	}

	void FreeMemIfPossible()
	{

	}
protected:
	T *m_Data;
	size_t m_Size;
	size_t m_CurrentUsedSize;
	size_t m_CurrentSize;
public:
	class iterator
	{
	protected:
		T *m_Ptr;
	public:
		// constructors / destructors
		iterator()
		{
			m_Ptr = NULL;
		}

		iterator(T * ptr)
		{
			m_Ptr = ptr;
		}

		// member functions
		T * base()
		{
			return m_Ptr;
		}

		const T * base() const
		{
			return m_Ptr;
		}

		// operators
		T & operator*()
		{
			return *m_Ptr;
		}

		T * operator->()
		{
			return m_Ptr;
		}

		iterator & operator++()		// preincrement
		{
			++m_Ptr;
			return (*this);
		}

		iterator operator++(int)	// postincrement
		{
			iterator tmp = *this;
			++m_Ptr;
			return tmp;
		}

		iterator & operator--()		// predecrement
		{
			--m_Ptr;
			return (*this);
		}

		iterator operator--(int)	// postdecrememnt
		{
			iterator tmp = *this;
			--m_Ptr;
			return tmp;
		}

		bool operator==(T * right) const
		{
			return (m_Ptr == right);
		}

		bool operator==(const iterator & right) const
		{
			return (m_Ptr == right.m_Ptr);
		}

		bool operator!=(T * right) const
		{
			return (m_Ptr != right);
		}

		bool operator!=(const iterator & right) const
		{
			return (m_Ptr != right.m_Ptr);
		}

		iterator & operator+=(size_t offset)
		{
			m_Ptr += offset;
			return (*this);
		}

		iterator & operator-=(size_t offset)
		{
			m_Ptr += offset;
			return (*this);
		}

		iterator operator+(size_t offset) const
		{
			iterator tmp(*this);
			tmp.m_Ptr += offset;
			return tmp;
		}

		iterator operator-(size_t offset) const
		{
			iterator tmp(*this);
			tmp.m_Ptr += offset;
			return tmp;
		}
		
		T & operator[](size_t offset)
		{
			return (*(*this + offset));
		}

		const T & operator[](size_t offset) const
		{
			return (*(*this + offset));
		}

		bool operator<(const iterator & right) const
		{
			return m_Ptr < right.m_Ptr;
		}

		bool operator>(const iterator & right) const
		{
			return m_Ptr > right.m_Ptr;
		}

		bool operator<=(const iterator & right) const
		{
			return m_Ptr <= right.m_Ptr;
		}

		bool operator>=(const iterator & right) const
		{
			return m_Ptr >= right.m_Ptr;
		}

		size_t operator-(const iterator & right) const
		{
			return m_Ptr - right.m_Ptr;
		}
	};

	// constructors / destructors
	CVector<T>()
	{
		m_Size = 0;
		m_CurrentUsedSize = 0;
		m_Data = NULL;
	}

	CVector<T>(const CVector<T> & other)
	{
		// copy data
		m_Data = new T [other.m_Size];
		m_Size = other.m_Size;
		m_CurrentUsedSize = other.m_CurrentUsedSize;
		memcpy(m_Data, other.m_Data, m_CurrentUsedSize * sizeof(T));
	}

	~CVector<T>()
	{
		clear();
	}

	// interface
	size_t size() const
	{
		return m_CurrentUsedSize;
	}

	size_t capacity() const
	{
		return m_Size;
	}

	iterator begin()
	{
		return iterator(m_Data);
	}

	iterator end()
	{
		return iterator(m_Data + m_CurrentUsedSize);
	}

	iterator iterAt(size_t pos)
	{
		if (pos > m_CurrentUsedSize)
			assert(0);
		return iterator(m_Data + pos);
	}

	bool reserve(size_t newSize)
	{
		return ChangeSize(newSize);
	}

	bool push_back(const T & elem)
	{
		++m_CurrentUsedSize;
		if (!GrowIfNeeded())
		{
			--m_CurrentUsedSize;
			return false;
		}

		m_Data[m_CurrentUsedSize - 1] = elem;
		return true;
	}

	void pop_back()
	{
		--m_CurrentUsedSize;
		if (m_CurrentUsedSize < 0)
			m_CurrentUsedSize = 0;
		// :TODO: free memory sometimes
	}

	bool resize(size_t newSize)
	{
		if (!ChangeSize(newSize))
			return false;
		FreeMemIfPossible();
		return true;
	}

	bool empty() const
	{
		return (m_CurrentUsedSize == 0);
	}

	T & at(size_t pos)
	{
		if (pos > m_CurrentUsedSize)
		{
			assert(0);
		}
		return m_Data[pos];
	}

	const  T & at(size_t pos) const
	{
		if (pos > m_CurrentUsedSize)
		{
			assert(0);
		}
		return m_Data[pos];
	}

	T & operator[](size_t pos)
	{
		return at(pos);
	}

	const T & operator[](size_t pos) const
	{
		return at(pos);
	}

	T & front()
	{
		if (m_CurrentUsedSize < 1)
		{
			assert(0);
		}
		return m_Data[0];
	}

	const T & front() const
	{
		if (m_CurrentUsedSize < 1)
		{
			assert(0);
		}
		return m_Data[0];
	}

	T & back()
	{
		if (m_CurrentUsedSize < 1)
		{
			assert(0);
		}
		return m_Data[m_CurrentUsedSize - 1];
	}

	const T & back() const
	{
		if (m_CurrentUsedSize < 1)
		{
			assert(0);
		}
		return m_Data[m_CurrentUsedSize - 1];
	}

	bool insert(iterator where, const T & value)
	{
		// we have to insert before
		// if it is begin, don't decrement
		if (where != m_Data)
			--where;
		// validate iter
		if (where < m_Data || where >= (m_Data + m_CurrentUsedSize))
			return false;

		++m_CurrentUsedSize;
		if (!GrowIfNeeded())
		{
			--m_CurrentUsedSize;
			return false;
		}

		memmove(where.base() + 1, where.base(), m_CurrentUsedSize - (where - m_Data));
		memcpy(where.base(), &value, sizeof(T));
		return true;
	}

	void erase(iterator where)
	{
		// validate iter
		if (where < m_Data || where >= (m_Data + m_CurrentUsedSize))
			return false;

		if (m_CurrentUsedSize > 1)
		{
			// move
			memmove(where.base(), where.base() + 1, m_CurrentUsedSize - 1);
		}

		--m_CurrentUsedSize;
		// :TODO: free memory sometimes
	}

	void clear()
	{
		m_Size = 0;
		m_CurrentUsedSize = 0;
		delete [] m_Data;
		m_Data = NULL;
	}
};

#endif // __CVECTOR_H__

