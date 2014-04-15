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

class CellVector
{
private:
	cell*  data;       // allocated with malloc
	size_t cellcount;  // how many cells per element
	size_t cursize;    // current size of the vector (maximum elements)
	size_t count;      // how many units of the vector are in use
	
	bool GrowIfNeeded(size_t howmany)
	{
		/* Shortcut out if we can store this */
		if (count + howmany <= cursize)
		{
			return true;
		}

		/* Set a base allocation size of 8 items */
		if (!cursize)
		{
			cursize = 8;
		}

		/* If it's not enough, keep doubling */
		while (count + howmany > cursize)
		{
			cursize *= 2;
		}

		if (data)
		{
			data = (cell*)realloc(data, (sizeof(cell)* cellcount) * cursize);
		}
		else
		{
			data = (cell*)malloc((sizeof(cell)* cellcount) * cursize);
		}

		return (data != NULL);
	};

public:
	CellVector(): data(NULL), cellcount(0), cursize(0), count(0)
	{
	};
	CellVector(int cellsize): data(NULL), cellcount(cellsize), cursize(0), count(0)
	{
	};
	~CellVector()
	{
		if (data)
		{
			free(data);
		}
	};
	size_t GetCellCount()
	{
		return cellcount;
	};
	void Grow(size_t howmany)
	{
		cursize+=howmany;
		if (data)
		{
			data=(cell*)realloc(data, (sizeof(cell) * cellcount) * cursize);
		}
		else
		{
			data=(cell*)malloc((sizeof(cell) * cellcount) * cursize);
		}
	};
	void FreeUnused(void)
	{
		if (cursize != count &&
			data != NULL)
		{
			cursize=count;
			data=(cell*)realloc(data, cursize * (sizeof(cell) * cellcount));
		}
	};
	// Returns 1 on success
	// 0 on out of bounds.
	int GetArray(size_t which, cell* output)
	{
		// make sure it is in bounds.
		if (which >= count)
		{
			return 0;
		}
		// align output data
		cell* out=data + (cellcount * which);
		
		memcpy(output, out, sizeof(cell) * cellcount);

		return 1;
	};
	// Returns 1 on success
	// 0 on out of bounds
	int GetCell(size_t which, cell* output)
	{
		// check bounds
		if (which >= count)
		{
			return 0;
		}
		*output=*(data + (cellcount * which));

		return 1;
	}
	// Returns 1 on success
	// 0 on out of bounds
	int GetString(size_t which, cell* output, size_t size)
	{
		// check bounds
		if (which >= count)
		{
			return 0;
		}
		cell* out=data + (cellcount * which);

		size_t count=cellcount;

		while (size-- && 
			count-- &&
			(*output++=*out++)!='\0')
			/* do nothing */ ;

		// If size is zero here, then the string was never null terminated.
		if (size==0)
		{
			*out='\0';
		}

		return 1;
	}
	// Returns 1 on success
	// 0 on out of bounds
	int SetArray(size_t which, cell* output)
	{
		if (which >= count)
		{
			return 0;
		}
		// align output
		cell* out=data + (cellcount * which);

		memcpy(out, output, sizeof(cell) * cellcount);

		return 1;
	};
	// Returns 1 on success
	// 0 on out of bounds
	int SetCell(size_t which, cell output)
	{
		if (which >= count)
		{
			return 0;
		}
		// align output
		*(data + (cellcount * which))=output;

		return 1;
	};
	// Returns 1 on success
	// 0 on out of bounds
	int SetString(size_t which, cell* output)
	{
		if (which >= count)
		{
			return 0;
		}
		// align output
		cell* out=data + (cellcount * which);

		memcpy(out, output, sizeof(cell) * cellcount);

		// now force a null terminator on the last entry.
		out+=(cellcount - 1);
		*out='\0';

		return 1;
	};
	int Push()
	{
		if (count >= cursize)
		{
			// Grow in 8s to cause less reallocation
			this->Grow(8);
		};
		
		this->count++;

		return this->count-1;
	};
	int Size() 
	{
		return this->count;
	};

	bool Resize(size_t newsize)
	{
		if (newsize <= count)
		{
			count = newsize;
			return true;
		}

		if (!GrowIfNeeded(newsize - count))
		{
			return false;
		}

		count = newsize;
		return true;
	}

	CellVector *Clone()
	{
		CellVector *array = new CellVector(cellcount);
		array->count = count;
		array->cursize = cursize;
		array->data = (cell *)malloc((sizeof(cell)* cellcount) * cursize);
		memcpy(array->data, data, (sizeof(cell)* cellcount) * count);
		return array;
	}

	void Clear()
	{
		free(data);
		data=(cell*)malloc(sizeof(cell) * cellcount);
		cursize=1;
		count=0;
	};
	cell* Base()
	{
		return data;
	}
	cell* GetCellPointer(size_t which)
	{
		if (which >= count)
		{
			return NULL;
		}
		return data + (which * cellcount);
	};
	// Shifts all items from this item, and including this item up 1.
	int ShiftUpFrom(size_t which)
	{
		// No point shifting this.
		if (which > this->count)
		{

			return 0;
		}
		// First make a new item.
		this->Push();

		// If we got an InsertAfter(lastitem), then which will equal this->count - 1
		// all we needed to do was Push()
		if (which == this->count || 
			which == this->count - 1)
		{
			return 1;
		}

		// Allocate a temporary buffer to store data in
		size_t tempbuffsize=(sizeof(cell) * cellcount) * (this->count - 1 - which);

		cell* temp=(cell*)malloc(tempbuffsize); 

		// Copy old data to temp buffer
		memcpy(temp, GetCellPointer(which), tempbuffsize);

		// Now copy temp buffer to adjusted location
		memcpy(GetCellPointer(which+1), temp, tempbuffsize);

		// cleanup
		free(temp);

		return 1;

	};
	// Shifts all items from this item, and including this item down 1.
	// This deletes the item specified.
	int Delete(size_t which)
	{
		// No point shifting this.
		if (which >= this->count)
		{
			return 0;
		}


		for (size_t i=which; i<this->count - 1; i++)
		{
			memcpy(GetCellPointer(i), GetCellPointer(i + 1), sizeof(cell) * cellcount);
		}
		this->count--;
		return 1;
	};
	int Swap(size_t item1, size_t item2)
	{
		if (item1 >= this->count ||
			item2 >= this->count)
		{
			return 0;
		}

		// Make a temp buffer to store item2
		cell* temp=(cell*)malloc(sizeof(cell) * cellcount);
		memcpy(temp, GetCellPointer(item2), sizeof(cell) * cellcount);
		
		// copy item1 to item2
		memcpy(GetCellPointer(item2), GetCellPointer(item1), sizeof(cell) * cellcount);

		// copy item2 to item1
		memcpy(GetCellPointer(item1), temp, sizeof(cell) * cellcount);

		// Cleanup
		free(temp);

		return 1;
	};

};

extern CVector<CellVector*> VectorHolder;


inline CellVector* HandleToVector(AMX* amx, int handle)
{
	if (handle <= 0 ||
		handle > (int)VectorHolder.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", handle);

		return NULL;
	}

	CellVector* ret=VectorHolder[handle-1];

	if (ret == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", handle);

		return NULL;
	}

	return ret;
}


#endif
