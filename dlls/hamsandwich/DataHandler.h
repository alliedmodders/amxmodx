/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#ifndef RETURNHANDLER_H
#define RETURNHANDLER_H

#include "ham_utils.h" 
#include "CVector.h"
#include "CString.h"
#include "sh_stack.h"

enum
{
	RET_VOID,
	RET_INTEGER,
	RET_FLOAT,
	RET_VECTOR,
	RET_STRING,
	RET_CBASE,
	RET_ENTVAR,
	RET_TRACE,
	RET_ITEMINFO
};
// Container for return and parameter data.
// Contains a void pointer, and a flag telling what it contains.
class Data
{
private:
	void		*m_data;
	int			*m_index;
	int			 m_type;

	bool IsSet(void)
	{
		return (m_type != RET_VOID &&
				m_data != NULL);
	};
	bool IsType(const int type)
	{
		return (m_type == type);
	};

public:
	Data() : m_data(NULL), m_index(NULL), m_type(RET_VOID)
	{ /* nothing */	};

	Data(int type, void *ptr) : m_data(ptr), m_index(NULL), m_type(type)
	{ /* nothing */ };

	Data(int type, void *ptr, int *cptr) : m_data(ptr), m_index(NULL), m_type(type)
	{ /* nothing */ };

	~Data()
	{ /* nothing */	};

	int GetType()
	{
		return m_type;
	};

	// All Get/Set value natives return < 0 on failure.
	// -1: Wrong type
	// -2: Bad data pointer (void, etc).
	int SetInt(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (IsType(RET_INTEGER))
		{
			*(reinterpret_cast<int *>(m_data))=*data;
			return 0;
		}
		else if (IsType(RET_TRACE))
		{
			*(reinterpret_cast<int *>(m_data))=*data;
			return 0;
		}

		return -1;
	};

	int SetFloat(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_FLOAT))
		{
			return -1;
		}
		*(reinterpret_cast<REAL *>(m_data))=amx_ctof2(*data);

		return 0;
	};
	int SetVector(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_VECTOR))
		{
			return -1;
		}
		Vector *vec=reinterpret_cast<Vector *>(m_data);

		vec->x=amx_ctof2(data[0]);
		vec->y=amx_ctof2(data[1]);
		vec->z=amx_ctof2(data[2]);

		return 0;
	};
	int SetString(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_STRING))
		{
			return -1;
		}

		String *str=reinterpret_cast<String *>(m_data);

		cell *i=data;
		size_t len=0;

		while (*i!=0)
		{
			i++;
			len++;
		};
		char *temp=new char[len+1];
		i=data;
		char *j=temp;

		while ((*j++=*i++)!=0)
		{
			/* nothing */
		}

		str->assign(temp);

		delete[] temp;

		return 0;
	};

	int SetEntity(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (IsType(RET_CBASE))
		{
			*(reinterpret_cast<void **>(m_data))=IndexToPrivate(*data);
			if (m_index != 0)
			{
				*m_index=*data;
			}

			return 0;
		}
		else if (IsType(RET_ENTVAR))
		{
			*(reinterpret_cast<entvars_t **>(m_data))=IndexToEntvar(*data);
			if (m_index != 0)
			{
				*m_index=*data;
			}

			return 0;
		}
		return -1;
	};

	int GetInt(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (IsType(RET_INTEGER))
		{
			*data=*(reinterpret_cast<int *>(m_data));

			return 0;
		}
		else if (IsType(RET_TRACE))
		{
			*data=*(reinterpret_cast<int *>(m_data));

			return 0;
		}

		return -1;
	};
	int GetFloat(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_FLOAT))
		{
			return -1;
		}
		*data=amx_ftoc2(*(reinterpret_cast<REAL *>(m_data)));

		return 0;
	};
	int GetVector(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_VECTOR))
		{
			return -1;
		}
		Vector *vec=reinterpret_cast<Vector *>(m_data);
		data[0]=amx_ftoc2(vec->x);
		data[1]=amx_ftoc2(vec->y);
		data[2]=amx_ftoc2(vec->z);

		return 0;
	};
	int GetString(cell *data, int len)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (!IsType(RET_STRING))
		{
			return -1;
		}
		const char *i=(reinterpret_cast<String *>(m_data)->c_str());

		while (len-- && 
			  (*data++=*i++)!='\0')
		{
			/* nothing */
		};
		return 0;
	};
	int GetEntity(cell *data)
	{
		if (!IsSet())
		{
			return -2;
		}
		if (IsType(RET_CBASE))
		{
			*data=PrivateToIndex(m_data);

			return 0;
		}
		else if (IsType(RET_ENTVAR))
		{
			*data=EntvarToIndex(reinterpret_cast<entvars_t *>(m_data));

			return 0;
		}
		return -1;
	}
};

extern CStack< Data * > ReturnStack;
extern CStack< Data * > OrigReturnStack;
extern CStack< CVector< Data * > * > ParamStack;
extern CStack< int * > ReturnStatus;
#endif
