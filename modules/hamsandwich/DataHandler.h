// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#ifndef RETURNHANDLER_H
#define RETURNHANDLER_H

#include "ham_utils.h" 
#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include <sh_stack.h>

enum
{
	RET_VOID,
	RET_BOOL,
	RET_INTEGER,
	RET_SHORT,
	RET_FLOAT,
	RET_VECTOR,
	RET_STRING,
	RET_CBASE,
	RET_ENTVAR,
	RET_EDICT,
	RET_TRACE,
	RET_ITEMINFO
};

typedef struct
{
	int iSlot;
	int iPosition;
	const char *pszAmmo1;
	int iMaxAmmo1;
	const char *pszAmmo2;
	int iMaxAmmo2;
	const char *pszName;
	int iMaxClip;
	int iId;
	int iFlags;
	int iWeight;
}
ItemInfo;

enum
{
	ItemInfo_iSlot,
	ItemInfo_iPosition,
	ItemInfo_pszAmmo1,
	ItemInfo_iMaxAmmo1,
	ItemInfo_pszAmmo2,
	ItemInfo_iMaxAmmo2,
	ItemInfo_pszName,
	ItemInfo_iMaxClip,
	ItemInfo_iId,
	ItemInfo_iFlags,
	ItemInfo_iWeight
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
		else if (IsType(RET_BOOL))
		{
			*(reinterpret_cast<bool *>(m_data)) = *data > 0;
			return 0;
		}
		else if (IsType(RET_SHORT))
		{
			*(reinterpret_cast<short *>(m_data)) = *data;
			return 0;
		}
		else if (IsType(RET_ITEMINFO))
		{
			*(reinterpret_cast<int *>(m_data)) = *data;
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
		*(reinterpret_cast<REAL *>(m_data))=amx_ctof(*data);

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

		vec->x=amx_ctof(data[0]);
		vec->y=amx_ctof(data[1]);
		vec->z=amx_ctof(data[2]);

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

		ke::AString *str=reinterpret_cast<ke::AString *>(m_data);

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

		*str = temp;

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
			*(reinterpret_cast<void **>(m_data))= TypeConversion.id_to_cbase(*data);
			if (m_index != 0)
			{
				*m_index=*data;
			}

			return 0;
		}
		else if (IsType(RET_ENTVAR))
		{
			*(reinterpret_cast<entvars_t **>(m_data))= TypeConversion.id_to_entvars(*data);
			if (m_index != 0)
			{
				*m_index=*data;
			}

			return 0;
		}
		else if (IsType(RET_EDICT))
		{
			*(reinterpret_cast<edict_t **>(m_data)) = TypeConversion.id_to_edict(*data);
			if (m_index != 0)
			{
				*m_index = *data;
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
		else if (IsType(RET_BOOL))
		{
			*data = *(reinterpret_cast<bool *>(m_data));

			return 0;
		}

		else if (IsType(RET_SHORT))
		{
			*data = *(reinterpret_cast<short *>(m_data));

			return 0;
		}
		else if (IsType(RET_ITEMINFO))
		{
			*data = *(reinterpret_cast<int *>(m_data));

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
		*data=amx_ftoc(*(reinterpret_cast<REAL *>(m_data)));

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
		data[0]=amx_ftoc(vec->x);
		data[1]=amx_ftoc(vec->y);
		data[2]=amx_ftoc(vec->z);

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
		const char *i=(reinterpret_cast<ke::AString *>(m_data)->chars());

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
			*data= TypeConversion.cbase_to_id(m_data);

			return 0;
		}
		else if (IsType(RET_ENTVAR))
		{
			*data= TypeConversion.entvars_to_id(reinterpret_cast<entvars_t *>(m_data));

			return 0;
		}
		else if (IsType(RET_EDICT))
		{
			*data = TypeConversion.edict_to_id(reinterpret_cast<edict_t *>(m_data));

			return 0;
		}
		return -1;
	}
};

extern CStack< Data * > ReturnStack;
extern CStack< Data * > OrigReturnStack;
extern CStack< ke::Vector< Data * > * > ParamStack;
extern CStack< int * > ReturnStatus;
#endif
