//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _HL_CONVERSION_TYPE_H_
#define _HL_CONVERSION_TYPE_H_

#include <stddef.h>   // size_t
#include <extdll.h>   // edict_t, etc.
#include <sdk_util.h> // FNullEnt, INDEXENT, etc.

template <typename T> static inline T& ref_pdata(void *pPrivateData, int offset, int element = 0)
{
	return *reinterpret_cast<T*>((reinterpret_cast<int8*>(pPrivateData) + offset + (element * sizeof(T))));
}


template <typename T> inline T get_pdata_direct(void *pPrivateData, int offset, int element = 0, int size = 0)
{
	return reinterpret_cast<T>(reinterpret_cast<int8*>(pPrivateData) + offset + (element * size));
}

template <typename T> inline T get_pdata_direct(edict_t *pEntity, int offset, int element = 0, int size = 0)
{
	return get_pdata_direct<T>(pEntity->pvPrivateData, offset, element, size);
}


template <typename T> inline T& get_pdata(void *pPrivateData, int offset, int element = 0)
{
	return ref_pdata<T>(pPrivateData, offset, element);
}

template <typename T> inline T& get_pdata(edict_t *pEntity, int offset, int element = 0)
{
	return get_pdata<T>(pEntity->pvPrivateData, offset, element);
}


template <typename T> inline void set_pdata(void *pPrivateData, int offset, T value, int element = 0)
{
	ref_pdata<T>(pPrivateData, offset, element) = value;
}

template <typename T>inline void set_pdata(edict_t *pEntity, int offset, T value, int element = 0)
{
	set_pdata<T>(pEntity->pvPrivateData, offset, value, element);
}


extern globalvars_t *gpGlobals;

class HLTypeConversion
{
	public:

		HLTypeConversion() : m_FirstEdict(nullptr), m_PevOffset(0)
		{}

	public:

		void init()
		{
			if (!m_FirstEdict)
			{
				m_FirstEdict = INDEXENT(0);

				search_pev();
			}
		}

	public: // Edict -> Index

		int edict_to_id(edict_t *pEdict)
		{
			if (!pEdict)
			{
				return -1;
			}

			return static_cast<int>(pEdict - m_FirstEdict);
		}

	public: // Entvars -> Edict/Index

		edict_t* entvar_to_edict(entvars_t *pev)
		{
			if (!pev || !(pev->pContainingEntity))
			{
				return nullptr;
			}

			return pev->pContainingEntity;
		}

		int entvars_to_id(entvars_t *pev)
		{
			return edict_to_id(entvar_to_edict(pev));
		}

	public: // Index -> CBase*/Edict/Entvars

		void* id_to_cbase(int index)
		{
			auto pEdict = id_to_edict(index);
			return pEdict ? pEdict->pvPrivateData : nullptr;
		}

		edict_t* id_to_edict(int index)
		{
			if (index < 0 || index >= gpGlobals->maxEntities)
			{
				return nullptr;
			}

			if (!index)
			{
				return m_FirstEdict;
			}

			auto pEdict = static_cast<edict_t*>(m_FirstEdict + index);

			if (pEdict && (pEdict->free || (index > gpGlobals->maxClients && !pEdict->pvPrivateData)))
			{
				return nullptr;
			}

			return pEdict;
		}

		entvars_t* id_to_entvars(int index)
		{
			auto pEdict = id_to_edict(index);
			return pEdict ? VARS(pEdict) : nullptr;
		}

	public: // CBase* -> Entvars/Index

		entvars_t* cbase_to_entvar(void *cbase)
		{
			if (!cbase)
			{
				return nullptr;
			}

			return get_pdata<entvars_t*>(cbase, m_PevOffset);
		}

		edict_t* cbase_to_edict(void *cbase)
		{
			return entvar_to_edict(cbase_to_entvar(cbase));
		}

		int cbase_to_id(void *cbase)
		{
			return entvars_to_id(cbase_to_entvar(cbase));
		}

	public:

		size_t get_pev()
		{
			return m_PevOffset;
		}

	private:

		void search_pev()
		{
			auto pev = VARS(m_FirstEdict);
			auto privateData = reinterpret_cast<byte*>(m_FirstEdict->pvPrivateData);

			for (size_t i = 0; i < 0xFFF; ++i)
			{
				if (*reinterpret_cast<entvars_t**>(privateData + i) == pev)
				{
					m_PevOffset = i;
					return;
				}
			}

			m_PevOffset = 0; // This should not happen.
		}

	private:

		edict_t* m_FirstEdict;
		size_t   m_PevOffset;
};

class EHANDLE
{
	private:

		edict_t* m_pent;
		int		 m_serialnumber;

	public:

		edict_t* Get(void)
		{
			if (m_pent)
			{
				if (m_pent->serialnumber == m_serialnumber)
				{
					return m_pent;
				}

				return nullptr;
			}

			return nullptr;
		};

		edict_t* Set(edict_t *pent)
		{
			if (pent)
			{
				m_pent = pent;
				m_serialnumber = m_pent->serialnumber;
			}
			else
			{
				m_pent = nullptr;
				m_serialnumber = 0;
			}

			return pent;
		};
};

#endif // _HL_CONVERSION_TYPE_H_
