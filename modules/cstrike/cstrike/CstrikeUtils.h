// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#ifndef CSTRIKE_UTILS_H
#define CSTRIKE_UTILS_H

bool UTIL_IsPlayer(edict_t *pPlayer);
void UTIL_TextMsg_Generic(edict_t* pPlayer, const char* message);
bool UTIL_CheckForPublic(const char *publicname);

#define GETINFOKEYBUFFER	(*g_engfuncs.pfnGetInfoKeyBuffer)
#define	SETCLIENTKEYVALUE	(*g_engfuncs.pfnSetClientKeyValue)
#define GETCLIENTKEYVALUE	(*g_engfuncs.pfnInfoKeyValue)
#define CREATENAMEDENTITY	(*g_engfuncs.pfnCreateNamedEntity)

#define CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(INDEXENT(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}

#define CHECK_PLAYER(x) \
	if (x < 1 || x > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
	} else { \
		if (!MF_IsPlayerIngame(x) || FNullEnt(MF_GetPlayerEdict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", x); \
			return 0; \
		} \
	}

#define CHECK_NONPLAYER(x) \
	if (x < 1 || x <= gpGlobals->maxClients || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Non-player entity %d out of range", x); \
		return 0; \
	} else { \
		if (FNullEnt(INDEXENT(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid non-player entity %d", x); \
			return 0; \
		} \
	}

#define CHECK_HOSTAGE(x)                                                                                                   \
	if (strcmp(STRING(x->v.classname), "hostage_entity") != 0 && strcmp(STRING(x->v.classname), "monster_scientist") != 0) \
	{                                                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(x->v.classname));            \
		return 0;                                                                                                          \
	}

#define GETEDICT(n) \
	((n >= 1 && n <= gpGlobals->maxClients) ? MF_GetPlayerEdict(n) : INDEXENT(n))


#define GET_OFFSET(classname, member)												\
	static int member = -1;															\
	if (member == -1)																\
	{																				\
		if (!CommonConfig->GetOffsetByClass(classname, #member, &member) || member < 0)\
		{																			\
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid %s offset. Native %s is disabled", #member, __FUNCTION__);\
			return 0;																\
		}																			\
	}

#define GET_OFFSET_NO_ERROR(classname, member)										\
	static int member = -1;															\
	if (member == -1)																\
	{																				\
		if (!CommonConfig->GetOffsetByClass(classname, #member, &member) || member < 0)\
		{																			\
			return;																	\
		}																			\
	}

template <typename T>
inline T& get_pdata(edict_t *pEntity, int offset, int element = 0)
{
	return *reinterpret_cast<T*>(reinterpret_cast<int8*>(pEntity->pvPrivateData) + offset + element * sizeof(T));
}

template <typename T>
inline T& get_pdata(void *pEntity, int offset, int element = 0)
{
	return *reinterpret_cast<T*>(reinterpret_cast<int8*>(pEntity) + offset + element * sizeof(T));
}

template <typename T>
inline void set_pdata(edict_t *pEntity, int offset, T value, int element = 0)
{
	*reinterpret_cast<T*>(reinterpret_cast<int8*>(pEntity->pvPrivateData) + offset + element * sizeof(T)) = value;
}

template <typename T>
inline void set_pdata(void *pEntity, int offset, T value, int element = 0)
{
	*reinterpret_cast<T*>(reinterpret_cast<int8*>(pEntity) + offset + element * sizeof(T)) = value;
}

class EHANDLE
{
	private:

		edict_t* m_pent;
		int		 m_serialnumber;

	public:

		edict_t* Get(void)
		{
			if (!FNullEnt(m_pent))
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
			if (!FNullEnt(pent))
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

class CUnifiedSignals
{
	public:

		void Update(void)
		{
			m_flState = m_flSignal;
			m_flSignal = 0;
		}

		void Signal(int flags) 
		{
			m_flSignal |= flags;
		}

		int GetSignal(void) 
		{
			return m_flSignal;
		}

		int GetState(void) 
		{
			return m_flState;
		}

	private:

		int m_flSignal;
		int m_flState;
};

#endif // CSTRIKE_UTILS_H
