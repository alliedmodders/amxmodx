// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fun Module
//

#pragma once

#include <amxxmodule.h>

// Fun-specific defines below
#define GETCLIENTLISTENING		(*g_engfuncs.pfnVoice_GetClientListening)
#define SETCLIENTLISTENING		(*g_engfuncs.pfnVoice_SetClientListening)
#define SETCLIENTMAXSPEED		(*g_engfuncs.pfnSetClientMaxspeed)
#define GETPLAYERAUTHID			(*g_engfuncs.pfnGetPlayerAuthId)
#define	SF_NORESPAWN			(1 << 30)// !!!set this bit on guns and stuff that should never respawn.
#define STANDARDTIMESTEPSOUND	400

#define HITGROUP_GENERIC		0 // none
#define HITGROUP_HEAD			1 // 1 << 1 = 2
#define HITGROUP_CHEST			2 // 1 << 2 = 4
#define HITGROUP_STOMACH		3 // 8
#define HITGROUP_LEFTARM		4 // 16
#define HITGROUP_RIGHTARM		5 // 32
#define HITGROUP_LEFTLEG		6 // 64
#define HITGROUP_RIGHTLEG		7 // 128
#define HITGROUP_MAX            8

extern DLL_FUNCTIONS *g_pFunctionTable;
extern enginefuncs_t *g_pengfuncsTable_Post;

void PlayerPreThink(edict_t *pEntity);
void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *shooter, TraceResult *ptr);

static const auto kHitGroupsBits = (1 << HITGROUP_MAX) - 1;
static const auto kMaxClients = 32u;

class CPlayer
{
	public:

		CPlayer()
		{
			Clear();
		}

	public:

		bool HasBodyHits() const
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				if (GetBodyHits(i) != kHitGroupsBits)
				{
					return true;
				}
			}

			return false;
		}

		int GetBodyHits(const int other) const
		{
			return bodyHits_[other];
		}

		void SetBodyHits(const int other, const int flags)
		{
			bodyHits_[other] = flags;
		}

		void SetBodyHits(const int flags)
		{
			memset(bodyHits_, flags, sizeof bodyHits_);
		}

	public:

		bool HasSilentFootsteps() const
		{
			return silentFootsteps_;
		}

		void SetSilentFootsteps(const bool state)
		{
			silentFootsteps_ = state;
		}

	public:

		void Clear()
		{
			SetBodyHits(kHitGroupsBits);
			SetSilentFootsteps(false);
		}

	private:

		int  bodyHits_[kMaxClients + 1];
		bool silentFootsteps_ {};
};

class CPlayers
{
	using Internal = CPlayer;

	public:

		bool HaveBodyHits() const
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				if (players_[i].HasBodyHits())
				{
					return true;
				}
			}

			return false;
		}

		void SetBodyHits(const int attacker, const int target, const int flags)
		{
			players_[attacker].SetBodyHits(target, flags);
		}

		void SetTargetsBodyHits(const int attacker, const int flags)
		{
			players_[attacker].SetBodyHits(flags);
		}

		void SetAttackersBodyHits(const int target, const int flags)
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				players_[i].SetBodyHits(target, flags);
			}
		}

		void SetEveryoneBodyHits(const int flags)
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				players_[i].SetBodyHits(flags);
			}
		}

	public:

		bool HaveSilentFootsteps() const
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				if (players_[i].HasSilentFootsteps())
				{
					return true;
				}
			}

			return false;
		}

	public:

		void Clear()
		{
			for (auto i = 1; i <= gpGlobals->maxClients; ++i)
			{
				players_[i].Clear();
			}
		}

	public:

		Internal& operator [](const size_t index)
		{
			return players_[index];
		}

		const Internal& operator [](const size_t index) const
		{
			return players_[index];
		}

	private:

		Internal players_[kMaxClients + 1];
};

#define CHECK_ENTITY(x) \
	if ((x) < 0 || (x) > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if ((x) <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if ((x) != 0 && FNullEnt(TypeConversion.id_to_edict(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}

#define CHECK_PLAYER(x) \
	if ((x) < 1 || (x) > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
	} else { \
		if (!MF_IsPlayerIngame(x) || FNullEnt(TypeConversion.id_to_edict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", x); \
			return 0; \
		} \
	}
