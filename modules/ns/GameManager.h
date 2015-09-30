// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

/* This file includes game-related stuff, such as message IDs
 * and forwards
 */
#ifndef GAMEMANAGER_H
#define GAMEMANAGER_H

#include <amtl/am-string.h>

class GameManager
{
private:
	// Basic game variables
	int				 m_IsCombat;

	int				 m_HudText2; // Message IDS
	int				 m_SetFOV;

	// Forwards
	int				 m_ChangeclassForward;
	int				 m_BuiltForward;
	int				 m_SpawnForward;
	int				 m_TeamForward;
	int				 m_RoundStartForward;
	int				 m_RoundEndForward;
	int				 m_MapResetForward;

	REAL			 m_fRoundStartTime;  // Time the match should start
	int				 m_RoundInProgress;  // Whether or not there is a match in progress

	edict_t			*m_SavedEdict;		 // saved edict for client_built

	int				 m_iTitlesMap;
	
	bool			 m_SendMapReset;

public:
	GameManager()
	{
		m_SendMapReset = true;
		m_IsCombat=0;
		m_HudText2=0;
		m_SetFOV=0;

		m_SavedEdict=NULL;

		m_RoundInProgress=0;

		ResetForwards();
	};
	inline void CheckMap(void)
	{
		ke::AString MapName;

		MapName = UTIL_ToLowerCase(STRING(gpGlobals->mapname));

		m_iTitlesMap=0;

		if (MapName.compare("ns_bast")==0 ||
			MapName.compare("ns_bast_classic") == 0 ||
			MapName.compare("ns_hera") == 0 ||
			MapName.compare("ns_nothing") == 0 ||
			MapName.compare("ns_caged") == 0 ||
			MapName.compare("ns_tanith") == 0 ||
			MapName.compare("ns_eclipse") == 0 ||
			MapName.compare("ns_veil") == 0 ||
			MapName.compare("ns_nancy") == 0)
		{
			m_iTitlesMap=1;
		}


	};
	inline int &TitleMap(void)
	{
		return m_iTitlesMap;
	};
	inline void SetCombat(int value)
	{
		m_IsCombat=value;
	};
	inline int &IsCombat(void)
	{
		return m_IsCombat;
	};

	inline void UpdateHudText2(void)
	{
		if (!m_HudText2)
		{
			GetMessageIDs();
		}
	};
	inline void UpdateSetFOV(void)
	{
		if (!m_SetFOV)
		{
			GetMessageIDs();
		}
	};
	inline int &GetHudText2(void)
	{
		return m_HudText2;
	};
	inline int &GetSetFOV(void)
	{
		return m_SetFOV;
	};
	inline void GetMessageIDs(void)
	{
		m_HudText2=GET_USER_MSG_ID(&Plugin_info,"HudText2",NULL);
		m_SetFOV=GET_USER_MSG_ID(&Plugin_info,"SetFOV",NULL);
	};

	inline void EvaluateCombat(void)
	{
		const char *Map=STRING(gpGlobals->mapname);

		// if map starts with co_ it is combat
		// otherwise it is classic gameplay
		if ((Map[0]=='c' || Map[0]=='C') &&
			(Map[1]=='o' || Map[1]=='O') &&
			(Map[2]=='_'))
		{
			SetCombat(1);
			return;
		}

		SetCombat(0);
	};

	inline void ResetForwards(void)
	{
		m_ChangeclassForward=-1;
		m_BuiltForward=-1;
		m_SpawnForward=-1;
		m_TeamForward=-1;
		m_RoundStartForward=-1;
		m_RoundEndForward=-1;
		m_MapResetForward=-1;
	};
	inline void RegisterForwards(void)
	{
		ResetForwards();


		m_SpawnForward = MF_RegisterForward("client_spawn",ET_IGNORE,FP_CELL/*id*/,FP_DONE);
		m_TeamForward = MF_RegisterForward("client_changeteam",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*new team*/,FP_CELL/*old team*/,FP_DONE);
		m_ChangeclassForward = MF_RegisterForward("client_changeclass", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		m_RoundStartForward = MF_RegisterForward("round_start", ET_IGNORE, FP_DONE);
		m_RoundEndForward = MF_RegisterForward("round_end", ET_IGNORE, FP_FLOAT, FP_DONE);
		m_MapResetForward = MF_RegisterForward("map_reset", ET_IGNORE, FP_CELL, FP_DONE);
		

	};

	inline void StartFrame(void)
	{
		if (gpGlobals->time >= m_fRoundStartTime)
		{
			g_pFunctionTable->pfnStartFrame=NULL;
			RoundStart();
		}
	};

	/**
	 * This is called from MessageHandler's Countdown
	 * hook.  It passes the only byte sent (time)
	 */
	inline void HandleCountdown(int &Time)
	{
		// Begin hooking start frame
		g_pFunctionTable->pfnStartFrame=::StartFrame;

		// set time of round start
		m_fRoundStartTime=gpGlobals->time + Time;
		
		m_SendMapReset = true;
	};

	/**
	 * This is called from MessageHandler's GameStatus
	 * hook.  It passes the first byte sent.
	 * 2 = Round End
	 */
	inline void HandleGameStatus(int &FirstByte)
	{
		switch (FirstByte)
		{
		case 0:
			if (m_SendMapReset)
			{
				MF_ExecuteForward(m_MapResetForward, 0);
			}
			m_SendMapReset = false;
			break;
			
		case 1:
			MF_ExecuteForward(m_MapResetForward, 1);
			break;
			
		case 2:
			RoundEnd();
			break;
		}
	};

	inline void RoundStart()
	{
		m_RoundInProgress=1;
		MF_ExecuteForward(m_RoundStartForward);
	};
	inline void RoundEnd()
	{
		m_RoundInProgress=0;

		MF_ExecuteForward(m_RoundEndForward,gpGlobals->time - m_fRoundStartTime);
	};
	inline int &RoundInProgress()
	{
		return m_RoundInProgress;
	};
	// no need to check -1 forwards in these
	// amxmodx checks it anyway
	inline void ExecuteClientBuilt(int &PlayerID, int StructureID, int &StructureType, int &Impulse)
	{
		MF_ExecuteForward(m_BuiltForward,PlayerID, StructureID, StructureType, Impulse);
	};
	inline void ExecuteClientSpawn(int &PlayerID)
	{
		MF_ExecuteForward(m_SpawnForward,PlayerID);
	};
	inline void ExecuteClientChangeTeam(int &PlayerID, int &NewTeam, int &OldTeam)
	{
		MF_ExecuteForward(m_TeamForward, PlayerID, NewTeam, OldTeam);
	};
	inline void ExecuteClientChangeClass(int &PlayerID, int &NewClass, int &OldClass)
	{
		MF_ExecuteForward(m_ChangeclassForward,PlayerID,NewClass,OldClass);
	};
	inline void ExecuteRoundStart(void)
	{
		MF_ExecuteForward(m_RoundStartForward);
	};
	inline void ExecuteRoundEnd(void)
	{
		MF_ExecuteForward(m_RoundEndForward);
	};
	

	/**
	 * The next few functions tell the module what metamod hooks
	 * i need.  This tries to run-time optimize out what we 
	 * do not need.
	 */
	void HookPreThink(void);
	void HookPostThink_Post(void);
	void HookPreThink_Post(void);
	void HookUpdateClientData(void);
	void HookLogs(void); // AlertMessage AND CreateNamedEntity

	inline void CheckAllHooks(void)
	{
		HookPreThink();
		HookPostThink_Post();
		HookPreThink_Post();
		HookUpdateClientData();
		HookLogs();
	};
	
	inline void SetTemporaryEdict(edict_t *Edict)
	{
		m_SavedEdict=Edict;
	};
	inline edict_t *GetTemporaryEdict(void)
	{
		return m_SavedEdict;
	};

};

extern GameManager GameMan;

#endif // GAMEMANAGER_H
