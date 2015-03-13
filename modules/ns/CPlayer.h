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

/* This is the basic data stored for players */
#ifndef CPLAYER_H
#define CPLAYER_H

#include <string.h>

#include "GameManager.h"

class CPlayer
{
private:
	edict_t			*m_pEdict;
	entvars_t		*m_pev;

	int				 m_iIndex;

	int				 m_iIsBot;
	int				 m_iIsConnected;

	int				 m_iOldTeam;
	int				 m_iOldImpulse;
	int				 m_iOldDeadFlag;

	int				 m_iSpeedChange;
	REAL			 m_fMaxSpeed;

	int				 m_iClass;

	int				 m_iHasCustomModel;
	int				 m_iHasCustomSkin;
	int				 m_iHasCustomBody;

	char			 m_szModel[128];
	int				 m_iSkin;
	int				 m_iBody;

	int				 m_iHasFOV;
	REAL			 m_fFOV;

	int				 m_iInitialized;




public:

	CPlayer()
	{
		memset(this,0x0,sizeof(*this));
	}

	inline void SetEdict(edict_t *Ent)
	{
		m_pEdict=Ent;

		m_pev=&(Ent->v);

		m_iIndex=ENTINDEX_NEW(Ent);
	};

	inline edict_t *GetEdict(void)
	{
		return m_pEdict;
	};
	inline entvars_t *GetPev(void)
	{
		return m_pev;
	};
	inline int IsBot(void)
	{
		return m_iIsBot;
	};
	inline void SetBot(int onoff)
	{
		m_iIsBot=onoff;
	};
	inline int IsConnected(void)
	{
		return m_iIsConnected;
	};

	inline BOOL HasPrivateData(void)
	{
		if (m_pEdict && m_pEdict->pvPrivateData != NULL)
		{
			return TRUE;
		}

		return FALSE;
	};

	inline int IsInitialized()
	{
		return m_iInitialized;
	};
	inline void Initialize()
	{
		if (!IsConnected())
		{
			// This usually means it's a bot...
			// So just emulate connections
			Connect();
			SetBot(1);
		}

		

		m_iInitialized=1;
	};
	inline void PreThink()
	{
		if (!IsInitialized())
		{
			Initialize();
		}
		if (m_iOldTeam != m_pev->team)
		{
			GameMan.ExecuteClientChangeTeam(index(),m_pev->team,m_iOldTeam);
		}


		if (m_iOldDeadFlag && m_pev->deadflag == 0)
		{
			GameMan.ExecuteClientSpawn(index());
		}

		
		int tClass = GetClass();

		if (tClass != m_iClass)
		{
			ChangeClass(tClass);
		}

		m_iOldImpulse=m_pev->impulse;
		m_iOldDeadFlag=m_pev->deadflag;
		m_iOldTeam=m_pev->team;
	};

	inline void PreThink_Post()
	{
		// Trying to incorperate this into PostThink_Post led to really *weird* results (i don't think it was propagated to the client properly).
		// Change the users speed here
		m_fMaxSpeed=m_pev->maxspeed;
		m_pev->maxspeed+=m_iSpeedChange;
	};

	/**
	 * This is only hooked if at least 1 
	 * player has custom skins/models/bodies
	 */
	inline void PostThink_Post()
	{
		if (m_iHasCustomModel)
		{
			SET_MODEL(m_pEdict,m_szModel);
		}
		if (m_iHasCustomSkin)
		{
			m_pev->skin=m_iSkin;
		}
		if (m_iHasCustomBody)
		{
			m_pev->body=m_iBody;
		}
	};

	void ChangeClass(int newclass)
	{
		GameMan.ExecuteClientChangeClass(index(), newclass, m_iClass);

		m_iClass=newclass;
	}
	void Connect()
	{
		m_iIsConnected=1;
		m_iIsBot=0;

		Reset();
	}
	void Disconnect(int scanhooks=0)
	{
		// If this client had any hooks upon disconnect
		// then rescan the hooks to see if we can stop
		// intercepting any of them.
		if (scanhooks!=0)
		{
			if (this->NeedPreThink_Post())
			{
				GameMan.HookPreThink_Post();
			}
			if (this->NeedPostThink_Post())
			{
				GameMan.HookPostThink_Post();
			}
			if (this->NeedUpdateClientData())
			{
				GameMan.HookUpdateClientData();
			}
		}
		m_iIsConnected=0;
		m_iIsBot=0;

		Reset();
	}
	void FullReset()
	{
		memset(this,0x0,sizeof(*this));
	};
	void Reset()
	{
		m_iHasCustomModel=0;
		m_iHasCustomSkin=0;
		m_iHasCustomBody=0;

		m_iOldTeam=0;
		m_iOldDeadFlag=0;
		m_iSpeedChange=0;
		m_iClass=0;
	};

	int GetClass()
	{
	  if (m_pev->deadflag)
	  {
		return CLASS_DEAD;
	  }

	  if (!m_pev->team)
	  {
		return CLASS_NOTEAM;
	  }
	  switch (m_pev->iuser3)
	  {
	  case 1:
		// Light armor marine..
		if (m_pev->iuser4 & MASK_HEAVYARMOR)
		{
		  return CLASS_HEAVY;
		}
		if (m_pev->iuser4 & MASK_JETPACK)
		{
		  return CLASS_JETPACK;
		}

		return CLASS_MARINE;

	  case 2:
		return CLASS_COMMANDER;

	  case 3:
		return CLASS_SKULK;

	  case 4:
		return CLASS_GORGE;

	  case 5:
		return CLASS_LERK;

	  case 6:
		return CLASS_FADE;

	  case 7:
		return CLASS_ONOS;

	  case 8:
		return CLASS_GESTATE;

	  }

	  return CLASS_UNKNOWN;
	};

	inline int &index() 
	{ 
		return m_iIndex; 
	};

	inline int &GetSpeedChange(void)
	{
		return m_iSpeedChange;
	};
	inline int GetMaxSpeed(void)
	{
		return (int)m_fMaxSpeed;
	};
	inline void SetSpeedChange(cell &SpeedChange)
	{
		m_iSpeedChange=SpeedChange;
	};

	inline void SetModel(char *Model)
	{
		if (strcmp(Model,"")!=0)
		{
			PRECACHE_MODEL(Model);
			
			strncpy(m_szModel,Model,sizeof(m_szModel)-1);

			m_iHasCustomModel=1;

		}
		else
		{
			m_iHasCustomModel=0;
		}
	};

	inline void SetSkin(cell &skin)
	{
		if (skin<0)
		{
			m_iHasCustomSkin=0;
			return;
		}

		m_iHasCustomSkin=1;
		m_iSkin=skin;
	};

	inline void SetBody(cell &body)
	{
		if (body<0)
		{
			m_iHasCustomBody=0;
			return;
		}

		m_iHasCustomBody=1;
		m_iBody=body;
	};

	/**
	 * Called during pfnUpdateClientData()
	 * Sets the player's FOV continuously
	 */
	inline void UpdateFOV(void)
	{
		if (m_iHasFOV)
		{
			GetPev()->fov=m_fFOV;
		}
	};

	/**
	 * Called from the native directly.
	 * Changes members, and gets the SetFOV 
	 * message id if needed.
	 */
	inline int SetFOV(REAL &Amount)
	{

		GameMan.UpdateSetFOV();
		if (Amount == 0.0)
		{
			m_iHasFOV=0;
			m_fFOV=0.0;

			MESSAGE_BEGIN(MSG_ONE,GameMan.GetSetFOV(),NULL,GetEdict());
			WRITE_BYTE(0);
			MESSAGE_END();
			return 1;
		}
		if (Amount > 0)
		{
			m_iHasFOV=1;
			m_fFOV=Amount;
			MESSAGE_BEGIN(MSG_ONE,GameMan.GetSetFOV(),NULL,GetEdict());
			WRITE_BYTE((int)Amount);
			MESSAGE_END();
			return 1;
		}
		return 0;

	};

	/**
	 * Returns 1 if this entity needs PreThink_Post hooked
	 * (eg: this entity has custom max speeds)
	 */
	inline int NeedPreThink_Post(void)
	{
		return m_iSpeedChange != 0;
	};

	/**
	 * Returns 1 if this entity needs PostThink_Post
	 * eg: This entity has a custom model/skin/body
	 */
	inline int NeedPostThink_Post(void)
	{
		return (m_iHasCustomModel != 0 || m_iHasCustomSkin != 0 || m_iHasCustomBody != 0);
	};
	/**
	 * Returns 1 if this entity needs UpdateClientData
	 * eg: This entity has a custom FOV set
	 */
	inline int NeedUpdateClientData(void)
	{
		return m_iHasFOV != 0;
	};

};
#endif

