// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#include "fakemeta_amxx.h"
#include "sh_stack.h"

TraceResult g_tr_2;

KVD_Wrapper g_kvd_glb;
KVD_Wrapper g_kvd_ext;

ke::Vector<KVD_Wrapper *>g_KVDWs;
ke::Vector<KVD_Wrapper *>g_FreeKVDWs;

clientdata_t g_cd_glb;
entity_state_t g_es_glb;
usercmd_t g_uc_glb;

static cell AMX_NATIVE_CALL set_tr2(AMX *amx, cell *params)
{
	TraceResult *tr;
	if (params[1] == 0)
		tr = &g_tr_2;
	else
		tr = reinterpret_cast<TraceResult *>(params[1]);

	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	cell *ptr = MF_GetAmxAddr(amx, params[3]);

	switch (params[2])
	{
	case TR_AllSolid:
		{
			tr->fAllSolid = *ptr;
			return 1;
		}
	case TR_InOpen:
		{
			tr->fInOpen = *ptr;
			return 1;
		}
	case TR_StartSolid:
		{
			tr->fStartSolid = *ptr;
			return 1;
		}
	case TR_InWater:
		{
			tr->fInWater = *ptr;
			return 1;
		}
	case TR_flFraction:
		{
			tr->flFraction = amx_ctof(*ptr);
			return 1;
		}
	case TR_vecEndPos:
		{
			tr->vecEndPos.x = amx_ctof(ptr[0]);
			tr->vecEndPos.y = amx_ctof(ptr[1]);
			tr->vecEndPos.z = amx_ctof(ptr[2]);
			return 1;
		}
	case TR_flPlaneDist:
		{
			tr->flPlaneDist = amx_ctof(*ptr);
			return 1;
		}
	case TR_vecPlaneNormal:
		{
			tr->vecPlaneNormal.x = amx_ctof(ptr[0]);
			tr->vecPlaneNormal.y = amx_ctof(ptr[1]);
			tr->vecPlaneNormal.z = amx_ctof(ptr[2]);
			return 1;
		}
	case TR_pHit:
		{
			const auto pEdict = TypeConversion.id_to_edict(*ptr);
			if (pEdict == nullptr)
			{
				return 0;
			}
			tr->pHit = pEdict;
			return 1;
		}
	case TR_iHitgroup:
		{
			tr->iHitgroup = *ptr;
			return 1;
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown TraceResult member %d", params[2]);
	
	return 0;
}

static cell AMX_NATIVE_CALL get_tr2(AMX *amx, cell *params)
{
	TraceResult *tr;
	if (params[1] == 0)
		tr = &g_tr_2;
	else
		tr = reinterpret_cast<TraceResult *>(params[1]);

	cell *ptr;

	switch (params[2])
	{
	case TR_AllSolid:
		{
			return tr->fAllSolid;
		}
	case TR_InOpen:
		{
			return tr->fInOpen;
		}
	case TR_StartSolid:
		{
			return tr->fStartSolid;
		}
	case TR_InWater:
		{
			return tr->fInWater;
		}
	case TR_flFraction:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			*ptr = amx_ftoc(tr->flFraction);
			return 1;
		}
	case TR_vecEndPos:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			ptr[0] = amx_ftoc(tr->vecEndPos.x);
			ptr[1] = amx_ftoc(tr->vecEndPos.y);
			ptr[2] = amx_ftoc(tr->vecEndPos.z);
			return 1;
		}
	case TR_flPlaneDist:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			*ptr = amx_ftoc(tr->flPlaneDist);
			return 1;
		}
	case TR_vecPlaneNormal:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			ptr[0] = amx_ftoc(tr->vecPlaneNormal.x);
			ptr[1] = amx_ftoc(tr->vecPlaneNormal.y);
			ptr[2] = amx_ftoc(tr->vecPlaneNormal.z);
			return 1;
		}
	case TR_pHit:
		{
			if (FNullEnt(tr->pHit))
				return -1;
			return ENTINDEX(tr->pHit);
		}
	case TR_iHitgroup:
		{
			return tr->iHitgroup;
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown TraceResult member %d", params[2]);
	
	return 0;
}

static cell AMX_NATIVE_CALL get_kvd(AMX *amx, cell *params)
{
	KeyValueData *kvd;
	if (params[1] == 0)
		kvd = &(g_kvd_glb.kvd);
	else
		kvd = reinterpret_cast<KeyValueData *>(params[1]);

	switch (params[2])
	{
	case KV_fHandled:
		{
			return kvd->fHandled;
		}
	case KV_ClassName:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szClassName, (int)*ptr);
		}
	case KV_KeyName:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szKeyName, (int)*ptr);
		}
	case KV_Value:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szValue, (int)*ptr);
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid KeyValueData member: %d", params[2]);
	
	return 0;
}

static cell AMX_NATIVE_CALL set_kvd(AMX *amx, cell *params)
{
	KVD_Wrapper *kvdw = nullptr;
	KeyValueData *kvd = nullptr;
	
	KVD_Wrapper *tmpw = reinterpret_cast<KVD_Wrapper *>(params[1]);
	if (params[1] == 0 || tmpw == &g_kvd_glb) {
		kvdw = &g_kvd_glb;
		kvd = &(kvdw->kvd);
	} else {
		for (size_t i = 0; i < g_KVDWs.length(); ++i) {
			if (g_KVDWs[i] == tmpw) {
				kvdw = tmpw;
				kvd = &(kvdw->kvd);
			}
		}

		if (kvdw == nullptr) {
			kvdw = &g_kvd_ext;
			kvd = reinterpret_cast<KeyValueData *>(tmpw);
		}
	}

	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	cell *ptr = MF_GetAmxAddr(amx, params[3]);
	int len;

	switch (params[2])
	{
	case KV_fHandled:
		{
			kvd->fHandled = (int)*ptr;
			return 1;
		}
	case KV_ClassName:
		{
			kvdw->cls = MF_GetAmxString(amx, params[3], 0, &len);
			kvd->szClassName = const_cast<char *>(kvdw->cls.chars());
			return 1;
		}
	case KV_KeyName:
		{
			kvdw->key = MF_GetAmxString(amx, params[3], 0, &len);
			kvd->szKeyName = const_cast<char *>(kvdw->key.chars());
			return 1;
		}
	case KV_Value:
		{
			kvdw->val = MF_GetAmxString(amx, params[3], 0, &len);
			kvd->szValue = const_cast<char *>(kvdw->val.chars());
			return 1;
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid KeyValueData member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL get_cd(AMX *amx, cell *params)
{
	clientdata_s *cd;
	if (params[1] == 0)
		cd = &g_cd_glb;
	else
		cd = reinterpret_cast<clientdata_t *>(params[1]);

	cell *ptr;

	switch(params[2])
	{
	case CD_Origin:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->origin.x);
		ptr[1] = amx_ftoc(cd->origin.y);
		ptr[2] = amx_ftoc(cd->origin.z);
		return 1;
	case CD_Velocity:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->velocity.x);
		ptr[1] = amx_ftoc(cd->velocity.y);
		ptr[2] = amx_ftoc(cd->velocity.z);
		return 1;
	case CD_ViewModel:
		return cd->viewmodel;
	case CD_PunchAngle:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->punchangle.x);
		ptr[1] = amx_ftoc(cd->punchangle.y);
		ptr[2] = amx_ftoc(cd->punchangle.z);
		return 1;
	case CD_Flags:
		return cd->flags;
	case CD_WaterLevel:
		return cd->waterlevel;
	case CD_WaterType:
		return cd->watertype;
	case CD_ViewOfs:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->view_ofs.x);
		ptr[1] = amx_ftoc(cd->view_ofs.y);
		ptr[2] = amx_ftoc(cd->view_ofs.z);
		return 1;
	case CD_Health:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->health);
		return 1;
	case CD_bInDuck:
		return cd->bInDuck;
	case CD_Weapons:
		return cd->weapons;
	case CD_flTimeStepSound:
		return cd->flTimeStepSound;
	case CD_flDuckTime:
		return cd->flDuckTime;
	case CD_flSwimTime:
		return cd->flSwimTime;
	case CD_WaterJumpTime:
		return cd->waterjumptime;
	case CD_MaxSpeed:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->maxspeed);
		return 1;
	case CD_FOV:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->fov);
		return 1;
	case CD_WeaponAnim:
		return cd->weaponanim;
	case CD_ID:
		return cd->m_iId;
	case CD_AmmoShells:
		return cd->ammo_shells;
	case CD_AmmoNails:
		return cd->ammo_nails;
	case CD_AmmoCells:
		return cd->ammo_cells;
	case CD_AmmoRockets:
		return cd->ammo_rockets;
	case CD_flNextAttack:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->m_flNextAttack);
		return 1;
	case CD_tfState:
		return cd->tfstate;
	case CD_PushMsec:
		return cd->pushmsec;
	case CD_DeadFlag:
		return cd->deadflag;
	case CD_PhysInfo:
		ptr = MF_GetAmxAddr(amx, params[4]);
		return MF_SetAmxString(amx, params[3], cd->physinfo, (int)*ptr);
	case CD_iUser1:
		return cd->iuser1;
	case CD_iUser2:
		return cd->iuser2;
	case CD_iUser3:
		return cd->iuser3;
	case CD_iUser4:
		return cd->iuser4;
	case CD_fUser1:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->fuser1);
		return 1;
	case CD_fUser2:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->fuser2);
		return 1;
	case CD_fUser3:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->fuser3);
		return 1;
	case CD_fUser4:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(cd->fuser4);
		return 1;
	case CD_vUser1:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->vuser1.x);
		ptr[1] = amx_ftoc(cd->vuser1.y);
		ptr[2] = amx_ftoc(cd->vuser1.z);
		return 1;
	case CD_vUser2:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->vuser2.x);
		ptr[1] = amx_ftoc(cd->vuser2.y);
		ptr[2] = amx_ftoc(cd->vuser2.z);
		return 1;
	case CD_vUser3:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->vuser3.x);
		ptr[1] = amx_ftoc(cd->vuser3.y);
		ptr[2] = amx_ftoc(cd->vuser3.z);
		return 1;
	case CD_vUser4:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(cd->vuser4.x);
		ptr[1] = amx_ftoc(cd->vuser4.y);
		ptr[2] = amx_ftoc(cd->vuser4.z);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid ClientData member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL set_cd(AMX *amx, cell *params)
{
	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	clientdata_s *cd;
	if (params[1] == 0)
		cd = &g_cd_glb;
	else
		cd = reinterpret_cast<clientdata_t *>(params[1]);

	cell *ptr = MF_GetAmxAddr(amx, params[3]);
	char *phys;

	switch(params[2])
	{
	case CD_Origin:
		cd->origin.x = amx_ctof(ptr[0]);
		cd->origin.y = amx_ctof(ptr[1]);
		cd->origin.z = amx_ctof(ptr[2]);
		return 1;
	case CD_Velocity:
		cd->velocity.x = amx_ctof(ptr[0]);
		cd->velocity.y = amx_ctof(ptr[1]);
		cd->velocity.z = amx_ctof(ptr[2]);
		return 1;
	case CD_ViewModel:
		cd->viewmodel = *ptr;
		return 1;
	case CD_PunchAngle:
		cd->punchangle.x = amx_ctof(ptr[0]);
		cd->punchangle.y = amx_ctof(ptr[1]);
		cd->punchangle.z = amx_ctof(ptr[2]);
		return 1;
	case CD_Flags:
		cd->flags = *ptr;
		return 1;
	case CD_WaterLevel:
		cd->waterlevel = *ptr;
		return 1;
	case CD_WaterType:
		cd->watertype = *ptr;
		return 1;
	case CD_ViewOfs:
		cd->view_ofs.x = amx_ctof(ptr[0]);
		cd->view_ofs.y = amx_ctof(ptr[1]);
		cd->view_ofs.z = amx_ctof(ptr[2]);
		return 1;
	case CD_Health:
		cd->health = amx_ctof(*ptr);
		return 1;
	case CD_bInDuck:
		cd->bInDuck = *ptr;
		return 1;
	case CD_Weapons:
		cd->weapons = *ptr;
		return 1;
	case CD_flTimeStepSound:
		cd->flTimeStepSound = *ptr;
		return 1;
	case CD_flDuckTime:
		cd->flDuckTime = *ptr;
		return 1;
	case CD_flSwimTime:
		cd->flSwimTime = *ptr;
		return 1;
	case CD_WaterJumpTime:
		cd->waterjumptime = *ptr;
		return 1;
	case CD_MaxSpeed:
		cd->maxspeed = amx_ctof(*ptr);
		return 1;
	case CD_FOV:
		cd->fov = amx_ctof(*ptr);
		return 1;
	case CD_WeaponAnim:
		cd->weaponanim = *ptr;
		return 1;
	case CD_ID:
		cd->m_iId = *ptr;
		return 1;
	case CD_AmmoShells:
		cd->ammo_shells = *ptr;
		return 1;
	case CD_AmmoNails:
		cd->ammo_nails = *ptr;
		return 1;
	case CD_AmmoCells:
		cd->ammo_cells = *ptr;
		return 1;
	case CD_AmmoRockets:
		cd->ammo_rockets = *ptr;
		return 1;
	case CD_flNextAttack:
		cd->m_flNextAttack = amx_ctof(*ptr);
		return 1;
	case CD_tfState:
		cd->tfstate = *ptr;
		return 1;
	case CD_PushMsec:
		cd->pushmsec = *ptr;
		return 1;
	case CD_DeadFlag:
		cd->deadflag = *ptr;
		return 1;
	case CD_PhysInfo:
		int len;
		phys = MF_GetAmxString(amx, params[3], 0, &len);
		strncpy(cd->physinfo, phys, len);
		return 1;
	case CD_iUser1:
		cd->iuser1 = *ptr;
		return 1;
	case CD_iUser2:
		cd->iuser2 = *ptr;
		return 1;
	case CD_iUser3:
		cd->iuser3 = *ptr;
		return 1;
	case CD_iUser4:
		cd->iuser4 = *ptr;
		return 1;
	case CD_fUser1:
		cd->fuser1 = amx_ctof(*ptr);
		return 1;
	case CD_fUser2:
		cd->fuser2 = amx_ctof(*ptr);
		return 1;
	case CD_fUser3:
		cd->fuser3 = amx_ctof(*ptr);
		return 1;
	case CD_fUser4:
		cd->fuser4 = amx_ctof(*ptr);
		return 1;
	case CD_vUser1:
		cd->vuser1.x = amx_ctof(ptr[0]);
		cd->vuser1.y = amx_ctof(ptr[1]);
		cd->vuser1.z = amx_ctof(ptr[2]);
		return 1;
	case CD_vUser2:
		cd->vuser2.x = amx_ctof(ptr[0]);
		cd->vuser2.y = amx_ctof(ptr[1]);
		cd->vuser2.z = amx_ctof(ptr[2]);
		return 1;
	case CD_vUser3:
		cd->vuser3.x = amx_ctof(ptr[0]);
		cd->vuser3.y = amx_ctof(ptr[1]);
		cd->vuser3.z = amx_ctof(ptr[2]);
		return 1;
	case CD_vUser4:
		cd->vuser4.x = amx_ctof(ptr[0]);
		cd->vuser4.y = amx_ctof(ptr[1]);
		cd->vuser4.z = amx_ctof(ptr[2]);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid ClientData member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL get_es(AMX *amx, cell *params)
{
	entity_state_t *es;
	if (params[1] == 0)
		es = &g_es_glb;
	else
		es = reinterpret_cast<entity_state_t *>(params[1]);

	cell *ptr;

	switch(params[2])
	{
	case ES_EntityType:
		return es->entityType;
	case ES_Number:
		return es->number;
	case ES_MsgTime:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->msg_time);
		return 1;
	case ES_MessageNum:
		return es->messagenum;
	case ES_Origin:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->origin.x);
		ptr[1] = amx_ftoc(es->origin.y);
		ptr[2] = amx_ftoc(es->origin.z);
		return 1;
	case ES_Angles:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->angles.x);
		ptr[1] = amx_ftoc(es->angles.y);
		ptr[2] = amx_ftoc(es->angles.z);
		return 1;
	case ES_ModelIndex:
		return es->modelindex;
	case ES_Sequence:
		return es->sequence;
	case ES_Frame:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->frame);
		return 1;
	case ES_ColorMap:
		return es->colormap;
	case ES_Skin:
		return es->skin;
	case ES_Solid:
		return es->solid;
	case ES_Effects:
		return es->effects;
	case ES_Scale:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->scale);
		return 1;
	case ES_eFlags:
		return es->eflags;
	case ES_RenderMode:
		return es->rendermode;
	case ES_RenderAmt:
		return es->renderamt;
	case ES_RenderColor:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = es->rendercolor.r;
		ptr[1] = es->rendercolor.b;
		ptr[2] = es->rendercolor.g;
		return 1;
	case ES_RenderFx:
		return es->renderfx;
	case ES_MoveType:
		return es->movetype;
	case ES_AnimTime:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->animtime);
		return 1;
	case ES_FrameRate:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->framerate);
		return 1;
	case ES_Body:
		return es->body;
	case ES_Controller:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = es->controller[0];
		ptr[1] = es->controller[1];
		ptr[2] = es->controller[2];
		ptr[3] = es->controller[3];
		return 1;
	case ES_Blending:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = es->blending[0];
		ptr[1] = es->blending[1];
		ptr[2] = es->blending[2];
		ptr[3] = es->blending[3];
		return 1;
	case ES_Velocity:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->velocity.x);
		ptr[1] = amx_ftoc(es->velocity.y);
		ptr[2] = amx_ftoc(es->velocity.z);
		return 1;
	case ES_Mins:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->mins.x);
		ptr[1] = amx_ftoc(es->mins.y);
		ptr[2] = amx_ftoc(es->mins.z);
		return 1;
	case ES_Maxs:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->maxs.x);
		ptr[1] = amx_ftoc(es->maxs.y);
		ptr[2] = amx_ftoc(es->maxs.z);
		return 1;
	case ES_AimEnt:
		return es->aiment;
	case ES_Owner:
		return es->owner;
	case ES_Friction:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->friction);
		return 1;
	case ES_Gravity:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->gravity);
		return 1;
	case ES_Team:
		return es->team;
	case ES_PlayerClass:
		return es->playerclass;
	case ES_Health:
		return es->health;
	case ES_Spectator:
		return es->spectator;
	case ES_WeaponModel:
		return es->weaponmodel;
	case ES_GaitSequence:
		return es->gaitsequence;
	case ES_BaseVelocity:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->basevelocity.x);
		ptr[1] = amx_ftoc(es->basevelocity.y);
		ptr[2] = amx_ftoc(es->basevelocity.z);
		return 1;
	case ES_UseHull:
		return es->usehull;
	case ES_OldButtons:
		return es->oldbuttons;
	case ES_OnGround:
		return es->onground;
	case ES_iStepLeft:
		return es->iStepLeft;
	case ES_flFallVelocity:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->flFallVelocity);
		return 1;
	case ES_FOV:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->fov);
		return 1;
	case ES_WeaponAnim:
		return es->weaponanim;
	case ES_StartPos:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->startpos.x);
		ptr[1] = amx_ftoc(es->startpos.y);
		ptr[2] = amx_ftoc(es->startpos.z);
		return 1;
	case ES_EndPos:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->endpos.x);
		ptr[1] = amx_ftoc(es->endpos.y);
		ptr[2] = amx_ftoc(es->endpos.z);
		return 1;
	case ES_ImpactTime:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->impacttime);
		return 1;
	case ES_StartTime:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->starttime);
		return 1;
	case ES_iUser1:
		return es->iuser1;
	case ES_iUser2:
		return es->iuser2;
	case ES_iUser3:
		return es->iuser3;
	case ES_iUser4:
		return es->iuser4;
	case ES_fUser1:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->fuser1);
		return 1;
	case ES_fUser2:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->fuser2);
		return 1;
	case ES_fUser3:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->fuser3);
		return 1;
	case ES_fUser4:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(es->fuser4);
		return 1;
	case ES_vUser1:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->vuser1.x);
		ptr[1] = amx_ftoc(es->vuser1.y);
		ptr[2] = amx_ftoc(es->vuser1.z);
		return 1;
	case ES_vUser2:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->vuser2.x);
		ptr[1] = amx_ftoc(es->vuser2.y);
		ptr[2] = amx_ftoc(es->vuser2.z);
		return 1;
	case ES_vUser3:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->vuser3.x);
		ptr[1] = amx_ftoc(es->vuser3.y);
		ptr[2] = amx_ftoc(es->vuser3.z);
		return 1;
	case ES_vUser4:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(es->vuser4.x);
		ptr[1] = amx_ftoc(es->vuser4.y);
		ptr[2] = amx_ftoc(es->vuser4.z);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid EntityState member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL set_es(AMX *amx, cell *params)
{
	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	entity_state_t *es;
	if (params[1] == 0)
		es = &g_es_glb;
	else
		es = reinterpret_cast<entity_state_t *>(params[1]);

	cell *ptr = MF_GetAmxAddr(amx, params[3]);

	switch(params[2])
	{
	case ES_EntityType:
		es->entityType = *ptr;
		return 1;
	case ES_Number:
		es->number = *ptr;
		return 1;
	case ES_MsgTime:
		es->msg_time = amx_ctof(*ptr);
		return 1;
	case ES_MessageNum:
		es->messagenum = *ptr;
		return 1;
	case ES_Origin:
		es->origin.x = amx_ctof(ptr[0]);
		es->origin.y = amx_ctof(ptr[1]);
		es->origin.z = amx_ctof(ptr[2]);
		return 1;
	case ES_Angles:
		es->angles.x = amx_ctof(ptr[0]);
		es->angles.y = amx_ctof(ptr[1]);
		es->angles.z = amx_ctof(ptr[2]);
		return 1;
	case ES_ModelIndex:
		es->modelindex = *ptr;
		return 1;
	case ES_Sequence:
		es->sequence = *ptr;
		return 1;
	case ES_Frame:
		es->frame = amx_ctof(*ptr);
		return 1;
	case ES_ColorMap:
		es->colormap = *ptr;
		return 1;
	case ES_Skin:
		es->skin = *ptr;
		return 1;
	case ES_Solid:
		es->solid = *ptr;
		return 1;
	case ES_Effects:
		es->effects = *ptr;
		return 1;
	case ES_Scale:
		es->scale = amx_ctof(*ptr);
		return 1;
	case ES_eFlags:
		es->eflags = *ptr;
		return 1;
	case ES_RenderMode:
		es->rendermode = *ptr;
		return 1;
	case ES_RenderAmt:
		es->renderamt = *ptr;
		return 1;
	case ES_RenderColor:
		es->rendercolor.r = ptr[0];
		es->rendercolor.g = ptr[1];
		es->rendercolor.b = ptr[2];
		return 1;
	case ES_RenderFx:
		es->renderfx = *ptr;
		return 1;
	case ES_MoveType:
		es->movetype = *ptr;
		return 1;
	case ES_AnimTime:
		es->animtime = amx_ctof(*ptr);
		return 1;
	case ES_FrameRate:
		es->framerate = amx_ctof(*ptr);
		return 1;
	case ES_Body:
		es->body = *ptr;
		return 1;
	case ES_Controller:
		es->controller[0] = ptr[0];
		es->controller[1] = ptr[1];
		es->controller[2] = ptr[2];
		es->controller[3] = ptr[3];
		return 1;
	case ES_Blending:
		es->blending[0] = ptr[0];
		es->blending[1] = ptr[1];
		es->blending[2] = ptr[2];
		es->blending[3] = ptr[3];
		return 1;
	case ES_Velocity:
		es->velocity.x = amx_ctof(ptr[0]);
		es->velocity.y = amx_ctof(ptr[1]);
		es->velocity.z = amx_ctof(ptr[2]);
		return 1;
	case ES_Mins:
		es->mins.x = amx_ctof(ptr[0]);
		es->mins.y = amx_ctof(ptr[1]);
		es->mins.z = amx_ctof(ptr[2]);
		return 1;
	case ES_Maxs:
		es->maxs.x = amx_ctof(ptr[0]);
		es->maxs.y = amx_ctof(ptr[1]);
		es->maxs.z = amx_ctof(ptr[2]);
		return 1;
	case ES_AimEnt:
		es->aiment = *ptr;
		return 1;
	case ES_Owner:
		es->owner = *ptr;
		return 1;
	case ES_Friction:
		es->friction = amx_ctof(*ptr);
		return 1;
	case ES_Gravity:
		es->gravity = amx_ctof(*ptr);
		return 1;
	case ES_Team:
		es->team = *ptr;
		return 1;
	case ES_PlayerClass:
		es->playerclass = *ptr;
		return 1;
	case ES_Health:
		es->health = *ptr;
		return 1;
	case ES_Spectator:
		es->spectator = *ptr;
		return 1;
	case ES_WeaponModel:
		es->weaponmodel = *ptr;
		return 1;
	case ES_GaitSequence:
		es->gaitsequence = *ptr;
		return 1;
	case ES_BaseVelocity:
		es->basevelocity.x = amx_ctof(ptr[0]);
		es->basevelocity.y = amx_ctof(ptr[1]);
		es->basevelocity.z = amx_ctof(ptr[2]);
		return 1;
	case ES_UseHull:
		es->usehull = *ptr;
		return 1;
	case ES_OldButtons:
		es->oldbuttons = *ptr;
		return 1;
	case ES_OnGround:
		es->onground = *ptr;
		return 1;
	case ES_iStepLeft:
		es->iStepLeft = *ptr;
		return 1;
	case ES_flFallVelocity:
		es->flFallVelocity = amx_ctof(*ptr);
		return 1;
	case ES_FOV:
		es->fov = amx_ctof(*ptr);
		return 1;
	case ES_WeaponAnim:
		es->weaponanim = *ptr;
		return 1;
	case ES_StartPos:
		es->startpos.x = amx_ctof(ptr[0]);
		es->startpos.y = amx_ctof(ptr[1]);
		es->startpos.z = amx_ctof(ptr[2]);
		return 1;
	case ES_EndPos:
		es->endpos.x = amx_ctof(ptr[0]);
		es->endpos.y = amx_ctof(ptr[1]);
		es->endpos.z = amx_ctof(ptr[2]);
		return 1;
	case ES_ImpactTime:
		es->impacttime= amx_ctof(*ptr);
		return 1;
	case ES_StartTime:
		es->starttime = amx_ctof(*ptr);
		return 1;
	case ES_iUser1:
		es->iuser1 = *ptr;
		return 1;
	case ES_iUser2:
		es->iuser2 = *ptr;
		return 1;
	case ES_iUser3:
		es->iuser3 = *ptr;
		return 1;
	case ES_iUser4:
		es->iuser4 = *ptr;
		return 1;
	case ES_fUser1:
		es->fuser1 = amx_ctof(*ptr);
		return 1;
	case ES_fUser2:
		es->fuser2 = amx_ctof(*ptr);
		return 1;
	case ES_fUser3:
		es->fuser3 = amx_ctof(*ptr);
		return 1;
	case ES_fUser4:
		es->fuser4 = amx_ctof(*ptr);
		return 1;
	case ES_vUser1:
		es->vuser1.x = amx_ctof(ptr[0]);
		es->vuser1.y = amx_ctof(ptr[1]);
		es->vuser1.z = amx_ctof(ptr[2]);
		return 1;
	case ES_vUser2:
		es->vuser2.x = amx_ctof(ptr[0]);
		es->vuser2.y = amx_ctof(ptr[1]);
		es->vuser2.z = amx_ctof(ptr[2]);
		return 1;
	case ES_vUser3:
		es->vuser3.x = amx_ctof(ptr[0]);
		es->vuser3.y = amx_ctof(ptr[1]);
		es->vuser3.z = amx_ctof(ptr[2]);
		return 1;
	case ES_vUser4:
		es->vuser4.x = amx_ctof(ptr[0]);
		es->vuser4.y = amx_ctof(ptr[1]);
		es->vuser4.z = amx_ctof(ptr[2]);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid EntityState member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL get_uc(AMX *amx, cell *params)
{
	usercmd_t *uc;
	if (params[1] == 0)
		uc = &g_uc_glb;
	else
		uc = reinterpret_cast<usercmd_t *>(params[1]);

	cell *ptr;

	switch(params[2])
	{
	case UC_LerpMsec:
		return uc->lerp_msec;
	case UC_Msec:
		return uc->msec;
	case UC_ViewAngles:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(uc->viewangles.x);
		ptr[1] = amx_ftoc(uc->viewangles.y);
		ptr[2] = amx_ftoc(uc->viewangles.z);
		return 1;
	case UC_ForwardMove:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(uc->forwardmove);
		return 1;
	case UC_SideMove:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(uc->sidemove);
		return 1;
	case UC_UpMove:
		ptr = MF_GetAmxAddr(amx, params[3]);
		*ptr = amx_ftoc(uc->upmove);
		return 1;
	case UC_LightLevel:
		return uc->lightlevel;
	case UC_Buttons:
		return uc->buttons;
	case UC_Impulse:
		return uc->impulse;
	case UC_WeaponSelect:
		return uc->weaponselect;
	case UC_ImpactIndex:
		return uc->impact_index;
	case UC_ImpactPosition:
		ptr = MF_GetAmxAddr(amx, params[3]);
		ptr[0] = amx_ftoc(uc->impact_position.x);
		ptr[1] = amx_ftoc(uc->impact_position.y);
		ptr[2] = amx_ftoc(uc->impact_position.z);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid UserCmd member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL set_uc(AMX *amx, cell *params)
{
	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	usercmd_t *uc;
	if (params[1] == 0)
		uc = &g_uc_glb;
	else
		uc = reinterpret_cast<usercmd_t *>(params[1]);

	cell *ptr = MF_GetAmxAddr(amx, params[3]);

	switch(params[2])
	{
	case UC_LerpMsec:
		uc->lerp_msec = *ptr;
		return 1;
	case UC_Msec:
		uc->msec = *ptr;
		return 1;
	case UC_ViewAngles:
		uc->viewangles.x = amx_ctof(ptr[0]);
		uc->viewangles.y = amx_ctof(ptr[1]);
		uc->viewangles.z = amx_ctof(ptr[2]);
		return 1;
	case UC_ForwardMove:
		uc->forwardmove = amx_ctof(*ptr);
		return 1;
	case UC_SideMove:
		uc->sidemove = amx_ctof(*ptr);
		return 1;
	case UC_UpMove:
		uc->upmove = amx_ctof(*ptr);
		return 1;
	case UC_LightLevel:
		uc->lightlevel = *ptr;
		return 1;
	case UC_Buttons:
		uc->buttons = *ptr;
		return 1;
	case UC_Impulse:
		uc->impulse = *ptr;
		return 1;
	case UC_WeaponSelect:
		uc->weaponselect = *ptr;
		return 1;
	case UC_ImpactIndex:
		uc->impact_index = *ptr;
		return 1;
	case UC_ImpactPosition:
		uc->impact_position.x = amx_ctof(ptr[0]);
		uc->impact_position.y = amx_ctof(ptr[1]);
		uc->impact_position.z = amx_ctof(ptr[2]);
		return 1;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid UserCmd member: %d", params[2]);

	return 0;
}

CStack<TraceResult *> g_FreeTRs;

static cell AMX_NATIVE_CALL create_tr2(AMX *amx, cell *params)
{
	TraceResult *tr;
	if (g_FreeTRs.empty())
	{
		tr = new TraceResult;
	} else {
		tr = g_FreeTRs.front();
		g_FreeTRs.pop();
	}
	memset(static_cast<void *>(tr), 0, sizeof(TraceResult));
	return reinterpret_cast<cell>(tr);
}

static cell AMX_NATIVE_CALL free_tr2(AMX *amx, cell *params)
{
	TraceResult *tr = reinterpret_cast<TraceResult *>(params[1]);
	if (!tr)
	{
		return 0;
	}

	g_FreeTRs.push(tr);

	return 1;
}

static cell AMX_NATIVE_CALL create_kvd(AMX *amx, cell *params)
{
	KVD_Wrapper *kvdw;
	if (g_FreeKVDWs.empty()) {
		kvdw = new KVD_Wrapper;
	} else {
		kvdw = g_FreeKVDWs.popCopy();
	}

	kvdw->cls = "";
	kvdw->kvd.szClassName = const_cast<char*>(kvdw->cls.chars());
	kvdw->key = "";
	kvdw->kvd.szKeyName = const_cast<char*>(kvdw->key.chars());
	kvdw->val = "";
	kvdw->kvd.szValue = const_cast<char*>(kvdw->val.chars());
	kvdw->kvd.fHandled = 0;

	g_KVDWs.append(kvdw);

	return reinterpret_cast<cell>(kvdw);
}

static cell AMX_NATIVE_CALL free_kvd(AMX *amx, cell *params) {
	if (params[1] == 0) {
		return 0;
	}

	KVD_Wrapper *kvdw = reinterpret_cast<KVD_Wrapper *>(params[1]);

	for (size_t i = 0; i < g_KVDWs.length(); ++i) {
		if (g_KVDWs[i] == kvdw) {
			g_KVDWs.remove(i);
			g_FreeKVDWs.append(kvdw);

			return 1;
		}
	}

	return 0;
}

AMX_NATIVE_INFO ext2_natives[] = 
{
	{"create_tr2",		create_tr2},
	{"free_tr2",		free_tr2},
	{"get_tr2",			get_tr2},
	{"set_tr2",			set_tr2},
	{"create_kvd",		create_kvd},
	{"free_kvd",		free_kvd},
	{"get_kvd",			get_kvd},
	{"set_kvd",			set_kvd},
	{"get_cd",			get_cd},
	{"set_cd",			set_cd},
	{"get_es",			get_es},
	{"set_es",			set_es},
	{"get_uc",			get_uc},
	{"set_uc",			set_uc},
	{NULL,				NULL},
};

