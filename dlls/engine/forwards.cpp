#include "engine.h"

bool incmd = false;
int DispatchUseForward = 0;
int SpawnForward = 0;
int ChangelevelForward = 0;
int PlaybackForward = 0;
int DispatchKeyForward = 0;
int pfnTouchForward = 0;
int pfnThinkForward = 0;
int PlayerPreThinkForward = 0;
int PlayerPostThinkForward = 0;
int ClientKillForward = 0;
int CmdStartForward = 0;
int StartFrameForward = 0;

void DispatchUse(edict_t *pentUsed, edict_t *pentOther)
{
	if (DispatchUseForward) {
		int retVal = 0;
		int used = ENTINDEX(pentUsed);
		int user = ENTINDEX(pentOther);
		retVal = MF_ExecuteForward(DispatchUseForward, user, used);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

int DispatchSpawn(edict_t *pEntity) {
/*	if (SpawnForward) {
		int retVal = 0;
		int id = ENTINDEX(pEntity);
		retVal = MF_ExecuteForward(SpawnForward, id);
		if (retVal)
			RETURN_META_VALUE(MRES_SUPERCEDE, 0);
	}*/
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ChangeLevel(char* s1, char* s2)
{
	if (ChangelevelForward) {
		int retVal = 0;
		char *map = s1;
		cell amxMap = MF_PrepareCharArray(map, strlen(map));
		retVal = MF_ExecuteForward(ChangelevelForward, amxMap);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void PlaybackEvent(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2)
{
	if (PlaybackForward) {
		edict_t *e = (edict_t *)pInvoker;
		int retVal = 0;
		static cell cOrigin[3];
		static cell cAngles[3];
		Vector vOrigin = (Vector)origin;
		Vector vAngles = (Vector)angles;
		cOrigin[0] = amx_ftoc(vOrigin.x);
		cOrigin[1] = amx_ftoc(vOrigin.y);
		cOrigin[2] = amx_ftoc(vOrigin.z);
		cAngles[0] = amx_ftoc(vAngles.x);
		cAngles[1] = amx_ftoc(vAngles.y);
		cAngles[2] = amx_ftoc(vAngles.z);
		cell CellOrigin = MF_PrepareCellArray(cOrigin, 3);
		cell CellAngles = MF_PrepareCellArray(cAngles, 3);
		retVal = MF_ExecuteForward(PlaybackForward, flags, ENTINDEX(e), eventindex, amx_ftoc(delay), CellOrigin, CellAngles, amx_ftoc(fparam1), amx_ftoc(fparam2), iparam1, iparam2, bparam2);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);

}

#if 0
void KeyValue(edict_t *pEntity, KeyValueData *pkvd)
{
	if (DispatchKeyForward) {
		inKeyValue=true;
		int retVal = 0;
		g_pkvd=pkvd;
		int index = ENTINDEX(pEntity);
		retVal = MF_ExecuteForward(DispatchKeyForward, index);
		inKeyValue=false;
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_HANDLED);
}
#endif

void StartFrame()
{
	if (glinfo.bCheckLights) {
		if (!FStrEq((const char*)glinfo.szLastLights, "")) {
			(g_engfuncs.pfnLightStyle)(0, (char *)glinfo.szLastLights);
			glinfo.fNextLights = gpGlobals->time + 1;
		}
	}

	if (StartFrameForward)
		MF_ExecuteForward(StartFrameForward);

	RETURN_META(MRES_IGNORED);
}

void CmdStart(const edict_t *player, const struct usercmd_s *_cmd, unsigned int random_seed)
{
	int retVal = 0;
	edict_t *pEntity = (edict_t *)player;
	struct usercmd_s *g_cmd = (struct usercmd_s *)_cmd;
	if (CmdStartForward) {
		incmd = true;
		retVal = MF_ExecuteForward(CmdStartForward, ENTINDEX(pEntity), g_cmd->impulse);
		incmd = false;
		if (retVal) {
			g_cmd->impulse = 0;
			RETURN_META(MRES_SUPERCEDE);
		}
	}

	RETURN_META(MRES_IGNORED);
}

void ClientKill(edict_t *pEntity)
{
	int retVal = 0;

	if (ClientKillForward) {
		retVal = MF_ExecuteForward(ClientKillForward, ENTINDEX(pEntity));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink(edict_t *pEntity)
{
	int retVal = 0;

	if (pfnTouchForward) {
		retVal = MF_ExecuteForward(PlayerPreThinkForward, ENTINDEX(pEntity));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void PlayerPostThink(edict_t *pEntity)
{
	int retVal = 0;

	if(plinfo[ENTINDEX(pEntity)].pViewEnt) {
		edict_t *pCamEnt = plinfo[ENTINDEX(pEntity)].pViewEnt;

		MAKE_VECTORS(pEntity->v.v_angle + pEntity->v.punchangle);
		Vector vecSrc	 = pEntity->v.origin + pEntity->v.view_ofs;
		Vector vecAiming = gpGlobals->v_forward;
		TraceResult tr;

		switch(plinfo[ENTINDEX(pEntity)].iViewType) {
			case CAMERA_3RDPERSON:
				TRACE_LINE(vecSrc, vecSrc - (vecAiming * 128), ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.angles = pEntity->v.v_angle;
				break;
			case CAMERA_UPLEFT:
				TRACE_LINE(vecSrc, vecSrc - ((vecAiming * 32) - ((gpGlobals->v_right * 15) + (gpGlobals->v_up * 15))), ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.angles = pEntity->v.v_angle;
				break;
			case CAMERA_TOPDOWN:
				TRACE_LINE(vecSrc, vecSrc + Vector(0,0,2048), dont_ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.origin.z -= 40;
				pCamEnt->v.angles = Vector(90,pEntity->v.v_angle.y,0);
				break;
			default:
				SET_VIEW(pEntity, pEntity);
				REMOVE_ENTITY(plinfo[ENTINDEX(pEntity)].pViewEnt);
				plinfo[ENTINDEX(pEntity)].iViewType = CAMERA_NONE;
				plinfo[ENTINDEX(pEntity)].pViewEnt = NULL;
				break;
		}
	}

	if (pfnTouchForward) {
		retVal = MF_ExecuteForward(PlayerPostThinkForward, ENTINDEX(pEntity));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void DispatchTouch(edict_t *pToucher, edict_t *pTouched)
{
	int retVal = 0;
	/* Execute pfnTouch forwards */
	if (pfnTouchForward) {
		retVal = MF_ExecuteForward(pfnTouchForward, ENTINDEX(pToucher), ENTINDEX(pTouched));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void DispatchThink(edict_t *pent)
{
	int retVal = 0;

	if (pfnThinkForward) {
		retVal = MF_ExecuteForward(pfnThinkForward, ENTINDEX(pent));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}