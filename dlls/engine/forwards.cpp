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
int VexdTouchForward = 0;
int VexdServerForward = 0;
CVector<Impulse *> Impulses;
CVector<EntClass *> Thinks;
CVector<Touch *> Touches;
KeyValueData *g_pkvd;
bool g_inKeyValue=false;

int fstrcmp(const char *s1, const char *s2)
{
	int i=0;
	int len1 = strlen(s1);
	int len2 = strlen(s2);
	if (len1 != len2)
		return 0;
	for (i=0; i<len1; i++)
	{
		if (s1[i] != s2[i])
			return 0;
	}
	return 1;
}

int Spawn(edict_t *pEntity) {
	if (SpawnForward != -1) {
		int retVal = 0;
		int id = ENTINDEX(pEntity);
		retVal = MF_ExecuteForward(SpawnForward, id);
		if (retVal)
			RETURN_META_VALUE(MRES_SUPERCEDE, 0);
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ChangeLevel(char* s1, char* s2)
{
	if (ChangelevelForward != -1) {
		int retVal = 0;
		char *map = s1;
		retVal = MF_ExecuteForward(ChangelevelForward, map);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void PlaybackEvent(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2)
{
	if (PlaybackForward != -1) {
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

void KeyValue(edict_t *pEntity, KeyValueData *pkvd)
{
	int retVal = 0;
	g_inKeyValue=true;
	g_pkvd=pkvd;
	int index = ENTINDEX(pEntity);
	if (DispatchKeyForward != -1) {
		retVal = MF_ExecuteForward(DispatchKeyForward, index);
		g_inKeyValue=false;
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	g_inKeyValue=false;
	RETURN_META(MRES_IGNORED);
}

void StartFrame()
{
	if (glinfo.bCheckLights) {
		if (!FStrEq((const char*)glinfo.szLastLights, "")) {
			(g_engfuncs.pfnLightStyle)(0, (char *)glinfo.szLastLights);
			glinfo.fNextLights = gpGlobals->time + 1;
		}
	}

	if (StartFrameForward != -1)
		MF_ExecuteForward(StartFrameForward);
	else if (VexdServerForward != -1)
		MF_ExecuteForward(VexdServerForward);

	RETURN_META(MRES_IGNORED);
}

void CmdStart(const edict_t *player, const struct usercmd_s *_cmd, unsigned int random_seed)
{
	unsigned int i = 0;
	int retVal = 0;
	edict_t *pEntity = (edict_t *)player;
	struct usercmd_s *g_cmd = (struct usercmd_s *)_cmd;
	META_RES res = MRES_IGNORED;
	int origImpulse = g_cmd->impulse; // incase a plugin alters it
	for (i=0; i<Impulses.size(); i++)
	{
		if (Impulses[i]->Check == g_cmd->impulse)
		{
			retVal = MF_ExecuteForward(Impulses[i]->Forward, ENTINDEX(pEntity));
			if (retVal & 2 /*PLUGIN_HANDLED_MAIN*/)
			{
				g_cmd->impulse=0;
				RETURN_META(MRES_SUPERCEDE);
			}
			else if (retVal)
				res = MRES_SUPERCEDE;
		}
	}
	if (CmdStartForward != -1) {
		incmd = true;
		retVal = MF_ExecuteForward(CmdStartForward, ENTINDEX(pEntity), origImpulse);
		incmd = false;
		if (retVal) {
			g_cmd->impulse = 0;
			RETURN_META(MRES_SUPERCEDE);
		}
	}

	RETURN_META(res);
}

void ClientKill(edict_t *pEntity)
{
	int retVal = 0;

	if (ClientKillForward != -1) {
		retVal = MF_ExecuteForward(ClientKillForward, ENTINDEX(pEntity));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink(edict_t *pEntity)
{
	MF_ExecuteForward(PlayerPreThinkForward, ENTINDEX(pEntity));
	RETURN_META(MRES_IGNORED);
}

void PlayerPostThink_Post(edict_t *pEntity)
{
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

	if (PlayerPostThinkForward != -1)
	{
		if (MF_ExecuteForward(PlayerPostThinkForward, ENTINDEX(pEntity)))
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void pfnTouch(edict_t *pToucher, edict_t *pTouched)
{
	unsigned int i = 0;
	int retVal = 0;
	const char *ptrClass = STRING(pToucher->v.classname);
	const char *ptdClass = STRING(pTouched->v.classname);
	META_RES res=MRES_IGNORED;
	for (i=0; i<Touches.size(); i++)
	{
		if (Touches[i]->Toucher == 0)
		{
			if (Touches[i]->Touched == 0)
			{
				retVal = MF_ExecuteForward(Touches[i]->Forward, ENTINDEX(pToucher), ENTINDEX(pTouched));
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			} else if (fstrcmp(Touches[i]->Touched, ptdClass)) {
				retVal = MF_ExecuteForward(Touches[i]->Forward, ENTINDEX(pToucher), ENTINDEX(pTouched));
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			}
		} else if (fstrcmp(Touches[i]->Toucher, ptrClass)) {
			if (Touches[i]->Touched == 0)
			{
				retVal = MF_ExecuteForward(Touches[i]->Forward, ENTINDEX(pToucher), ENTINDEX(pTouched));
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			} else if (fstrcmp(Touches[i]->Touched, ptdClass)) {
				retVal = MF_ExecuteForward(Touches[i]->Forward, ENTINDEX(pToucher), ENTINDEX(pTouched));
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			}
		}
	}
	/* Execute pfnTouch forwards */
	if (pfnTouchForward != -1) {
		retVal = MF_ExecuteForward(pfnTouchForward, ENTINDEX(pToucher), ENTINDEX(pTouched));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	} else if (VexdTouchForward != -1) {
		retVal = MF_ExecuteForward(VexdTouchForward, ENTINDEX(pToucher), ENTINDEX(pTouched));
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(res);
}

void Think(edict_t *pent)
{
	unsigned int i = 0;
	const char *cls = STRING(pent->v.classname);
	META_RES res=MRES_IGNORED;
	int retVal=0;
	for (i=0; i<Thinks.size(); i++)
	{
		if (fstrcmp(cls, Thinks[i]->Class))
		{
			retVal=MF_ExecuteForward(Thinks[i]->Forward, ENTINDEX(pent));
			if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
				RETURN_META(MRES_SUPERCEDE);
			else if (retVal)
				res=MRES_SUPERCEDE;
		}
	}
	retVal=MF_ExecuteForward(pfnThinkForward, ENTINDEX(pent));
	if (retVal)
		res=MRES_SUPERCEDE;

	RETURN_META(res);
}
