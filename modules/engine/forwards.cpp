// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Module
//

#include "engine.h"

bool incmd = false;
int DispatchUseForward = 0;
int SpawnForward = 0;
int PlaybackForward = 0;
int DispatchKeyForward = 0;
int pfnTouchForward = 0;
int pfnThinkForward = 0;
int PlayerPreThinkForward = 0;
int PlayerPostThinkForward = 0;
int ClientKillForward = 0;
int ClientImpulseForward = 0;
int CmdStartForward = 0;
int StartFrameForward = 0;
int VexdTouchForward = 0;
int VexdServerForward = 0;
ke::Vector<Impulse *> Impulses;
ke::Vector<EntClass *> Thinks;
ke::Vector<Touch *> Touches;
KeyValueData *g_pkvd;
bool g_inKeyValue=false;
bool g_precachedStuff = false;

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

int Spawn(edict_t *pEntity)
{
	if (!g_precachedStuff) {
		// Used for SetView, added by JGHG
		PRECACHE_MODEL("models/rpgrocket.mdl");
		g_precachedStuff = true;
	}
	if (SpawnForward != -1)
	{
		int id = TypeConversion.edict_to_id(pEntity);

		if (MF_ExecuteForward(SpawnForward, (cell)id))
		{
			RETURN_META_VALUE(MRES_SUPERCEDE, -1);
		}
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void PlaybackEvent(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2)
{
	if (PlaybackForward != -1) {
		edict_t *e = (edict_t *)pInvoker;
		int invoker = 0;
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
		if (!FNullEnt(e))
			invoker = TypeConversion.edict_to_id(e);
		retVal = MF_ExecuteForward(PlaybackForward, (cell)flags, (cell)invoker, (cell)eventindex, delay, CellOrigin, CellAngles, fparam1, fparam2, (cell)iparam1, (cell)iparam2, (cell)bparam1, (cell)bparam2);
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
	if (DispatchKeyForward != -1) {
		retVal = MF_ExecuteForward(DispatchKeyForward, (cell)TypeConversion.edict_to_id(pEntity));
		g_inKeyValue=false;
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	g_inKeyValue=false;
	RETURN_META(MRES_IGNORED);
}

void StartFrame()
{
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
	g_cmd = (struct usercmd_s *)_cmd;
	int origImpulse = g_cmd->impulse; // incase a plugin alters it

	auto index = TypeConversion.edict_to_id(pEntity);

	for (i=0; i<Impulses.length(); i++)
	{
		if (Impulses[i]->Check == g_cmd->impulse)
		{
			retVal = MF_ExecuteForward(Impulses[i]->Forward, (cell)index, (cell)origImpulse);
			
			// don't return SUPERCEDE in any way here, 
			// we don't want to break client_impulse forward and access to cmd with [g/s]et_usercmd
			if (retVal)
				g_cmd->impulse = 0;
		}
	}

	// client_impulse
	if (ClientImpulseForward != -1 && origImpulse != 0) 
	{
		retVal = MF_ExecuteForward(ClientImpulseForward, (cell)index, (cell)origImpulse);

		if (retVal)
			g_cmd->impulse = 0;
	}

	// client_CmdStart
	if (CmdStartForward != -1)
	{
		incmd = true;
		retVal = MF_ExecuteForward(CmdStartForward, (cell)index);
		incmd = false;

		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void ClientKill(edict_t *pEntity)
{
	if (ClientKillForward != -1)
	{
		if (MF_ExecuteForward(ClientKillForward, (cell)TypeConversion.edict_to_id(pEntity)))
		{
			RETURN_META(MRES_SUPERCEDE);
		}
	}

	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink(edict_t *pEntity)
{
	if (PlayerPreThinkForward != -1)
	{
		MF_ExecuteForward(PlayerPreThinkForward, (cell)TypeConversion.edict_to_id(pEntity));
	}

	RETURN_META(MRES_IGNORED);
}

void PlayerPostThink_Post(edict_t *pEntity)
{
	auto index = TypeConversion.edict_to_id(pEntity);

	if(plinfo[index].pViewEnt) {
		edict_t *pCamEnt = plinfo[index].pViewEnt;

		MAKE_VECTORS(pEntity->v.v_angle + pEntity->v.punchangle);
		Vector vecSrc	 = pEntity->v.origin + pEntity->v.view_ofs;
		Vector vecAiming = gpGlobals->v_forward;
		TraceResult tr;

		switch(plinfo[index].iViewType) {
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
				REMOVE_ENTITY(plinfo[index].pViewEnt);
				plinfo[index].iViewType = CAMERA_NONE;
				plinfo[index].pViewEnt = NULL;
				break;
		}
	}

	if (PlayerPostThinkForward != -1)
	{
		if (MF_ExecuteForward(PlayerPostThinkForward, (cell)index))
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
	int ptrIndex = TypeConversion.edict_to_id(pToucher);
	int ptdIndex = TypeConversion.edict_to_id(pTouched);
	META_RES res=MRES_IGNORED;
	for (i=0; i<Touches.length(); i++)
	{
		if (Touches[i]->Toucher.length() == 0)
		{
			if (Touches[i]->Touched.length() == 0)
			{
				retVal = MF_ExecuteForward(Touches[i]->Forward, (cell)ptrIndex, (cell)ptdIndex);
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			} else if (Touches[i]->Touched.compare(ptdClass)==0) {
				retVal = MF_ExecuteForward(Touches[i]->Forward, (cell)ptrIndex, (cell)ptdIndex);
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			}
		} else if (Touches[i]->Toucher.compare(ptrClass)==0) {
			if (Touches[i]->Touched.length() == 0)
			{
				retVal = MF_ExecuteForward(Touches[i]->Forward, (cell)ptrIndex, (cell)ptdIndex);
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			} else if (Touches[i]->Touched.compare(ptdClass)==0) {
				retVal = MF_ExecuteForward(Touches[i]->Forward, (cell)ptrIndex, (cell)ptdIndex);
				if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
					RETURN_META(MRES_SUPERCEDE);
				else if (retVal)
					res=MRES_SUPERCEDE;
			}
		}
	}
	/* Execute pfnTouch forwards */
	if (pfnTouchForward != -1) {
		retVal = MF_ExecuteForward(pfnTouchForward, (cell)ptrIndex, (cell)ptdIndex);
		if (retVal)
			RETURN_META(MRES_SUPERCEDE);
	}
	if (VexdTouchForward != -1) {
		retVal = MF_ExecuteForward(VexdTouchForward, (cell)ptrIndex, (cell)ptdIndex);
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
	for (i=0; i<Thinks.length(); i++)
	{
		if (Thinks[i]->Class.compare(cls)==0)
		{
			retVal=MF_ExecuteForward(Thinks[i]->Forward, (cell)TypeConversion.edict_to_id(pent));
			if (retVal & 2/*PLUGIN_HANDLED_MAIN*/)
				RETURN_META(MRES_SUPERCEDE);
			else if (retVal)
				res=MRES_SUPERCEDE;
		}
	}
	retVal=MF_ExecuteForward(pfnThinkForward, (cell)TypeConversion.edict_to_id(pent));
	if (retVal)
		res=MRES_SUPERCEDE;

	RETURN_META(res);
}
