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

struct usercmd_s *g_cmd;
struct PlayerInfo plinfo[33];
struct GlobalInfo glinfo;

int g_CameraCount;

TraceResult g_tr;

char g_buffer[1024];

void UTIL_SetSize(edict_t *pev, const Vector &vecMin, const Vector &vecMax)
{
	SET_SIZE(ENT(pev), vecMin, vecMax);
}

edict_t *UTIL_FindEntityInSphere(edict_t *pStart, const Vector &vecCenter, float flRadius) {
	if (!pStart) pStart = NULL;

	pStart = FIND_ENTITY_IN_SPHERE(pStart, vecCenter, flRadius);

	if (!FNullEnt(pStart)) return pStart;
	return NULL;
}

static cell AMX_NATIVE_CALL register_think(AMX *amx, cell *params)
{
	int len;

	EntClass *p = new EntClass;
	const char *clsname = MF_GetAmxString(amx, params[1], 0, &len);
	p->Class = clsname;

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[2], 0, &len), FP_CELL, FP_DONE);

	Thinks.append(p);

	if (!g_pFunctionTable->pfnThink)
		g_pFunctionTable->pfnThink=Think;

	return p->Forward;
}

static cell AMX_NATIVE_CALL unregister_think(AMX *amx, cell *params)
{
	int fwd = params[1];
	for (size_t i = 0; i < Thinks.length(); ++i)
	{
		EntClass *p = Thinks.at(i);
		if (p->Forward == fwd)
		{
			Thinks.remove(i);
			delete p;

			if (!Thinks.length())
				g_pFunctionTable->pfnThink = NULL;

			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL register_impulse(AMX *amx, cell *params)
{
	int len;

	Impulse *p = new Impulse;
	p->Check = params[1];

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[2], 0, &len), FP_CELL, FP_CELL, FP_DONE);

	Impulses.append(p);

	if (!g_pFunctionTable->pfnCmdStart)
		g_pFunctionTable->pfnCmdStart=CmdStart;

	return p->Forward;
}

static cell AMX_NATIVE_CALL unregister_impulse(AMX *amx, cell *params)
{
	int fwd = params[1];
	for (size_t i = 0; i < Impulses.length(); ++i)
	{
		Impulse *p = Impulses.at(i);
		if (p->Forward == fwd)
		{
			Impulses.remove(i);
			delete p;

			if (!Impulses.length())
				g_pFunctionTable->pfnCmdStart = NULL;

			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL register_touch(AMX *amx, cell *params)
{
	int len;

	char *Toucher = MF_GetAmxString(amx, params[1], 0, &len);
	char *Touched = MF_GetAmxString(amx, params[2], 1, &len);

	Touch *p = new Touch;

	if (!strlen(Toucher) || strcmp(Toucher, "*")==0) {
		p->Toucher = "";
	} else {
		p->Toucher = Toucher;
	}
	if (!strlen(Touched) || strcmp(Touched, "*")==0) {
		p->Touched = "";
	} else {
		p->Touched = Touched;
	}

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[3], 2, &len), FP_CELL, FP_CELL, FP_DONE);

	Touches.append(p);

	if (!g_pFunctionTable->pfnTouch)
		g_pFunctionTable->pfnTouch=pfnTouch;

	return p->Forward;
}

static cell AMX_NATIVE_CALL unregister_touch(AMX *amx, cell *params)
{
	int fwd = params[1];
	for (size_t i = 0; i < Touches.length(); ++i)
	{
		Touch *p = Touches.at(i);
		if (p->Forward == fwd)
		{
			Touches.remove(i);
			delete p;

			if (!Touches.length())
				g_pFunctionTable->pfnTouch = NULL;

			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL halflife_time(AMX *amx, cell *params)
{
	REAL fVal = gpGlobals->time;

	return amx_ftoc(fVal);
}

// RadiusDamage. Damages players within a certain radius. ToDo: add the
// damage messaging so players know where the damage is coming from
// (the red arrow-like things on the screen).
static cell AMX_NATIVE_CALL RadiusDamage(AMX *amx, cell *params)
{
	cell *cAddr = MF_GetAmxAddr(amx,params[1]);

	REAL fCurrentX = amx_ctof(cAddr[0]);
	REAL fCurrentY = amx_ctof(cAddr[1]);
	REAL fCurrentZ = amx_ctof(cAddr[2]);
	int iDamageMultiplier = params[2];
	int iRadiusMultiplier = params[3];

	Vector vOrigin = Vector(fCurrentX, fCurrentY, fCurrentZ);

	edict_t *pSearchEnt = NULL;
	while ((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 5 * iRadiusMultiplier)) != NULL)
	{
		if (FStrEq(STRING(pSearchEnt->v.classname), "player")) 
		{
			if (pSearchEnt->v.takedamage != DAMAGE_NO) 
			{
				pSearchEnt->v.health -= 10 + RANDOM_FLOAT(0, 1 * iDamageMultiplier);
				if (pSearchEnt->v.health < 1)
				{
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while ((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 4 * iRadiusMultiplier)) != NULL)
	{
		if (FStrEq(STRING(pSearchEnt->v.classname), "player"))
		{
			if (pSearchEnt->v.takedamage != DAMAGE_NO) 
			{
				pSearchEnt->v.health -= 25 + RANDOM_FLOAT(0, 2 * iDamageMultiplier);
				if (pSearchEnt->v.health < 1) 
				{
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while ((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 3 * iRadiusMultiplier)) != NULL)
	{
		if (FStrEq(STRING(pSearchEnt->v.classname), "player"))
		{
			if (pSearchEnt->v.takedamage != DAMAGE_NO)
			{
				pSearchEnt->v.health -= 50 + RANDOM_FLOAT(0, 3 * iDamageMultiplier);
				if (pSearchEnt->v.health < 1) 
				{
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while ((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 2 * iRadiusMultiplier)) != NULL)
	{
		if (FStrEq(STRING(pSearchEnt->v.classname), "player"))
		{
			if (pSearchEnt->v.takedamage != DAMAGE_NO)
			{
				MDLL_ClientKill(pSearchEnt);
			}
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL PointContents(AMX *amx, cell *params)
{
	cell *cAddr = MF_GetAmxAddr(amx, params[1]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);

	Vector vPoint = Vector(fX, fY, fZ);

	return POINT_CONTENTS(vPoint);
}

static cell AMX_NATIVE_CALL trace_normal(AMX *amx, cell *params)
{
	int iEnt = params[1];
	if (iEnt > 0) {
		CHECK_ENTITY(iEnt);
	}

	cell *cStart = MF_GetAmxAddr(amx, params[2]);
	cell *cEnd = MF_GetAmxAddr(amx, params[3]);
	REAL fStartX = amx_ctof(cStart[0]);
	REAL fStartY = amx_ctof(cStart[1]);
	REAL fStartZ = amx_ctof(cStart[2]);
	REAL fEndX = amx_ctof(cEnd[0]);
	REAL fEndY = amx_ctof(cEnd[1]);
	REAL fEndZ = amx_ctof(cEnd[2]);

	cell *vRet = MF_GetAmxAddr(amx, params[4]);

	Vector vStart = Vector(fStartX, fStartY, fStartZ);
	Vector vEnd = Vector(fEndX, fEndY, fEndZ);

	TRACE_LINE(vStart, vEnd, dont_ignore_monsters, iEnt > 0 ? TypeConversion.id_to_edict(iEnt) : NULL, &g_tr);

	vRet[0] = amx_ftoc(g_tr.vecPlaneNormal.x);
	vRet[1] = amx_ftoc(g_tr.vecPlaneNormal.y);
	vRet[2] = amx_ftoc(g_tr.vecPlaneNormal.z);

	if (g_tr.flFraction >= 1.0)
		return 0;

	return 1;
}

static cell AMX_NATIVE_CALL trace_line(AMX *amx, cell *params)
{
	int iEnt = params[1];
	if (iEnt > 0) {
		CHECK_ENTITY(iEnt);
	}

	cell *cStart = MF_GetAmxAddr(amx, params[2]);
	cell *cEnd = MF_GetAmxAddr(amx, params[3]);
	REAL fStartX = amx_ctof(cStart[0]);
	REAL fStartY = amx_ctof(cStart[1]);
	REAL fStartZ = amx_ctof(cStart[2]);
	REAL fEndX = amx_ctof(cEnd[0]);
	REAL fEndY = amx_ctof(cEnd[1]);
	REAL fEndZ = amx_ctof(cEnd[2]);

	cell *vRet = MF_GetAmxAddr(amx, params[4]);

	Vector vStart = Vector(fStartX, fStartY, fStartZ);
	Vector vEnd = Vector(fEndX, fEndY, fEndZ);
	
	if (iEnt > 0)
		TRACE_LINE(vStart, vEnd, dont_ignore_monsters, TypeConversion.id_to_edict(iEnt), &g_tr);
	else
		TRACE_LINE(vStart, vEnd, ignore_monsters, NULL, &g_tr);

	edict_t *pHit = g_tr.pHit;

	vRet[0] = amx_ftoc(g_tr.vecEndPos.x);
	vRet[1] = amx_ftoc(g_tr.vecEndPos.y);
	vRet[2] = amx_ftoc(g_tr.vecEndPos.z);

	if (FNullEnt(pHit))
		return 0;

	return ENTINDEX(pHit);
}

static cell AMX_NATIVE_CALL set_speak(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iNewSpeakFlags = params[2];

	if (iIndex > gpGlobals->maxClients || !MF_IsPlayerIngame(iIndex)) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", iIndex);
		return 0;
	}

	plinfo[iIndex].iSpeakFlags = iNewSpeakFlags;

	return 1;
}

static cell AMX_NATIVE_CALL get_speak(AMX *amx, cell *params) {
	int iIndex = params[1];

	if (iIndex > gpGlobals->maxClients || !MF_IsPlayerIngame(iIndex)) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", iIndex);
		return 0;
	}

	return plinfo[iIndex].iSpeakFlags;
}

static cell AMX_NATIVE_CALL get_decal_index(AMX *amx, cell *params)
{
	int len;
	char *szDecal = MF_GetAmxString(amx, params[1], 0, &len);
	return DECAL_INDEX(szDecal);
}

static cell AMX_NATIVE_CALL get_info_keybuffer(AMX *amx, cell *params)
{
	int iEnt = params[1];
	if (iEnt != -1) {
		CHECK_ENTITY(iEnt);
	}

	char *info = GETINFOKEYBUFFER((iEnt == -1) ? NULL : TypeConversion.id_to_edict(iEnt));
	
	return MF_SetAmxStringUTF8Char(amx, params[2], info, strlen(info), params[3]);
}

//from jghg, who says it doesn't work
// it works, it's just a picky engine call -sawce
static cell AMX_NATIVE_CALL drop_to_floor(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY(iEnt);

	edict_t *e = TypeConversion.id_to_edict(iEnt);

	return DROP_TO_FLOOR(e);
}

// Attachview, this allows you to attach a player's view to an entity.
// use AttachView(player, player) to reset view.
//(vexd)
static cell AMX_NATIVE_CALL attach_view(AMX *amx, cell *params)
{ 
	int iIndex = params[1];
	int iTargetIndex = params[2];

	CHECK_ENTITY(iIndex);
	CHECK_ENTITY(iTargetIndex);

	SET_VIEW(TypeConversion.id_to_edict(iIndex), TypeConversion.id_to_edict(iTargetIndex));

	return 1;
}

// SetView, this sets the view of a player. This is done by
// Creating a camera entity, which follows the player.
//(vexd)
static cell AMX_NATIVE_CALL set_view(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iCameraType = params[2];

	if (iIndex > gpGlobals->maxClients || !MF_IsPlayerIngame(iIndex)) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", iIndex);
		return 0;
	}

	edict_t *pPlayer = TypeConversion.id_to_edict(iIndex);
	edict_t *pNewCamera;

	switch(iCameraType)
	{
		case CAMERA_NONE:
			SET_VIEW(pPlayer, pPlayer);
			if(plinfo[ENTINDEX(pPlayer)].pViewEnt) {
				REMOVE_ENTITY(plinfo[ENTINDEX(pPlayer)].pViewEnt);
			}
			if (plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) // Verify that they were originally in a modified view
			{
				g_CameraCount--;
				if (g_CameraCount < 0)
					g_CameraCount=0;
				if (g_CameraCount==0) // Reset the AddToFullPack pointer if there's no more cameras in use...
					g_pFunctionTable->pfnAddToFullPack=NULL;
			}

			plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_NONE;
			plinfo[ENTINDEX(pPlayer)].pViewEnt = NULL;
			return 1;
			break;
		case CAMERA_3RDPERSON:
			if(plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
				return 1;
			}
			g_CameraCount++;
			g_pFunctionTable_Post->pfnAddToFullPack=AddToFullPack_Post;
			g_pFunctionTable_Post->pfnPlayerPostThink=PlayerPostThink_Post;

			plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_NOCLIP;
			pNewCamera->v.solid = SOLID_NOT;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			plinfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		case CAMERA_UPLEFT:
			if(plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_UPLEFT;
				return 1;
			}

			g_CameraCount++;
			g_pFunctionTable_Post->pfnAddToFullPack=AddToFullPack_Post;
			g_pFunctionTable_Post->pfnPlayerPostThink=PlayerPostThink_Post;

			plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_UPLEFT;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_NOCLIP;
			pNewCamera->v.solid = SOLID_NOT;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			plinfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		case CAMERA_TOPDOWN:
			if(plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_TOPDOWN;
				return 1;
			}

			g_CameraCount++;
			g_pFunctionTable_Post->pfnAddToFullPack=AddToFullPack_Post;
			g_pFunctionTable_Post->pfnPlayerPostThink=PlayerPostThink_Post;

			plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_TOPDOWN;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_NOCLIP;
			pNewCamera->v.solid = SOLID_NOT;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			plinfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		default:
			break;
	}

	return 1;
}

// SetLights, this sets the lights for the map.
//(vexd)
static cell AMX_NATIVE_CALL set_lights(AMX *amx, cell *params) { 
	int iLength;
	char *szLights = MF_GetAmxString(amx, params[1], 0, &iLength);

	if (FStrEq(szLights, "#OFF")) {
		glinfo.bCheckLights = false;
		memset(glinfo.szLastLights, 0x0, 128);
		LIGHT_STYLE(0, glinfo.szRealLights);
		return 1;
	}
	
	glinfo.bCheckLights = true;

	//Reset LastLights and store custom lighting
	memset(glinfo.szLastLights, 0x0, 128);
	memcpy(glinfo.szLastLights, szLights, ke::Min(iLength, 127));

	LightStyleDetour->DisableDetour();
	LIGHT_STYLE(0, szLights);
	LightStyleDetour->EnableDetour();

	// These make it so that players/weaponmodels look like whatever the lighting is
	// at. otherwise it would color players under the skybox to these values.
	SERVER_COMMAND("sv_skycolor_r 0\n");
	SERVER_COMMAND("sv_skycolor_g 0\n");
	SERVER_COMMAND("sv_skycolor_b 0\n");

	return 1;
}

//(mahnsawce)
static cell AMX_NATIVE_CALL trace_hull(AMX *amx,cell *params)
{
	int iEnt = params[3];
	if (iEnt > 0) {
		CHECK_ENTITY(iEnt);
	}

	int iResult=0;
	Vector vStart;
	Vector vEnd;
	cell *vCell;

	vCell = MF_GetAmxAddr(amx, params[1]);

	vStart.x = amx_ctof(vCell[0]);
	vStart.y = amx_ctof(vCell[1]);
	vStart.z = amx_ctof(vCell[2]);

	if (params[0] / sizeof(cell) >= 5 && (vCell = MF_GetAmxVectorNull(amx, params[5])))
	{

		vEnd.x = amx_ctof(vCell[0]);
		vEnd.y = amx_ctof(vCell[1]);
		vEnd.z = amx_ctof(vCell[2]);
	}
	else
		vEnd = vStart;


	TRACE_HULL(vStart, vEnd, params[4], params[2], iEnt > 0 ? TypeConversion.id_to_edict(iEnt) : NULL, &g_tr);

	if (g_tr.fStartSolid) {
		iResult += 1;
	}
	if (g_tr.fAllSolid) {
		iResult += 2;
	}
	if (!g_tr.fInOpen) {
		iResult += 4;
	}
	return iResult;
}

//(mahnsawce)
static cell AMX_NATIVE_CALL playback_event(AMX *amx, cell *params)
{
	/* Params:
	 * native playback_event(flags,invoker,eventindex,Float:delay,Float:origin[3],Float:angles[3],Float:fparam1,Float:fparam2,iparam1,iparam2,bparam1,bparam2)
	 *                         1     2         3          4            5                 6               7           8           9      10      11      12
	 */
	int flags;
	edict_t *pInvoker;
	unsigned short eventindex;
	REAL delay;
	vec3_t origin;
	vec3_t angles;
	REAL fparam1;
	REAL fparam2;
	int iparam1;
	int iparam2;
	int bparam1;
	int bparam2;
	flags = params[1];
	if (params[2] > 0) {
		CHECK_ENTITY(params[2]);
	}
	pInvoker=TypeConversion.id_to_edict(params[2]);
	eventindex=params[3];
	delay=amx_ctof(params[4]);
	cell *cOrigin=MF_GetAmxAddr(amx, params[5]);
	cell *cAngles=MF_GetAmxAddr(amx, params[6]);
	origin.x=amx_ctof(cOrigin[0]);
	origin.y=amx_ctof(cOrigin[1]);
	origin.z=amx_ctof(cOrigin[2]);
	angles.x=amx_ctof(cAngles[0]);
	angles.y=amx_ctof(cAngles[1]);
	angles.z=amx_ctof(cAngles[2]);
	fparam1=amx_ctof(params[7]);
	fparam2=amx_ctof(params[8]);
	iparam1=params[9];
	iparam2=params[10];
	bparam1=params[11];
	bparam2=params[12];
	PLAYBACK_EVENT_FULL(flags, pInvoker,eventindex, delay, origin, angles, fparam1, fparam2, iparam1, iparam2, bparam1, bparam2);
	return 1;
}

//(mahnsawce)
static cell AMX_NATIVE_CALL get_usercmd(AMX *amx, cell *params)
{
	if (!incmd)
		return 0;
	int type = params[1];
	if (type > usercmd_int_start && type < usercmd_int_end)
	{
		// Requesting an integer value...
		switch(type)
		{
		case usercmd_lerp_msec:
			return g_cmd->lerp_msec;
		case usercmd_msec:
			return g_cmd->msec;
		case usercmd_lightlevel:
			return g_cmd->lightlevel;
		case usercmd_buttons:
			return g_cmd->buttons;
		case usercmd_weaponselect:
			return g_cmd->weaponselect;
		case usercmd_impact_index:
			return g_cmd->impact_index;
		default:
			return 0;
		}
	}
	else if (type > usercmd_float_start && type < usercmd_float_end)
	{
		// Requesting a single float value
		// The second parameter needs to be the float variable.

		cell *cRet = MF_GetAmxAddr(amx, params[2]);
		switch(type)
		{
		case usercmd_forwardmove:
			*cRet = amx_ftoc(g_cmd->forwardmove);
			return 1;
		case usercmd_sidemove:
			*cRet = amx_ftoc(g_cmd->sidemove);
			return 1;
		case usercmd_upmove:
			*cRet = amx_ftoc(g_cmd->upmove);
			return 1;
		default:
			return 0;
		}
	}
	else if (type > usercmd_vec_start && type < usercmd_vec_end)
	{
		// Requesting a Vector value.
		cell *cRet = MF_GetAmxAddr(amx,params[2]);
		switch (type)
		{
		case usercmd_viewangles:
			cRet[0] = amx_ftoc(g_cmd->viewangles.x);
			cRet[1] = amx_ftoc(g_cmd->viewangles.y);
			cRet[2] = amx_ftoc(g_cmd->viewangles.z);
			return 1;
		case usercmd_impact_position:
			cRet[0] = amx_ftoc(g_cmd->impact_position.x);
			cRet[1] = amx_ftoc(g_cmd->impact_position.y);
			cRet[2] = amx_ftoc(g_cmd->impact_position.z);
			return 1;
		default:
			return 0;
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL set_usercmd(AMX *amx, cell *params)
{
	if (!incmd)
		return 0;
	int type = params[1];
	if (type > usercmd_int_start && type < usercmd_int_end)
	{
		// Setting an integer value...
		cell *blah = MF_GetAmxAddr(amx,params[2]);
		int iValue = blah[0];
		switch(type)
		{
		case usercmd_lerp_msec:
			g_cmd->lerp_msec = iValue;
			return 1;
		case usercmd_msec:
			g_cmd->msec = iValue;
			return 1;
		case usercmd_lightlevel:
			g_cmd->lightlevel = iValue;
			return 1;
		case usercmd_buttons:
			g_cmd->buttons = iValue;
			return 1;
		case usercmd_weaponselect:
			g_cmd->weaponselect = iValue;
			return 1;
		case usercmd_impact_index:
			g_cmd->impact_index = iValue;
			return 1;
		default:
			return 0;
		}
	}
	else if (type > usercmd_float_start && type < usercmd_float_end)
	{
		// Requesting a single float value
		// The second parameter needs to be the float variable.

		cell *blah = MF_GetAmxAddr(amx,params[2]);
		REAL fValue = amx_ctof(blah[0]);
		switch(type)
		{
		case usercmd_forwardmove:
			g_cmd->forwardmove = fValue;
			return 1;
		case usercmd_sidemove:
			g_cmd->sidemove = fValue;
			return 1;
		case usercmd_upmove:
			g_cmd->upmove = fValue;
			return 1;
		default:
			return 0;
		}
	}
	else if (type > usercmd_vec_start && type < usercmd_vec_end)
	{
		// Requesting a Vector value.
		Vector vValue;
		cell *blah = MF_GetAmxAddr(amx,params[2]);
		vValue.x = amx_ctof(blah[0]);
		vValue.y = amx_ctof(blah[1]);
		vValue.z = amx_ctof(blah[2]);
		switch (type)
		{
		case usercmd_viewangles:
			g_cmd->viewangles = vValue;
			return 1;
		case usercmd_impact_position:
			g_cmd->impact_position = vValue;
			return 1;
		default:
			return 0;
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL is_visible(AMX *amx, cell *params)
{
	int src = params[1];
	int dest = params[2];
	CHECK_ENTITY(src);
	CHECK_ENTITY(dest);

	edict_t *pEntity = TypeConversion.id_to_edict(src);
	edict_t *pTarget = TypeConversion.id_to_edict(dest);

	if (pTarget->v.flags & FL_NOTARGET)
		return 0;

	Vector vLooker = pEntity->v.origin + pEntity->v.view_ofs;
	Vector vTarget = pTarget->v.origin + pTarget->v.view_ofs;

	TraceResult tr;

	TRACE_LINE(vLooker, vTarget, FALSE, pEntity, &tr);

	if (tr.fInOpen && tr.fInWater)
		return 0;
	else if (tr.flFraction == 1.0)
		return 1;

	return 0;
}

//taken from dlls\combat.cpp
static cell AMX_NATIVE_CALL in_view_cone(AMX *amx, cell *params)
{
	int src = params[1];

	CHECK_ENTITY(src);

	edict_t *pEdictSrc = TypeConversion.id_to_edict(src);

	Vector vecLOS, vecForward;
	float flDot;

	cell *addr = MF_GetAmxAddr(amx, params[2]);
	Vector origin(amx_ctof(addr[0]), amx_ctof(addr[1]), amx_ctof(addr[2]));

	bool use2D = (params[0] / sizeof(cell)) == 2 || params[3] == 0;

	if (use2D)
	{
		MAKE_VECTORS(pEdictSrc->v.angles);
		vecForward = gpGlobals->v_forward;

		vecLOS = origin - pEdictSrc->v.origin;

		vecForward.z = 0;
		vecLOS.z = 0;
	} 
	else 
	{
		MAKE_VECTORS(pEdictSrc->v.v_angle);
		vecForward = gpGlobals->v_forward;

		vecLOS = origin - (pEdictSrc->v.origin + pEdictSrc->v.view_ofs);
	}

	vecLOS = vecLOS.Normalize();

	flDot = DotProduct(vecLOS, vecForward);

	/* Dividing fov by two and then converting it to radians - don't ask about this ever again, please :\ */
	if (flDot >= cos(pEdictSrc->v.fov * (M_PI / 360)))
		return 1;
	else
		return 0;
}

static cell AMX_NATIVE_CALL traceresult(AMX *amx, cell *params)
{
	int type = params[1];
	cell *cRet;
/*
	TR_AllSolid,			// (int) if true, plane is not valid
	TR_StartSolid,		// (int) if true, the initial point was in a solid area
	TR_InOpen,		// (int)
	TR_InWater,	// (int)
	TR_Fraction,			// (float) time completed, 1.0 = didn't hit anything
	TR_EndPos,			// (vector) final position
	TR_PlaneDist,		// (float)
	TR_PlaneNormal,		// (vector) surface normal at impact
	TR_Hit,				// (entity) entity the surface is on
	TR_Hitgroup			// (int) 0 == generic, non zero is specific body part
*/
	switch (type)
	{
	case TR_AllSolid:
		return g_tr.fAllSolid;
	case TR_StartSolid:
		return g_tr.fStartSolid;
	case TR_InOpen:
		return g_tr.fInOpen;
	case TR_InWater:
		return g_tr.fInWater;
	case TR_Hitgroup:
		return g_tr.iHitgroup;
	case TR_Hit:
		if (!FNullEnt(g_tr.pHit))
			return ENTINDEX(g_tr.pHit);
		else
			return -1;
	case TR_Fraction:
		cRet = MF_GetAmxAddr(amx,params[2]);
		cRet[0] = amx_ftoc(g_tr.flFraction);
		return 1;
	case TR_EndPos:
		cRet = MF_GetAmxAddr(amx,params[2]);
		cRet[0] = amx_ftoc(g_tr.vecEndPos[0]);
		cRet[1] = amx_ftoc(g_tr.vecEndPos[1]);
		cRet[2] = amx_ftoc(g_tr.vecEndPos[2]);
		return 1;
	case TR_PlaneDist:
		cRet = MF_GetAmxAddr(amx,params[2]);
		cRet[0] = amx_ftoc(g_tr.flPlaneDist);
		return 1;
	case TR_PlaneNormal:
		cRet = MF_GetAmxAddr(amx,params[2]);
		cRet[0] = amx_ftoc(g_tr.vecPlaneNormal[0]);
		cRet[1] = amx_ftoc(g_tr.vecPlaneNormal[1]);
		cRet[2] = amx_ftoc(g_tr.vecPlaneNormal[2]);
		return 1;		
	}
	return 0;
}

// (jghg)
static cell AMX_NATIVE_CALL get_string(AMX *amx, cell *params) // (string, returnstring[], length)
{
	ke::SafeSprintf(g_buffer, sizeof(g_buffer), "%s", STRING(params[1]));
	return MF_SetAmxString(amx, params[2], g_buffer, params[3]);
}

//contributed by twistedeuphoria
static cell AMX_NATIVE_CALL trace_forward(AMX *amx, cell *params)
//native trace_forward(Float:start[3], Float:angles[3], give, ignoreEnt, &Float:hitX, &Float:hitY, &Float:shortestDistance, &Float:shortestDistLow, &Float:shortestDistHigh)
{
   cell *cStart = MF_GetAmxAddr(amx, params[1]);
   cell *cAngles = MF_GetAmxAddr(amx, params[2]);
   REAL fGive = amx_ctof(params[3]);
   int iIgnoreEnt = params[4];
   if (iIgnoreEnt > 0) {
	   CHECK_ENTITY(iIgnoreEnt);
   }

   cell *hitX = MF_GetAmxAddr(amx, params[5]);
   cell *hitY = MF_GetAmxAddr(amx, params[6]);
   cell *shortestDistance = MF_GetAmxAddr(amx, params[7]);
   cell *shortestDistLow = MF_GetAmxAddr(amx, params[8]);
   cell *shortestDistHigh = MF_GetAmxAddr(amx, params[9]);

   if(fGive < 0.0)
	  fGive = 20.0;

   REAL fStartX = amx_ctof(cStart[0]);
   REAL fStartY = amx_ctof(cStart[1]);
   REAL fStartZ = amx_ctof(cStart[2]);

   REAL fAnglesX = amx_ctof(cAngles[0]);
   REAL fAnglesY = amx_ctof(cAngles[1]);
   REAL fAnglesZ = amx_ctof(cAngles[2]);
   Vector playerAngleVector = Vector(fAnglesX,fAnglesY,fAnglesZ);
   MAKE_VECTORS(playerAngleVector);
   
   Vector forwardVector = gpGlobals->v_forward;
   REAL fEndX = forwardVector[0] * 4000;
   REAL fEndY = forwardVector[1] * 4000;

   REAL fClosestDist = 999999.9;
   REAL fClosestLow = 0.0;
   REAL fClosestHigh = 0.0;
   REAL fClosestX = 0.0;
   REAL fClosestY = 0.0;
   TraceResult tr;
   REAL fRetX;
   REAL fRetY;
   REAL fRetZ;

   for(int inum=-36;inum<=36;inum++)
   {
	  REAL fUseZ = fStartZ + (REAL)inum;
	  Vector vStart =  Vector(fStartX, fStartY, fUseZ);
	  Vector vEnd = Vector(fEndX, fEndY, fUseZ);
	  if(iIgnoreEnt > 0)
		  TRACE_LINE(vStart, vEnd, dont_ignore_monsters, TypeConversion.id_to_edict(iIgnoreEnt), &tr);
	  else
		  TRACE_LINE(vStart, vEnd, ignore_monsters, NULL, &tr);
	  fRetX = tr.vecEndPos.x;
	  fRetY = tr.vecEndPos.y;
	  fRetZ = tr.vecEndPos.z;
	  Vector vHit = Vector(fRetX, fRetY, fRetZ);

	  REAL fLength = (vStart - vHit).Length();
	  if(fabs(fLength - fClosestDist) < fGive)
			fClosestHigh = fUseZ - fStartZ;
	  else if(fLength < fClosestDist)
	  {
		 fClosestDist = fLength;
		 fClosestLow = fUseZ - fStartZ;
		 fClosestHigh = fUseZ - fStartZ;
		 fClosestX = fRetX;
		 fClosestY = fRetY;
	  }
   }
   fClosestLow += 36.0;
   fClosestHigh += 36.0;

   *hitX = amx_ftoc(fClosestX);
   *hitY = amx_ftoc(fClosestY);
   *shortestDistance = amx_ftoc(fClosestDist);
   *shortestDistLow = amx_ftoc(fClosestLow);
   *shortestDistHigh = amx_ftoc(fClosestHigh);
   return 1;
}

AMX_NATIVE_INFO engine_NewNatives[] = 
{
	{"trace_line",			trace_line},
	{NULL,					NULL}
};

AMX_NATIVE_INFO engine_Natives[] = {
	{"halflife_time",		halflife_time},

	//These are mostly from original VexD

	{"radius_damage",		RadiusDamage},
	{"point_contents",		PointContents},
	{"trace_normal",		trace_normal},
	{"trace_hull",			trace_hull},
	{"traceresult",			traceresult},

	{"set_speak",			set_speak},
	{"get_speak",			get_speak},

	{"playback_event",		playback_event},

	{"set_view",			set_view},
	{"attach_view",			attach_view},

	{"get_decal_index",		get_decal_index},
	{"get_info_keybuffer",	get_info_keybuffer},
	{"set_lights",			set_lights},
	{"drop_to_floor",		drop_to_floor},

	{"get_usercmd",			get_usercmd},
	{"set_usercmd",			set_usercmd},

	{"register_impulse",	register_impulse},
	{"register_think",		register_think},
	{"register_touch",		register_touch},
	{"unregister_impulse",	unregister_impulse},
	{"unregister_think",	unregister_think},
	{"unregister_touch",	unregister_touch},

	{"eng_get_string",		get_string},
	{"is_in_viewcone",		in_view_cone},
	{"is_visible",			is_visible},
	{"trace_forward",		trace_forward},

	{NULL,					NULL}
	 ///////////////////
};
