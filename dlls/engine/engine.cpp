#include "engine.h"

bool inKeyValue=false;
KeyValueData *g_pkvd;
struct usercmd_s *g_cmd;
struct PlayerInfo plinfo[33];
struct GlobalInfo glinfo;

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

static cell AMX_NATIVE_CALL halflife_time(AMX *amx, cell *params)
{
	REAL fVal = gpGlobals->time;

	return amx_ftoc(fVal);
}

static cell AMX_NATIVE_CALL VelocityByAim(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int iVelocity = params[2];
	cell *vRet = MF_GetAmxAddr(amx, params[3]);
	Vector vVector = Vector(0, 0, 0);

	if (!is_ent_valid(iEnt)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	edict_t *pEnt = INDEXENT(iEnt);

	MAKE_VECTORS(pEnt->v.v_angle);
	vVector = gpGlobals->v_forward * iVelocity;

	vRet[0] = amx_ftoc(vVector.x);
	vRet[1] = amx_ftoc(vVector.y);
	vRet[2] = amx_ftoc(vVector.z);

	return 1;
}

// RadiusDamage. Damages players within a certain radius. ToDo: add the
// damage messaging so players know where the damage is coming from
// (the red arrow-like things on the screen).
//(vexd)
static cell AMX_NATIVE_CALL RadiusDamage(AMX *amx, cell *params) {
	cell *cAddr = MF_GetAmxAddr(amx,params[1]);

	REAL fCurrentX = amx_ctof(cAddr[0]);
	REAL fCurrentY = amx_ctof(cAddr[1]);
	REAL fCurrentZ = amx_ctof(cAddr[2]);
	int iDamageMultiplier = params[2];
	int iRadiusMultiplier = params[3];

	Vector vOrigin = Vector(fCurrentX, fCurrentY, fCurrentZ);

	edict_t *pSearchEnt = NULL;
	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 5 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 10 + RANDOM_FLOAT(0,1 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 4 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 25 + RANDOM_FLOAT(0,2 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 3 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 50 + RANDOM_FLOAT(0,3 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 2 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) MDLL_ClientKill(pSearchEnt);
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

static cell AMX_NATIVE_CALL vector_to_angle(AMX *amx, cell *params)
{
	cell *cAddr = MF_GetAmxAddr(amx, params[1]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);

	Vector vVector = Vector(fX, fY, fZ);
	Vector vAngle = Vector(0, 0, 0);
	VEC_TO_ANGLES(vVector, vAngle);

	cell *vRet = MF_GetAmxAddr(amx, params[2]);
	vRet[0] = amx_ftoc(vAngle.x);
	vRet[1] = amx_ftoc(vAngle.y);
	vRet[2] = amx_ftoc(vAngle.z);

	return 1;
}

static cell AMX_NATIVE_CALL vector_length(AMX *amx, cell *params)
{
	cell *cAddr = MF_GetAmxAddr(amx, params[1]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);

	Vector vVector = Vector(fX, fY, fZ);

	REAL fLength = vVector.Length();

	return amx_ftoc(fLength);
}

static cell AMX_NATIVE_CALL vector_distance(AMX *amx, cell *params)
{
	cell *cAddr = MF_GetAmxAddr(amx, params[1]);
	cell *cAddr2 = MF_GetAmxAddr(amx, params[2]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);
	REAL fX2 = amx_ctof(cAddr2[0]);
	REAL fY2 = amx_ctof(cAddr2[1]);
	REAL fZ2 = amx_ctof(cAddr2[2]);

	Vector vVector = Vector(fX, fY, fZ);
	Vector vVector2 = Vector(fX2, fY2, fZ2);

	REAL fLength = (vVector - vVector2).Length();

	return amx_ftoc(fLength);
}

static cell AMX_NATIVE_CALL trace_normal(AMX *amx, cell *params)
{
	int iEnt = params[1];

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

	TraceResult tr;
	TRACE_LINE(vStart, vEnd, dont_ignore_monsters, INDEXENT(iEnt), &tr);

	vRet[0] = amx_ftoc(tr.vecPlaneNormal.x);
	vRet[1] = amx_ftoc(tr.vecPlaneNormal.y);
	vRet[2] = amx_ftoc(tr.vecPlaneNormal.z);

	if (tr.flFraction >= 1.0)
		return 0;

	return 1;
}

static cell AMX_NATIVE_CALL trace_line(AMX *amx, cell *params)
{
	int iEnt = params[1];

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

	TraceResult tr;

	if (iEnt == -1)
		TRACE_LINE(vStart, vEnd, ignore_monsters, NULL, &tr);
	else
		TRACE_LINE(vStart, vEnd, dont_ignore_monsters, INDEXENT(iEnt), &tr);

	edict_t *pHit = tr.pHit;

	vRet[0] = amx_ftoc(tr.vecEndPos.x);
	vRet[1] = amx_ftoc(tr.vecEndPos.y);
	vRet[2] = amx_ftoc(tr.vecEndPos.z);

	if (FNullEnt(pHit))
		return 0;

	return ENTINDEX(pHit);
}

static cell AMX_NATIVE_CALL set_speak(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iNewSpeakFlags = params[2];

	if (iIndex> 32 || !is_ent_valid(iIndex)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	plinfo[iIndex].iSpeakFlags = iNewSpeakFlags;

	return 1;
}

static cell AMX_NATIVE_CALL get_speak(AMX *amx, cell *params) {
	int iIndex = params[1];

	if (!is_ent_valid(iIndex) || iIndex > 32) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
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

static cell AMX_NATIVE_CALL precache_event(AMX *amx, cell *params)
{
	int len;
	char *szEvent = MF_FormatAmxString(amx, params, 2, &len);
	PRECACHE_EVENT(params[1], (char *)STRING(ALLOC_STRING(szEvent)));

	return 1;
}

static cell AMX_NATIVE_CALL get_info_keybuffer(AMX *amx, cell *params)
{
	int iEnt = params[1];
	
	if (!is_ent_valid(iEnt)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t *e = INDEXENT(iEnt);

	char *info = GETINFOKEYBUFFER(e);
	
	return MF_SetAmxString(amx, params[2], info, params[3]);
}

//from jghg, who says it doesn't work
static cell AMX_NATIVE_CALL drop_to_floor(AMX *amx, cell *params)
{
	int iEnt = params[1];

	if (!is_ent_valid(iEnt)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t *e = INDEXENT(iEnt);

	return DROP_TO_FLOOR(e);
}

static cell AMX_NATIVE_CALL precache_generic(AMX *amx, cell *params)
{
	int len;
	char* szPreCache = MF_GetAmxString(amx,params[1],0,&len);
	PRECACHE_GENERIC((char*)STRING(ALLOC_STRING(szPreCache)));
	return 1;
}

// Attachview, this allows you to attach a player's view to an entity.
// use AttachView(player, player) to reset view.
//(vexd)
static cell AMX_NATIVE_CALL attach_view(AMX *amx, cell *params)
{ 
	int iIndex = params[1];
	int iTargetIndex = params[2];

	if (iIndex > 32 || !is_ent_valid(iIndex)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	if (!is_ent_valid(iTargetIndex)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	SET_VIEW(INDEXENT(iIndex), INDEXENT(iTargetIndex));

	return 1;
}

// SetView, this sets the view of a player. This is done by
// Creating a camera entity, which follows the player.
//(vexd)
static cell AMX_NATIVE_CALL set_view(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iCameraType = params[2];

	if (iIndex > 32 || !is_ent_valid(iIndex)) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t *pPlayer = INDEXENT(iIndex);
	edict_t *pNewCamera;

	switch(iCameraType)
	{
		case CAMERA_NONE:
			SET_VIEW(pPlayer, pPlayer);
			if(plinfo[ENTINDEX(pPlayer)].pViewEnt) {
				REMOVE_ENTITY(plinfo[ENTINDEX(pPlayer)].pViewEnt);
			}
			plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_NONE;
			plinfo[ENTINDEX(pPlayer)].pViewEnt = NULL;

			pPlayer->v.rendermode = plinfo[ENTINDEX(pPlayer)].iRenderMode;
			pPlayer->v.renderamt = plinfo[ENTINDEX(pPlayer)].fRenderAmt;

			plinfo[ENTINDEX(pPlayer)].iRenderMode = 0;
			plinfo[ENTINDEX(pPlayer)].fRenderAmt = 0;

			return 1;
			break;
		case CAMERA_3RDPERSON:
			if(plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
				return 1;
			}

			plinfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			plinfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

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

			plinfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			plinfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

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
		case CAMERA_TOPDOWN:
			if(plinfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				plinfo[ENTINDEX(pPlayer)].iViewType = CAMERA_TOPDOWN;
				return 1;
			}

			plinfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			plinfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

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
		glinfo.bLights = false;
		memset(glinfo.szLastLights, 0x0, 128);
		(g_engfuncs.pfnLightStyle)(0, (char *)glinfo.szRealLights);
		return 1;
	}

	glinfo.bLights = true;
	glinfo.bCheckLights = true;

	//Reset LastLights
	memset(glinfo.szLastLights, 0x0, 128);
	//Store the previous lighting.
	memcpy(glinfo.szLastLights, szLights, strlen(szLights));

	(g_engfuncs.pfnLightStyle)(0, szLights);

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
	int iResult=0;
	TraceResult tr;
	Vector vPos;
	cell *vCell;

	vCell = MF_GetAmxAddr(amx, params[1]);

	vPos.x = amx_ctof(vCell[0]);
	vPos.y = amx_ctof(vCell[1]);
	vPos.z = amx_ctof(vCell[2]);

	TRACE_HULL(vPos,vPos, params[4], params[2], params[3] > 0 ? INDEXENT(params[3]) : 0 , &tr);

	if (tr.fStartSolid) {
		iResult += 1;
	}
	if (tr.fAllSolid) {
		iResult += 2;
	}
	if (!tr.fInOpen) {
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
	pInvoker=INDEXENT(params[2]);
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
static cell AMX_NATIVE_CALL angle_vector(AMX *amx, cell *params)
{
	Vector v_angles,v_forward,v_right,v_up,v_return;
	cell *vCell = MF_GetAmxAddr(amx, params[1]);
	v_angles.x = amx_ctof(vCell[0]);
	v_angles.y = amx_ctof(vCell[1]);
	v_angles.z = amx_ctof(vCell[2]);
	g_engfuncs.pfnAngleVectors(v_angles,v_forward,v_right,v_up);
	if (params[2] == ANGLEVECTORS_FORWARD)
		v_return = v_forward;
	if (params[2] == ANGLEVECTORS_RIGHT)
		v_return = v_right;
	if (params[2] == ANGLEVECTORS_UP)
		v_return = v_up;
	vCell = MF_GetAmxAddr(amx,params[3]);
	vCell[0] = amx_ftoc(v_return.x);
	vCell[1] = amx_ftoc(v_return.y);
	vCell[2] = amx_ftoc(v_return.z);
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
			*cRet = *(cell*)&g_cmd->forwardmove;
			return 1;
		case usercmd_sidemove:
			*cRet = *(cell*)&g_cmd->sidemove;
			return 1;
		case usercmd_upmove:
			*cRet = *(cell*)&g_cmd->upmove;
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
			cRet[0] = *(cell*)&g_cmd->viewangles.x;
			cRet[1] = *(cell*)&g_cmd->viewangles.y;
			cRet[2] = *(cell*)&g_cmd->viewangles.z;
			return 1;
		case usercmd_impact_position:
			cRet[0] = *(cell*)&g_cmd->impact_position.x;
			cRet[1] = *(cell*)&g_cmd->impact_position.y;
			cRet[2] = *(cell*)&g_cmd->impact_position.z;
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

//(mahnsawce)
static cell AMX_NATIVE_CALL take_damage(AMX *amx, cell *params)
{
	int indexa = params[1];
	int indexb = params[2];
	int indexc = params[3];
	if (!is_ent_valid(indexa) || !is_ent_valid(indexb) || !is_ent_valid(indexc)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}
	REAL fnDamage = amx_ctof(params[4]);
	int inType = params[5];
	edict_t* pEntitya = INDEXENT(indexa);
	edict_t* pEntityb = INDEXENT(indexb);
	edict_t* pEntityc = INDEXENT(indexc);
	CBaseEntity *pCEntity = NULL;
	pCEntity = CBaseEntity::Instance(INDEXENT(indexa));
	pCEntity->TakeDamage(VARS(pEntityb),VARS(pEntityc),fnDamage,inType);
	return 1;
}

AMX_NATIVE_INFO engine_Natives[] = {
	{"halflife_time",		halflife_time},

	//These are mostly from original VexD
	{"VelocityByAim",		VelocityByAim},
	{"RadiusDamage",		RadiusDamage},
	{"PointContents",		PointContents},
	{"vector_to_angle",		vector_to_angle},
	{"angle_vector",		angle_vector},
	{"vector_length",		vector_length},
	{"vector_distance",		vector_distance},
	{"trace_normal",		trace_normal},
	{"trace_line",			trace_line},
	{"trace_hull",			trace_hull},
	{"take_damage",			take_damage},

	{"set_speak",			set_speak},
	{"get_speak",			get_speak},

	{"precache_event",		precache_event},
	{"precache_generic",	precache_generic},
	{"playback_event",		playback_event},

	{"set_view",			set_view},
	{"attach_view",			attach_view},

	{"get_decal_index",		get_decal_index},
	{"get_info_keybuffer",	get_info_keybuffer},
	{"set_lights",			set_lights},
	{"drop_to_floor",		drop_to_floor},

	{"get_usercmd",			get_usercmd},
	{"set_usercmd",			set_usercmd},

	{NULL,					NULL},
	 ///////////////////
};
