#include "engine.h"

struct usercmd_s *g_cmd;
struct PlayerInfo plinfo[33];
struct GlobalInfo glinfo;

TraceResult g_tr;

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
	p->Class = new char[strlen(clsname)+1];
	strcpy(p->Class, clsname);

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[2], 0, &len), FP_CELL);

	Thinks.push_back(p);

	return p->Forward;
}

static cell AMX_NATIVE_CALL register_impulse(AMX *amx, cell *params)
{
	int len;

	Impulse *p = new Impulse;
	p->Check = params[1];

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[2], 0, &len), FP_CELL);

	Impulses.push_back(p);

	return p->Forward;
}

static cell AMX_NATIVE_CALL register_touch(AMX *amx, cell *params)
{
	int len;

	char *Toucher = MF_GetAmxString(amx, params[1], 0, &len);
	char *Touched = MF_GetAmxString(amx, params[2], 1, &len);

	Touch *p = new Touch;

	if (!strlen(Toucher) || strcmp(Toucher, "*")==0) {
		p->Toucher = 0;
	} else {
		p->Toucher = new char[strlen(Toucher)+1];
		strcpy(p->Toucher, Toucher);
	}
	if (!strlen(Touched) || strcmp(Touched, "*")==0) {
		p->Touched = 0;
	} else {
		p->Touched = new char[strlen(Touched)+1];
		strcpy(p->Touched, Touched);
	}

	p->Forward = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[3], 2, &len), FP_CELL, FP_CELL);

	Touches.push_back(p);

	return p->Forward;
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

//(mahnsawce)
static cell AMX_NATIVE_CALL engfunc(AMX *amx, cell *params)
{
	// Variables I will need throughout all the different calls.
	int type = params[1];
//	LOG_CONSOLE(PLID,"Called: %i %i",type,*params/sizeof(cell));
	int len;
	char *temp;
	char *temp2;
	cell *cRet;
	vec3_t	Vec1;
	vec3_t	Vec2;
	vec3_t	Vec3;
	vec3_t	Vec4;
	int iparam1;
	int iparam2;
	int iparam3;
	int iparam4;
	int iparam5;
	int iparam6;
	float fparam1;
	float fparam2;
	float fparam3;
//	float fTemp[3];
	int index;
	edict_t *pRet=NULL;
	// Now start calling.. :/
	switch (type)
	{
		// pfnPrecacheModel
	case 	EngFunc_PrecacheModel:	// int  )			(char* s);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		if (temp[0]==0)
			return 0;
		return (*g_engfuncs.pfnPrecacheModel)((char*)STRING(ALLOC_STRING(temp)));


		// pfnPrecacheSound
	case	EngFunc_PrecacheSound:	// int  )			(char* s);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		if (temp[0]==0)
			return 0;
		return (*g_engfuncs.pfnPrecacheSound)((char*)STRING(ALLOC_STRING(temp)));


		// pfnSetModel
	case	EngFunc_SetModel:		// void )				(edict_t *e, const char *m);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnSetModel)(INDEXENT(index),(char*)STRING(ALLOC_STRING(temp)));
		return 1;


		// pfnModelIndex
	case 	EngFunc_ModelIndex:
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return (*g_engfuncs.pfnModelIndex)(temp);


		// pfnModelFrames
	case	EngFunc_ModelFrames:	// int	)			(int modelIndex);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		return (*g_engfuncs.pfnModelFrames)(index);


		// pfnSetSize
	case	EngFunc_SetSize:		// void )				(edict_t *e, const float *rgflMin, const float *rgflMax);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		(*g_engfuncs.pfnSetSize)(INDEXENT(index),Vec1,Vec2);
		return 1;


		// pfnChangeLevel (is this needed?)
	case		EngFunc_ChangeLevel:			// void )			(char* s1, char* s2);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		temp2 = MF_GetAmxString(amx,params[3],1,&len);
		(*g_engfuncs.pfnChangeLevel)(temp,temp2);
		return 1;
		

		// pfnVecToYaw
	case		EngFunc_VecToYaw:			// float)				(const float *rgflVector);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1= (*g_engfuncs.pfnVecToYaw)(Vec1);
		cRet[0] = amx_ftoc(fparam1);
		return 1;


		// pfnVecToAngles
	case		EngFunc_VecToAngles:			// void )			(const float *rgflVectorIn, float *rgflVectorOut);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);

		(*g_engfuncs.pfnVecToAngles)(Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[3]);
		cRet[0]=amx_ftoc(Vec2[0]);
		cRet[1]=amx_ftoc(Vec2[1]);
		cRet[2]=amx_ftoc(Vec2[2]);
		return 1;


		// pfnMoveToOrigin
	case		EngFunc_MoveToOrigin:		// void )			(edict_t *ent, const float *pflGoal, float dist, int iMoveType);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam1=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam1=cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnMoveToOrigin)(INDEXENT(index),Vec1,fparam1,iparam1);
		return 1;


		// pfnChangeYaw
	case		EngFunc_ChangeYaw:			// void )				(edict_t* ent);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnChangeYaw)(INDEXENT(index));
		return 1;


		// pfnChangePitch
	case		EngFunc_ChangePitch:			// void )			(edict_t* ent);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnChangePitch)(INDEXENT(index));
		return 1;


		// pfnFindEntityByString
	case		EngFunc_FindEntityByString:	// edict)	(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		pRet = (*g_engfuncs.pfnFindEntityByString)(index == -1 ? NULL : INDEXENT(index),temp,temp2);
		if (pRet)
			return ENTINDEX(pRet);
		return -1;


		// pfnGetEntityIllum
	case	EngFunc_GetEntityIllum:		// int	)		(edict_t* pEnt);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		return (*g_engfuncs.pfnGetEntityIllum)(INDEXENT(index));


		// pfnFindEntityInSphere
	case 	EngFunc_FindEntityInSphere:	// edict)	(edict_t *pEdictStartSearchAfter, const float *org, float rad);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam1 = amx_ctof(cRet[0]);
		pRet = (*g_engfuncs.pfnFindEntityInSphere)(index == -1 ? NULL : INDEXENT(index),Vec1,fparam1);
		if (pRet)
				return ENTINDEX(pRet);
		return -1;


		// pfnFindClientsInPVS
	case	EngFunc_FindClientInPVS:		// edict)		(edict_t *pEdict);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		pRet=(*g_engfuncs.pfnFindClientInPVS)(INDEXENT(index));
		return ENTINDEX(pRet);


		// pfnEntitiesInPVS
	case	EngFunc_EntitiesInPVS:		// edict)			(edict_t *pplayer);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		pRet=(*g_engfuncs.pfnEntitiesInPVS)(INDEXENT(index));
		return ENTINDEX(pRet);


		// pfnMakeVectors
	case	EngFunc_MakeVectors:			// void )			(const float *rgflVector);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		(*g_engfuncs.pfnMakeVectors)(Vec1);
		return 1;

		
		// pfnAngleVectors
	case	EngFunc_AngleVectors:		// void )			(const float *rgflVector, float *forward, float *right, float *up);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		(*g_engfuncs.pfnAngleVectors)(Vec1,Vec2,Vec3,Vec4);
		cRet = MF_GetAmxAddr(amx,params[3]);
		cRet[0] = amx_ftoc(Vec2[0]);
		cRet[1] = amx_ftoc(Vec2[1]);
		cRet[2] = amx_ftoc(Vec2[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0] = amx_ftoc(Vec3[0]);
		cRet[1] = amx_ftoc(Vec3[1]);
		cRet[2] = amx_ftoc(Vec3[2]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		cRet[0] = amx_ftoc(Vec4[0]);
		cRet[1] = amx_ftoc(Vec4[1]);
		cRet[2] = amx_ftoc(Vec4[2]);
		return 1;


		// pfnCreateEntity
	case	EngFunc_CreateEntity:		// edict)			(void);
		pRet = (*g_engfuncs.pfnCreateEntity)();
		if (pRet)
			return ENTINDEX(pRet);
		return 0;


		// pfnRemoveEntity
	case	EngFunc_RemoveEntity:		// void )			(edict_t* e);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		if (index == 0)
			return 0;
		(*g_engfuncs.pfnRemoveEntity)(INDEXENT(index));
		return 1;


		// pfnCreateNamedEntity
	case	EngFunc_CreateNamedEntity:	// edict)		(int className);
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		pRet = (*g_engfuncs.pfnCreateNamedEntity)(iparam1);
		if (pRet)
			return ENTINDEX(pRet);
		return 0;


		// pfnMakeStatic
	case	EngFunc_MakeStatic:			// void )			(edict_t *ent);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnMakeStatic)(INDEXENT(index));
		return 1;


		// pfnEntIsOnFloor
	case	EngFunc_EntIsOnFloor:		// int  )			(edict_t *e);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		return (*g_engfuncs.pfnEntIsOnFloor)(INDEXENT(index));


		// pfnDropToFloor
	case	EngFunc_DropToFloor:			// int  )			(edict_t* e);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		return (*g_engfuncs.pfnDropToFloor)(INDEXENT(index));


		// pfnWalkMove
	case	EngFunc_WalkMove:			// int  )				(edict_t *ent, float yaw, float dist, int iMode);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam2 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam1 = cRet[0];
		return (*g_engfuncs.pfnWalkMove)(INDEXENT(index),fparam1,fparam2,iparam1);


		// pfnSetOrigin
	case	EngFunc_SetOrigin:			// void )				(edict_t *e, const float *rgflOrigin);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		(*g_engfuncs.pfnSetOrigin)(INDEXENT(index),Vec1);
		return 1;


		// pfnEmitSound
	case	EngFunc_EmitSound:			// void )				(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1=cRet[0];
		temp = MF_GetAmxString(amx,params[4],0,&len);
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam1=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		fparam2=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[7]);
		iparam2=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[8]);
		iparam3=cRet[0];
		(*g_engfuncs.pfnEmitSound)(INDEXENT(index),iparam1,temp,fparam1,fparam2,iparam2,iparam3);
		return 1;


		// pfnEmitAmbientSound
	case	EngFunc_EmitAmbientSound:	// void )		(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		temp = MF_GetAmxString(amx,params[4],0,&len);
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam1=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		fparam2=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[7]);
		iparam1=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[8]);
		iparam2=cRet[0];
		(*g_engfuncs.pfnEmitAmbientSound)(INDEXENT(index),Vec1,temp,fparam1,fparam2,iparam1,iparam2);
		return 1;

		// pfnTraceLine
	case EngFunc_TraceLine:			// void )				(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam1=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		index=cRet[0];
		(*g_engfuncs.pfnTraceLine)(Vec1,Vec2,iparam1,index != -1 ? INDEXENT(index) : NULL, &g_tr);
		return 1;


		// pfnTraceToss
	case	EngFunc_TraceToss:			// void )				(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1 = cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnTraceToss)(INDEXENT(index),iparam1 == -1 ? NULL : INDEXENT(iparam1),&g_tr);
		return 1;


		// pfnTraceMonsterHull
	case	EngFunc_TraceMonsterHull:	// int  )		(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam1=cRet[0];
		cRet = MF_GetAmxAddr(amx,params[6]);
		iparam2=cRet[0];
		(*g_engfuncs.pfnTraceMonsterHull)(INDEXENT(index),Vec1,Vec2,iparam1,iparam2 == 0 ? NULL : INDEXENT(iparam2),&g_tr);
		return 1;


		// pfnTraceHull
	case	EngFunc_TraceHull:			// void )				(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[6]);
		iparam3 = cRet[0];
		(*g_engfuncs.pfnTraceHull)(Vec1,Vec2,iparam1,iparam2,iparam3 == 0 ? 0 : INDEXENT(iparam3),&g_tr);
		return 1;


		// pfnTraceModel
	case	EngFunc_TraceModel:			// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam2 = cRet[0];
		(*g_engfuncs.pfnTraceModel)(Vec1,Vec2,iparam1,iparam2 == 0 ? NULL : INDEXENT(iparam2),&g_tr);
		return 1;


		// pfnTraceTexture
	case EngFunc_TraceTexture:		// const char *)			(edict_t *pTextureEntity, const float *v1, const float *v2 );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		temp = (char*)(*g_engfuncs.pfnTraceTexture)(INDEXENT(index),Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[6]);
		MF_SetAmxString(amx, params[5], temp, cRet[0]);
		return 1;

		
		// pfnTraceSphere
	case EngFunc_TraceSphere:			// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam1 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		index = cRet[0];
		(*g_engfuncs.pfnTraceSphere)(Vec1,Vec2,iparam1,fparam1,index == 0 ? NULL : INDEXENT(index),&g_tr);
		return 1;


		// pfnGetAimVector
	case	EngFunc_GetAimVector:		// void )			(edict_t* ent, float speed, float *rgflReturn);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnGetAimVector)(INDEXENT(index),fparam1,Vec1);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0] = amx_ftoc(Vec1[0]);
		cRet[1] = amx_ftoc(Vec1[1]);
		cRet[2] = amx_ftoc(Vec1[2]);
		return 1;


		// pfnParticleEffect
	case	EngFunc_ParticleEffect:		// void )		(const float *org, const float *dir, float color, float count);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam1=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam2=amx_ctof(cRet[0]);
		(*g_engfuncs.pfnParticleEffect)(Vec1,Vec2,fparam1,fparam2);
		return 1;

		
		// pfnLightStyle
	case	EngFunc_LightStyle:			// void )			(int style, char* val);
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1=cRet[0];
		temp = MF_GetAmxString(amx,params[3],0,&len);
		(*g_engfuncs.pfnLightStyle)(iparam1,temp);
		return 1;


		// pfnDecalIndex
	case	EngFunc_DecalIndex:			// int  )			(const char *name);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return (*g_engfuncs.pfnDecalIndex)(temp);


		// pfnPointContents
	case	EngFunc_PointContents:		// int )			(const float *rgflVector);
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		return (*g_engfuncs.pfnPointContents)(Vec1);


		// pfnFreeEntPrivateData
	case	EngFunc_FreeEntPrivateData:	// void )	(edict_t *pEdict);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		(*g_engfuncs.pfnFreeEntPrivateData)(INDEXENT(index));


		// pfnSzFromIndex
	case	EngFunc_SzFromIndex:			// const char * )			(int iString);
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		temp = (char*)(*g_engfuncs.pfnSzFromIndex)(iparam1);
		cRet = MF_GetAmxAddr(amx,params[4]);
		MF_SetAmxString(amx, params[3], temp, cRet[0]);
		return 1;
		

		// pfnAllocString
	case	EngFunc_AllocString:			// int )			(const char *szValue);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return (*g_engfuncs.pfnAllocString)((const char *)temp);


		// pfnRegUserMsg
	case	EngFunc_RegUserMsg:			// int	)			(const char *pszName, int iSize);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1 = cRet[0];
		return (*g_engfuncs.pfnRegUserMsg)(temp,iparam1);


		// pfnAnimationAutomove
	case	EngFunc_AnimationAutomove:	// void )		(const edict_t* pEdict, float flTime);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnAnimationAutomove)(INDEXENT(index),fparam1);
		return 1;


		// pfnGetBonePosition
	case	EngFunc_GetBonePosition:		// void )		(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1=cRet[0];
		(*g_engfuncs.pfnGetBonePosition)(INDEXENT(index),iparam1,Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0]=amx_ftoc(Vec1[0]);
		cRet[1]=amx_ftoc(Vec1[1]);
		cRet[2]=amx_ftoc(Vec1[2]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		cRet[0]=amx_ftoc(Vec2[0]);
		cRet[1]=amx_ftoc(Vec2[1]);
		cRet[2]=amx_ftoc(Vec2[2]);
		return 1;


		// pfnGetAttachment
	case	EngFunc_GetAttachment:		// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1=cRet[0];
		(*g_engfuncs.pfnGetAttachment)(INDEXENT(index),iparam1,Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0]=amx_ftoc(Vec1[0]);
		cRet[1]=amx_ftoc(Vec1[1]);
		cRet[2]=amx_ftoc(Vec1[2]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		cRet[0]=amx_ftoc(Vec2[0]);
		cRet[1]=amx_ftoc(Vec2[1]);
		cRet[2]=amx_ftoc(Vec2[2]);
		return 1;


		// pfnSetView
	case	EngFunc_SetView:				// void )				(const edict_t *pClient, const edict_t *pViewent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam2 = cRet[0];
		CHECK_ENTITY(iparam1);
		CHECK_ENTITY(iparam2);
		(*g_engfuncs.pfnSetView)(INDEXENT(iparam1),INDEXENT(iparam2));
		return 1;


		// pfnTime
	case	EngFunc_Time:				// float)					( void );
		fparam1 = (*g_engfuncs.pfnTime)();
		return amx_ftoc(fparam1);


		// pfnCrosshairAngle
	case	EngFunc_CrosshairAngle:		// void )		(const edict_t *pClient, float pitch, float yaw);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam2 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnCrosshairAngle)(INDEXENT(index),fparam1,fparam2);
		return 1;


		// pfnFadeClientVolume
	case	EngFunc_FadeClientVolume:	// void )      (const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam3 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[6]);
		iparam4 = cRet[0];
		(*g_engfuncs.pfnFadeClientVolume)(INDEXENT(index),iparam1,iparam2,iparam3,iparam4);
		return 1;


		// pfnSetClientMaxSpeed
	case	EngFunc_SetClientMaxspeed:	// void )     (const edict_t *pEdict, float fNewMaxspeed);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		fparam1 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnSetClientMaxspeed)(INDEXENT(index),fparam1);
		return 1;


		// pfnCreateFakeClient
	case	EngFunc_CreateFakeClient:	// edict)		(const char *netname);	// returns NULL if fake client can't be created
		temp = MF_GetAmxString(amx,params[2],0,&len);
		pRet = (*g_engfuncs.pfnCreateFakeClient)(STRING(ALLOC_STRING(temp)));
		if (pRet == 0)
			return 0;
		return ENTINDEX(pRet);

		
		// pfnRunPlayerMove
	case	EngFunc_RunPlayerMove:		// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		fparam1=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam2=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		fparam3=amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[7]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[8]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[9]);
		iparam3 = cRet[0];
		(*g_engfuncs.pfnRunPlayerMove)(INDEXENT(index),Vec1,fparam1,fparam2,fparam3,iparam1,iparam2,iparam3);
		return 1;


		// pfnNumberOfEntities
	case	EngFunc_NumberOfEntities:	// int  )		(void);
		return (*g_engfuncs.pfnNumberOfEntities)();


		// pfnStaticDecal
	case	EngFunc_StaticDecal:			// void )			( const float *origin, int decalIndex, int entityIndex, int modelIndex );
		cRet = MF_GetAmxAddr(amx,params[2]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam3 = cRet[0];
		(*g_engfuncs.pfnStaticDecal)(Vec1,iparam1,iparam2,iparam3);
		return 1;


		// pfnPrecacheGeneric
	case	EngFunc_PrecacheGeneric:		// int  )		(char* s);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return (*g_engfuncs.pfnPrecacheGeneric)((char*)STRING(ALLOC_STRING(temp)));


		// pfnBuildSoundMsg
	case	EngFunc_BuildSoundMsg:		// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam1 = cRet[0];
		temp = MF_GetAmxString(amx,params[4],0,&len);
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam1 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		fparam2 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[7]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[8]);
		iparam3 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[9]);
		iparam4 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[10]);
		iparam5 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[11]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet=MF_GetAmxAddr(amx,params[12]);
		iparam6=cRet[0];
		/* don't check, it might not be included 
		CHECK_ENTITY(iparam5); 
		*/
		(*g_engfuncs.pfnBuildSoundMsg)(INDEXENT(index),iparam1,temp,fparam1,fparam2,iparam2,iparam3,iparam4,iparam5,Vec1,iparam6 == 0 ? NULL : INDEXENT(iparam6));
		return 1;


		// pfnGetPhysicsKeyValue
	case	EngFunc_GetPhysicsKeyValue:	// const char* )	( const edict_t *pClient, const char *key );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = (char*)(*g_engfuncs.pfnGetPhysicsKeyValue)(INDEXENT(index),(const char *)temp);
		cRet = MF_GetAmxAddr(amx,params[5]);
		MF_SetAmxString(amx,params[4],temp2,cRet[0]);
		return 1;


		// pfnSetPhysicsKeyValue
	case	EngFunc_SetPhysicsKeyValue:	// void )	( const edict_t *pClient, const char *key, const char *value );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],0,&len);
		(*g_engfuncs.pfnSetPhysicsKeyValue)(INDEXENT(index),STRING(ALLOC_STRING(temp)),STRING(ALLOC_STRING(temp2)));
		return 1;


		// pfnGetPhysicsInfoString
	case	EngFunc_GetPhysicsInfoString:	// const char* )	( const edict_t *pClient );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		temp = (char*)(*g_engfuncs.pfnGetPhysicsInfoString)(INDEXENT(index));
		cRet = MF_GetAmxAddr(amx,params[4]);

		MF_SetAmxString(amx,params[3],temp,cRet[0]);
		return 1;


		// pfnPrecacheEvent
	case	EngFunc_PrecacheEvent:		// unsigned short )		( int type, const char*psz );
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		temp = MF_GetAmxString(amx,params[3],0,&len);
		return (*g_engfuncs.pfnPrecacheEvent)(iparam1,(char*)STRING(ALLOC_STRING(temp)));


		// pfnPlaybackEvent (grr)
	case	EngFunc_PlaybackEvent:		// void )			
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		fparam1 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[6]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[7]);
		Vec2[0]=amx_ctof(cRet[0]);
		Vec2[1]=amx_ctof(cRet[1]);
		Vec2[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[8]);
		fparam2 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[9]);
		fparam3 = amx_ctof(cRet[0]);
		cRet = MF_GetAmxAddr(amx,params[10]);
		iparam3 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[11]);
		iparam4 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[12]);
		iparam5 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[13]);
		iparam6 = cRet[0];
		(*g_engfuncs.pfnPlaybackEvent)(iparam1,INDEXENT(index),iparam2,fparam1,Vec1,Vec2,fparam2,fparam3,iparam3,iparam4,iparam5,iparam6);
		return 1;

		// pfnGetCurrentPlayer
	case	EngFunc_GetCurrentPlayer:			// int )		( void );
		return (*g_engfuncs.pfnGetCurrentPlayer)();


		// pfnCanSkipPlayer
	case	EngFunc_CanSkipPlayer:			// int )			( const edict_t *player );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		return (*g_engfuncs.pfnCanSkipPlayer)(INDEXENT(index));


		// pfnSetGroupMask
	case	EngFunc_SetGroupMask:				// void )			( int mask, int op );
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam2 = cRet[0];
		(*g_engfuncs.pfnSetGroupMask)(iparam1,iparam2);
		return 1;


		// pfnGetClientListening
	case	EngFunc_GetClientListening:	// bool (int iReceiver, int iSender)
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam2 = cRet[0];
		return (*g_engfuncs.pfnVoice_GetClientListening)(iparam1,iparam2);


		// pfnSetClientListening
	case	EngFunc_SetClientListening:	// bool (int iReceiver, int iSender, bool Listen)
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[4]);
		iparam3 = cRet[0];
		return (*g_engfuncs.pfnVoice_SetClientListening)(iparam1,iparam2,iparam3);


		// pfnMessageBegin (AMX doesn't support MSG_ONE_UNRELIABLE, so I should add this incase anyone needs it.)
	case	EngFunc_MessageBegin:	// void (int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[3]);
		iparam2 = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[4]);
		Vec1[0]=amx_ctof(cRet[0]);
		Vec1[1]=amx_ctof(cRet[1]);
		Vec1[2]=amx_ctof(cRet[2]);
		cRet = MF_GetAmxAddr(amx,params[5]);
		index = cRet[0];
		(*g_engfuncs.pfnMessageBegin)(iparam1,iparam2,Vec1,index == 0 ? NULL : INDEXENT(index));
		return 1;


		// pfnWriteCoord
	case	EngFunc_WriteCoord:		// void (float)
		cRet = MF_GetAmxAddr(amx,params[2]);
		fparam1 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnWriteCoord)(fparam1);
		return 1;


		// pfnWriteAngle
	case	EngFunc_WriteAngle:		// void (float)
		cRet = MF_GetAmxAddr(amx,params[2]);
		fparam1 = amx_ctof(cRet[0]);
		(*g_engfuncs.pfnWriteAngle)(fparam1);
		return 1;
	case	EngFunc_InfoKeyValue:	// char*	)			(char *infobuffer, char *key);
		// Modify the syntax a bit.
		// index, key
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		cRet = MF_GetAmxAddr(amx,params[5]);
		iparam1 = cRet[0];
		CHECK_ENTITY(index);
		temp2 = MF_GetAmxString(amx,params[3],0,&len);
		temp = (*g_engfuncs.pfnInfoKeyValue)((*g_engfuncs.pfnGetInfoKeyBuffer)(INDEXENT(index)),temp2);
		MF_SetAmxString(amx,params[4],temp,iparam1);
		return 1;

	case	EngFunc_SetKeyValue:	// void )			(char *infobuffer, char *key, char *value);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		(*g_engfuncs.pfnSetKeyValue)((*g_engfuncs.pfnGetInfoKeyBuffer)(INDEXENT(index)),temp,temp2);
		return 1;
	case	EngFunc_SetClientKeyValue:	 // void )		(int clientIndex, char *infobuffer, char *key, char *value);
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		(*g_engfuncs.pfnSetClientKeyValue)(index,(*g_engfuncs.pfnGetInfoKeyBuffer)(INDEXENT(index)),temp,temp2);
		return 1;

	default:
		LOG_CONSOLE(PLID,"[NS2AMX] Unknown engfunc type provided.");
		return 0;
	}
}

//by mahnsawce
static cell AMX_NATIVE_CALL dllfunc(AMX *amx,cell *params)
{
	int type;
	int index;
	int indexb;
	char *temp = "";
	char *temp2 = "";
	char *temp3 = "";
	vec3_t Vec1;
	vec3_t Vec2;
	int iparam1;
	int len;
	cell *cRet;
	type = params[1];
	switch(type)
	{

		// pfnGameInit
	case	DLLFunc_GameInit:	// void)			( void );				
		gpGamedllFuncs->dllapi_table->pfnGameInit();
		return 1;

		// pfnSpawn
	case	DLLFunc_Spawn:	// int )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		return gpGamedllFuncs->dllapi_table->pfnSpawn(INDEXENT(index));

		// pfnThink
	case	DLLFunc_Think:	// void )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnThink(INDEXENT(index));
		return 1;

		// pfnUse
	case	DLLFunc_Use:	// void )				( edict_t *pentUsed, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[2]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnUse(INDEXENT(index),INDEXENT(indexb));
		return 1;

		// pfnTouch
	case	DLLFunc_Touch:	// void )				( edict_t *pentTouched, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[2]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnTouch(INDEXENT(index),INDEXENT(indexb));
		return 1;

	case	DLLFunc_Blocked:	// void )			( edict_t *pentBlocked, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[2]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnBlocked(INDEXENT(index),INDEXENT(indexb));
		return 1;

		
	case	DLLFunc_SetAbsBox:			// void )			( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSetAbsBox(INDEXENT(index));
		return 1;

	case	DLLFunc_ClientConnect:		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
		// index,szName,szAddress,szRetRejectReason,size
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		//temp3 = GET_AMXSTRING(amx,params[5],2,len);
		iparam1 = MDLL_ClientConnect(INDEXENT(index),STRING(ALLOC_STRING(temp)),STRING(ALLOC_STRING(temp2)),temp3);
		cRet = MF_GetAmxAddr(amx,params[6]);
		MF_SetAmxString(amx,params[5],temp3,cRet[0]);
		return 1;
	
	case	DLLFunc_ClientDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientDisconnect(INDEXENT(index));
		return 1;

	case	DLLFunc_ClientKill:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientKill(INDEXENT(index));
		return 1;

	case	DLLFunc_ClientPutInServer:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientPutInServer(INDEXENT(index));
		return 1;

	case	DLLFunc_ServerDeactivate:	// void)	( void );
		gpGamedllFuncs->dllapi_table->pfnServerDeactivate();
		return 1;

	case	DLLFunc_PlayerPreThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPreThink(INDEXENT(index));
		return 1;

	case	DLLFunc_PlayerPostThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPostThink(INDEXENT(index));
		return 1;

	case	DLLFunc_StartFrame:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnStartFrame();
		return 1;

	case	DLLFunc_ParmsNewLevel:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnParmsNewLevel();


	case	DLLFunc_ParmsChangeLevel:	// void )	( void );
		gpGamedllFuncs->dllapi_table->pfnParmsChangeLevel();

	 // Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	case	DLLFunc_GetGameDescription:	 // const char * )( void );     
		temp = (char*)gpGamedllFuncs->dllapi_table->pfnGetGameDescription();
		cRet = MF_GetAmxAddr(amx,params[3]);
		MF_SetAmxString(amx,params[2],temp,cRet[0]);
		return 1;

		// Spectator funcs
	case	DLLFunc_SpectatorConnect:	// void)		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorConnect(INDEXENT(index));
		return 1;
	case	DLLFunc_SpectatorDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorDisconnect(INDEXENT(index));
		return 1;
	case	DLLFunc_SpectatorThink:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorThink(INDEXENT(index));
		return 1;

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	case	DLLFunc_Sys_Error:		// void )			( const char *error_string );
		temp = MF_GetAmxString(amx,params[2],0,&len);
		gpGamedllFuncs->dllapi_table->pfnSys_Error(STRING(ALLOC_STRING(temp)));
		return 1;

	case	DLLFunc_PM_FindTextureType:	// char )( char *name );
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return gpGamedllFuncs->dllapi_table->pfnPM_FindTextureType(temp);

	case	DLLFunc_RegisterEncoders:	// void )	( void );
		gpGamedllFuncs->dllapi_table->pfnRegisterEncoders;
		return 1;

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	case	DLLFunc_GetHullBounds:	// int)	( int hullnumber, float *mins, float *maxs );
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = gpGamedllFuncs->dllapi_table->pfnGetHullBounds(cRet[0],Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[3]);
		cRet[0]=amx_ftoc(Vec1[0]);
		cRet[1]=amx_ftoc(Vec1[1]);
		cRet[2]=amx_ftoc(Vec1[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0]=amx_ftoc(Vec2[0]);
		cRet[1]=amx_ftoc(Vec2[1]);
		cRet[2]=amx_ftoc(Vec2[2]);
		return iparam1;

	// Create baselines for certain "unplaced" items.
	case	DLLFunc_CreateInstancedBaselines:	// void ) ( void );
		gpGamedllFuncs->dllapi_table->pfnCreateInstancedBaselines();
		return 1;

	case	DLLFunc_pfnAllowLagCompensation:	// int )( void );
		return gpGamedllFuncs->dllapi_table->pfnAllowLagCompensation();
		// I know this doesn't fit with dllfunc, but I dont want to create another native JUST for this.
	case	MetaFunc_CallGameEntity:	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		cRet = MF_GetAmxAddr(amx,params[3]);
		index = cRet[0];
		CHECK_ENTITY(index);
		iparam1 = gpMetaUtilFuncs->pfnCallGameEntity(PLID,STRING(ALLOC_STRING(temp)),VARS(INDEXENT(index)));
		return iparam1;
	default:
		MF_Log("Unknown dllfunc entry.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}
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
	{"traceresult",			traceresult},
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

	{"engfunc",				engfunc},
	{"dllfunc",				dllfunc},

	{"register_impulse",	register_impulse},
	{"register_think",		register_think},
	{"register_touch",		register_touch},

	{NULL,					NULL},
	 ///////////////////
};
