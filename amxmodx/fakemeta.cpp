/* AMX Mod X
*
* by the AMX Mod X Development Team
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

// Fake metamod api
#include "amxmodx.h"
#include "fakemeta.h"

// for varargs
#define MAX_STRBUF_LEN 512


// Engine normal
#define FAKEMETA_ENGINE_HANDLE_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetEngineFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetEngineFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);

#define FAKEMETA_ENGINE_HANDLE(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetEngineFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetEngineFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);

#define FAKEMETA_ENGINE_HANDLE_void_varargs(pfnName, pfnArg, fmtArg) \
	char buf[MAX_STRBUF_LEN]; \
	va_list ap; \
	va_start(ap, fmtArg); \
	vsprintf(buf, fmtArg, ap); \
	va_end(ap); \
	FAKEMETA_ENGINE_HANDLE_void(pfnName, (pfnArg, "%s", buf));

// Engine post
#define FAKEMETA_ENGINE_HANDLE_POST_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetEngineFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetEngineFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
			if (mres == MRES_SUPERCEDE) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has set meta result in \"%s\" to supercede", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);


#define FAKEMETA_ENGINE_HANDLE_POST(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetEngineFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetEngineFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
			if (mres == MRES_SUPERCEDE) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has set meta result in \"%s\" to supercede", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);

#define FAKEMETA_ENGINE_HANLDE_POST_void_varargs(pfnName, pfnArg, fmtArg) \
	char buf[MAX_STRBUF_LEN]; \
	va_list ap; \
	va_start(ap, fmtArg); \
	vsprintf(buf, fmtArg, ap); \
	va_end(ap); \
	FAKEMETA_ENGINE_HANDLE_POST_void(pfnName, (pfnArg, "%s", buf));



// Dll normal
#define FAKEMETA_DLL_HANDLE_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetDllFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetDllFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);

#define FAKEMETA_DLL_HANDLE(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetDllFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetDllFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);

// Dll post
#define FAKEMETA_DLL_HANDLE_POST_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetDllFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetDllFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);

#define FAKEMETA_DLL_HANDLE_POST(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetDllFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetDllFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
			if (mres == MRES_SUPERCEDE) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has set meta result in \"%s\" to supercede", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnName); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);

// New Dll normal
#define FAKEMETA_NEWDLL_HANDLE_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetNewDllFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetNewDllFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnArgs); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);

#define FAKEMETA_NEWDLL_HANDLE(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetNewDllFuncTable().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetNewDllFuncTable().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnArgs); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);

// Dll post
#define FAKEMETA_NEWDLL_HANDLE_POST_void(pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetNewDllFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			(*iter).GetNewDllFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnArgs); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META(status);

#define FAKEMETA_NEWDLL_HANDLE_POST(ret_type, ret_init, pfnName, pfnArgs) \
	META_RES mres = MRES_IGNORED, status = MRES_IGNORED, prev_mres = MRES_UNSET; \
	ret_type returnValue = ret_init;  /* value that will be returned */ \
	ret_type curRet = ret_init; /* current return value */ \
	bool mayOverride = true; /* if this is false, overried will be ignored */ \
	for (CList<CFakeMeta::CFakeMetaPlugin>::iterator iter = g_FakeMeta.m_Plugins.begin(); iter; ++iter) \
	{ \
		if ((*iter).GetStatus() == PL_RUNNING && (*iter).GetNewDllFuncTable_Post().pfn##pfnName) \
		{ \
			/* Initialize meta globals */ \
			gpMetaGlobals->mres = MRES_UNSET; \
			gpMetaGlobals->prev_mres = prev_mres; \
			gpMetaGlobals->status = status; \
			/* Actual call */ \
			curRet = (*iter).GetNewDllFuncTable_Post().pfn##pfnName pfnArgs; \
			/* Process return value */ \
			mres = gpMetaGlobals->mres; \
			if (mres >= MRES_OVERRIDE && mayOverride) \
			{ \
				mayOverride = false; \
				returnValue = curRet; \
			} \
			if (mres > status) \
				status = mres; \
			prev_mres = mres; \
			if (mres == MRES_UNSET) \
				AMXXLOG_Log("[AMXX] Module \"%s\" (\"%s\") has not set meta result in \"%s\"", \
				(*iter).GetInfo()->name, (*iter).GetPath(), #pfnArgs); \
		} \
	} \
	/* Set meta result to the highest value */ \
	RETURN_META_VALUE(status, returnValue);


// ****** Meta hooks ******
//   **** Engine api

int PrecacheModel(char *s) {
	FAKEMETA_ENGINE_HANDLE(int, 0, PrecacheModel, (s));
}
int PrecacheSound(char *s) {
	FAKEMETA_ENGINE_HANDLE(int, 0, PrecacheSound, (s));
}
void SetModel(edict_t *e, const char *m) {
	FAKEMETA_ENGINE_HANDLE_void(SetModel, (e,m));
}

int ModelIndex(const char *m) {
	FAKEMETA_ENGINE_HANDLE(int, 0, ModelIndex, (m));
}
int ModelFrames(int modelIndex) {
	FAKEMETA_ENGINE_HANDLE(int, 0, ModelFrames, (modelIndex));
}

void SetSize(edict_t *e, const float *rgflMin, const float *rgflMax) {
	FAKEMETA_ENGINE_HANDLE_void(SetSize, (e,rgflMin,rgflMax));
}
void ChangeLevel(char *s1, char *s2) {
	FAKEMETA_ENGINE_HANDLE_void(ChangeLevel, (s1,s2));
}
void GetSpawnParms(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_void(GetSpawnParms, (ent));
}
void SaveSpawnParms(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_void(SaveSpawnParms, (ent));
}

float VecToYaw(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE(float, 0, VecToYaw, (rgflVector));
}
void VecToAngles(const float *rgflVectorIn, float *rgflVectorOut) {
	FAKEMETA_ENGINE_HANDLE_void(VecToAngles, (rgflVectorIn,rgflVectorOut));
}
void MoveToOrigin(edict_t *ent, const float *pflGoal, float dist, int iMoveType) {
	FAKEMETA_ENGINE_HANDLE_void(MoveToOrigin, (ent,pflGoal,dist,iMoveType));
}
void ChangeYaw(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_void(ChangeYaw, (ent));
}
void ChangePitch(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_void(ChangePitch, (ent));
}

edict_t *FindEntityByString(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, FindEntityByString, (pEdictStartSearchAfter, pszField, pszValue));
}
int GetEntityIllum(edict_t *pEnt) {
	FAKEMETA_ENGINE_HANDLE(int, 0, GetEntityIllum, (pEnt));
}
edict_t *FindEntityInSphere(edict_t *pEdictStartSearchAfter, const float *org, float rad) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, FindEntityInSphere, (pEdictStartSearchAfter,org,rad));
}
edict_t *FindClientInPVS(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, FindClientInPVS, (pEdict));
}
edict_t *EntitiesInPVS(edict_t *pplayer) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, EntitiesInPVS, (pplayer));
}

void MakeVectors(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE_void(MakeVectors, (rgflVector));
}
void AngleVectors(const float *rgflVector, float *forward, float *right, float *up) {
	FAKEMETA_ENGINE_HANDLE_void(AngleVectors, (rgflVector,forward,right,up));
}

edict_t *CreateEntity(void) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, CreateEntity, ());
}
void RemoveEntity(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_void(RemoveEntity, (e));
}
edict_t *CreateNamedEntity(int className) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, CreateNamedEntity, (className));
}

void MakeStatic(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_void(MakeStatic, (ent));
}
int EntIsOnFloor(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE(int, 0, EntIsOnFloor, (e));
}
int DropToFloor(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE(int, 0, DropToFloor, (e));
}

int WalkMove(edict_t *ent, float yaw, float dist, int iMode) {
	FAKEMETA_ENGINE_HANDLE(int, 0, WalkMove, (ent, yaw, dist, iMode));
}
void SetOrigin(edict_t *e, const float *rgflOrigin) {
	FAKEMETA_ENGINE_HANDLE_void(SetOrigin, (e,rgflOrigin));
}

void EmitSound(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch) {
	FAKEMETA_ENGINE_HANDLE_void(EmitSound, (entity,channel,sample,volume,attenuation,fFlags,pitch));
}
void EmitAmbientSound(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch) {
	FAKEMETA_ENGINE_HANDLE_void(EmitAmbientSound, (entity,pos,samp,vol,attenuation,fFlags,pitch));
}

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_void(TraceLine, (v1,v2,fNoMonsters,pentToSkip,ptr));
}
void TraceToss(edict_t *pent, edict_t *pentToIgnore, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_void(TraceToss, (pent,pentToIgnore,ptr));
}
int TraceMonsterHull(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE(int, 0, TraceMonsterHull, (pEdict, v1, v2, fNoMonsters, pentToSkip, ptr));
}
void TraceHull(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_void(TraceHull, (v1,v2,fNoMonsters,hullNumber,pentToSkip,ptr));
}
void TraceModel(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_void(TraceModel, (v1,v2,hullNumber,pent,ptr));
}
const char *TraceTexture(edict_t *pTextureEntity, const float *v1, const float *v2 ) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, TraceTexture, (pTextureEntity,v1,v2));
}
void TraceSphere(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_void(TraceSphere, (v1,v2,fNoMonsters,radius,pentToSkip,ptr));
}
void GetAimVector(edict_t *ent, float speed, float *rgflReturn) {
	FAKEMETA_ENGINE_HANDLE_void(GetAimVector, (ent,speed,rgflReturn));
}

void ServerCommand(char *str) {
	FAKEMETA_ENGINE_HANDLE_void(ServerCommand, (str));
}
void ServerExecute(void) {
	FAKEMETA_ENGINE_HANDLE_void(ServerExecute, ());
}
void engClientCommand(edict_t *pEdict, char *szFmt, ...) {
	FAKEMETA_ENGINE_HANDLE_void_varargs(ClientCommand, pEdict, szFmt);
}

void ParticleEffect(const float *org, const float *dir, float color, float count) {
	FAKEMETA_ENGINE_HANDLE_void(ParticleEffect, (org,dir,color,count));
}
void LightStyle(int style, char *val) {
	FAKEMETA_ENGINE_HANDLE_void(LightStyle, (style,val));
}
int DecalIndex(const char *name) {
	FAKEMETA_ENGINE_HANDLE(int, 0, DecalIndex, (name));
}
int PointContents(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE(int, 0, PointContents, (rgflVector));
}

void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	FAKEMETA_ENGINE_HANDLE_void(MessageBegin, (msg_dest,msg_type,pOrigin,ed));
}
void MessageEnd(void) {
	FAKEMETA_ENGINE_HANDLE_void(MessageEnd, ());
}

void WriteByte(int iValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteByte, (iValue));
}
void WriteChar(int iValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteChar, (iValue));
}
void WriteShort(int iValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteShort, (iValue));
}
void WriteLong(int iValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteLong, (iValue));
}
void WriteAngle(float flValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteAngle, (flValue));
}
void WriteCoord(float flValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteCoord, (flValue));
}
void WriteString(const char *sz) {
	FAKEMETA_ENGINE_HANDLE_void(WriteString, (sz));
}
void WriteEntity(int iValue) {
	FAKEMETA_ENGINE_HANDLE_void(WriteEntity, (iValue));
}

void CVarRegister(cvar_t *pCvar) {
	FAKEMETA_ENGINE_HANDLE_void(CVarRegister, (pCvar));
}
float CVarGetFloat(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE(float, 0, CVarGetFloat, (szVarName));
}
const char* CVarGetString(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, CVarGetString, (szVarName));
}
void CVarSetFloat(const char *szVarName, float flValue) {
	FAKEMETA_ENGINE_HANDLE_void(CVarSetFloat, (szVarName, flValue));
}
void CVarSetString(const char *szVarName, const char *szValue) {
	FAKEMETA_ENGINE_HANDLE_void(CVarSetString, (szVarName, szValue));
}

void AlertMessage(ALERT_TYPE atype, char *szFmt, ...) {
	FAKEMETA_ENGINE_HANDLE_void_varargs(AlertMessage, atype, szFmt);
}

#ifdef HLSDK_2_3_OLD_EIFACE
void EngineFprintf(FILE *pfile, char *szFmt, ...) {
#else
void EngineFprintf(void *pfile, char *szFmt, ...) {
#endif
	FAKEMETA_ENGINE_HANDLE_void_varargs(EngineFprintf, pfile, szFmt);
}

#ifdef HLSDK_2_3_OLD_EIFACE
void *PvAllocEntPrivateData(edict_t *pEdict, long cb) {
#else
void *PvAllocEntPrivateData(edict_t *pEdict, int cb) {
#endif
	FAKEMETA_ENGINE_HANDLE(void*, 0, PvAllocEntPrivateData, (pEdict, cb));
}
void *PvEntPrivateData(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(void*, 0, PvEntPrivateData, (pEdict));
}
void FreeEntPrivateData(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_void(FreeEntPrivateData, (pEdict));
}

const char *SzFromIndex(int iString) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, SzFromIndex, (iString));
}
int AllocString(const char *szValue) {
	FAKEMETA_ENGINE_HANDLE(int, 0, AllocString, (szValue));
}

struct entvars_s *GetVarsOfEnt(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(struct entvars_s*, 0, GetVarsOfEnt, (pEdict));
}
edict_t *PEntityOfEntOffset(int iEntOffset) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, PEntityOfEntOffset, (iEntOffset));
}
int EntOffsetOfPEntity(const edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(int, 0, EntOffsetOfPEntity, (pEdict));
}
int IndexOfEdict(const edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(int, 0, IndexOfEdict, (pEdict));
}
edict_t *PEntityOfEntIndex(int iEntIndex) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, PEntityOfEntIndex, (iEntIndex));
}
edict_t *FindEntityByVars(struct entvars_s *pvars) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, FindEntityByVars, (pvars));
}
void *GetModelPtr(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE(void*, 0, GetModelPtr, (pEdict));
}

int RegUserMsg(const char *pszName, int iSize) {
	FAKEMETA_ENGINE_HANDLE(int, 0, RegUserMsg, (pszName, iSize));
}

void AnimationAutomove(const edict_t *pEdict, float flTime) {
	FAKEMETA_ENGINE_HANDLE_void(AnimationAutomove, (pEdict, flTime));
}
void GetBonePosition(const edict_t *pEdict, int iBone, float *rgflOrigin, float *rgflAngles ) {
	FAKEMETA_ENGINE_HANDLE_void(GetBonePosition, (pEdict, iBone, rgflOrigin, rgflAngles));
}

#ifdef HLSDK_2_3_OLD_EIFACE
unsigned long FunctionFromName( const char *pName ) {
	FAKEMETA_ENGINE_HANDLE(unsigned long, 0, FunctionFromName, (pName));
#else
unsigned int FunctionFromName( const char *pName ) {
	FAKEMETA_ENGINE_HANDLE(unsigned int, 0, FunctionFromName, (pName));
#endif
}

#ifdef HLSDK_2_3_OLD_EIFACE
const char *NameForFunction( unsigned long function ) {
#else
const char *NameForFunction( unsigned int function ) {
#endif
	FAKEMETA_ENGINE_HANDLE(const char*, 0, NameForFunction, (function));
}

void ClientPrintf( edict_t *pEdict, PRINT_TYPE ptype, const char *szMsg ) {
	FAKEMETA_ENGINE_HANDLE_void(ClientPrintf, (pEdict,ptype,szMsg));
}

void ServerPrint( const char *szMsg ) {
	FAKEMETA_ENGINE_HANDLE_void(ServerPrint, (szMsg));
}

const char *Cmd_Args( void ) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, Cmd_Args, ());
}

const char *Cmd_Argv( int argc )
{
	FAKEMETA_ENGINE_HANDLE(const char*, 0, Cmd_Argv, (argc));
}

int Cmd_Argc( void ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, Cmd_Argc, ());
}

void GetAttachment(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles ) {
	FAKEMETA_ENGINE_HANDLE_void(GetAttachment, (pEdict,iAttachment,rgflOrigin,rgflAngles));
}

void CRC32_Init(CRC32_t *pulCRC) {
	FAKEMETA_ENGINE_HANDLE_void(CRC32_Init, (pulCRC));
}
void CRC32_ProcessBuffer(CRC32_t *pulCRC, void *p, int len) {
	FAKEMETA_ENGINE_HANDLE_void(CRC32_ProcessBuffer, (pulCRC,p,len));
}
void CRC32_ProcessByte(CRC32_t *pulCRC, unsigned char ch) {
	FAKEMETA_ENGINE_HANDLE_void(CRC32_ProcessByte, (pulCRC,ch));
}
CRC32_t CRC32_Final(CRC32_t pulCRC) {
	FAKEMETA_ENGINE_HANDLE(CRC32_t, 0, CRC32_Final, (pulCRC));
}

#ifdef HLSDK_2_3_OLD_EIFACE
long RandomLong(long lLow, long lHigh) {
	FAKEMETA_ENGINE_HANDLE(long, 0, RandomLong, (lLow, lHigh));
#else
int RandomLong(int lLow, int lHigh) {
	FAKEMETA_ENGINE_HANDLE(int, 0, RandomLong, (lLow, lHigh));
#endif
}
float RandomFloat(float flLow, float flHigh) {
	FAKEMETA_ENGINE_HANDLE(float, 0, RandomFloat, (flLow, flHigh));
}

void SetView(const edict_t *pClient, const edict_t *pViewent ) {
	FAKEMETA_ENGINE_HANDLE_void(SetView, (pClient,pViewent));
}
float Time( void ) {
	FAKEMETA_ENGINE_HANDLE(float, 0, Time, ());
}
void CrosshairAngle(const edict_t *pClient, float pitch, float yaw) {
	FAKEMETA_ENGINE_HANDLE_void(CrosshairAngle, (pClient,pitch,yaw));
}

byte * LoadFileForMe(char *filename, int *pLength) {
	FAKEMETA_ENGINE_HANDLE(byte *, 0, LoadFileForMe, (filename, pLength));
}
void FreeFile(void *buffer) {
	FAKEMETA_ENGINE_HANDLE_void(FreeFile, (buffer));
}

void EndSection(const char *pszSectionName) {
	FAKEMETA_ENGINE_HANDLE_void(EndSection, (pszSectionName));
}
int CompareFileTime(char *filename1, char *filename2, int *iCompare) {
	FAKEMETA_ENGINE_HANDLE(int, 0, CompareFileTime, (filename1, filename2, iCompare));
}
void GetGameDir(char *szGetGameDir) {
	FAKEMETA_ENGINE_HANDLE_void(GetGameDir, (szGetGameDir));
}
void Cvar_RegisterVariable(cvar_t *variable) {
	FAKEMETA_ENGINE_HANDLE_void(Cvar_RegisterVariable, (variable));
}
void FadeClientVolume(const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds) {
	FAKEMETA_ENGINE_HANDLE_void(FadeClientVolume, (pEdict, fadePercent, fadeOutSeconds, holdTime, fadeInSeconds));
}
void SetClientMaxspeed(const edict_t *pEdict, float fNewMaxspeed) {
	FAKEMETA_ENGINE_HANDLE_void(SetClientMaxspeed, (pEdict,fNewMaxspeed));
}
edict_t * CreateFakeClient(const char *netname) {
	FAKEMETA_ENGINE_HANDLE(edict_t*, 0, CreateFakeClient, (netname));
}
void RunPlayerMove(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec ) {
	FAKEMETA_ENGINE_HANDLE_void(RunPlayerMove, (fakeclient,viewangles,forwardmove,sidemove,upmove,buttons,impulse,msec));
}
int NumberOfEntities(void) {
	FAKEMETA_ENGINE_HANDLE(int, 0, NumberOfEntities, ());
}

char *GetInfoKeyBuffer(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE(char*, 0, GetInfoKeyBuffer, (e));
}
char *InfoKeyValue(char *infobuffer, char *key) {
	FAKEMETA_ENGINE_HANDLE(char*, 0, InfoKeyValue, (infobuffer, key));
}
void SetKeyValue(char *infobuffer, char *key, char *value) {
	FAKEMETA_ENGINE_HANDLE_void(SetKeyValue, (infobuffer,key,value));
}
void SetClientKeyValue(int clientIndex, char *infobuffer, char *key, char *value) {
	FAKEMETA_ENGINE_HANDLE_void(SetClientKeyValue, (clientIndex,infobuffer,key,value));
}

int IsMapValid(char *filename) {
	FAKEMETA_ENGINE_HANDLE(int, 0, IsMapValid, (filename));
}
void StaticDecal( const float *origin, int decalIndex, int entityIndex, int modelIndex ) {
	FAKEMETA_ENGINE_HANDLE_void(StaticDecal, (origin,decalIndex,entityIndex,modelIndex));
}
int PrecacheGeneric(char *s) {
	FAKEMETA_ENGINE_HANDLE(int, 0, PrecacheGeneric, (s));
}
int GetPlayerUserId(edict_t *e ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, GetPlayerUserId, (e));
}
void BuildSoundMsg(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	FAKEMETA_ENGINE_HANDLE_void(BuildSoundMsg, (entity,channel,sample,volume,attenuation,fFlags,pitch,msg_dest,msg_type,pOrigin,ed));
}
int IsDedicatedServer(void) {
	FAKEMETA_ENGINE_HANDLE(int, 0, IsDedicatedServer, ());
}
cvar_t *CVarGetPointer(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE(cvar_t*, 0, CVarGetPointer, (szVarName));
}
unsigned int GetPlayerWONId(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE(unsigned int, 0, GetPlayerWONId, (e));
}

//! YWB 8/1/99 TFF Physics additions
void Info_RemoveKey( char *s, const char *key ) {
	FAKEMETA_ENGINE_HANDLE_void(Info_RemoveKey, (s,key));
}
const char *GetPhysicsKeyValue( const edict_t *pClient, const char *key ) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, GetPhysicsKeyValue, (pClient, key));
}
void SetPhysicsKeyValue( const edict_t *pClient, const char *key, const char *value ) {
	FAKEMETA_ENGINE_HANDLE_void(SetPhysicsKeyValue, (pClient,key,value));
}
const char *GetPhysicsInfoString( const edict_t *pClient ) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, GetPhysicsInfoString, (pClient));
}
unsigned short PrecacheEvent( int type, const char *psz ) {
	FAKEMETA_ENGINE_HANDLE(unsigned short, 0, PrecacheEvent, (type, psz));
}
void PlaybackEvent( int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2 ) {
	FAKEMETA_ENGINE_HANDLE_void(PlaybackEvent, (flags,pInvoker,eventindex,delay,origin,angles,fparam1,fparam2,iparam1,iparam2,bparam1,bparam2));
}

unsigned char *SetFatPVS( float *org ) {
	FAKEMETA_ENGINE_HANDLE(unsigned char*, 0, SetFatPVS, (org));
}
unsigned char *SetFatPAS( float *org ) {
	FAKEMETA_ENGINE_HANDLE(unsigned char*, 0, SetFatPAS, (org));
}

int CheckVisibility( const edict_t *entity, unsigned char *pset ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, CheckVisibility, (entity, pset));
}

void DeltaSetField( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE_void(DeltaSetField, (pFields,fieldname));
}
void DeltaUnsetField( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE_void(DeltaUnsetField, (pFields,fieldname));
}
void DeltaAddEncoder( char *name, void (*conditionalencode)( struct delta_s *pFields, const unsigned char *from, const unsigned char *to ) ) {
	FAKEMETA_ENGINE_HANDLE_void(DeltaAddEncoder, (name,conditionalencode));
}
int GetCurrentPlayer( void ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, GetCurrentPlayer, ());
}
int CanSkipPlayer( const edict_t *player ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, CanSkipPlayer, (player));
}
int DeltaFindField( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, DeltaFindField, (pFields, fieldname));
}
void DeltaSetFieldByIndex( struct delta_s *pFields, int fieldNumber ) {
	FAKEMETA_ENGINE_HANDLE_void(DeltaSetFieldByIndex, (pFields,fieldNumber));
}
void DeltaUnsetFieldByIndex( struct delta_s *pFields, int fieldNumber ) {
	FAKEMETA_ENGINE_HANDLE_void(DeltaUnsetFieldByIndex, (pFields,fieldNumber));
}

void SetGroupMask( int mask, int op ) {
	FAKEMETA_ENGINE_HANDLE_void(SetGroupMask, (mask,op));
}

int engCreateInstancedBaseline( int classname, struct entity_state_s *baseline ) {
	FAKEMETA_ENGINE_HANDLE(int, 0, CreateInstancedBaseline, (classname, baseline));
}
void Cvar_DirectSet( struct cvar_s *var, char *value ) {
	FAKEMETA_ENGINE_HANDLE_void(Cvar_DirectSet, (var,value));
}

//! Forces the client and server to be running with the same version of the specified file
//!( e.g., a player model ).
//! Calling this has no effect in single player
void ForceUnmodified( FORCE_TYPE type, float *mins, float *maxs, const char *filename ) {
	FAKEMETA_ENGINE_HANDLE_void(ForceUnmodified, (type,mins,maxs,filename));
}

void GetPlayerStats( const edict_t *pClient, int *ping, int *packet_loss ) {
	FAKEMETA_ENGINE_HANDLE_void(GetPlayerStats, (pClient,ping,packet_loss));
}

void AddServerCommand( char *cmd_name, void (*function) (void) ) {
	FAKEMETA_ENGINE_HANDLE_void(AddServerCommand, (cmd_name,function));
}
// Added in SDK 2.2:
qboolean Voice_GetClientListening(int iReceiver, int iSender) {
	FAKEMETA_ENGINE_HANDLE(qboolean, 0, Voice_GetClientListening, (iReceiver,iSender));
}
qboolean Voice_SetClientListening(int iReceiver, int iSender, qboolean bListen) {
	FAKEMETA_ENGINE_HANDLE(qboolean, 0, Voice_SetClientListening, (iReceiver, iSender, bListen));
}

// Added for HL 1109 (no SDK update):
const char *GetPlayerAuthId(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE(const char*, 0, GetPlayerAuthId, (e));
}

//   **** Engine api Post

int PrecacheModel_Post(char *s) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, PrecacheModel, (s));
}
int PrecacheSound_Post(char *s) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, PrecacheSound, (s));
}
void SetModel_Post(edict_t *e, const char *m) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetModel, (e,m));
}

int ModelIndex_Post(const char *m) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, ModelIndex, (m));
}
int ModelFrames_Post(int modelIndex) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, ModelFrames, (modelIndex));
}

void SetSize_Post(edict_t *e, const float *rgflMin, const float *rgflMax) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetSize, (e,rgflMin,rgflMax));
}
void ChangeLevel_Post(char *s1, char *s2) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ChangeLevel, (s1,s2));
}
void GetSpawnParms_Post(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetSpawnParms, (ent));
}
void SaveSpawnParms_Post(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SaveSpawnParms, (ent));
}

float VecToYaw_Post(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE_POST(float, 0, VecToYaw, (rgflVector));
}
void VecToAngles_Post(const float *rgflVectorIn, float *rgflVectorOut) {
	FAKEMETA_ENGINE_HANDLE_POST_void(VecToAngles, (rgflVectorIn,rgflVectorOut));
}
void MoveToOrigin_Post(edict_t *ent, const float *pflGoal, float dist, int iMoveType) {
	FAKEMETA_ENGINE_HANDLE_POST_void(MoveToOrigin, (ent,pflGoal,dist,iMoveType));
}
void ChangeYaw_Post(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ChangeYaw, (ent));
}
void ChangePitch_Post(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ChangePitch, (ent));
}

edict_t *FindEntityByString_Post(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, FindEntityByString, (pEdictStartSearchAfter, pszField, pszValue));
}
int GetEntityIllum_Post(edict_t *pEnt) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, GetEntityIllum, (pEnt));
}
edict_t *FindEntityInSphere_Post(edict_t *pEdictStartSearchAfter, const float *org, float rad) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, FindEntityInSphere, (pEdictStartSearchAfter,org,rad));
}
edict_t *FindClientInPVS_Post(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, FindClientInPVS, (pEdict));
}
edict_t *EntitiesInPVS_Post(edict_t *pplayer) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, EntitiesInPVS, (pplayer));
}

void MakeVectors_Post(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE_POST_void(MakeVectors, (rgflVector));
}
void AngleVectors_Post(const float *rgflVector, float *forward, float *right, float *up) {
	FAKEMETA_ENGINE_HANDLE_POST_void(AngleVectors, (rgflVector,forward,right,up));
}

edict_t *CreateEntity_Post(void) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, CreateEntity, ());
}
void RemoveEntity_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST_void(RemoveEntity, (e));
}
edict_t *CreateNamedEntity_Post(int className) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, CreateNamedEntity, (className));
}

void MakeStatic_Post(edict_t *ent) {
	FAKEMETA_ENGINE_HANDLE_POST_void(MakeStatic, (ent));
}
int EntIsOnFloor_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, EntIsOnFloor, (e));
}
int DropToFloor_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, DropToFloor, (e));
}

int WalkMove_Post(edict_t *ent, float yaw, float dist, int iMode) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, WalkMove, (ent, yaw, dist, iMode));
}
void SetOrigin_Post(edict_t *e, const float *rgflOrigin) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetOrigin, (e,rgflOrigin));
}

void EmitSound_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch) {
	FAKEMETA_ENGINE_HANDLE_POST_void(EmitSound, (entity,channel,sample,volume,attenuation,fFlags,pitch));
}
void EmitAmbientSound_Post(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch) {
	FAKEMETA_ENGINE_HANDLE_POST_void(EmitAmbientSound, (entity,pos,samp,vol,attenuation,fFlags,pitch));
}

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST_void(TraceLine, (v1,v2,fNoMonsters,pentToSkip,ptr));
}
void TraceToss_Post(edict_t *pent, edict_t *pentToIgnore, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST_void(TraceToss, (pent,pentToIgnore,ptr));
}
int TraceMonsterHull_Post(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, TraceMonsterHull, (pEdict, v1, v2, fNoMonsters, pentToSkip, ptr));
}
void TraceHull_Post(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST_void(TraceHull, (v1,v2,fNoMonsters,hullNumber,pentToSkip,ptr));
}
void TraceModel_Post(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST_void(TraceModel, (v1,v2,hullNumber,pent,ptr));
}
const char *TraceTexture_Post(edict_t *pTextureEntity, const float *v1, const float *v2 ) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, TraceTexture, (pTextureEntity,v1,v2));
}
void TraceSphere_Post(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr) {
	FAKEMETA_ENGINE_HANDLE_POST_void(TraceSphere, (v1,v2,fNoMonsters,radius,pentToSkip,ptr));
}
void GetAimVector_Post(edict_t *ent, float speed, float *rgflReturn) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetAimVector, (ent,speed,rgflReturn));
}

void ServerCommand_Post(char *str) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ServerCommand, (str));
}
void ServerExecute_Post(void) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ServerExecute, ());
}
void engClientCommand_Post(edict_t *pEdict, char *szFmt, ...) {
	FAKEMETA_ENGINE_HANLDE_POST_void_varargs(ClientCommand, pEdict, szFmt);
}

void ParticleEffect_Post(const float *org, const float *dir, float color, float count) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ParticleEffect, (org,dir,color,count));
}
void LightStyle_Post(int style, char *val) {
	FAKEMETA_ENGINE_HANDLE_POST_void(LightStyle, (style,val));
}
int DecalIndex_Post(const char *name) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, DecalIndex, (name));
}
int PointContents_Post(const float *rgflVector) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, PointContents, (rgflVector));
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	FAKEMETA_ENGINE_HANDLE_POST_void(MessageBegin, (msg_dest,msg_type,pOrigin,ed));
}
void MessageEnd_Post(void) {
	FAKEMETA_ENGINE_HANDLE_POST_void(MessageEnd, ());
}

void WriteByte_Post(int iValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteByte, (iValue));
}
void WriteChar_Post(int iValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteChar, (iValue));
}
void WriteShort_Post(int iValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteShort, (iValue));
}
void WriteLong_Post(int iValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteLong, (iValue));
}
void WriteAngle_Post(float flValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteAngle, (flValue));
}
void WriteCoord_Post(float flValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteCoord, (flValue));
}
void WriteString_Post(const char *sz) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteString, (sz));
}
void WriteEntity_Post(int iValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(WriteEntity, (iValue));
}

void CVarRegister_Post(cvar_t *pCvar) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CVarRegister, (pCvar));
}
float CVarGetFloat_Post(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE_POST(float, 0, CVarGetFloat, (szVarName));
}
const char* CVarGetString_Post(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, CVarGetString, (szVarName));
}
void CVarSetFloat_Post(const char *szVarName, float flValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CVarSetFloat, (szVarName, flValue));
}
void CVarSetString_Post(const char *szVarName, const char *szValue) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CVarSetString, (szVarName, szValue));
}

void AlertMessage_Post(ALERT_TYPE atype, char *szFmt, ...) {
	FAKEMETA_ENGINE_HANLDE_POST_void_varargs(AlertMessage, atype, szFmt);
}

#ifdef HLSDK_2_3_OLD_EIFACE
void EngineFprintf_Post(FILE *pfile, char *szFmt, ...) {
#else
void EngineFprintf_Post(void *pfile, char *szFmt, ...) {
#endif
	FAKEMETA_ENGINE_HANLDE_POST_void_varargs(EngineFprintf, pfile, szFmt);
}

#ifdef HLSDK_2_3_OLD_EIFACE
void *PvAllocEntPrivateData_Post(edict_t *pEdict, long cb) {
#else
void *PvAllocEntPrivateData_Post(edict_t *pEdict, int cb) {
#endif
	FAKEMETA_ENGINE_HANDLE_POST(void*, 0, PvAllocEntPrivateData, (pEdict, cb));
}
void *PvEntPrivateData_Post(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(void*, 0, PvEntPrivateData, (pEdict));
}
void FreeEntPrivateData_Post(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST_void(FreeEntPrivateData, (pEdict));
}

const char *SzFromIndex_Post(int iString) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, SzFromIndex, (iString));
}
int AllocString_Post(const char *szValue) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, AllocString, (szValue));
}

struct entvars_s *GetVarsOfEnt_Post(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(struct entvars_s*, 0, GetVarsOfEnt, (pEdict));
}
edict_t *PEntityOfEntOffset_Post(int iEntOffset) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, PEntityOfEntOffset, (iEntOffset));
}
int EntOffsetOfPEntity_Post(const edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, EntOffsetOfPEntity, (pEdict));
}
int IndexOfEdict_Post(const edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, IndexOfEdict, (pEdict));
}
edict_t *PEntityOfEntIndex_Post(int iEntIndex) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, PEntityOfEntIndex, (iEntIndex));
}
edict_t *FindEntityByVars_Post(struct entvars_s *pvars) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, FindEntityByVars, (pvars));
}
void *GetModelPtr_Post(edict_t *pEdict) {
	FAKEMETA_ENGINE_HANDLE_POST(void*, 0, GetModelPtr, (pEdict));
}

int RegUserMsg_Post(const char *pszName, int iSize) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, RegUserMsg, (pszName, iSize));
}

void AnimationAutomove_Post(const edict_t *pEdict, float flTime) {
	FAKEMETA_ENGINE_HANDLE_POST_void(AnimationAutomove, (pEdict, flTime));
}
void GetBonePosition_Post(const edict_t *pEdict, int iBone, float *rgflOrigin, float *rgflAngles ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetBonePosition, (pEdict, iBone, rgflOrigin, rgflAngles));
}

#ifdef HLSDK_2_3_OLD_EIFACE
unsigned long FunctionFromName_Post( const char *pName ) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned long, 0, FunctionFromName, (pName));
#else
unsigned int FunctionFromName_Post( const char *pName ) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned int, 0, FunctionFromName, (pName));
#endif
}

#ifdef HLSDK_2_3_OLD_EIFACE
const char *NameForFunction_Post( unsigned long function ) {
#else
const char *NameForFunction_Post( unsigned int function ) {
#endif
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, NameForFunction, (function));
}

void ClientPrintf_Post( edict_t *pEdict, PRINT_TYPE ptype, const char *szMsg ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ClientPrintf, (pEdict,ptype,szMsg));
}

void ServerPrint_Post( const char *szMsg ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ServerPrint, (szMsg));
}

const char *Cmd_Args_Post( void ) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, Cmd_Args, ());
}

const char *Cmd_Argv_Post( int argc ) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, Cmd_Argv, (argc));
}

int Cmd_Argc_Post( void ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, Cmd_Argc, ());
}

void GetAttachment_Post(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetAttachment, (pEdict,iAttachment,rgflOrigin,rgflAngles));
}

void CRC32_Init_Post(CRC32_t *pulCRC) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CRC32_Init, (pulCRC));
}
void CRC32_ProcessBuffer_Post(CRC32_t *pulCRC, void *p, int len) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CRC32_ProcessBuffer, (pulCRC,p,len));
}
void CRC32_ProcessByte_Post(CRC32_t *pulCRC, unsigned char ch) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CRC32_ProcessByte, (pulCRC,ch));
}
CRC32_t CRC32_Final_Post(CRC32_t pulCRC) {
	FAKEMETA_ENGINE_HANDLE_POST(CRC32_t, 0, CRC32_Final, (pulCRC));
}

#ifdef HLSDK_2_3_OLD_EIFACE
long RandomLong_Post(long lLow, long lHigh) {
	FAKEMETA_ENGINE_HANDLE_POST(long, 0, RandomLong, (lLow, lHigh));
#else
int RandomLong_Post(int lLow, int lHigh) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, RandomLong, (lLow, lHigh));
#endif
}
float RandomFloat_Post(float flLow, float flHigh) {
	FAKEMETA_ENGINE_HANDLE_POST(float, 0, RandomFloat, (flLow, flHigh));
}

void SetView_Post(const edict_t *pClient, const edict_t *pViewent ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetView, (pClient,pViewent));
}
float Time_Post( void ) {
	FAKEMETA_ENGINE_HANDLE_POST(float, 0, Time, ());
}
void CrosshairAngle_Post(const edict_t *pClient, float pitch, float yaw) {
	FAKEMETA_ENGINE_HANDLE_POST_void(CrosshairAngle, (pClient,pitch,yaw));
}

byte * LoadFileForMe_Post(char *filename, int *pLength) {
	FAKEMETA_ENGINE_HANDLE_POST(byte *, 0, LoadFileForMe, (filename, pLength));
}
void FreeFile_Post(void *buffer) {
	FAKEMETA_ENGINE_HANDLE_POST_void(FreeFile, (buffer));
}

void EndSection_Post(const char *pszSectionName) {
	FAKEMETA_ENGINE_HANDLE_POST_void(EndSection, (pszSectionName));
}
int CompareFileTime_Post(char *filename1, char *filename2, int *iCompare) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, CompareFileTime, (filename1, filename2, iCompare));
}
void GetGameDir_Post(char *szGetGameDir) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetGameDir, (szGetGameDir));
}
void Cvar_RegisterVariable_Post(cvar_t *variable) {
	FAKEMETA_ENGINE_HANDLE_POST_void(Cvar_RegisterVariable, (variable));
}
void FadeClientVolume_Post(const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds) {
	FAKEMETA_ENGINE_HANDLE_POST_void(FadeClientVolume, (pEdict, fadePercent, fadeOutSeconds, holdTime, fadeInSeconds));
}
void SetClientMaxspeed_Post(const edict_t *pEdict, float fNewMaxspeed) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetClientMaxspeed, (pEdict,fNewMaxspeed));
}
edict_t * CreateFakeClient_Post(const char *netname) {
	FAKEMETA_ENGINE_HANDLE_POST(edict_t*, 0, CreateFakeClient, (netname));
}
void RunPlayerMove_Post(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(RunPlayerMove, (fakeclient,viewangles,forwardmove,sidemove,upmove,buttons,impulse,msec));
}
int NumberOfEntities_Post(void) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, NumberOfEntities, ());
}

char *GetInfoKeyBuffer_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST(char*, 0, GetInfoKeyBuffer, (e));
}
char *InfoKeyValue_Post(char *infobuffer, char *key) {
	FAKEMETA_ENGINE_HANDLE_POST(char*, 0, InfoKeyValue, (infobuffer, key));
}
void SetKeyValue_Post(char *infobuffer, char *key, char *value) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetKeyValue, (infobuffer,key,value));
}
void SetClientKeyValue_Post(int clientIndex, char *infobuffer, char *key, char *value) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetClientKeyValue, (clientIndex,infobuffer,key,value));
}

int IsMapValid_Post(char *filename) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, IsMapValid, (filename));
}
void StaticDecal_Post( const float *origin, int decalIndex, int entityIndex, int modelIndex ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(StaticDecal, (origin,decalIndex,entityIndex,modelIndex));
}
int PrecacheGeneric_Post(char *s) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, PrecacheGeneric, (s));
}
int GetPlayerUserId_Post(edict_t *e ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, GetPlayerUserId, (e));
}
void BuildSoundMsg_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	FAKEMETA_ENGINE_HANDLE_POST_void(BuildSoundMsg, (entity,channel,sample,volume,attenuation,fFlags,pitch,msg_dest,msg_type,pOrigin,ed));
}
int IsDedicatedServer_Post(void) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, IsDedicatedServer, ());
}
cvar_t *CVarGetPointer_Post(const char *szVarName) {
	FAKEMETA_ENGINE_HANDLE_POST(cvar_t*, 0, CVarGetPointer, (szVarName));
}
unsigned int GetPlayerWONId_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned int, 0, GetPlayerWONId, (e));
}

//! YWB 8/1/99 TFF Physics additions
void Info_RemoveKey_Post( char *s, const char *key ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(Info_RemoveKey, (s,key));
}
const char *GetPhysicsKeyValue_Post( const edict_t *pClient, const char *key ) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, GetPhysicsKeyValue, (pClient, key));
}
void SetPhysicsKeyValue_Post( const edict_t *pClient, const char *key, const char *value ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetPhysicsKeyValue, (pClient,key,value));
}
const char *GetPhysicsInfoString_Post( const edict_t *pClient ) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, GetPhysicsInfoString, (pClient));
}
unsigned short PrecacheEvent_Post( int type, const char *psz ) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned short, 0, PrecacheEvent, (type, psz));
}
void PlaybackEvent_Post( int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2 ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(PlaybackEvent, (flags,pInvoker,eventindex,delay,origin,angles,fparam1,fparam2,iparam1,iparam2,bparam1,bparam2));
}

unsigned char *SetFatPVS_Post( float *org ) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned char*, 0, SetFatPVS, (org));
}
unsigned char *SetFatPAS_Post( float *org ) {
	FAKEMETA_ENGINE_HANDLE_POST(unsigned char*, 0, SetFatPAS, (org));
}

int CheckVisibility_Post( const edict_t *entity, unsigned char *pset ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, CheckVisibility, (entity, pset));
}

void DeltaSetField_Post( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(DeltaSetField, (pFields,fieldname));
}
void DeltaUnsetField_Post( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(DeltaUnsetField, (pFields,fieldname));
}
void DeltaAddEncoder_Post( char *name, void (*conditionalencode)( struct delta_s *pFields, const unsigned char *from, const unsigned char *to ) ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(DeltaAddEncoder, (name,conditionalencode));
}
int GetCurrentPlayer_Post( void ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, GetCurrentPlayer, ());
}
int CanSkipPlayer_Post( const edict_t *player ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, CanSkipPlayer, (player));
}
int DeltaFindField_Post( struct delta_s *pFields, const char *fieldname ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, DeltaFindField, (pFields, fieldname));
}
void DeltaSetFieldByIndex_Post( struct delta_s *pFields, int fieldNumber ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(DeltaSetFieldByIndex, (pFields,fieldNumber));
}
void DeltaUnsetFieldByIndex_Post( struct delta_s *pFields, int fieldNumber ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(DeltaUnsetFieldByIndex, (pFields,fieldNumber));
}

void SetGroupMask_Post( int mask, int op ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(SetGroupMask, (mask,op));
}

int engCreateInstancedBaseline_Post( int classname, struct entity_state_s *baseline ) {
	FAKEMETA_ENGINE_HANDLE_POST(int, 0, CreateInstancedBaseline, (classname, baseline));
}
void Cvar_DirectSet_Post( struct cvar_s *var, char *value ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(Cvar_DirectSet, (var,value));
}

//! Forces the client and server to be running with the same version of the specified file
//!( e.g., a player model ).
//! Calling this has no effect in single player
void ForceUnmodified_Post( FORCE_TYPE type, float *mins, float *maxs, const char *filename ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(ForceUnmodified, (type,mins,maxs,filename));
}

void GetPlayerStats_Post( const edict_t *pClient, int *ping, int *packet_loss ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(GetPlayerStats, (pClient,ping,packet_loss));
}

void AddServerCommand_Post( char *cmd_name, void (*function)(void) ) {
	FAKEMETA_ENGINE_HANDLE_POST_void(AddServerCommand, (cmd_name,function));
}
// Added in SDK 2.2:
qboolean Voice_GetClientListening_Post(int iReceiver, int iSender) {
	FAKEMETA_ENGINE_HANDLE_POST(qboolean, 0, Voice_GetClientListening, (iReceiver,iSender));
}
qboolean Voice_SetClientListening_Post(int iReceiver, int iSender, qboolean bListen) {
	FAKEMETA_ENGINE_HANDLE_POST(qboolean, 0, Voice_SetClientListening, (iReceiver, iSender, bListen));
}

// Added for HL 1109 (no SDK update):
const char *GetPlayerAuthId_Post(edict_t *e) {
	FAKEMETA_ENGINE_HANDLE_POST(const char*, 0, GetPlayerAuthId, (e));
}

//   **** dllapi

// from SDK dlls/cbase.h:
void GameDLLInit(void) {
	FAKEMETA_DLL_HANDLE_void(GameInit, ());
}
int DispatchSpawn( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE(int, 0, Spawn, (pent));
}
void DispatchThink( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE_void(Think, (pent));
}
void DispatchUse( edict_t *pentUsed, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_void(Use, (pentUsed, pentOther));
}
void DispatchTouch( edict_t *pentTouched, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_void(Touch, (pentTouched, pentOther));
}
void DispatchBlocked( edict_t *pentBlocked, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_void(Blocked, (pentBlocked, pentOther));
}
void DispatchKeyValue( edict_t *pentKeyvalue, KeyValueData *pkvd ) {
	FAKEMETA_DLL_HANDLE_void(KeyValue, (pentKeyvalue, pkvd));
}
void DispatchSave( edict_t *pent, SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_void(Save, (pent, pSaveData));
}
int  DispatchRestore( edict_t *pent, SAVERESTOREDATA *pSaveData, int globalEntity ) {
	FAKEMETA_DLL_HANDLE(int , 0, Restore, (pent,pSaveData,globalEntity));
}
void DispatchObjectCollsionBox( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE_void(SetAbsBox, (pent));
}
void SaveWriteFields( SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount ) {
	FAKEMETA_DLL_HANDLE_void(SaveWriteFields, (pSaveData,pname,pBaseData,pFields,fieldCount));
}
void SaveReadFields( SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount ) {
	FAKEMETA_DLL_HANDLE_void(SaveReadFields, (pSaveData,pname,pBaseData,pFields,fieldCount));
}
void SaveGlobalState( SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_void(SaveGlobalState, (pSaveData));
}
void RestoreGlobalState( SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_void(RestoreGlobalState, (pSaveData));
}
void ResetGlobalState( void ) {
	FAKEMETA_DLL_HANDLE_void(ResetGlobalState, ());
}

// from SDK dlls/client.h:
BOOL ClientConnect( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] ) {
	FAKEMETA_DLL_HANDLE(BOOL, 0, ClientConnect, (pEntity,pszName,pszAddress,szRejectReason));
}
void ClientDisconnect( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(ClientDisconnect, (pEntity));
}
void ClientKill( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(ClientKill, (pEntity));
}
void ClientPutInServer( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(ClientPutInServer, (pEntity));
}
void ClientCommand( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(ClientCommand, (pEntity));
}
void ClientUserInfoChanged( edict_t *pEntity, char *infobuffer ) {
	FAKEMETA_DLL_HANDLE_void(ClientUserInfoChanged, (pEntity,infobuffer));
}
void ServerActivate( edict_t *pEdictList, int edictCount, int clientMax ) {
	FAKEMETA_DLL_HANDLE_void(ServerActivate, (pEdictList,edictCount,clientMax));
}
void ServerDeactivate( void ) {
	FAKEMETA_DLL_HANDLE_void(ServerDeactivate, ());
}
void PlayerPreThink( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(PlayerPreThink, (pEntity));
}
void PlayerPostThink( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(PlayerPostThink, (pEntity));
}
void StartFrame( void ) {
	FAKEMETA_DLL_HANDLE_void(StartFrame, ());
}
void ParmsNewLevel( void ) {
	FAKEMETA_DLL_HANDLE_void(ParmsNewLevel, ());
}
void ParmsChangeLevel( void ) {
	FAKEMETA_DLL_HANDLE_void(ParmsChangeLevel, ());
}
const char *GetGameDescription( void ) {
	FAKEMETA_DLL_HANDLE(const char*, 0, GetGameDescription, ());
}
void PlayerCustomization( edict_t *pEntity, customization_t *pCust ) {
	FAKEMETA_DLL_HANDLE_void(PlayerCustomization, (pEntity,pCust));
}
void SpectatorConnect( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(SpectatorConnect, (pEntity));
}
void SpectatorDisconnect( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(SpectatorDisconnect, (pEntity));
}
void SpectatorThink( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_void(SpectatorThink, (pEntity));
}
void Sys_Error( const char *error_string ) {
	FAKEMETA_DLL_HANDLE_void(Sys_Error, (error_string));
}

// from SDK pm_shared/pm_shared.h:
void PM_Move( struct playermove_s *ppmove, int server ) {
	FAKEMETA_DLL_HANDLE_void(PM_Move, (ppmove,server));
}
void PM_Init( struct playermove_s *ppmove ) {
	FAKEMETA_DLL_HANDLE_void(PM_Init, (ppmove));
}

char PM_FindTextureType( char *name ) {
	FAKEMETA_DLL_HANDLE(char, 0, PM_FindTextureType, (name));
}

// from SDK dlls/client.h:
void SetupVisibility( edict_t *pViewEntity, edict_t *pClient, unsigned char **pvs, unsigned char **pas ) {
	FAKEMETA_DLL_HANDLE_void(SetupVisibility, (pViewEntity,pClient,pvs,pas));
}
void UpdateClientData( const struct edict_s *ent, int sendweapons, struct clientdata_s *cd ) {
	FAKEMETA_DLL_HANDLE_void(UpdateClientData, (ent,sendweapons,cd));
}
int AddToFullPack( struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet ) {
	FAKEMETA_DLL_HANDLE(int, 0, AddToFullPack, (state,e,ent,host,hostflags,player,pSet));
}
void CreateBaseline( int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs ) {
	FAKEMETA_DLL_HANDLE_void(CreateBaseline, (player,eindex,baseline,entity,playermodelindex,player_mins,player_maxs));
}
void RegisterEncoders( void ) {
	FAKEMETA_DLL_HANDLE_void(RegisterEncoders, ());
}
int GetWeaponData( struct edict_s *player, struct weapon_data_s *info ) {
	FAKEMETA_DLL_HANDLE(int, 0, GetWeaponData, (player,info));
}
void CmdStart( const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed ) {
	FAKEMETA_DLL_HANDLE_void(CmdStart, (player,cmd,random_seed));
}
void CmdEnd( const edict_t *player ) {
	FAKEMETA_DLL_HANDLE_void(CmdEnd, (player));
}
int ConnectionlessPacket( const struct netadr_s *net_from, const char *args, char *response_buffer, int *response_buffer_size ) {
	FAKEMETA_DLL_HANDLE(int, 0, ConnectionlessPacket, (net_from,args,response_buffer,response_buffer_size));
}
int GetHullBounds( int hullnumber, float *mins, float *maxs ) {
	FAKEMETA_DLL_HANDLE(int, 0, GetHullBounds, (hullnumber,mins,maxs));
}
void CreateInstancedBaselines( void ) {
	FAKEMETA_DLL_HANDLE_void(CreateInstancedBaselines, ());
}
int InconsistentFile( const edict_t *player, const char *filename, char *disconnect_message ) {
	FAKEMETA_DLL_HANDLE(int, 0, InconsistentFile, (player,filename,disconnect_message));
}
int AllowLagCompensation( void )
{
	FAKEMETA_DLL_HANDLE(int, 0, AllowLagCompensation, ());
}

//    **** dllapi post

// from SDK dlls/cbase.h:
void GameDLLInit_Post(void) {
	FAKEMETA_DLL_HANDLE_POST_void(GameInit, ());
}
int DispatchSpawn_Post( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, Spawn, (pent));
}
void DispatchThink_Post( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE_POST_void(Think, (pent));
}
void DispatchUse_Post( edict_t *pentUsed, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_POST_void(Use, (pentUsed, pentOther));
}
void DispatchTouch_Post( edict_t *pentTouched, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_POST_void(Touch, (pentTouched, pentOther));
}
void DispatchBlocked_Post( edict_t *pentBlocked, edict_t *pentOther ) {
	FAKEMETA_DLL_HANDLE_POST_void(Blocked, (pentBlocked, pentOther));
}
void DispatchKeyValue_Post( edict_t *pentKeyvalue, KeyValueData *pkvd ) {
	FAKEMETA_DLL_HANDLE_POST_void(KeyValue, (pentKeyvalue, pkvd));
}
void DispatchSave_Post( edict_t *pent, SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_POST_void(Save, (pent, pSaveData));
}
int  DispatchRestore_Post( edict_t *pent, SAVERESTOREDATA *pSaveData, int globalEntity ) {
	FAKEMETA_DLL_HANDLE_POST(int , 0, Restore, (pent,pSaveData,globalEntity));
}
void DispatchObjectCollsionBox_Post( edict_t *pent ) {
	FAKEMETA_DLL_HANDLE_POST_void(SetAbsBox, (pent));
}
void SaveWriteFields_Post( SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount ) {
	FAKEMETA_DLL_HANDLE_POST_void(SaveWriteFields, (pSaveData,pname,pBaseData,pFields,fieldCount));
}
void SaveReadFields_Post( SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount ) {
	FAKEMETA_DLL_HANDLE_POST_void(SaveReadFields, (pSaveData,pname,pBaseData,pFields,fieldCount));
}
void SaveGlobalState_Post( SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_POST_void(SaveGlobalState, (pSaveData));
}
void RestoreGlobalState_Post( SAVERESTOREDATA *pSaveData ) {
	FAKEMETA_DLL_HANDLE_POST_void(RestoreGlobalState, (pSaveData));
}
void ResetGlobalState_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(ResetGlobalState, ());
}

// from SDK dlls/client.h:
BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] ) {
	FAKEMETA_DLL_HANDLE_POST(BOOL, 0, ClientConnect, (pEntity,pszName,pszAddress,szRejectReason));
}
void ClientDisconnect_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(ClientDisconnect, (pEntity));
}
void ClientKill_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(ClientKill, (pEntity));
}
void ClientPutInServer_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(ClientPutInServer, (pEntity));
}
void ClientCommand_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(ClientCommand, (pEntity));
}
void ClientUserInfoChanged_Post( edict_t *pEntity, char *infobuffer ) {
	FAKEMETA_DLL_HANDLE_POST_void(ClientUserInfoChanged, (pEntity,infobuffer));
}
void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ) {
	FAKEMETA_DLL_HANDLE_POST_void(ServerActivate, (pEdictList,edictCount,clientMax));
}
void ServerDeactivate_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(ServerDeactivate, ());
}
void PlayerPreThink_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(PlayerPreThink, (pEntity));
}
void PlayerPostThink_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(PlayerPostThink, (pEntity));
}
void StartFrame_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(StartFrame, ());
}
void ParmsNewLevel_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(ParmsNewLevel, ());
}
void ParmsChangeLevel_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(ParmsChangeLevel, ());
}
const char *GetGameDescription_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST(const char*, 0, GetGameDescription, ());
}
void PlayerCustomization_Post( edict_t *pEntity, customization_t *pCust ) {
	FAKEMETA_DLL_HANDLE_POST_void(PlayerCustomization, (pEntity,pCust));
}
void SpectatorConnect_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(SpectatorConnect, (pEntity));
}
void SpectatorDisconnect_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(SpectatorDisconnect, (pEntity));
}
void SpectatorThink_Post( edict_t *pEntity ) {
	FAKEMETA_DLL_HANDLE_POST_void(SpectatorThink, (pEntity));
}
void Sys_Error_Post( const char *error_string ) {
	FAKEMETA_DLL_HANDLE_POST_void(Sys_Error, (error_string));
}

// from SDK pm_shared/pm_shared.h:
void PM_Move_Post( struct playermove_s *ppmove, int server ) {
	FAKEMETA_DLL_HANDLE_POST_void(PM_Move, (ppmove,server));
}
void PM_Init_Post( struct playermove_s *ppmove ) {
	FAKEMETA_DLL_HANDLE_POST_void(PM_Init, (ppmove));
}

char PM_FindTextureType_Post( char *name ) {
	FAKEMETA_DLL_HANDLE_POST(char, 0, PM_FindTextureType, (name));
}

// from SDK dlls/client.h:
void SetupVisibility_Post( edict_t *pViewEntity, edict_t *pClient, unsigned char **pvs, unsigned char **pas ) {
	FAKEMETA_DLL_HANDLE_POST_void(SetupVisibility, (pViewEntity,pClient,pvs,pas));
}
void UpdateClientData_Post( const struct edict_s *ent, int sendweapons, struct clientdata_s *cd ) {
	FAKEMETA_DLL_HANDLE_POST_void(UpdateClientData, (ent,sendweapons,cd));
}
int AddToFullPack_Post( struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, AddToFullPack, (state,e,ent,host,hostflags,player,pSet));
}
void CreateBaseline_Post( int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs ) {
	FAKEMETA_DLL_HANDLE_POST_void(CreateBaseline, (player,eindex,baseline,entity,playermodelindex,player_mins,player_maxs));
}
void RegisterEncoders_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(RegisterEncoders, ());
}
int GetWeaponData_Post( struct edict_s *player, struct weapon_data_s *info ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, GetWeaponData, (player,info));
}
void CmdStart_Post( const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed ) {
	FAKEMETA_DLL_HANDLE_POST_void(CmdStart, (player,cmd,random_seed));
}
void CmdEnd_Post( const edict_t *player ) {
	FAKEMETA_DLL_HANDLE_POST_void(CmdEnd, (player));
}
int ConnectionlessPacket_Post( const struct netadr_s *net_from, const char *args, char *response_buffer, int *response_buffer_size ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, ConnectionlessPacket, (net_from,args,response_buffer,response_buffer_size));
}
int GetHullBounds_Post( int hullnumber, float *mins, float *maxs ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, GetHullBounds, (hullnumber,mins,maxs));
}
void CreateInstancedBaselines_Post( void ) {
	FAKEMETA_DLL_HANDLE_POST_void(CreateInstancedBaselines, ());
}
int InconsistentFile_Post( const edict_t *player, const char *filename, char *disconnect_message ) {
	FAKEMETA_DLL_HANDLE_POST(int, 0, InconsistentFile, (player,filename,disconnect_message));
}
int AllowLagCompensation_Post( void ) {
	FAKEMETA_DLL_HANDLE(int, 0, AllowLagCompensation, ());
}

// *** newdllapi
void OnFreeEntPrivateData(edict_t *pEnt) {
	FAKEMETA_NEWDLL_HANDLE_void(OnFreeEntPrivateData, (pEnt));
}
void GameShutdown(void) {
	FAKEMETA_NEWDLL_HANDLE_void(GameShutdown, ());
}
int ShouldCollide(edict_t *pentTouched, edict_t *pentOther) {
	FAKEMETA_NEWDLL_HANDLE(int, 0, ShouldCollide, (pentTouched,pentOther));
}

// *** newdllapi post
void OnFreeEntPrivateData_Post(edict_t *pEnt) {
	FAKEMETA_NEWDLL_HANDLE_POST_void(OnFreeEntPrivateData, (pEnt));
}
void GameShutdown_Post(void) {
	FAKEMETA_NEWDLL_HANDLE_POST_void(GameShutdown, ());
}
int ShouldCollide_Post(edict_t *pentTouched, edict_t *pentOther)
{	FAKEMETA_NEWDLL_HANDLE_POST(int, 0, ShouldCollide, (pentTouched,pentOther));

}

// ***** Global function tables
DLL_FUNCTIONS g_DllFunctionTable = 
{
	GameDLLInit,
	DispatchSpawn,
	DispatchThink,
	DispatchUse,
	DispatchTouch,
	DispatchBlocked,
	DispatchKeyValue,
	DispatchSave,
	DispatchRestore,
	DispatchObjectCollsionBox,

	SaveWriteFields,
	SaveReadFields,

	SaveGlobalState,
	RestoreGlobalState,
	ResetGlobalState,

	ClientConnect,
	ClientDisconnect,
	ClientKill,
	ClientPutInServer,
	ClientCommand,
	ClientUserInfoChanged,
	ServerActivate,
	ServerDeactivate,

	PlayerPreThink,
	PlayerPostThink,

	StartFrame,
	ParmsNewLevel,
	ParmsChangeLevel,

	GetGameDescription,
	PlayerCustomization,

	SpectatorConnect,
	SpectatorDisconnect,
	SpectatorThink,
	
	Sys_Error,

	PM_Move,
	PM_Init,
	PM_FindTextureType,
	
	SetupVisibility,
	UpdateClientData,
	AddToFullPack,
	CreateBaseline,
	RegisterEncoders,
	GetWeaponData,
	CmdStart,
	CmdEnd,
	ConnectionlessPacket,
	GetHullBounds,
	CreateInstancedBaselines,
	InconsistentFile,
	AllowLagCompensation,
};

DLL_FUNCTIONS g_DllFunctionTable_Post = 
{
	GameDLLInit_Post,
	DispatchSpawn_Post,
	DispatchThink_Post,
	DispatchUse_Post,
	DispatchTouch_Post,
	DispatchBlocked_Post,
	DispatchKeyValue_Post,
	DispatchSave_Post,
	DispatchRestore_Post,
	DispatchObjectCollsionBox_Post,

	SaveWriteFields_Post,
	SaveReadFields_Post,

	SaveGlobalState_Post,
	RestoreGlobalState_Post,
	ResetGlobalState_Post,

	ClientConnect_Post,
	ClientDisconnect_Post,
	ClientKill_Post,
	ClientPutInServer_Post,
	ClientCommand_Post,
	ClientUserInfoChanged_Post,
	ServerActivate_Post,
	ServerDeactivate_Post,

	PlayerPreThink_Post,
	PlayerPostThink_Post,

	StartFrame_Post,
	ParmsNewLevel_Post,
	ParmsChangeLevel_Post,

	GetGameDescription_Post,
	PlayerCustomization_Post,

	SpectatorConnect_Post,
	SpectatorDisconnect_Post,
	SpectatorThink_Post,
	
	Sys_Error_Post,

	PM_Move_Post,
	PM_Init_Post,
	PM_FindTextureType_Post,
	
	SetupVisibility_Post,
	UpdateClientData_Post,
	AddToFullPack_Post,
	CreateBaseline_Post,
	RegisterEncoders_Post,
	GetWeaponData_Post,
	CmdStart_Post,
	CmdEnd_Post,
	ConnectionlessPacket_Post,
	GetHullBounds_Post,
	CreateInstancedBaselines_Post,
	InconsistentFile_Post,
	AllowLagCompensation_Post
};

NEW_DLL_FUNCTIONS g_NewDllFunctionTable = 
{
	OnFreeEntPrivateData,
	GameShutdown,
	ShouldCollide
};

NEW_DLL_FUNCTIONS g_NewDllFunctionTable_Post = 
{
	OnFreeEntPrivateData_Post,
	GameShutdown_Post,
	ShouldCollide_Post
};

enginefuncs_t g_EngineFunctionTable = 
{
	PrecacheModel,
	PrecacheSound,
	SetModel,
	ModelIndex,
	ModelFrames,

	SetSize,
	ChangeLevel,
	GetSpawnParms,
	SaveSpawnParms,

	VecToYaw,
	VecToAngles,
	MoveToOrigin,
	ChangeYaw,
	ChangePitch,

	FindEntityByString,
	GetEntityIllum,
	FindEntityInSphere,
	FindClientInPVS,
	EntitiesInPVS,

	MakeVectors,
	AngleVectors,

	CreateEntity,
	RemoveEntity,
	CreateNamedEntity,

	MakeStatic,
	EntIsOnFloor,
	DropToFloor,

	WalkMove,
	SetOrigin,

	EmitSound,
	EmitAmbientSound,

	TraceLine,
	TraceToss,
	TraceMonsterHull,
	TraceHull,
	TraceModel,
	TraceTexture,
	TraceSphere,
	GetAimVector,

	ServerCommand,
	ServerExecute,
	engClientCommand,

	ParticleEffect,
	LightStyle,
	DecalIndex,
	PointContents,

	MessageBegin,
	MessageEnd,

	WriteByte,
	WriteChar,
	WriteShort,
	WriteLong,
	WriteAngle,
	WriteCoord,
	WriteString,
	WriteEntity,

	CVarRegister,
	CVarGetFloat,
	CVarGetString,
	CVarSetFloat,
	CVarSetString,

	AlertMessage,
	EngineFprintf,

	PvAllocEntPrivateData,
	PvEntPrivateData,
	FreeEntPrivateData,

	SzFromIndex,
	AllocString,

	GetVarsOfEnt, 			
	PEntityOfEntOffset,
	EntOffsetOfPEntity,
	IndexOfEdict,
	PEntityOfEntIndex,
	FindEntityByVars,
	GetModelPtr,

	RegUserMsg,

	AnimationAutomove,
	GetBonePosition,

	FunctionFromName,
	NameForFunction,

	ClientPrintf,
	ServerPrint,

	Cmd_Args,
	Cmd_Argv,
	Cmd_Argc,

	GetAttachment,

	CRC32_Init,
	CRC32_ProcessBuffer,
	CRC32_ProcessByte,
	CRC32_Final,

	RandomLong,
	RandomFloat,

	SetView,
	Time,
	CrosshairAngle,

	LoadFileForMe,
	FreeFile,

	EndSection,
	CompareFileTime,
	GetGameDir,
	Cvar_RegisterVariable,
	FadeClientVolume,
	SetClientMaxspeed,
	CreateFakeClient,
	RunPlayerMove,
	NumberOfEntities,

	GetInfoKeyBuffer,
	InfoKeyValue,
	SetKeyValue,
	SetClientKeyValue,

	IsMapValid,
	StaticDecal,
	PrecacheGeneric,
	GetPlayerUserId, 		
	BuildSoundMsg,
	IsDedicatedServer,
	CVarGetPointer,
	GetPlayerWONId,

	
	Info_RemoveKey,
	GetPhysicsKeyValue,
	SetPhysicsKeyValue,
	GetPhysicsInfoString,
	PrecacheEvent,
	PlaybackEvent,

	SetFatPVS,
	SetFatPAS,

	CheckVisibility,

	DeltaSetField,
	DeltaUnsetField,
	DeltaAddEncoder,
	GetCurrentPlayer,
	CanSkipPlayer,
	DeltaFindField,
	DeltaSetFieldByIndex,
	DeltaUnsetFieldByIndex,

	SetGroupMask,

	engCreateInstancedBaseline, 
	Cvar_DirectSet,

	ForceUnmodified,

	GetPlayerStats,

	AddServerCommand,

	
	Voice_GetClientListening,
	Voice_SetClientListening,

	
	GetPlayerAuthId,
};

enginefuncs_t g_EngineFunctionTable_Post = 
{
	PrecacheModel_Post,
	PrecacheSound_Post,
	SetModel_Post,
	ModelIndex_Post,
	ModelFrames_Post,

	SetSize_Post,
	ChangeLevel_Post,
	GetSpawnParms_Post,
	SaveSpawnParms_Post,

	VecToYaw_Post,
	VecToAngles_Post,
	MoveToOrigin_Post,
	ChangeYaw_Post,
	ChangePitch_Post,

	FindEntityByString_Post,
	GetEntityIllum_Post,
	FindEntityInSphere_Post,
	FindClientInPVS_Post,
	EntitiesInPVS_Post,

	MakeVectors_Post,
	AngleVectors_Post,

	CreateEntity_Post,
	RemoveEntity_Post,
	CreateNamedEntity_Post,

	MakeStatic_Post,
	EntIsOnFloor_Post,
	DropToFloor_Post,

	WalkMove_Post,
	SetOrigin_Post,

	EmitSound_Post,
	EmitAmbientSound_Post,

	TraceLine_Post,
	TraceToss_Post,
	TraceMonsterHull_Post,
	TraceHull_Post,
	TraceModel_Post,
	TraceTexture_Post,
	TraceSphere_Post,
	GetAimVector_Post,

	ServerCommand_Post,
	ServerExecute_Post,
	engClientCommand_Post,

	ParticleEffect_Post,
	LightStyle_Post,
	DecalIndex_Post,
	PointContents_Post,

	MessageBegin_Post,
	MessageEnd_Post,

	WriteByte_Post,
	WriteChar_Post,
	WriteShort_Post,
	WriteLong_Post,
	WriteAngle_Post,
	WriteCoord_Post,
	WriteString_Post,
	WriteEntity_Post,

	CVarRegister_Post,
	CVarGetFloat_Post,
	CVarGetString_Post,
	CVarSetFloat_Post,
	CVarSetString_Post,

	AlertMessage_Post,
	EngineFprintf_Post,

	PvAllocEntPrivateData_Post,
	PvEntPrivateData_Post,
	FreeEntPrivateData_Post,

	SzFromIndex_Post,
	AllocString_Post,

	GetVarsOfEnt_Post, 			
	PEntityOfEntOffset_Post,
	EntOffsetOfPEntity_Post,
	IndexOfEdict_Post,
	PEntityOfEntIndex_Post,
	FindEntityByVars_Post,
	GetModelPtr_Post,

	RegUserMsg_Post,

	AnimationAutomove_Post,
	GetBonePosition_Post,

	FunctionFromName_Post,
	NameForFunction_Post,

	ClientPrintf_Post,
	ServerPrint_Post,

	Cmd_Args_Post,
	Cmd_Argv_Post,
	Cmd_Argc_Post,

	GetAttachment_Post,

	CRC32_Init_Post,
	CRC32_ProcessBuffer_Post,
	CRC32_ProcessByte_Post,
	CRC32_Final_Post,

	RandomLong_Post,
	RandomFloat_Post,

	SetView_Post,
	Time_Post,
	CrosshairAngle_Post,

	LoadFileForMe_Post,
	FreeFile_Post,

	EndSection_Post,
	CompareFileTime_Post,
	GetGameDir_Post,
	Cvar_RegisterVariable_Post,
	FadeClientVolume_Post,
	SetClientMaxspeed_Post,
	CreateFakeClient_Post,
	RunPlayerMove_Post,
	NumberOfEntities_Post,

	GetInfoKeyBuffer_Post,
	InfoKeyValue_Post,
	SetKeyValue_Post,
	SetClientKeyValue_Post,

	IsMapValid_Post,
	StaticDecal_Post,
	PrecacheGeneric_Post,
	GetPlayerUserId_Post, 		
	BuildSoundMsg_Post,
	IsDedicatedServer_Post,
	CVarGetPointer_Post,
	GetPlayerWONId_Post,

	
	Info_RemoveKey_Post,
	GetPhysicsKeyValue_Post,
	SetPhysicsKeyValue_Post,
	GetPhysicsInfoString_Post,
	PrecacheEvent_Post,
	PlaybackEvent_Post,

	SetFatPVS_Post,
	SetFatPAS_Post,

	CheckVisibility_Post,

	DeltaSetField_Post,
	DeltaUnsetField_Post,
	DeltaAddEncoder_Post,
	GetCurrentPlayer_Post,
	CanSkipPlayer_Post,
	DeltaFindField_Post,
	DeltaSetFieldByIndex_Post,
	DeltaUnsetFieldByIndex_Post,

	SetGroupMask_Post,

	engCreateInstancedBaseline_Post, 
	Cvar_DirectSet_Post,

	ForceUnmodified_Post,

	GetPlayerStats_Post,

	AddServerCommand_Post,

	
	Voice_GetClientListening_Post,
	Voice_SetClientListening_Post,

	
	GetPlayerAuthId_Post,
};

// ***** CFakeMetaPlugin
CFakeMeta::CFakeMetaPlugin::CFakeMetaPlugin(const char *path)
{
	m_Path.set(path);
	m_Status = PL_EMPTY;
	m_Info = NULL;
	memset((void *)&m_DllFuncTable, 0, sizeof(DLL_FUNCTIONS));
	memset((void *)&m_EngineFuncTable, 0, sizeof(enginefuncs_t));
	memset((void *)&m_NewDllFuncTable, 0, sizeof(NEW_DLL_FUNCTIONS));
	memset((void *)&m_DllFuncTable_Post, 0, sizeof(DLL_FUNCTIONS));
	memset((void *)&m_EngineFuncTable_Post, 0, sizeof(enginefuncs_t));
	memset((void *)&m_NewDllFuncTable_Post, 0, sizeof(NEW_DLL_FUNCTIONS));
	memset((void *)&m_MetaFuncTable, 0, sizeof(META_FUNCTIONS));
	m_Handle = NULL;
}

CFakeMeta::CFakeMetaPlugin::~CFakeMetaPlugin()
{
	if (m_Handle)
	{
		DLCLOSE(m_Handle);
		m_Handle = NULL;
	}
}

int CFakeMeta::CFakeMetaPlugin::Query(mutil_funcs_t *pMetaUtilFuncs)
{
	// Load the library
	// We don't have to DLCLOSE here.
	m_Handle = DLOPEN(build_pathname("%s", m_Path.str()));
	if (!m_Handle)
	{
		m_Status = PL_BADFILE;
		return 0;
	}


	META_QUERY_FN queryFn = (META_QUERY_FN)DLSYM(m_Handle, "Meta_Query");
	GIVE_ENGINE_FUNCTIONS_FN giveEngFuncsFn = (GIVE_ENGINE_FUNCTIONS_FN)DLSYM(m_Handle, "GiveFnptrsToDll");

	// So we can print all problems before quitting the function
	bool missingFunc = false;
	if (!queryFn)
	{
		AMXXLOG_Log("[AMXX] Module \"%s\" doesn't provide a Query function.", m_Path.str());
		missingFunc = true;
	}

	if (!giveEngFuncsFn)
	{
		AMXXLOG_Log("[AMXX] Module \"%s\" doesn't provide a GiveFnptrsToDll function.", m_Path.str());
		missingFunc = true;
	}

	// Also check for Attach and Detach
	if (DLSYM(m_Handle, "Meta_Attach") == NULL)
	{
		AMXXLOG_Log("[AMXX] Module \"%s\" doesn't provide a Meta_Attach function.", m_Path.str());
		missingFunc = true;
	}
	if (DLSYM(m_Handle, "Meta_Detach") == NULL)
	{
		AMXXLOG_Log("[AMXX] Module \"%s\" doesn't provide a Meta_Detach function.", m_Path.str());
		missingFunc = true;
	}

	if (missingFunc)
	{
		m_Status = PL_BADFILE;
		return 0;
	}
	giveEngFuncsFn(&g_engfuncs, gpGlobals);

	if (queryFn(META_INTERFACE_VERSION, &m_Info, pMetaUtilFuncs) != 1)
	{
		AMXXLOG_Log("[AMXX] Query Module \"%s\" failed.", m_Path.str());
		m_Status = PL_BADFILE;
		return 0;
	}

	m_Status = PL_OPENED;
	return 1;
}

int CFakeMeta::CFakeMetaPlugin::Attach(PLUG_LOADTIME now, meta_globals_t *pMGlobals, gamedll_funcs_t *pGameDllFuncs)
{
	if (!m_Handle)
		return 0;
	META_ATTACH_FN attachFn = (META_ATTACH_FN)DLSYM(m_Handle, "Meta_Attach");
	if (!attachFn)
	{
		// Should not be possible tho
		m_Status = PL_BADFILE;
		return 0;
	}
	if (attachFn(now, &m_MetaFuncTable, pMGlobals, pGameDllFuncs) != 1)
	{
		AMXXLOG_Log("[AMXX] Can't Attach Module \"%s\" (\"%s\").", m_Info->name, m_Path.str());
		m_Status = PL_FAILED;
		return 0;
	}

	m_Status = PL_RUNNING;
	return 1;
}

int CFakeMeta::CFakeMetaPlugin::Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason)
{
	if (!m_Handle)
		return 0;
	META_DETACH_FN detachFn = (META_DETACH_FN)DLSYM(m_Handle, "Meta_Detach");
	if (!detachFn)
	{
		// Should not be possible tho
		m_Status = PL_BADFILE;
		return 0;
	}
	if (detachFn(now, reason) != 1)
	{
		AMXXLOG_Log("[AMXX] Can't Detach Module \"%s\" (\"%s\").", m_Info->name, m_Path.str());
		m_Status = PL_FAILED;
		return 0;
	}

	m_Status = PL_OPENED;
	return 1;
}

#define FAKEMETA_GET_FUNCTABLE(getFunc, interfaceVersion, table) \
	if (!m_Handle) \
		return 0; \
	int ifVers = interfaceVersion; \
	if (m_MetaFuncTable.pfn##getFunc) \
	{ \
		if (!m_MetaFuncTable.pfn##getFunc(&table, &ifVers)) \
		{ \
			AMXXLOG_Log("[AMXX] Failed calling \"%s\" in module \"%s\" (\"%s\")", #getFunc, m_Info->name, m_Path.str()); \
			return 0; \
		} \
	} \
	return 1;

int CFakeMeta::CFakeMetaPlugin::GetEntityAPI2(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetEntityAPI2, interfaceVersion, m_DllFuncTable);
}

int CFakeMeta::CFakeMetaPlugin::GetEntityAPI2_Post(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetEntityAPI2_Post, interfaceVersion, m_DllFuncTable_Post);
}

int CFakeMeta::CFakeMetaPlugin::GetEngineFunctions(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetEngineFunctions, interfaceVersion, m_EngineFuncTable);
}

int CFakeMeta::CFakeMetaPlugin::GetEngineFunctions_Post(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetEngineFunctions_Post, interfaceVersion, m_EngineFuncTable_Post);
}

int CFakeMeta::CFakeMetaPlugin::GetNewDLLFunctions(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetNewDLLFunctions, interfaceVersion, m_NewDllFuncTable);
}

int CFakeMeta::CFakeMetaPlugin::GetNewDLLFunctions_Post(int interfaceVersion)
{
	FAKEMETA_GET_FUNCTABLE(GetNewDLLFunctions_Post, interfaceVersion, m_NewDllFuncTable_Post);
}

// ***** CFakeMeta

CFakeMeta::CFakeMeta()
{
	// We don't call AddCorePlugin here because CList adds the elements to the front...
}

CFakeMeta::~CFakeMeta()
{
	ReleasePlugins();
}

bool CFakeMeta::AddPlugin(const char *path)
{
	CFakeMetaPlugin *pPlugin = new CFakeMetaPlugin(path);
	if (!pPlugin)
		return true;
	m_Plugins.put(pPlugin);
	return false;
}

void CFakeMeta::ReleasePlugins()
{
	m_Plugins.clear();
}

bool CFakeMeta::AddCorePlugin()
{
	// Check whether there already is a core plugin
	if (m_Plugins.begin() && strcmp((*m_Plugins.begin()).GetPath(), "[AMXX Core]") == 0)
		return true;

	// make a fake plugin for the core
	CFakeMetaPlugin *pPlugin = new CFakeMetaPlugin("[AMXX Core]");
	if (!pPlugin)
		return false;
	m_Plugins.put_front(pPlugin);
	// Set its status to running so functions are called in it
	pPlugin->SetStatus(PL_RUNNING);
	return true;
}

// Query all added plugins
void CFakeMeta::Meta_Query(mutil_funcs_t *pMetaUtilFuncs)
{
	AddCorePlugin();

	// Query all plugins except core
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		(*iter).Query(pMetaUtilFuncs);
	}
}

// Attach all added plugins
void CFakeMeta::Meta_Attach(PLUG_LOADTIME now, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs)
{
	AddCorePlugin();

	// Attach all plugins except core
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		(*iter).Attach(now, pMGlobals, pGamedllFuncs);
	}
}

// Detach all added plugins
void CFakeMeta::Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason)
{
	// Detach all plugins except core
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		(*iter).Detach(now, reason);
	}
}

int CFakeMeta::GetEntityAPI2(DLL_FUNCTIONS *pFunctionTable /*from metamod*/, int *interfaceVersion /*from metamod*/,
	DLL_FUNCTIONS *pAMXXFunctionTable /*Functions amxx needs*/)
{
 	if(*interfaceVersion!=INTERFACE_VERSION)
	{
		LOG_ERROR(PLID,	"GetEntityAPI2 version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	INTERFACE_VERSION;
		return(FALSE);
	}
	memcpy( pFunctionTable, &g_DllFunctionTable, sizeof( DLL_FUNCTIONS ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetDllFuncTable()), pAMXXFunctionTable, sizeof(DLL_FUNCTIONS));

	// Call in all attached plugins except core
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetEntityAPI2(*interfaceVersion);
	}

	return TRUE;
}

int CFakeMeta::GetEntityAPI2_Post(DLL_FUNCTIONS *pFunctionTable /*from metamod*/, int *interfaceVersion /*from metamod*/,
	DLL_FUNCTIONS *pAMXXFunctionTable /*Functions amxx needs*/)
{
	if(*interfaceVersion!=INTERFACE_VERSION)
	{
		LOG_ERROR(PLID,	"GetEntityAPI2_Post version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	INTERFACE_VERSION;
		return(FALSE);
	}
	memcpy( pFunctionTable, &g_DllFunctionTable_Post, sizeof( DLL_FUNCTIONS ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetDllFuncTable_Post()), pAMXXFunctionTable, sizeof(DLL_FUNCTIONS));

	// Call in all attached plugins excpet core
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetEntityAPI2_Post(*interfaceVersion);
	}

	return TRUE;
}

int CFakeMeta::GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion,
	enginefuncs_t *pAMXXFunctionTable /*Fucntions amxx needs*/)
{
	if(*interfaceVersion!=ENGINE_INTERFACE_VERSION)
	{
		LOG_ERROR(PLID,	"GetEngineFunctions version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	ENGINE_INTERFACE_VERSION;
		return FALSE;
	}
	memcpy( pengfuncsFromEngine, &g_EngineFunctionTable, sizeof( enginefuncs_t ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetEngineFuncTable()), pAMXXFunctionTable, sizeof(enginefuncs_t));

	// Call in all attached plugins
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetEngineFunctions(*interfaceVersion);
	}
	return TRUE;
}

int CFakeMeta::GetEngineFunctions_Post(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion,
	enginefuncs_t *pAMXXFunctionTable /*Fucntions amxx needs*/)
{
	if(*interfaceVersion!=ENGINE_INTERFACE_VERSION)
	{
		LOG_ERROR(PLID,	"GetEngineFunctions_Post version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	ENGINE_INTERFACE_VERSION;
		return FALSE;
	}
	memcpy( pengfuncsFromEngine, &g_EngineFunctionTable_Post, sizeof( enginefuncs_t ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetEngineFuncTable_Post()), pAMXXFunctionTable, sizeof(enginefuncs_t));

	// Call in all attached plugins
	++iter;			// skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetEngineFunctions_Post(*interfaceVersion);
	}

	return TRUE;
}

int CFakeMeta::GetNewDLLFunctions(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion,
	NEW_DLL_FUNCTIONS *pAMXXFunctionTable)
{
	if(!pNewFunctionTable)
	{
		LOG_ERROR(PLID, "GetNewDLLFunctions called with null pNewFunctionTable");
		return(FALSE);
	}

	if(*interfaceVersion!=NEW_DLL_FUNCTIONS_VERSION)
	{
		LOG_ERROR(PLID,	"GetNewDllFunctions version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	NEW_DLL_FUNCTIONS_VERSION;
		return(FALSE);
	}
	memcpy( pNewFunctionTable, &g_NewDllFunctionTable, sizeof( DLL_FUNCTIONS ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetNewDllFuncTable()), pAMXXFunctionTable, sizeof(NEW_DLL_FUNCTIONS));

	// Call in all attached plugins
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetNewDLLFunctions(*interfaceVersion);
	}

	return TRUE;
}

int CFakeMeta::GetNewDLLFunctions_Post(NEW_DLL_FUNCTIONS *pNewFunctionTable, int *interfaceVersion,
	NEW_DLL_FUNCTIONS *pAMXXFunctionTable)
{
	if(!pNewFunctionTable)
	{
		LOG_ERROR(PLID, "GetNewDLLFunctions_Post called with null pNewFunctionTable");
		return(FALSE);
	}

	if(*interfaceVersion!=NEW_DLL_FUNCTIONS_VERSION)
	{
		LOG_ERROR(PLID,	"GetNewDllFunctions_Post version mismatch; requested=%d ours=%d",	*interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion =	NEW_DLL_FUNCTIONS_VERSION;
		return(FALSE);
	}
	memcpy( pNewFunctionTable, &g_NewDllFunctionTable_Post, sizeof( DLL_FUNCTIONS ) );

	// Make sure there is a core plugin
	AddCorePlugin();

	// Copy Core Function table
	CList<CFakeMetaPlugin>::iterator iter = m_Plugins.begin();
	memcpy(&((*iter).GetNewDllFuncTable_Post()), pAMXXFunctionTable, sizeof(NEW_DLL_FUNCTIONS));

	// Call in all attached plugins
	++iter;			// Skip core
	for (; iter; ++iter)
	{
		if ((*iter).GetStatus() == PL_RUNNING)
			(*iter).GetNewDLLFunctions_Post(*interfaceVersion);
	}

	return TRUE;
}