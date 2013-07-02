#include "fakemeta_amxx.h"

#if defined PAWN_CELL_SIZE
# if PAWN_CELL_SIZE == 16
#  define CELL_MIN SHRT_MIN
# elif PAWN_CELL_SIZE == 32
#  define CELL_MIN INT_MIN
# elif PAWN_CELL_SIZE == 64
#  define CELL_MIN _I64_MIN
# endif
#else
#  define CELL_MIN _I32_MIN
#endif

#if defined WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#else
//implement these with setjmp later.
bool IsBadReadPtr(void *l, size_t size)
{
	return false;
}
bool IsBadWritePtr(void *l, size_t size)
{
	return false;
}
#endif

static cell AMX_NATIVE_CALL set_pdata_int(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset); 

#if defined( __linux__ )
	iOffset += params[4];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		iOffset += params[4];
	else
		iOffset += params[5];
#endif
	int iValue=params[3];
	*((int *)INDEXENT2(index)->pvPrivateData + iOffset) = iValue;
	return 1;
}
static cell AMX_NATIVE_CALL get_pdata_int(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset); 

#if defined( __linux__ )
	iOffset += params[3];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		iOffset += params[3];
	else
		iOffset += params[4];
#endif

	return *((int *)INDEXENT2(index)->pvPrivateData + iOffset);
}
// Float
static cell AMX_NATIVE_CALL set_pdata_float(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset);

#if defined( __linux__ )
	iOffset += params[4];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		iOffset += params[4];
	else
		iOffset += params[5];
#endif

	float fValue=amx_ctof(params[3]);
	*((float *)INDEXENT2(index)->pvPrivateData + iOffset) = fValue;
	return 1;
}
static cell AMX_NATIVE_CALL get_pdata_float(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset); 

#if defined( __linux__ )
	iOffset += params[3];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		iOffset += params[3];
	else
		iOffset += params[4];
#endif

	return amx_ftoc(*((float *)INDEXENT2(index)->pvPrivateData + iOffset));
}

static cell AMX_NATIVE_CALL get_pdata_string(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset);

#if defined( __linux__ )
	iOffset += params[6];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 6 || params[7] == CELL_MIN)
		iOffset += params[6];
	else
		iOffset += params[7];
#endif

	edict_t *pEdict = INDEXENT2(index);

	char *szData;
	if (params[5])
	{
		szData = *((char **)pEdict->pvPrivateData + iOffset);
	} else {
		szData = (char *)pEdict->pvPrivateData + iOffset;
	}

	if (IsBadReadPtr(szData, 1))
	{
		return 0;
	}

	MF_SetAmxString(amx, params[3], szData, params[4]);

	return 1;
}

static cell AMX_NATIVE_CALL set_pdata_string(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset);

#if defined( __linux__ )
	iOffset += params[5];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 5 || params[6] == CELL_MIN)
		iOffset += params[5];
	else
		iOffset += params[6];
#endif

	edict_t *pEdict = INDEXENT2(index);

	char *szData;
	int len;
	char *data = MF_GetAmxString(amx, params[3], 0, &len);
	if (params[4] == -1)
	{
		szData = (char *)pEdict->pvPrivateData + iOffset;
		if (IsBadWritePtr(szData, 1))
			return 0;
		strcpy(szData, data);
	} else {
		szData = *((char **)pEdict->pvPrivateData + iOffset);
		if (IsBadWritePtr(szData, 1))
			return 0;
		if (params[4] == 1)
		{
			free(szData);
			szData = (char *)malloc(len + 1);
		} else if (params[4] == 2) {
			delete [] szData;
			szData = new char[len + 1];
		}
		strcpy(szData, data);
		*((char **)pEdict->pvPrivateData + iOffset) = szData;
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_ent(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);

	int iOffset=params[2];
	CHECK_OFFSET(iOffset); 

#if defined( __linux__ )
	iOffset += params[3];
#elif defined( __APPLE__ )
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		iOffset += params[3];
	else
		iOffset += params[4];
#endif

	edict_t *pEdict = *(edict_t **)((char *)(INDEXENT2(index)->pvPrivateData) + iOffset);

	if (pEdict == NULL)
	{
		return -1;
	}

	edict_t *pWorld = INDEXENT(0);
	int ent = pEdict - pWorld;

	if (ent < 0 || ent > gpGlobals->maxEntities)
	{
		return -2;
	}

	if (pEdict->free || pEdict->pvPrivateData == NULL)
	{
		return -1;
	}

	return ent;
}

static cell AMX_NATIVE_CALL set_pdata_ent(AMX *amx, cell *params)
{
	int index  = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

	int entity = params[3];
	CHECK_ENTITY(entity);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	*(edict_t **)((char *)(INDEXENT2(index)->pvPrivateData) + offset) = INDEXENT2(entity);

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_bool(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[3];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		offset += params[3];
	else
		offset += params[4];
#endif

	return *(bool *)((char *)INDEXENT2(index)->pvPrivateData + offset) ? TRUE : FALSE;
}

static cell AMX_NATIVE_CALL set_pdata_bool(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

	bool value = params[3] ? true : false;

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	*(bool *)((char *)INDEXENT2(index)->pvPrivateData + offset) = value;

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_byte(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[3];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		offset += params[3];
	else
		offset += params[4];
#endif
	
	return static_cast<cell>(*((byte *)INDEXENT2(index)->pvPrivateData + offset));
}

static cell AMX_NATIVE_CALL set_pdata_byte(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

	byte value = static_cast<byte>(params[3]);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	*((byte *)INDEXENT2(index)->pvPrivateData + offset) = value;

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_short(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[3];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		offset += params[3];
	else
		offset += params[4];
#endif

	return static_cast<cell>(*(short *)((char *)INDEXENT2(index)->pvPrivateData + offset));
}

static cell AMX_NATIVE_CALL set_pdata_short(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

	short value = static_cast<short>(params[3]);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	*(short *)((char *)INDEXENT2(index)->pvPrivateData + offset) = value;

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_vector(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	cell *cpvec = MF_GetAmxAddr(amx, params[3]);

	Vector vec = *(Vector *)((char *)INDEXENT2(index)->pvPrivateData + offset);

	cpvec[0] = amx_ftoc(vec.x);
	cpvec[1] = amx_ftoc(vec.y);
	cpvec[2] = amx_ftoc(vec.z);

	return 1;
}

static cell AMX_NATIVE_CALL set_pdata_vector(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	cell *pcvec = MF_GetAmxAddr(amx, params[3]);

	Vector vec(amx_ctof(pcvec[0]), amx_ctof(pcvec[1]), amx_ctof(pcvec[2]));

	*(Vector *)((char *)INDEXENT2(index)->pvPrivateData + offset) = vec;

	return 1;
}

static cell AMX_NATIVE_CALL get_pdata_ehandle(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

#if defined(__linux__)
	offset += params[3];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		offset += params[3];
	else
		offset += params[4];
#endif

	edict_t *pEdict = *(edict_t **)((char * )(INDEXENT2(index)->pvPrivateData) + offset);

	if (pEdict == NULL)
	{
		return -1;
	}

	edict_t *pWorld = INDEXENT(0);
	int ent = pEdict - pWorld;

	if (ent < 0 || ent > gpGlobals->maxEntities)
	{
		return -2;
	}

	if (pEdict->free || pEdict->pvPrivateData == NULL)
	{
		return -1;
	}

	int serialnumber = *(int *)((char *)INDEXENT2(index)->pvPrivateData + offset + 4);

	if (pEdict->serialnumber != serialnumber)
	{
		return 0;
	}

	return ent;
}

static cell AMX_NATIVE_CALL set_pdata_ehandle(AMX *amx, cell *params)
{
	int index  = params[1];
	CHECK_ENTITY(index);

	int offset = params[2];
	CHECK_OFFSET(offset);

	int entity = params[3];
	CHECK_ENTITY(entity);

#if defined(__linux__)
	offset += params[4];
#elif defined(__APPLE__)
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		offset += params[4];
	else
		offset += params[5];
#endif

	edict_t *pEntity = INDEXENT2(entity);

	*(edict_t **)((char* )(INDEXENT2(index)->pvPrivateData) + offset) = pEntity;

	if (pEntity)
	{
		*(int *)((char *)INDEXENT2(index)->pvPrivateData + offset + 4) = pEntity->serialnumber;
	}

	return 1;
}

AMX_NATIVE_INFO pdata_natives[] =
{
	{ "get_pdata_int",		get_pdata_int },
	{ "set_pdata_int",		set_pdata_int },
	{ "get_pdata_float",	get_pdata_float },
	{ "set_pdata_float",	set_pdata_float },
	{ "set_pdata_string",	set_pdata_string },
	{ "get_pdata_string",	get_pdata_string },
	{ "get_pdata_ent",		get_pdata_ent },
	{ "set_pdata_ent",		set_pdata_ent },
	{ "get_pdata_bool",		get_pdata_bool },
	{ "set_pdata_bool",		set_pdata_bool },
	{ "get_pdata_byte",		get_pdata_byte },
	{ "set_pdata_byte",		set_pdata_byte },
	{ "get_pdata_short",	get_pdata_short },
	{ "set_pdata_short",	set_pdata_short },
	{ "get_pdata_vector",	get_pdata_vector },
	{ "set_pdata_vector",	set_pdata_vector },
	{ "get_pdata_ehandle",	get_pdata_ehandle },
	{ "set_pdata_ehandle",	set_pdata_ehandle },
	{ NULL,					NULL }
};
