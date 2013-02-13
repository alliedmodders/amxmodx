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
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[4];
#elif defined __APPLE__
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
	if (iOffset <0)
		return 0;
#ifdef __linux__
	iOffset += params[3];
#elif defined __APPLE__
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
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[4];
#elif defined __APPLE__
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
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[3];
#elif defined __APPLE__
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
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[6];
#elif defined __APPLE__
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
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[5];
#elif defined __APPLE__
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
	int iOffset=params[2];

	CHECK_ENTITY(index);

#ifdef __linux__
	iOffset += params[3];
#elif defined __APPLE__
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

AMX_NATIVE_INFO pdata_natives[] =
{
	{ "get_pdata_int",		get_pdata_int },
	{ "set_pdata_int",		set_pdata_int },
	{ "get_pdata_float",	get_pdata_float },
	{ "set_pdata_float",	set_pdata_float },
	{ "set_pdata_string",	set_pdata_string },
	{ "get_pdata_string",	get_pdata_string },
	{ "get_pdata_ent",		get_pdata_ent },
	{ NULL,					NULL }
};
