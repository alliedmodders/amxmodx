#include "fakemeta_amxx.h"

static cell AMX_NATIVE_CALL set_pdata_int(AMX *amx, cell *params)
{
	int index=params[1];
	CHECK_ENTITY(index);
	int iOffset=params[2];
	if (iOffset <0)
		return 1;
#ifdef __linux__
	iOffset += params[4];
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
#endif
	return amx_ftoc(*((float *)INDEXENT2(index)->pvPrivateData + iOffset));
}
AMX_NATIVE_INFO pdata_natives[] = {
	{ "get_pdata_int",		get_pdata_int },
	{ "set_pdata_int",		set_pdata_int },
	{ "get_pdata_float",	get_pdata_float },
	{ "set_pdata_float",	set_pdata_float },
	{ NULL,					NULL }
};