#include "CBaseList.h"
#include "CArray.h"
#include "ComboArray.h"

#define KEY_TYPE cell
#define DYNAMIC_UNIT_TYPE CBaseList
#define STORAGE_TYPE Capsule
#define MASTER_NAME MasterList
#define EXPORT_NAME list_exports

#define SEARCH_ERROR_OFFSET 0

#define GET_KEY(params, num) params[num]
#define SET_KEY(stuff, parameter) stuff

#include "ListNativeFunctions.h"
#include "NativeIncludes.h"

static cell AMX_NATIVE_CALL array_create(AMX *amx,cell *params)
{
	DTYPE* Unit;
	M_ITYPE Index = params[1];

	JUDY_CREATE_INDEX(MNAME,Unit,Array,Index);
	return Index;
}

static cell AMX_NATIVE_CALL comboarray_create(AMX *amx,cell *params)
{
	DTYPE* Unit;
	M_ITYPE Index = params[1];

	JUDY_CREATE_INDEX(MNAME,Unit,ComboArray,Index);
	return Index;
}

AMX_NATIVE_INFO list_creation_exports[] = 
{
	{ "array_create", array_create },
	{ "comboarray_create", comboarray_create },

	{ NULL, NULL }
};