#include "CBaseMap.h"
#include "CKeytable.h"
#include "ComboTable.h"
#include "CHashtable.h"

#define KEY_TYPE char*
#define DYNAMIC_UNIT_TYPE CBaseMap
#define STORAGE_TYPE Capsule
#define MASTER_NAME MasterMap
#define EXPORT_NAME map_exports

#define SEARCH_ERROR_OFFSET 2

#define GET_KEY(params, num) MF_GetAmxString(amx,params[num],num,NULL)
#define SET_KEY(str, num) MF_SetAmxString(amx,params[num],str,params[num + 1])

#include "MapNativeFunctions.h"
#include "NativeIncludes.h"

static cell AMX_NATIVE_CALL keytable_create(AMX *amx,cell *params)
{
	DTYPE* Unit = NULL;
	M_ITYPE Index = NULL;

	JUDY_CREATE_INDEX(MNAME,Unit,Keytable,Index);
	return Index;
}

static cell AMX_NATIVE_CALL combotable_create(AMX *amx,cell *params)
{
	DTYPE* Unit = NULL;
	M_ITYPE Index = NULL;

	JUDY_CREATE_INDEX(MNAME,Unit,ComboTable,Index);
	return Index;
}

static cell AMX_NATIVE_CALL hashtable_create(AMX *amx,cell *params)
{
	DTYPE* Unit;
	M_ITYPE Index = params[1];

	JUDY_CREATE_INDEX(MNAME,Unit,Hashtable,Index);
	return Index;
}

AMX_NATIVE_INFO map_creation_exports[] = 
{
	{ "keytable_create", keytable_create },
	{ "combotable_create", combotable_create },
	{ "hashtable_create", hashtable_create },

	{ NULL, NULL }
};