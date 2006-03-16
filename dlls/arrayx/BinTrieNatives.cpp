#include "CBinTrie.h"

#define KEY_TYPE cell
#define DYNAMIC_UNIT_TYPE BinTrie
#define STORAGE_TYPE cell
#define MASTER_NAME MasterTrie
#define EXPORT_NAME bintrie_exports

#define SEARCH_ERROR_OFFSET 0

#define GET_KEY(params, num) params[num]
#define SET_KEY(stuff, parameter) stuff

#include "BinTrieNativeFunctions.h"
#include "NativeIncludes.h"

static cell AMX_NATIVE_CALL bintrie_create(AMX *amx,cell *params)
{
	DTYPE* Unit;
	M_ITYPE Index = params[1];

	JUDY_CREATE_INDEX(MNAME,Unit,BinTrie,Index);
	return Index;
}

static cell AMX_NATIVE_CALL bintrie_set(AMX *amx,cell *params)
{
	DTYPE* Unit = NULL;

	JUDY_GET_INDEX(MNAME,Unit, params[1]);
	ITYPE Indice = JUDY_GET_KEY(params,2);
	bool Value = (params[3] != NULL);
		
	try { return Unit->Set(Indice, Value ); }
	JUDY_ERROR_CATCH("Judy Error: (No error possible) - Slave Set Function ");
}

static cell AMX_NATIVE_CALL bintrie_get(AMX *amx,cell *params)
{
	DTYPE* Unit = NULL;

	JUDY_GET_INDEX(MNAME,Unit, params[1]);
	ITYPE Indice = JUDY_GET_KEY(params,2);
		
	try { return Unit->Get(Indice ); }
	JUDY_ERROR_CATCH("Judy Error: (No error possible) - Slave Get Function ");
}

static cell AMX_NATIVE_CALL bintrie_remove(AMX *amx,cell *params)
{
	DTYPE* Unit = NULL;

	JUDY_GET_INDEX(MNAME,Unit, params[1]);
	ITYPE Indice = JUDY_GET_KEY(params,2);
		
	try { return Unit->Delete(Indice ); }
	JUDY_ERROR_CATCH("Judy Error: (No error possible) - Slave Delete Function ");
}


AMX_NATIVE_INFO bintrie_usage_exports[] = 
{
	{ "bintrie_create", bintrie_create },
	{ "bintrie_set", bintrie_set },
	{ "bintrie_get", bintrie_get },
	{ "bintrie_remove", bintrie_remove },

	{ NULL, NULL }
};