#include "amxxmodule.h"
#include "ComboArray.h"

extern AMX_NATIVE_INFO bintrie_exports[];
extern AMX_NATIVE_INFO bintrie_usage_exports[];
extern ComboArray MasterTrie;

extern AMX_NATIVE_INFO list_exports[];
extern AMX_NATIVE_INFO list_creation_exports[];
extern ComboArray MasterList;

extern AMX_NATIVE_INFO map_exports[];
extern AMX_NATIVE_INFO map_creation_exports[];
extern ComboArray MasterMap;


void OnAmxxAttach( void )
{
	MF_AddNatives(bintrie_exports);
	MF_AddNatives(bintrie_usage_exports);

	MF_AddNatives(list_exports);
	MF_AddNatives(list_creation_exports);

	MF_AddNatives(map_exports);
	MF_AddNatives(map_creation_exports);
}

void OnAmxxDetach( void )
{
	JudyClearMasterTrie(&MasterTrie);
	JudyClearMasterList(&MasterList);
	JudyClearMasterMap(&MasterMap);
}