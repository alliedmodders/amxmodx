#ifndef _JUDYARRAY_SHARED_INCLUDED
#define _JUDYARRAY_SHARED_INCLUDED

#include "CBaseList.h"
#include "CBaseMap.h"

class BinTrie;

extern bool JudyClearMasterTrie(CBaseList* master);
extern bool JudyClearBinTrie(BinTrie* trie);
extern bool JudySaveBinTrie(BinTrie* trie, char* file);
extern bool JudyLoadBinTrie(BinTrie* trie, char* file);

extern bool JudyClearMasterList(CBaseList* master);
extern bool JudyClearList(CBaseList* list);
extern bool JudySaveList(CBaseList* list, char* file);
extern bool JudyLoadList(CBaseList* list, char* file);

extern bool JudyClearMasterMap(CBaseList* master);
extern bool JudyClearMap(CBaseMap* map);
extern bool JudySaveMap(CBaseMap* array, char* file);
extern bool JudyLoadMap(CBaseMap* array, char* file);

#endif