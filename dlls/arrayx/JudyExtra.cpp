#include "JudyExtra.h"
#include "CBinTrie.h"

bool JudyClearMasterTrie(CBaseList* master)
{
	cell master_iten = NULL;
	BinTrie* Storage = NULL;

	try { master_iten = master->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		try { Storage = reinterpret_cast<BinTrie*>(master->Get(master_iten) );  }
		catch(JudyEx& e) { break; }

		Storage->Remove();

		try { master->Delete(master_iten); }
		catch(JudyEx& e) { return false; }
		
		try { master_iten = master->Next(master_iten); }
		catch(JudyEx& e) { break; }
	}

	return true;
};

bool JudyClearBinTrie(BinTrie* trie)
{
	Word_t trie_iten = NULL;

	try { trie_iten = trie->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		trie->Delete(trie_iten);
		
		try { trie_iten = trie->Next(trie_iten); }
		catch(JudyEx& e) { break; }
	}
	return true;
};

bool JudySaveBinTrie(BinTrie* trie, char* file)
{
	Word_t trie_iten = NULL;

	try { trie_iten = trie->First(); }
	catch(JudyEx e) { return false; }

	FILE *trieDB = fopen(file,"wb");
	if (!trieDB) return false;

	bool value = false;

	while( true )
	{
		fwrite(&trie_iten, sizeof(Word_t), 1, trieDB);

		value = (trie->Get(trie_iten) != NULL);
		fwrite(&value, sizeof(bool), 1, trieDB);

		try { trie_iten = trie->Next(trie_iten); }
		catch(JudyEx& e) { break; }
	}

	fclose(trieDB);
	return true;
};

bool JudyLoadBinTrie(BinTrie* trie, char* file)
{
	FILE *trieDB = fopen(file, "ab+");
	if (!trieDB) return false;

	Word_t trie_iten = NULL;

	bool value = false;

	while(!feof(trieDB))
	{
		fread(&trie_iten, sizeof(Word_t), 1, trieDB);
		if (feof(trieDB) || ferror(trieDB)) break;

		fread(&value, sizeof(bool), 1, trieDB);

		trie->Set(trie_iten,value);
	}

	fclose(trieDB);
	return true;
}

bool JudyClearMasterList(CBaseList* master)
{
	cell master_iten = NULL;
	CBaseList* Storage = NULL;

	try { master_iten = master->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		try { Storage = reinterpret_cast<CBaseList*>(master->Get(master_iten) );  }
		catch(JudyEx& e) { break; }

		Storage->Remove();

		try { master->Delete(master_iten); }
		catch(JudyEx& e) { return false; }
		
		try { master_iten = master->Next(master_iten); }
		catch(JudyEx& e) { break; }
	}

	return true;
};

bool JudyClearList(CBaseList* list)
{
	Word_t list_iten = NULL;
	Capsule* Storage = NULL;

	try { list_iten = list->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		try { Storage = reinterpret_cast<Capsule*>(list->Get(list_iten) ); }
		catch(JudyEx& e) { break; }

		Storage->Remove();

		try { list->Delete(list_iten); }
		catch(JudyEx& e) { break; }
		
		try { list_iten = list->Next(list_iten); }
		catch(JudyEx& e) { break; }
	}
	return true;
};

bool JudySaveList(CBaseList* list, char* file)
{
	Capsule* Storage = NULL;
	Word_t list_iten = NULL;
	bool no_error = true;

	try { list_iten = list->First(); }
	catch(JudyEx e) { return false; }

	FILE* listDB = fopen(file,"wb");
	if (!listDB) return false;

	while( true )
	{
		try { Storage = reinterpret_cast<Capsule*>(list->Get(list_iten) ); }
		catch(JudyEx& e) { no_error = false; break; }

		fwrite(&list_iten, sizeof(Word_t), 1, listDB);
		try { Storage->Save(listDB); }
		catch(JudyEx& e) { no_error = false; break; }

		try { list_iten = list->Next(list_iten); }
		catch(JudyEx& e) { break; }
	}

	fclose(listDB);
	return no_error;
};

bool JudyLoadList(CBaseList* list, char* file)
{
	FILE *listDB = fopen(file, "ab+");
	if (!listDB) return false;

	Capsule* Storage = NULL;
	Word_t list_iten = NULL;
	bool no_error = true;

	while(!feof(listDB))
	{
		fread(&list_iten, sizeof(Word_t), 1, listDB);
		if (feof(listDB) || ferror(listDB)) break;
		
		Storage = new Capsule;
		try { Storage->Load(listDB); }
		catch(JudyEx& e) { no_error = false; break; }

		list->Set(list_iten,Storage);
	}

	fclose(listDB);
	return no_error;
}

bool JudyClearMasterMap(CBaseList* master)
{
	cell master_iten = NULL;
	CBaseMap* Storage = NULL;

	try { master_iten = master->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		try { Storage = reinterpret_cast<CBaseMap*>(master->Get(master_iten) );  }
		catch(JudyEx& e) { break; }

		Storage->Remove();

		try { master->Delete(master_iten); }
		catch(JudyEx& e) { return false; }
		
		try { master_iten = master->Next(master_iten); }
		catch(JudyEx& e) { break; }
	}

	return true;
};

bool JudyClearMap(CBaseMap* map)
{
	char* map_iten = NULL;
	Capsule* Storage = NULL;

	try { map_iten = map->First(); }
	catch(JudyEx& e) { return false; }

	while( true )
	{
		try { Storage = reinterpret_cast<Capsule*>(map->Get(map_iten) );  }
		catch(JudyEx& e) { break; }

		Storage->Remove();

		try { map->Delete(map_iten); }
		catch(JudyEx& e) { return false; }
		
		try { map_iten = map->Next(map_iten); }
		catch(JudyEx& e) { break; }
	}

	return true;
};


bool JudySaveMap(CBaseMap* map, char* file)
{
	Capsule* Storage = NULL;
	char* map_iten = NULL;
	size_t key_len = NULL;
	bool no_error = true;

	try { map_iten = map->First(); }
	catch(JudyEx& e) { return false; }

	FILE *mapDB = fopen(file,"wb");
	if (!mapDB) return false;

	while( true )
	{
		try { Storage = reinterpret_cast<Capsule*>(map->Get(map_iten) ); }
		catch(JudyEx& e) { return false; }

		key_len = strlen(map_iten);

		fwrite(&key_len, sizeof(size_t), 1, mapDB);
		fwrite(&map_iten, sizeof(char), strlen(map_iten), mapDB);

		try { Storage->Save(mapDB); }
		catch(JudyEx& e) { no_error = false; break; }

		try { map_iten = map->Next(map_iten); }
		catch(JudyEx& e) { break; }
	}

	fclose(mapDB);
	return no_error;
};

bool JudyLoadMap(CBaseMap* map, char* file)
{
	FILE *mapDB = fopen(file, "ab+");
	if (!mapDB) return false;

	Capsule* Storage = NULL;
	char* map_iten = NULL;
	size_t key_len = NULL;
	bool no_error = true;

	while(!feof(mapDB))
	{
		fread(&key_len,sizeof(size_t),1,mapDB);

		map_iten = new char[key_len+1];
		fgets(map_iten, key_len+1, mapDB);

		if (feof(mapDB) || ferror(mapDB)) break;

		Storage = new Capsule;
		try { Storage->Load(mapDB); }
		catch(JudyEx& e) { no_error = false; break; }

		map->Set(map_iten,Storage);
		delete map_iten;
	}

	fclose(mapDB);
	return no_error;
}