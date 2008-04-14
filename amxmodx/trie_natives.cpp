#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "amxmodx.h"
#include "sm_trie_tpl.h"
#include "trie_natives.h"

#ifndef NDEBUG
size_t trie_free_count = 0;
size_t trie_malloc_count = 0;
#endif

TrieHandles g_TrieHandles;
typedef KTrie<TrieData> celltrie;

void triedata_dtor(TrieData *ptr)
{
	ptr->freeCells();
}
// native Trie:TrieCreate();
static cell AMX_NATIVE_CALL TrieCreate(AMX *amx, cell *params)
{
	return static_cast<cell>(g_TrieHandles.create());
}

// native Trie::TrieClear(Trie:handle);
static cell AMX_NATIVE_CALL TrieClear(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}
	t->run_destructor(triedata_dtor);
	t->clear();
	return 1;
}
// native TrieSetCell(Trie:handle, const key[], any:value);
static cell AMX_NATIVE_CALL TrieSetCell(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}

	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		TrieData dummy;
		t->insert(key, dummy);

		td = t->retrieve(key);

		// should never, ever happen
		if (td == NULL)
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't KTrie::retrieve(), handle: %d", params[1]);
			return 0;
		}
	}
	
	td->setCell(params[3]);
	
	return 1;
}
// native TrieSetString(Trie:handle, const key[], const data[]);
static cell AMX_NATIVE_CALL TrieSetString(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}

	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		TrieData dummy;
		t->insert(key, dummy);
		td = t->retrieve(key);

		// should never, ever happen
		if (td == NULL)
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't KTrie::retrieve(), handle: %d", params[1]);
			return 0;
		}

	}
	
	td->setString(get_amxaddr(amx, params[3]));
	return 1;
}
// native TrieSetArray(Trie:handle, const key[], const any:buffer[], buffsize)
static cell AMX_NATIVE_CALL TrieSetArray(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[2]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}

	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		TrieData dummy;
		t->insert(key, dummy);
		td = t->retrieve(key);

		// should never, ever happen
		if (td == NULL)
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't KTrie::retrieve(), handle: %d", params[1]);
			return 0;
		}

	}

	td->setArray(get_amxaddr(amx, params[3]), params[4]);

	return 1;
}
// native bool:TrieGetCell(Trie:handle, const key[], &any:value);
static cell AMX_NATIVE_CALL TrieGetCell(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}


	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		return 0;
	}
	cell *ptr = get_amxaddr(amx, params[3]);
	if (!td->getCell(ptr))
	{
		return 0;
	}
	return 1;
}
// native bool:TrieGetString(Trie:handle, const key[], buff[], len);
static cell AMX_NATIVE_CALL TrieGetString(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}


	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		return 0;
	}
	cell *ptr = get_amxaddr(amx, params[3]);
	if (!td->getString(ptr, params[4]))
	{
		return 0;
	}
	return 1;
}
// native bool:TrieGetArray(Trie:handle, const key[], any:buff[], len);
static cell AMX_NATIVE_CALL TrieGetArray(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}


	TrieData *td = NULL;
	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	if ((td = t->retrieve(key)) == NULL)
	{
		return 0;
	}
	cell *ptr = get_amxaddr(amx, params[3]);
	if (!td->getArray(ptr, params[4]))
	{
		return 0;
	}
	return 1;
}
// native bool:TrieKeyExists(Trie:handle, const key[]);
static cell AMX_NATIVE_CALL TrieKeyExists(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}


	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	return t->retrieve(key) != NULL ? 1 : 0;
}

// native bool:TrieDeleteKey(Trie:handle, const key[]);
static cell AMX_NATIVE_CALL TrieDeleteKey(AMX *amx, cell *params)
{
	celltrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid trie handle provided (%d)", params[1]);
		return 0;
	}


	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	TrieData *td = t->retrieve(key);

	if (td != NULL)
	{
		td->freeCells();
	}
	return t->remove(key) ? 1 : 0;
}
//native TrieDestroy(&Trie:handle)
static cell AMX_NATIVE_CALL TrieDestroy(AMX *amx, cell *params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	celltrie *t = g_TrieHandles.lookup(*ptr);

	if (t == NULL)
	{
		return 0;
	}
	t->run_destructor(triedata_dtor);
	if (g_TrieHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}
	return 0;

}
#ifndef NDEBUG
static cell AMX_NATIVE_CALL TrieMallocCount(AMX *amx, cell *params)
{
	return trie_malloc_count;
}
static cell AMX_NATIVE_CALL TrieFreeCount(AMX *amx, cell *params)
{
	return trie_free_count;
}
#endif
AMX_NATIVE_INFO trie_Natives[] =
{
	{ "TrieCreate",		TrieCreate },
	{ "TrieClear",		TrieClear },

	{ "TrieSetCell",	TrieSetCell },
	{ "TrieSetString",	TrieSetString },
	{ "TrieSetArray",	TrieSetArray },

	{ "TrieGetCell",	TrieGetCell },
	{ "TrieGetString",	TrieGetString },
	{ "TrieGetArray",	TrieGetArray },

	{ "TrieDeleteKey",	TrieDeleteKey },
	{ "TrieKeyExists",	TrieKeyExists },
	{ "TrieDestroy",	TrieDestroy },

#ifndef NDEBUG
	{ "TrieMallocCount",	TrieMallocCount },
	{ "TrieFreeCount",	TrieFreeCount },
#endif

	{ NULL,			NULL }
};

