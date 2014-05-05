#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "amxmodx.h"
#include "sm_trie_tpl.h"
#include "trie_natives.h"

using namespace SourceMod;

TrieHandles<CellTrie> g_TrieHandles;
TrieHandles<TrieSnapshot> g_TrieSnapshotHandles;
TrieHandles<CellTrieIter> g_TrieIterHandles;

// native Trie:TrieCreate();
static cell AMX_NATIVE_CALL TrieCreate(AMX *amx, cell *params)
{
	return static_cast<cell>(g_TrieHandles.create());
}

// native Trie::TrieClear(Trie:handle);
static cell AMX_NATIVE_CALL TrieClear(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}
	t->map.clear();
	return 1;
}

// native TrieSetCell(Trie:handle, const key[], any:value, bool:replace = true);
static cell AMX_NATIVE_CALL TrieSetCell(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	StringHashMap<Entry>::Insert i = t->map.findForAdd(key);
	if (!i.found())
	{
		if (!t->map.add(i, key))
		{
			return 0;
		}

		i->value.setCell(params[3]);
		return 1;
	}

	// Old plugin doesn't have 'replace' parameter.
	if (*params / sizeof(cell) == 4 && !params[4])
	{
		return 0;
	}

	i->value.setCell(params[3]);
	return 1;
}

// native TrieSetString(Trie:handle, const key[], const data[], bool:replace = true);
static cell AMX_NATIVE_CALL TrieSetString(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	const char *value = get_amxstring(amx, params[3], 1, len);

	StringHashMap<Entry>::Insert i = t->map.findForAdd(key);
	if (!i.found())
	{
		if (!t->map.add(i, key))
		{
			return 0;
		}

		i->value.setString(value);
		return 1;
	}

	// Old plugin doesn't have 'replace' parameter.
	if (*params / sizeof(cell) == 4 && !params[4])
	{
		return 0;
	}

	i->value.setString(value);
	return 1;
}

// native TrieSetArray(Trie:handle, const key[], const any:buffer[], buffsize, bool:replace = true)
static cell AMX_NATIVE_CALL TrieSetArray(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	if (params[4] < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array size (%d)", params[4]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	cell *ptr = get_amxaddr(amx, params[3]);

	StringHashMap<Entry>::Insert i = t->map.findForAdd(key);
	if (!i.found())
	{
		if (!t->map.add(i, key))
		{
			return 0;
		}

		i->key = key;
		i->value.setArray(ptr, params[4]);
		return 1;
	}

	// Old plugin doesn't have 'replace' parameter.
	if (*params / sizeof(cell) == 4 && !params[5])
	{
		return 0;

	}
	i->value.setArray(ptr, params[4]);
	return 1;
}

// native bool:TrieGetCell(Trie:handle, const key[], &any:value);
static cell AMX_NATIVE_CALL TrieGetCell(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	StringHashMap<Entry>::Result r = t->map.find(key);

	if (!r.found())
	{
		return 0;
	}

	cell *ptr = get_amxaddr(amx, params[3]);

	if (r->value.isCell())
	{
		*ptr = r->value.cell_();
		return 1;
	}

	return 0;
}

// native bool:TrieGetString(Trie:handle, const key[], buff[], len, &size = 0);
static cell AMX_NATIVE_CALL TrieGetString(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	if (params[4] < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid buffer size (%d)", params[4]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	cell *pSize = get_amxaddr(amx, params[5]);

	StringHashMap<Entry>::Result r = t->map.find(key);
	if (!r.found() || !r->value.isString())
	{
		return 0;
	}

	*pSize = (cell)set_amxstring_utf8(amx, params[3], r->value.chars(), strlen(r->value.chars()), params[4] + 1); // + EOS

	return 1;
}

// native bool:TrieGetArray(Trie:handle, const key[], any:buff[], len, &size = 0);
static cell AMX_NATIVE_CALL TrieGetArray(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	if (params[4] < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array size (%d)", params[4]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	cell *pValue = get_amxaddr(amx, params[3]);
	cell *pSize = get_amxaddr(amx, params[5]);

	StringHashMap<Entry>::Result r = t->map.find(key);
	if (!r.found() || !r->value.isArray())
	{
		return 0;
	}

	if (!r->value.array())
	{
		*pSize = 0;
		return 1;
	}

	if (!params[4])
	{
		return 0;
	}

	size_t length = r->value.arrayLength();
	cell *base = r->value.array();

	if (length > size_t(params[4]))
		*pSize = params[4];
	else
		*pSize = length;

	memcpy(pValue, base, sizeof(cell) * pSize[0]);
	return 1;
}

// native bool:TrieKeyExists(Trie:handle, const key[]);
static cell AMX_NATIVE_CALL TrieKeyExists(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	return static_cast<cell>(t->map.contains(key));
}

// native bool:TrieDeleteKey(Trie:handle, const key[]);
static cell AMX_NATIVE_CALL TrieDeleteKey(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}


	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);

	StringHashMap<Entry>::Result r = t->map.find(key);
	if (!r.found())
	{
		return 0;
	}

	t->map.remove(r);

	return 1;
}

//native TrieDestroy(&Trie:handle)
static cell AMX_NATIVE_CALL TrieDestroy(AMX *amx, cell *params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	CellTrie *t = g_TrieHandles.lookup(*ptr);

	if (t == NULL)
	{
		return 0;
	}

	CellTrieIter *i;
	for (size_t index = 1; index <= g_TrieIterHandles.size(); index++)
	{
		if ((i = g_TrieIterHandles.lookup(index)) != NULL)
		{
			if (i->trie == t)
				i->trie = NULL;
		}
	}

	if (g_TrieHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}

	return 0;
}

// native TrieGetSize(Trie:handle);
static cell AMX_NATIVE_CALL TrieGetSize(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	return t->map.elements();
}

static cell AMX_NATIVE_CALL TrieSnapshotCreate(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int index = g_TrieSnapshotHandles.create();
	TrieSnapshot *snapshot = g_TrieSnapshotHandles.lookup(index);
	snapshot->length = t->map.elements();
	snapshot->keys = new int[snapshot->length];

	size_t i = 0;
	for (StringHashMap<Entry>::iterator iter = t->map.iter(); !iter.empty(); iter.next(), i++)
	{
		snapshot->keys[i] = snapshot->strings.AddString(iter->key.chars(), iter->key.length());
	}
	assert(i == snapshot->length);

	return static_cast<cell>(index);
}

static cell AMX_NATIVE_CALL TrieSnapshotLength(AMX *amx, cell *params)
{
	TrieSnapshot *snapshot = g_TrieSnapshotHandles.lookup(params[1]);

	if (snapshot == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid snapshot handle provided (%d)", params[1]);
		return 0;
	}

	return snapshot->length;
}

static cell AMX_NATIVE_CALL TrieSnapshotKeyBufferSize(AMX *amx, cell *params)
{
	TrieSnapshot *snapshot = g_TrieSnapshotHandles.lookup(params[1]);

	if (snapshot == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid snapshot handle provided (%d)", params[1]);
		return 0;
	}

	unsigned index = params[2];

	if (index >= snapshot->length)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d", index);
		return 0;
	}

	return strlen(snapshot->strings.GetString(snapshot->keys[index])) + 1;
}

static cell AMX_NATIVE_CALL TrieSnapshotGetKey(AMX *amx, cell *params)
{
	TrieSnapshot *snapshot = g_TrieSnapshotHandles.lookup(params[1]);

	if (snapshot == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid snapshot handle provided (%d)", params[1]);
		return 0;
	}

	unsigned index = params[2];

	if (index >= snapshot->length)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d", index);
		return 0;
	}

	const char *str = snapshot->strings.GetString(snapshot->keys[index]);
	return set_amxstring_utf8(amx, params[3], str, strlen(str), params[4] + 1);
}

//native TrieSnapshotDestroy(&Snapshot:handle)
static cell AMX_NATIVE_CALL TrieSnapshotDestroy(AMX *amx, cell *params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	TrieSnapshot *t = g_TrieSnapshotHandles.lookup(*ptr);

	if (t == NULL)
	{
		return 0;
	}

	if (g_TrieSnapshotHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}

	return 0;
}

// native TrieIter:TrieIterCreate(Trie:handle)
static cell AMX_NATIVE_CALL TrieIterCreate(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int index = g_TrieIterHandles.create();
	CellTrieIter *i = g_TrieIterHandles.lookup(index);
	i->trie = t;
	i->iter = t->map.iter_();

	return static_cast<cell>(index);
}

// native TrieIterNext(TrieIter:handle, key[] = "", outputsize = 0, &size = 0)
static cell AMX_NATIVE_CALL TrieIterNext(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return 0;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	if (!i->iter->valid())
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) is outdated", params[1]);
		return false;
	}

	if (i->iter->empty())
		return false;

	i->iter->next();

	if (i->iter->empty())
		return false;

	if (params[3] <= 0)
		return true;

	cell *pSize = get_amxaddr(amx, params[4]);

	*pSize = (cell)set_amxstring_utf8(amx, params[2], (*i->iter)->key.chars(), (*i->iter)->key.length(), params[3] + 1);

	return true;
}

// native TrieIterGetKey(TrieIter:handle, key[] = "", outputsize = 0, &size = 0)
static cell AMX_NATIVE_CALL TrieIterGetKey(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return false;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	if (!i->iter->valid())
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) is outdated", params[1]);
		return false;
	}

	if (params[3] <= 0)
		return i->iter->empty();

	cell *pSize = get_amxaddr(amx, params[4]);

	if (i->iter->empty())
		return false;

	*pSize = set_amxstring_utf8(amx, params[2], (*i->iter)->key.chars(), (*i->iter)->key.length(), params[3] + 1);
	
	return true;
}

// native TrieIterStatus:TrieIterGetStatus(TrieIter:handle)
static cell AMX_NATIVE_CALL TrieIterGetStatus(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return IterStatus_Closed;
	}

	if (i->trie == NULL)
		return IterStatus_Closed;

	if (!i->iter->valid())
		return IterStatus_Outdated;

	return IterStatus_Valid;
}

// native bool:TrieIterRefresh(TrieIter:handle)
static cell AMX_NATIVE_CALL TrieIterRefresh(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return false;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	i->iter->refresh();
	return true;
}

// native bool:TrieIterGetCell(TrieIter:handle, &any:value)
static cell AMX_NATIVE_CALL TrieIterGetCell(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return false;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	if (!i->iter->valid())
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) is outdated", params[1]);
		return false;
	}

	if (i->iter->empty())
		return false;

	cell *ptr = get_amxaddr(amx, params[2]);

	if ((*i->iter)->value.isCell())
	{
		*ptr = (*i->iter)->value.cell_();
		return true;
	}

	return false;
}

// native bool:TrieIterGetString(TrieIter:handle, buffer[], outputsize, &size = 0)
static cell AMX_NATIVE_CALL TrieIterGetString(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return false;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	if (!i->iter->valid())
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) is outdated", params[1]);
		return false;
	}

	if (i->iter->empty())
		return false;

	cell *pSize = get_amxaddr(amx, params[4]);

	if ((*i->iter)->value.isString())
	{
		*pSize = (cell) set_amxstring_utf8(amx, params[2], (*i->iter)->value.chars(), strlen((*i->iter)->value.chars()), params[3] + 1);
	}

	return false;
}

// native bool:TrieIterGetArray(TrieIter:handle, array[], outputsize, &size = 0)
static cell AMX_NATIVE_CALL TrieIterGetArray(AMX *amx, cell *params)
{
	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map iterator handle provided (%d)", params[1]);
		return false;
	}

	if (i->trie == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) has been closed", params[1]);
		return false;
	}

	if (!i->iter->valid())
	{
		LogError(amx, AMX_ERR_NATIVE, "Underlying map to iterator handle (%d) is outdated", params[1]);
		return false;
	}

	if (params[3] < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array size (%d)", params[4]);
		return false;
	}

	if (i->iter->empty())
		return false;

	cell *pValue = get_amxaddr(amx, params[2]);
	cell *pSize = get_amxaddr(amx, params[4]);

	if (!(*i->iter)->value.isArray() || !params[3])
		return false;

	if (!(*i->iter)->value.array())
	{
		*pSize = 0;
		return true;
	}

	size_t length = (*i->iter)->value.arrayLength();
	cell *base = (*i->iter)->value.array();

	if (length > size_t(params[3]))
		*pSize = params[3];
	else
		*pSize = length;

	memcpy(pValue, base, sizeof(cell) * pSize[0]);
	return true;
}

// native TrieIterDestroy(&TrieIter:handle)
static cell AMX_NATIVE_CALL TrieIterDestroy(AMX *amx, cell *params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	CellTrieIter *i = g_TrieIterHandles.lookup(params[1]);

	if (i == NULL)
	{
		return false;
	}
	
	delete i->iter;
	i->iter = NULL;
	i->trie = NULL;

	if (g_TrieIterHandles.destroy(*ptr))
	{
		*ptr = 0;
		return true;
	}

	return false;
}

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
	{ "TrieGetSize",	TrieGetSize },

	{ "TrieSnapshotCreate",			TrieSnapshotCreate },
	{ "TrieSnapshotLength",			TrieSnapshotLength },
	{ "TrieSnapshotKeyBufferSize",	TrieSnapshotKeyBufferSize },
	{ "TrieSnapshotGetKey",			TrieSnapshotGetKey },
	{ "TrieSnapshotDestroy",		TrieSnapshotDestroy },

	{ "TrieIterCreate", TrieIterCreate },
	{ "TrieIterNext", TrieIterNext },
	{ "TrieIterGetKey", TrieIterGetKey },
	{ "TrieIterGetStatus", TrieIterGetStatus },
	{ "TrieIterRefresh", TrieIterRefresh },
	{ "TrieIterGetCell", TrieIterGetCell },
	{ "TrieIterGetString", TrieIterGetString },
	{ "TrieIterGetArray", TrieIterGetArray },
	{ "TrieIterDestroy", TrieIterDestroy },

	{ NULL,			NULL }
};

