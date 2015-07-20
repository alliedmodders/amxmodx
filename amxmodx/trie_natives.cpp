// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "trie_natives.h"

NativeHandle<CellTrie> TrieHandles;
NativeHandle<TrieSnapshot> TrieSnapshotHandles;

// native Trie:TrieCreate();
static cell AMX_NATIVE_CALL TrieCreate(AMX *amx, cell *params)
{
	return static_cast<cell>(TrieHandles.create());
}

// native Trie::TrieClear(Trie:handle);
static cell AMX_NATIVE_CALL TrieClear(AMX *amx, cell *params)
{
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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

	*pSize = (cell)set_amxstring_utf8(amx, params[3], r->value.chars(), strlen(r->value.chars()), params[4]);

	return 1;
}

// native bool:TrieGetArray(Trie:handle, const key[], any:buff[], len, &size = 0);
static cell AMX_NATIVE_CALL TrieGetArray(AMX *amx, cell *params)
{
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
		return 1;
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
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

	CellTrie *t = TrieHandles.lookup(*ptr);

	if (!t)
	{
		return 0;
	}

	if (TrieHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}

	return 0;
}

// native TrieGetSize(Trie:handle);
static cell AMX_NATIVE_CALL TrieGetSize(AMX *amx, cell *params)
{
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	return t->map.elements();
}

static cell AMX_NATIVE_CALL TrieSnapshotCreate(AMX *amx, cell *params)
{
	CellTrie *t = TrieHandles.lookup(params[1]);

	if (!t)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int index = TrieSnapshotHandles.create();
	TrieSnapshot *snapshot = TrieSnapshotHandles.lookup(index);
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
	TrieSnapshot *snapshot = TrieSnapshotHandles.lookup(params[1]);

	if (!snapshot)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid snapshot handle provided (%d)", params[1]);
		return 0;
	}

	return snapshot->length;
}

static cell AMX_NATIVE_CALL TrieSnapshotKeyBufferSize(AMX *amx, cell *params)
{
	TrieSnapshot *snapshot = TrieSnapshotHandles.lookup(params[1]);

	if (!snapshot)
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
	TrieSnapshot *snapshot = TrieSnapshotHandles.lookup(params[1]);

	if (!snapshot)
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
	return set_amxstring_utf8(amx, params[3], str, strlen(str), params[4]);
}

//native TrieSnapshotDestroy(&Snapshot:handle)
static cell AMX_NATIVE_CALL TrieSnapshotDestroy(AMX *amx, cell *params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	TrieSnapshot *t = TrieSnapshotHandles.lookup(*ptr);

	if (!t)
	{
		return 0;
	}

	if (TrieSnapshotHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}

	return 0;
}

AMX_NATIVE_INFO trie_Natives[] =
{
	{ "TrieCreate"               ,	TrieCreate },
	{ "TrieClear"                ,	TrieClear },

	{ "TrieSetCell"              ,	TrieSetCell },
	{ "TrieSetString"            ,	TrieSetString },
	{ "TrieSetArray"             ,	TrieSetArray },

	{ "TrieGetCell"              ,	TrieGetCell },
	{ "TrieGetString"            ,	TrieGetString },
	{ "TrieGetArray"             ,	TrieGetArray },

	{ "TrieDeleteKey"            ,	TrieDeleteKey },
	{ "TrieKeyExists"            ,	TrieKeyExists },
	{ "TrieDestroy"              ,	TrieDestroy },
	{ "TrieGetSize"              ,	TrieGetSize },

	{ "TrieSnapshotCreate"       ,	TrieSnapshotCreate },
	{ "TrieSnapshotLength"       ,	TrieSnapshotLength },
	{ "TrieSnapshotKeyBufferSize",	TrieSnapshotKeyBufferSize },
	{ "TrieSnapshotGetKey"       ,	TrieSnapshotGetKey },
	{ "TrieSnapshotDestroy"      ,	TrieSnapshotDestroy },

	{ nullptr                    ,	nullptr}
};

