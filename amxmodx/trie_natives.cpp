#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "amxmodx.h"
#include "sm_trie_tpl.h"
#include "trie_natives.h"

using namespace SourceMod;

TrieHandles<CellTrie> g_TrieHandles;

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
// native TrieSetCell(Trie:handle, const key[], any:value);
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
	}

	i->value.setCell(params[3]);
	
	return 1;
}
// native TrieSetString(Trie:handle, const key[], const data[]);
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
	}

	i->value.setString(value);

	return 1;
}
// native TrieSetArray(Trie:handle, const key[], const any:buffer[], buffsize)
static cell AMX_NATIVE_CALL TrieSetArray(AMX *amx, cell *params)
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
	}

	i->value.setArray(get_amxaddr(amx, params[3]), params[4]);

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

	return 1;
}
// native bool:TrieGetString(Trie:handle, const key[], buff[], len);
static cell AMX_NATIVE_CALL TrieGetString(AMX *amx, cell *params)
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
	if (!r.found() || !r->value.isString())
	{
		return 0;
	}

	set_amxstring_utf8(amx, params[3], r->value.chars(), r->value.arrayLength(), params[4] + 1); // + EOS

	return 1;
}
// native bool:TrieGetArray(Trie:handle, const key[], any:buff[], len);
static cell AMX_NATIVE_CALL TrieGetArray(AMX *amx, cell *params)
{
	CellTrie *t = g_TrieHandles.lookup(params[1]);

	if (t == NULL)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid map handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *key = get_amxstring(amx, params[2], 0, len);
	cell *ptr = get_amxaddr(amx, params[3]);
	size_t size = params[4];

	StringHashMap<Entry>::Result r = t->map.find(key);
	if (!r.found() || !r->value.isArray())
	{
		return 0;
	}

	size_t length = r->value.arrayLength();
	cell *base = r->value.array();

	if (length <= size)
	{
		size = length;
	}

	memcpy(ptr, base, sizeof(cell)* size);
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

	{ NULL,			NULL }
};

