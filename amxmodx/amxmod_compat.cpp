#include "amxmodx.h"
#include "amxmod_compat.h"
#include "format.h"

struct translate_result
{
	int suki;
	int dest;
	int lang;
};

CVector<translate_result *> g_tr_results;
CStack<size_t> g_old_ids;

bool GetTranslation(int id, int &key, int &dest, int &lang)
{
	if (id < 0 || (unsigned int)id > g_tr_results.size())
	{
		return false;
	}

	translate_result *pRes = g_tr_results[id];

	key = pRes->suki;
	dest = pRes->dest;
	lang = pRes->lang;

	g_old_ids.push((size_t)id);

	return true;
}

void ClearTransCache()
{
	for (size_t i=0; i<g_tr_results.size(); i++)
	{
		delete g_tr_results[i];
	}
	g_tr_results.clear();

	while (!g_old_ids.empty())
	{
		g_old_ids.pop();
	}
}

bool translate_bcompat(AMX *amx, cell *source, const char **_key, const char **_def)
{
	unsigned int trans = static_cast<unsigned int>(*source);
	trans &= ~BCOMPAT_TRANSLATE_BITS;
	int key, _dest, lang;
	if (!GetTranslation(trans, key, _dest, lang))
	{
		return false;
	}

	cell amx_addr, *phys_addr;
	if (amx_Allot(amx, 3, &amx_addr, &phys_addr) != AMX_ERR_NONE)
	{
		return false;
	}

	if (_dest == -1)
	{
		*phys_addr = LANG_PLAYER;
	} else if (_dest == 0) {
		*phys_addr = LANG_SERVER;
	} else if (lang >= 0 && lang < g_langMngr.GetLangsNum()) {
		const char *name = g_langMngr.GetLangName(lang);
		phys_addr[0] = static_cast<cell>(name[0]);
		phys_addr[1] = static_cast<cell>(name[1]);
		phys_addr[2] = static_cast<cell>('\0');
	} else {
		*phys_addr = LANG_SERVER;
	}

	//not optimized but it works, eh
	//if someone cares they can make a translate() wrapper that takes the key # in directly
	const char *r_key = g_langMngr.GetKey(key);
	const char *def = translate(amx, amx_addr, r_key);
	if (!def)
	{
		def = r_key;
	}
	amx_Release(amx, amx_addr);

	*_key = g_langMngr.GetKey(key);
	*_def = def;

	return true;
}

static cell AMX_NATIVE_CALL amx_translate(AMX *amx, cell *params)
{
	int len;
	char *key = get_amxstring(amx, params[1], 0, len);

	translate_result *pRes;
	size_t id;
	if (g_old_ids.empty())
	{
		pRes = new translate_result;
		id = g_tr_results.size();
		g_tr_results.push_back(pRes);
	} else {
		if (g_tr_results.size() >= BCOMPAT_TRANSLATE_MAX)
		{
			LogError(amx, AMX_ERR_NATIVE, "Exceeded bcompat translation limit of %d!", BCOMPAT_TRANSLATE_MAX);
			return 0;
		}
		id = g_old_ids.front();
		g_old_ids.pop();
		pRes = g_tr_results[id];
	}

	pRes->suki = g_langMngr.GetKeyEntry(key);

	//Some AMX Mod plugins do not register everything they need.  Prevent a crash.
	if (pRes->suki == -1)
	{
		pRes->suki = g_langMngr.AddKeyEntry(key);
	}

	pRes->dest = params[2];
	pRes->lang = params[3];

	return (cell)((unsigned int)BCOMPAT_TRANSLATE_BITS | (unsigned int)id);
}

AMX_NATIVE_INFO g_BcompatNatives[] = 
{
	{"translate",				amx_translate},

	{NULL,						NULL},
};

