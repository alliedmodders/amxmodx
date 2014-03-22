#include "amxmodx.h"
#include "amxmod_compat.h"
#include "format.h"

bool GetTranslation(amxtrans_t trans, int &key, int &dest, int &lang)
{
	key = (trans & BCOMPAT_TRANSLATE_KEYMASK);
	dest = (trans >> BCOMPAT_TRANSLATE_DESTRSH) & BCOMPAT_TRANSLATE_DESTMASK;
	lang = (trans >> BCOMPAT_TRANSLATE_LANGRSH) & BCOMPAT_TRANSLATE_LANGMASK;

	if (dest == 0x3F)
	{
		dest = -1;
	} 

	if (lang == 0x1F)
	{
		lang = -1;
	}

	return true;
}

bool translate_bcompat(AMX *amx, cell *source, const char **_key, const char **_def)
{
	amxtrans_t trans = static_cast<amxtrans_t>(*source);
	int key, _dest, lang;
	if (!GetTranslation(trans, key, _dest, lang))
	{
		return false;
	}

	//not optimized but it works, eh
	//if someone cares they can make a translate() wrapper that takes the key # in directly
	const char *r_key = g_langMngr.GetKey(key);
	if (!r_key)
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

	amxtrans_t trans;

	int suki = g_langMngr.GetKeyEntry(key);
	//Some AMX Mod plugins do not register everything they need.  Prevent a crash.
	if (suki == -1)
	{
		suki = g_langMngr.AddKeyEntry(key);
	}

	if (suki > BCOMPAT_TRANSLATE_KEYMASK)
	{
		LogError(amx, AMX_ERR_NATIVE, "Not enough translation space, aborting!");
		return 0;
	}

	trans = suki & BCOMPAT_TRANSLATE_KEYMASK;
	int dest = static_cast<int>(params[2]);
	int lang = static_cast<int>(params[3]);
	if (dest == -1)
	{
		trans |= (0x3F << BCOMPAT_TRANSLATE_DESTRSH);
	} else {
		trans |= (dest & BCOMPAT_TRANSLATE_DESTMASK) << BCOMPAT_TRANSLATE_DESTRSH;
	}

	if (lang == -1)
	{
		trans |= (0x1F << BCOMPAT_TRANSLATE_LANGRSH);
	} else {
		trans |= (lang & BCOMPAT_TRANSLATE_LANGMASK) << BCOMPAT_TRANSLATE_LANGRSH;
	}

	trans |= BCOMPAT_TRANSLATE_BITS;

	return static_cast<cell>(trans);
}

AMX_NATIVE_INFO g_BcompatNatives[] = 
{
	{"translate",				amx_translate},

	{NULL,						NULL},
};

