#ifndef _INCLUDE_AMXMOD_CORE_COMPAT_H
#define _INCLUDE_AMXMOD_CORE_COMPAT_H

#define BCOMPAT_TRANSLATE_BITS		0xFFFFF400
#define BCOMPAT_TRANSLATE_MAX		0x400

bool GetTranslation(int id, int &key, int &dest, int &lang);
void ClearTransCache();

extern AMX_NATIVE_INFO g_BcompatNatives[];

#endif //_INCLUDE_AMXMOD_CORE_COMPAT_H
