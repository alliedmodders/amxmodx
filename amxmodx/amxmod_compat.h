#ifndef _INCLUDE_AMXMOD_CORE_COMPAT_H
#define _INCLUDE_AMXMOD_CORE_COMPAT_H

/**
 * New format for translation:
 * Note that we only support:
 *  4k keys
 *  32 languages
 * 0000 0000 0000 0000 0000 0000 0000 0000
 *                          |  key id    |
 *                  |     | <- dest id
 *           |     | <- lang id
 */

#define BCOMPAT_TRANSLATE_BITS		0xFF000000
#define BCOMPAT_TRANSLATE_KEYMASK	0xFFF
#define BCOMPAT_TRANSLATE_DESTMASK	0x3F
#define BCOMPAT_TRANSLATE_DESTRSH	12
#define BCOMPAT_TRANSLATE_LANGMASK  0x1F
#define BCOMPAT_TRANSLATE_LANGRSH	18

typedef unsigned int amxtrans_t;

bool GetTranslation(amxtrans_t trans, int &key, int &dest, int &lang);

extern AMX_NATIVE_INFO g_BcompatNatives[];

#endif //_INCLUDE_AMXMOD_CORE_COMPAT_H
