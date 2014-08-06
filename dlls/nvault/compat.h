// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// NVault Module
//

#ifndef _INCLUDE_COMPAT_H
#define _INCLUDE_COMPAT_H

#ifdef WIN32
typedef	__int16				int16_t;
typedef	unsigned __int16	uint16_t;
typedef	__int8				int8_t;
typedef unsigned __int8		uint8_t;

#else

#include <stdint.h>

#endif

#endif //_INCLUDE_COMPAT_H
