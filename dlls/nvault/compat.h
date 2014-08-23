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
	#if (_MSC_VER < 1300)
		typedef signed char       int8_t;
		typedef signed short      int16_t;
		typedef signed int        int32_t;
		typedef unsigned char     uint8_t;
		typedef unsigned short    uint16_t;
		typedef unsigned int      uint32_t;
	#else
		typedef signed __int8     int8_t;
		typedef signed __int16    int16_t;
		typedef signed __int32    int32_t;
		typedef unsigned __int8   uint8_t;
		typedef unsigned __int16  uint16_t;
		typedef unsigned __int32  uint32_t;
	#endif
#else
	#include <stdint.h>
#endif

#endif //_INCLUDE_COMPAT_H
