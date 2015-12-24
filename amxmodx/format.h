// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_FORMATTING_H
#define _INCLUDE_FORMATTING_H

//Amx Templatized Cell Printf
template <typename D, typename S>
size_t atcprintf(D *buffer, size_t maxlen, const S *format, AMX *amx, cell *params, int *param);

const char *playerlang(const cell index);
const char *translate(AMX *amx, const char *lang, const char *key);

#endif //_INCLUDE_FORMATTING_H
