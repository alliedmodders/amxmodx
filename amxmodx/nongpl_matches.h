// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_AMXMODX_NONGPL_MATCHES_H_
#define _INCLUDE_AMXMODX_NONGPL_MATCHES_H_

struct NONGPL_PLUGIN_T
{
	const char *author;
	const char *title;
	const char *filename;
};

struct NONGPL_CVAR_T
{
	const char *cvar;
	int type;
};

extern NONGPL_PLUGIN_T NONGPL_PLUGIN_LIST[];
extern NONGPL_CVAR_T NONGPL_CVAR_LIST[];
extern bool CheckBadConList(const char *cvar, int type);

#endif //_INCLUDE_AMXMODX_NONGPL_MATCHES_H_
