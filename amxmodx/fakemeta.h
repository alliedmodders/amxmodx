// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef __FAKEMETA_H__
#define __FAKEMETA_H__

int UnloadMetamodPlugin(void *handle);
int LoadMetamodPlugin(const char *path, void **handle, PLUG_LOADTIME now);

#endif // #ifndef __FAKEMETA_H__
