// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_SOURCEMOD_THREAD_SUPPORT_H
#define _INCLUDE_SOURCEMOD_THREAD_SUPPORT_H

#if defined __linux__ || defined __APPLE__
#include "PosixThreads.h"
#elif defined WIN32
#include "WinThreads.h"
#endif

#endif //_INCLUDE_SOURCEMOD_THREAD_SUPPORT_H
