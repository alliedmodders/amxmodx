// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#pragma once

#include <interface.h>  // Interface (HLSDK)

#define PLATFORM_WINDOWNS_NAME "windows"
#define PLATFORM_LINUX_NAME    "linux"
#define PLATFORM_MAC_NAME      "mac"
#if defined(WIN32)
#  ifndef PLATFORM_WINDOWS
#  define PLATFORM_WINDOWS  1
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#  define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#  include <direct.h>
#  include <io.h>
#  define PLATFORM_LIB_EXT      "dll"
#  define PLATFORM_NAME         PLATFORM_WINDOWNS_NAME
#  define PLATFORM_SEP_CHAR     '\\'
#  define PLATFORM_SEP_ALTCHAR  '/'
#  define PLATFORM_EXTERN_C     extern "C" __declspec(dllexport)
#elif defined(__linux__) || defined(__APPLE__)
#  if defined(__linux__)
#    define PLATFORM_LINUX      1
#    define PLATFORM_LIB_EXT    "so"
#    define PLATFORM_NAME       PLATFORM_LINUX_NAME
#    define PLATFORM_COMPAT_ALT PLATFORM_MAC_NAME
#  elif defined(__APPLE__)
#    define PLATFORM_APPLE      1
#    define PLATFORM_LIB_EXT    "dylib"
#    define PLATFORM_NAME       PLATFORM_MAC_NAME
#    define PLATFORM_COMPAT_ALT PLATFORM_LINUX_NAME
#  endif
#  ifndef PLATFORM_POSIX
#    define PLATFORM_POSIX      1
#  endif
#  include <stdio.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <errno.h>
#  include <unistd.h>
#  include <dirent.h>
#  include <dlfcn.h>
#  if defined(PLATFORM_APPLE)
#    include <sys/syslimits.h>
#  endif
#  define PLATFORM_SEP_CHAR     '/'
#  define PLATFORM_SEP_ALTCHAR  '\\'
#  define PLATFORM_EXTERN_C     extern "C" __attribute__((visibility("default")))
#  define WINAPI
#endif

#define PLATFORM_MAX_PATH 260

#if defined PLATFORM_WINDOWS
	typedef HANDLE   DirHandle;
#elif defined PLATFORM_POSIX
	typedef DIR*     DirHandle;
#endif

