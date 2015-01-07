// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _AMXXSC_INCLUDE_H
#define _AMXXSC_INCLUDE_H

#include <amxmodx_version.h>

#define MAGIC_HEADER2		0x414D5858
#define	MAGIC_VERSION		0x0300

#if defined(__linux__) || defined(__APPLE__)
# include <dlfcn.h>
#else
# include <windows.h>
#endif

#include <string.h>

#if defined(__linux__) || defined(__APPLE__)
# define dlmount(x)		dlopen(x, RTLD_NOW|RTLD_GLOBAL)
  typedef void*			HINSTANCE;
#else
# define dlsym(x, s)	GetProcAddress(x, s)
# define dlmount(x)		LoadLibrary(x)
# define dlclose(x)		FreeLibrary(x)
#endif

#include "zlib/zlib.h"
#include "Binary.h"

typedef int	(*COMPILER)(int argc, char **argv);
typedef int (*PRINTF)(const char *message, ...);

char *FindFileName(int argc, char **argv);
char *swiext(const char *file, const char *ext);
void show_help();


struct ablhdr
{
	int magic;
	char size;
};

struct abl
{
	long stp;
	char cellsize;
	int size;
	long cmpsize;
	char *data;
	char *cmp;
};

struct BinHeader
{
	int32_t magic;
	int16_t version;
	int8_t plugins;
};

struct BinPlugin
{
	int8_t cellsize;	//cell size
	int32_t imagesize;	//uncompressed image size
	int32_t disksize;	//compressed image size
	int32_t memsize;	//memory image size
	int32_t offs;		//file offset
};

#if defined(__linux__) || defined(__APPLE__)
bool FileExists(const char *file);
#endif

#endif //_AMXXSC_INCLUDE_H
