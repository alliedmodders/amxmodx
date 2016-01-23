// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_MEMFILE_H
#define _INCLUDE_MEMFILE_H

#ifdef _MSC_VER
	// MSVC8 - replace POSIX functions with ISO C++ conformant ones as they are deprecated
	#if _MSC_VER >= 1400	
		#define strdup _strdup
		#pragma warning(disable : 4996)
	#endif
#endif

#include <stdlib.h>

typedef struct memfile_s
{
	char *name;
	char *base;
	long offs;
	long usedoffs;
	size_t size;
	int _static;
} memfile_t;

memfile_t *memfile_creat(const char *name, size_t init);
void memfile_destroy(memfile_t *mf);
void memfile_seek(memfile_t *mf, long seek);
int memfile_write(memfile_t *mf, void *buffer, size_t size);
size_t memfile_read(memfile_t *mf, void *buffer, size_t maxsize);
long memfile_tell(memfile_t *mf);

#endif //_INCLUDE_MEMFILE_H
