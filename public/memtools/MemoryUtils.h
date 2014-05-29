/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2011 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 */

#ifndef _INCLUDE_SOURCEMOD_MEMORYUTILS_H_
#define _INCLUDE_SOURCEMOD_MEMORYUTILS_H_

#if defined(__linux__) || defined(__APPLE__)
	#include <stdint.h>
	#include <am-vector.h>
	#include <sm_symtable.h>
#endif

#if defined(__APPLE__)
	#include <CoreServices/CoreServices.h>
#endif

#if defined(WIN32)
	#include <windows.h>
#endif

#if defined _MSC_VER && _MSC_VER >= 1400
	#pragma warning (disable:4996) /* Disable deprecation warnings */
#endif

struct DynLibInfo
{
	void *baseAddress;
	size_t memorySize;
};

#if defined(__linux__) || defined(__APPLE__)
	struct LibSymbolTable
	{
		SymbolTable table;
		uintptr_t lib_base;
		uint32_t last_pos;
	};
#endif

class MemoryUtils
{
	public:
		MemoryUtils();
		~MemoryUtils();

	public: 
		void *DecodeAndFindPattern(const void *libPtr, const char *pattern);
		void *FindPattern(const void *libPtr, const char *pattern, size_t len);
		void *ResolveSymbol(void *handle, const char *symbol);

	public:
		bool GetLibraryInfo(const void *libPtr, DynLibInfo &lib);
		bool GetLibraryOfAddress(const void *libPtr, char *buffer, size_t maxlength, uintptr_t *base);

	public:
		size_t DecodeHexString(unsigned char *buffer, size_t maxlength, const char *hexstr);

#if defined(__linux__) || defined(__APPLE__)
	private:
		ke::Vector<LibSymbolTable *> m_SymTables;
	#if defined(__APPLE__)
		struct dyld_all_image_infos *m_ImageList;
		SInt32 m_OSXMajor;
		SInt32 m_OSXMinor;
	#endif // __APPLE__
#endif
};

extern MemoryUtils g_MemUtils;

#endif // _INCLUDE_SOURCEMOD_MEMORYUTILS_H_
