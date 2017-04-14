// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_LIBRARY_SYS_H_
#define _INCLUDE_LIBRARY_SYS_H_

#include "amx.h"        // cell
#include <platform_helpers.h>
#include <amtl/os/am-shared-library.h>

enum FileTimeType
{
	FileTime_LastAccess = 0,  /* Last access (not available on FAT) */
	FileTime_Created    = 1,  /* Creation (not available on FAT) */
	FileTime_LastChange = 2,  /* Last modification */
};

class CDirectory
{
	public:

		CDirectory(const char* path);
		~CDirectory();

	public:

		bool MoreFiles();
		void NextEntry();
		const char* GetEntryName();
		bool IsEntryDirectory();
		bool IsEntryFile();
		bool IsEntryValid();

	public:

		bool IsValid();
		DirHandle GetHandle();

	private:

#if defined PLATFORM_WINDOWS

		HANDLE           m_dir;
		WIN32_FIND_DATAA m_fd;

#elif defined PLATFORM_POSIX

		DIR*             m_dir;
		struct dirent*   m_ep;
		char             m_origpath[PLATFORM_MAX_PATH];
#endif
};

class CLibrary
{
	public:

		CLibrary(ke::RefPtr<ke::SharedLib> lib);

	public:

		void CloseLibrary();
		void *GetSymbolAddress(const char* symname);

	private:

		ke::RefPtr<ke::SharedLib> lib_;
};

class LibrarySystem
{
	public:

		CLibrary* OpenLibrary(const char* path, char* error = nullptr, size_t maxlength = 0);
		CDirectory* OpenDirectory(const char* path);
		void CloseDirectory(CDirectory *dir);

		bool PathExists(const char* path);
		bool IsPathFile(const char* path);
		bool IsPathDirectory(const char* path);

		void GetPlatformError(char* error, size_t maxlength);
		void GetPlatformErrorEx(int code, char* error, size_t maxlength);

		size_t PathFormat(char* buffer, size_t len, const char* fmt, ...);
		char*  PathFormat(const char* fmt, ...);

		const char* GetFileExtension(const char* filename);
		bool CreateFolder(const char* path);
		size_t GetFileFromPath(char* buffer, size_t maxlength, const char* path);

		bool FileTime(const char* path, FileTimeType type, time_t* pTime);
		void GetLoaderError(char* buffer, size_t maxlength);

		bool DoesPlatformMatch(const char* platform);
		bool IsPlatformCompatible(const char *platform, bool *hadPrimaryMatch);
};

extern LibrarySystem g_LibSys;

#endif // _INCLUDE_LIBRARY_SYS_H_
