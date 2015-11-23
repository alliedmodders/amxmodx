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
#include <interface.h>  // Interface (HLSDK)
#include <amtl/am-utility.h> // AutoPtr
#include <amtl/os/am-shared-library.h>

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

template <typename T>
bool GET_IFACE(const char* library, T*& var, const char* version)
{
	const char* path = g_LibSys.PathFormat("%s.%s", library, PLATFORM_LIB_EXT);

	ke::AutoPtr<CLibrary> lib(g_LibSys.OpenLibrary(path));

	if (lib)
	{
		CreateInterfaceFn factory = reinterpret_cast<CreateInterfaceFn>(lib->GetSymbolAddress(CREATEINTERFACE_PROCNAME));

		if (factory)
		{
			var = reinterpret_cast<T*>(factory(version, nullptr));
			return true;
		}
	}

	var = nullptr;
	return false;
}

#endif // _INCLUDE_LIBRARY_SYS_H_
