// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "CLibrarySys.h"
#include <amxmodx.h>
#include <amtl/os/am-fsutil.h>
#include <amtl/os/am-path.h>
#include <amtl/os/am-system-errors.h>

LibrarySystem g_LibSys;

/******************/
/* Directory Code */
/******************/

CDirectory::CDirectory(const char *path)
{
#if defined PLATFORM_WINDOWS

	char newpath[PLATFORM_MAX_PATH];
	ke::SafeSprintf(newpath, sizeof(newpath), "%s\\*.*", path);

	m_dir = FindFirstFile(newpath, &m_fd);

	if (!IsValid())
	{
		m_fd.cFileName[0] = '\0';
	}

#elif defined PLATFORM_POSIX

	m_dir = opendir(path);

	if (IsValid())
	{
		m_ep = readdir(m_dir); // TODO: we need to read past "." and ".."!
		ke::SafeSprintf(m_origpath, sizeof(m_origpath), "%s", path);
	}
	else
	{
		m_ep = nullptr;
	}
#endif
}

CDirectory::~CDirectory()
{
	if (IsValid())
	{
#if defined PLATFORM_WINDOWS

		FindClose(m_dir);

#elif defined PLATFORM_POSIX

		closedir(m_dir);
#endif
	}
}

DirHandle CDirectory::GetHandle()
{
	return m_dir;
}

void CDirectory::NextEntry()
{
#if defined PLATFORM_WINDOWS

	if (FindNextFile(m_dir, &m_fd) == 0)
	{
		FindClose(m_dir);
		m_dir = INVALID_HANDLE_VALUE;
	}

#elif defined PLATFORM_POSIX

	if (!(m_ep = readdir(m_dir)))
	{
		closedir(m_dir);
		m_dir = nullptr;
	}
#endif
}

bool CDirectory::IsEntryValid()
{
	return IsValid();
}

bool CDirectory::IsEntryDirectory()
{
#if defined PLATFORM_WINDOWS

	return ((m_fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY);

#elif defined PLATFORM_POSIX

	char temppath[PLATFORM_MAX_PATH];
	ke::SafeSprintf(temppath, sizeof(temppath), "%s/%s", m_origpath, GetEntryName());

	return ke::file::IsDirectory(temppath);

#endif
}

bool CDirectory::IsEntryFile()
{
#if defined PLATFORM_WINDOWS

	return !(m_fd.dwFileAttributes & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_DEVICE));

#elif defined PLATFORM_POSIX

	char temppath[PLATFORM_MAX_PATH];
	ke::SafeSprintf(temppath, sizeof(temppath), "%s/%s", m_origpath, GetEntryName());

	return ke::file::IsFile(temppath);

#endif
}

const char* CDirectory::GetEntryName()
{
#if defined PLATFORM_WINDOWS

	return m_fd.cFileName;

#elif defined PLATFORM_POSIX

	return m_ep ? m_ep->d_name : "";

#endif
}

bool CDirectory::MoreFiles()
{
	return IsValid();
}

bool CDirectory::IsValid()
{
#if defined PLATFORM_WINDOWS

	return (m_dir != INVALID_HANDLE_VALUE);

#elif defined PLATFORM_POSIX

	return (m_dir != nullptr);
#endif
}


/****************/
/* Library Code */
/****************/

CLibrary::CLibrary(ke::RefPtr<ke::SharedLib> lib) : lib_(lib)
{}

void CLibrary::CloseLibrary()
{
	delete this;
}

void *CLibrary::GetSymbolAddress(const char* symname)
{
	return lib_->lookup(symname);
}


/***********************/
/* Library System Code */
/***********************/

bool LibrarySystem::PathExists(const char *path)
{
	return ke::file::PathExists(path);
}

bool LibrarySystem::IsPathFile(const char* path)
{
	return ke::file::IsFile(path);
}

bool LibrarySystem::IsPathDirectory(const char* path)
{
	return ke::file::IsDirectory(path);
}

CDirectory *LibrarySystem::OpenDirectory(const char* path)
{
	CDirectory* dir = new CDirectory(path);

	if (!dir->IsValid())
	{
		delete dir;
		return nullptr;
	}

	return dir;
}

CLibrary* LibrarySystem::OpenLibrary(const char* path, char* error, size_t maxlength)
{
	ke::RefPtr<ke::SharedLib> lib = ke::SharedLib::Open(path, error, maxlength);

	if (!lib)
	{
		return nullptr;
	}

	return new CLibrary(lib);
}

void LibrarySystem::GetPlatformError(char* error, size_t maxlength)
{
#if defined PLATFORM_WINDOWS

	return GetPlatformErrorEx(GetLastError(), error, maxlength);

#elif defined PLATFORM_POSIX

	return GetPlatformErrorEx(errno, error, maxlength);
#endif
}

void LibrarySystem::GetPlatformErrorEx(int code, char* error, size_t maxlength)
{
	if (error && maxlength)
	{
		ke::FormatSystemErrorCode(code, error, maxlength);
	}
}

void LibrarySystem::GetLoaderError(char* buffer, size_t maxlength)
{
	ke::FormatSystemError(buffer, maxlength);
}

void LibrarySystem::CloseDirectory(CDirectory *dir)
{
	delete dir;
}

size_t LibrarySystem::PathFormat(char* buffer, size_t len, const char* fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	size_t mylen = ke::path::FormatVa(buffer, len, fmt, ap);
	va_end(ap);

	return mylen;
}

char* LibrarySystem::PathFormat(const char* fmt, ...)
{
	static char buffer[PLATFORM_MAX_PATH];

	va_list ap;
	va_start(ap, fmt);
	ke::path::FormatVa(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	return buffer;
}

const char* LibrarySystem::GetFileExtension(const char* filename)
{
	size_t len, end;

	len = strlen(filename);

	/* Minimum string length for filename with ext would be 3; example: a.a */
	if (len < 3)
	{
		return nullptr;
	}

	end = len - 1;

	for (size_t i = end; i <= end; i--)
	{
		if (filename[i] == PLATFORM_SEP_CHAR || filename[i] == PLATFORM_SEP_ALTCHAR)
		{
			break;
		}

		if (filename[i] == '.' && i != end && i != 0)
		{
			return &filename[++i];
		}
	}

	return nullptr;
}

bool LibrarySystem::CreateFolder(const char* path)
{
	return ke::file::CreateDirectory(path, 0775);
}

size_t LibrarySystem::GetFileFromPath(char* buffer, size_t maxlength, const char* path)
{
	size_t length = strlen(path);

	for (size_t i = length - 1; i <= length - 1; i--)
	{
		if (path[i] == '/'
#if defined PLATFORM_WINDOWS
			|| path[i] == '\\'
#endif
			)
		{
			return ke::SafeSprintf(buffer, maxlength, "%s", &path[i + 1]);
		}
	}

	/* We scanned and found no path separator */
	return ke::SafeSprintf(buffer, maxlength, "%s", path);
}

bool LibrarySystem::FileTime(const char* path, FileTimeType type, time_t* pTime)
{
	struct stat s;

	if (stat(path, &s) != 0)
	{
		return false;
	}

	switch (type)
	{
		case FileTime_LastAccess:
		{
			*pTime = s.st_atime;
			break;
		}
		case FileTime_Created:
		{
			*pTime = s.st_ctime;
			break;
		}
		case FileTime_LastChange:
		{
			*pTime = s.st_mtime;
			break;
		}
		default:
		{
			return false;
		}
	}

	return true;
}

bool LibrarySystem::DoesPlatformMatch(const char *platform)
{
	return strcmp(platform, PLATFORM_NAME) == 0;
}

bool LibrarySystem::IsPlatformCompatible(const char *platform, bool *hadPrimaryMatch)
{
	if (DoesPlatformMatch(platform))
	{
#if defined PLATFORM_COMPAT_ALT
		*hadPrimaryMatch = true;
#endif
		return true;
	}

#if defined PLATFORM_COMPAT_ALT
	/* If entry hasn't been found for the primary platform name, check for compatible alternate */
	if (!*hadPrimaryMatch)
	{
		return strcmp(platform, PLATFORM_COMPAT_ALT) == 0;
	}
#endif

	return false;
}