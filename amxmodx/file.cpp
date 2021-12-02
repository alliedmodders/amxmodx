// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "CFileSystem.h"
#include "CLibrarySys.h"

using namespace ke;

// native read_dir(const dirname[], pos, output[], len, &outlen = 0);
static cell AMX_NATIVE_CALL read_dir(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);
	const char* realpath = build_pathname("%s", path);

	AutoPtr<CDirectory> dir(g_LibSys.OpenDirectory(realpath));

	if (!dir)
	{
		return 0;
	}

	cell offset = Max(0, params[2]);

	if (offset >= 0)
	{
#if defined PLATFORM_WINDOWS
		// Should be declared after so entry starts to '.' and not '..'
		// But old code did that, so keep this for compatibility.
		++offset;

		for (cell i = 0; i < offset && dir->MoreFiles(); ++i)
		{
			dir->NextEntry();
		}

#elif defined PLATFORM_POSIX

		seekdir(dir->GetHandle(), offset);

		dir->NextEntry();

		if (dir->IsValid())
		{
			offset = telldir(dir->GetHandle());
		}
#endif
	}

	if (!dir->IsValid())
	{
		return 0;
	}

	const char* entry = dir->GetEntryName();
	cell* outputLen = get_amxaddr(amx, params[5]);

	*outputLen = set_amxstring_utf8(amx, params[3], entry, strlen(entry), params[4]);

	return offset;
}

// native read_file(const file[], line, text[], len, &txtlen = 0);
static cell AMX_NATIVE_CALL read_file(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);
	const char* realpath = build_pathname("%s", path);

	AutoPtr<SystemFile> fp(SystemFile::Open(realpath, "r"));

	if (!fp)
	{
		LogError(amx, AMX_ERR_NATIVE, "Couldn't read file \"%s\"", path);
		return 0;
	}

	static char buffer[2048];

	size_t currentLine = 0;
	size_t targetLine = Max(0, params[2]);

	while (currentLine <= targetLine && fp->ReadLine(buffer, sizeof(buffer) - 1))
	{
		++currentLine;
	}

	if (currentLine > targetLine)
	{
		length = strlen(buffer);

		if (length > 0)
		{
			if (buffer[length - 1] == '\n')
			buffer[--length] = '\0';

			if (buffer[length - 1] == '\r')
			buffer[--length] = '\0';
		}
		cell* textLen = get_amxaddr(amx, params[5]);
		*textLen = set_amxstring_utf8(amx, params[3], buffer, length, params[4]);

		return currentLine;
	}

	return 0;
}

// native write_file(const file[], const text[], line = -1);
static cell AMX_NATIVE_CALL write_file(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);
	const char* text = get_amxstring(amx, params[2], 1, length);
	int targetLine = params[3];

	const char* realpath = build_pathname("%s", path);

	AutoPtr<SystemFile>fp;

	if (targetLine < 0)
	{
		if (!(fp = SystemFile::Open(realpath, "a")))
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't write file \"%s\"", realpath);
			return 0;
		}

		fp->Write(text, length);
		fp->Write("\n", 1);

		return 1;
	}

	if (!(fp = SystemFile::Open(realpath, "r")))
	{
		if (!(fp = SystemFile::Open(realpath, "w")))
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't write file \"%s\"", realpath);
			return 0;
		}

		for (int i = 0; i < targetLine; ++i)
		{
			fp->Write("\n", 1);
		}

		fp->Write(text, length);
		fp->Write("\n", 1);

		return 1;
	}

	SystemFile fptemp(tmpfile());

	if (!fptemp.handle())
	{
		LogError(amx, AMX_ERR_NATIVE, "Couldn't create temp file");
		return 0;
	}

	static char buffer[2048];

	for (int i = 0;; ++i)
	{
		if (i == targetLine)
		{
			fp->ReadLine(buffer, sizeof(buffer) - 1);

			fptemp.Write(text, length);
			fptemp.Write("\n", 1);
		}
		else if (fp->ReadLine(buffer, sizeof(buffer) - 1))
		{
			fptemp.Write(buffer, strlen(buffer));
		}
		else if (i < targetLine)
		{
			fptemp.Write("\n", 1);
		}
		else
		{
			break;
		}
	}

	rewind(fptemp.handle());

	if (!(fp = AutoPtr<SystemFile>(SystemFile::Open(realpath, "w"))))
	{
		LogError(amx, AMX_ERR_NATIVE, "Couldn't write file \"%s\"", realpath);
		return 0;
	}

	while (fptemp.ReadLine(buffer, sizeof(buffer) - 1))
	{
		fp->Write(buffer, strlen(buffer));
	}

	return 1;
}

// native delete_file(const file[], bool:use_valve_fs = false, const valve_path_id[] = "GAMECONFIG");
static cell AMX_NATIVE_CALL delete_file(AMX *amx, cell *params)
{
	int length;
	const char* file = get_amxstring(amx, params[1], 0, length);

	if (*params / sizeof(cell) >= 2 && params[2] > 0)
	{
		const char* pathID = get_amxstring_null(amx, params[3], 1, length);

		return ValveFile::Delete(file, pathID);
	}

	return SystemFile::Delete(build_pathname("%s", file));
}

// native file_exists(const file[], bool:use_valve_fs = false);
static cell AMX_NATIVE_CALL file_exists(AMX *amx, cell *params)
{
	int length;
	const char* file = get_amxstring(amx, params[1], 0, length);

	if (*params / sizeof(cell) >= 2 && params[2] > 0)
	{
		return g_FileSystem->FileExists(file);
	}

	return g_LibSys.IsPathFile(build_pathname("%s", file));
}

// native dir_exists(const dir[], bool:use_valve_fs = false);
static cell AMX_NATIVE_CALL dir_exists(AMX *amx, cell *params)
{
	int length;
	const char *path = get_amxstring(amx, params[1], 0, length);

	if (*params / sizeof(cell) >= 2 && params[2] > 0)
	{
		return g_FileSystem->IsDirectory(path);
	}

	return g_LibSys.IsPathDirectory(build_pathname("%s", path));
}

#define FSOPT_BYTES_COUNT  0
#define FSOPT_LINES_COUNT  1
#define FSOPT_END_WITH_LF  2

// native file_size(const file[], flag = 0, bool:use_valve_fs = false, const valve_path_id[] = "GAME");
static cell AMX_NATIVE_CALL file_size(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);
	int flag = FSOPT_BYTES_COUNT;

	AutoPtr<FileObject> fp;

	size_t numParams = *params / sizeof(cell);

	if (numParams >= 3 && params[3] > 0)
	{
		const char* pathID = get_amxstring_null(amx, params[4], 1, length);

		fp = ValveFile::Open(path, "r", pathID);
	}
	else
	{
		fp = SystemFile::Open(build_pathname("%s", path), "r");
	}

	if (!fp)
	{
		return -1;
	}

	if (numParams >= 2)
	{
		flag = params[2];
	}

	switch (flag)
	{
		case FSOPT_BYTES_COUNT:
		{
			fp->Seek(0, SEEK_END);

			return fp->Tell();
		}
		case FSOPT_LINES_COUNT:
		{
			int8_t ch = 0;
			size_t lines = 0;

			while (!fp->EndOfFile() && !fp->HasError())
			{
				++lines;
				while (fp->Read(&ch, 1) == 1 && ch != '\n' && ch != EOF);
			}

			return lines;
		}
		case FSOPT_END_WITH_LF:
		{
			int8_t ch = 0;

			fp->Seek(-1, SEEK_END);
			fp->Read(&ch, 1);

			return ch == '\n';
		}
	}

	return -1;
}

// native fopen(const filename[], const mode[], bool:use_valve_fs = false, const valve_path_id[] = "GAME");
static cell AMX_NATIVE_CALL amx_fopen(AMX *amx, cell *params)
{
	int length;
	const char* file  = get_amxstring(amx, params[1], 0, length);
	const char* flags = get_amxstring(amx, params[2], 1, length);

	FileObject* fp = nullptr;

	if (*params / sizeof(cell) >= 3 && params[3] > 0)
	{
		const char* pathID = get_amxstring_null(amx, params[4], 2, length);

		fp = ValveFile::Open(file, flags, pathID);
	}
	else
	{
		fp = SystemFile::Open(build_pathname("%s", file), flags);
	}

	if (!fp)
	{
		return 0;
	}

	return reinterpret_cast<cell>(fp);
}

#define BLOCK_INT	4
#define BLOCK_SHORT	2
#define BLOCK_CHAR	1

// native fwrite_blocks(file, const data[], blocks, mode);
static cell AMX_NATIVE_CALL amx_fwrite_blocks(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell* data  = get_amxaddr(amx, params[2]);
	cell blocks = params[3];
	cell size   = params[4];

	size_t read = 0;

	switch (size)
	{
		case BLOCK_CHAR:
		{
			for (cell i = 0; i < blocks; ++i)
			{
				char value = data[i];

				if (fp->Write(&value, sizeof(value)) != sizeof(value))
				{
					break;
				}

				read += sizeof(value);
			}
			break;
		}
		case BLOCK_SHORT:
		{
			for (cell i = 0; i < blocks; ++i)
			{
				short value = data[i];

				if (fp->Write(&value, sizeof(value)) != sizeof(value))
				{
					break;
				}

				read += sizeof(value);
			}
			break;
		}
		case BLOCK_INT:
		{
			read = fp->Write(data, sizeof(cell) * blocks);
			break;
		}
		default:
		{
			return 0;
		}
	}

	return read / size;
}

// native fwrite(file, data, mode);
static cell AMX_NATIVE_CALL amx_fwrite(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell   data = params[2];
	size_t size = params[3];

	switch (size)
	{
		case BLOCK_CHAR:
		{
			char value = static_cast<char>(data);
			return fp->Write(&value, sizeof(value));
		}
		case BLOCK_SHORT:
		{
			short value = static_cast<short>(data);
			return fp->Write(&value, sizeof(value));
		}
		case BLOCK_INT:
		{
			int value = static_cast<int>(data);
			return fp->Write(&value, sizeof(value));
		}
	}

	return 0;
}

// native fwrite_raw(file, const stream[], blocksize, mode);
static cell AMX_NATIVE_CALL amx_fwrite_raw(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell* data = get_amxaddr(amx, params[2]);

	return fp->Write(&data, params[3] * params[4]);
}

// native fread_raw(file, stream[], blocksize, blocks);
static cell AMX_NATIVE_CALL amx_fread_raw(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell* data = get_amxaddr(amx, params[2]);

	return fp->Read(data, params[3] * params[4]);
}

// native fread(file, &data, mode);
static cell AMX_NATIVE_CALL amx_fread(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell *data = get_amxaddr(amx, params[2]);

	switch (params[3])
	{
		case BLOCK_CHAR:
		{
			char value;
			size_t res = fp->Read(&value, sizeof(value));
			*data = static_cast<cell>(value);
			return res;
		}
		case BLOCK_SHORT:
		{
			short value;
			size_t res = fp->Read(&value, sizeof(value));
			*data = static_cast<cell>(value);
			return res;
		}
		case BLOCK_INT:
		{
			int value;
			size_t res = fp->Read(&value, sizeof(value));
			*data = static_cast<cell>(value);
			return res;
		}
	}

	return 0;
}

// native fread_blocks(file, data[], blocks, mode);
static cell AMX_NATIVE_CALL amx_fread_blocks(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell *data  = get_amxaddr(amx, params[2]);
	cell blocks = params[3];
	cell size   = params[4];

	size_t read = 0;

	switch (size)
	{
		case BLOCK_CHAR:
		{
			for (cell i = 0; i < blocks; ++i)
			{
				char value;

				if (fp->Read(&value, sizeof(value)) != sizeof(value))
				{
					break;
				}

				read += sizeof(value);
				*data++ = value;
			}
			break;
		}
		case BLOCK_SHORT:
		{
			for (cell i = 0; i < blocks; ++i)
			{
				short value;

				if (fp->Read(&value, sizeof(value)) != sizeof(value))
				{
					break;
				}

				read += sizeof(value);
				*data++ = value;
			}
			break;
		}
		case BLOCK_INT:
		{
			read = fp->Read(data, sizeof(cell) * blocks);
			break;
		}
		default:
		{
			return 0;
		}
	}

	return read / size;
}

// native fputs(file, const text[], bool:null_term = false);
static cell AMX_NATIVE_CALL amx_fputs(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	int length;
	char *string = get_amxstring(amx, params[2], 0, length);

	if (*params / sizeof(cell) >= 3 && params[3] > 0)
	{
		++length;
	}

	if (fp->Write(string, length) != (size_t)length)
	{
		return -1;
	}

	return 0;
}

// native fgets(file, buffer[], maxlength);
static cell AMX_NATIVE_CALL amx_fgets(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	static char buffer[4096];
	buffer[0] = '\0';

	fp->ReadLine(buffer, sizeof(buffer) - 1);

	return set_amxstring_utf8(amx, params[2], buffer, strlen(buffer), params[3]);
}

// native fseek(file, position, start);
static cell AMX_NATIVE_CALL amx_fseek(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	return !fp->Seek(params[2], params[3]);
}

// native ftell(file);
static cell AMX_NATIVE_CALL amx_ftell(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	return fp->Tell();
}

// native fprintf(file, const fmt[], any:...);
static cell AMX_NATIVE_CALL amx_fprintf(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	int length;
	const char* string = format_amxstring(amx, params, 2, length);

	if (ValveFile *vfile = fp->AsValveFile())
	{
		return g_FileSystem->FPrintf(vfile->handle(), const_cast<char*>("%s"), string);
	}
	else if (SystemFile *sysfile = fp->AsSystemFile())
	{
		return fprintf(sysfile->handle(), "%s", string);
	}
	else
	{
		assert(false);
	}

	return 0;
}

// native feof(file);
static cell AMX_NATIVE_CALL amx_feof(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (fp)
	{
		return fp->EndOfFile();
	}

	return 1;
}

// native fclose(file);
static cell AMX_NATIVE_CALL amx_fclose(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (fp)
	{
		fp->Close();
	}

	return 1;
}

// native filesize(const filename[], any:...);
static cell AMX_NATIVE_CALL amx_filesize(AMX *amx, cell *params)
{
	int length;
	const char *realpath = build_pathname("%s", format_amxstring(amx, params, 1, length));

	AutoPtr<SystemFile> fp(SystemFile::Open(realpath, "rb"));

	if (fp)
	{
		fp->Seek(0, SEEK_END);

		return fp->Tell();
	}

	return -1;
}

// Undocumented.
static cell AMX_NATIVE_CALL amx_build_pathname(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);

	return set_amxstring(amx, params[2], build_pathname("%s", path), params[3]);
}

enum FileType
{
	FileType_Unknown,       /* Unknown file type (device/socket) */
	FileType_Directory,     /* File is a directory */
	FileType_File,	        /* File is a file */
};

struct DirectoryHandle
{
	DirectoryHandle(CDirectory *handle_) : handle(handle_), valvefs(false) {}
	DirectoryHandle(FileFindHandle_t handle_) : handle_vfs(handle_), valvefs(true) {}

	union
	{
		CDirectory*      handle;
		FileFindHandle_t handle_vfs;
	};

	bool valvefs;
};

// native open_dir(const dir[], firstfile[], length, &FileType:type = FileType_Unknown, bool:use_valve_fs=false, const valve_path_id[] = "GAME");
static cell AMX_NATIVE_CALL amx_open_dir(AMX *amx, cell *params)
{
	int length;
	const char* path = get_amxstring(amx, params[1], 0, length);

	if (!*path)
	{
		return 0;
	}

	size_t numParams = *params / sizeof(cell);

	if (numParams >= 4 && params[5] > 0)
	{
		const char* wildcardedPath = g_LibSys.PathFormat("%s%s*", path, (path[length - 1] != '/' && path[length - 1] != '\\') ? "/" : "");
		const char* pathID = get_amxstring_null(amx, params[6], 1, length);

		FileFindHandle_t handle;
		const char* pFirst = g_FileSystem->FindFirst(wildcardedPath, &handle, pathID);

		if (!pFirst)
		{
			return 0;
		}

		set_amxstring_utf8(amx, params[2], pFirst, strlen(pFirst), params[3]);
		cell* fileType = get_amxaddr(amx, params[4]);

		*fileType = g_FileSystem->FindIsDirectory(handle) ? FileType_Directory : FileType_File;

		return reinterpret_cast<cell>(new DirectoryHandle(handle));
	}

	CDirectory* dir = g_LibSys.OpenDirectory(build_pathname("%s", path));

	if (!dir)
	{
		return 0;
	}

	if (numParams >= 4)
	{
		cell* fileType = get_amxaddr(amx, params[4]);
		*fileType = dir->IsEntryDirectory() ? FileType_Directory : FileType_File;
	}

	const char* entry = dir->GetEntryName();
	set_amxstring_utf8(amx, params[2], entry, strlen(entry), params[3]);

	return reinterpret_cast<cell>(new DirectoryHandle(dir));
}

// native close_dir(dirh);
static cell AMX_NATIVE_CALL amx_close_dir(AMX *amx, cell *params)
{
	AutoPtr<DirectoryHandle> p(reinterpret_cast<DirectoryHandle*>(params[1]));

	if (!p)
	{
		return 0;
	}

	if (p->valvefs)
	{
		FileFindHandle_t handle = p->handle_vfs;
		g_FileSystem->FindClose(handle);
	}
	else
	{
		CDirectory* handle = reinterpret_cast<CDirectory*>(p->handle);
		g_LibSys.CloseDirectory(handle);
	}

	return 1;
}

// native next_file(dirh, buffer[], length, &FileType:type = FileType_Unknown);
static cell AMX_NATIVE_CALL amx_get_dir(AMX *amx, cell *params)
{
	DirectoryHandle* p = reinterpret_cast<DirectoryHandle*>(params[1]);

	if (!p)
	{
		return 0;
	}

	size_t numParams = *params / sizeof(cell);

	if (p->valvefs)
	{
		FileFindHandle_t handle = p->handle_vfs;

		const char* entry = g_FileSystem->FindNext(handle);

		if (!entry)
		{
			return 0;
		}

		if (numParams >= 4)
		{
			cell* fileType = get_amxaddr(amx, params[4]);
			*fileType = g_FileSystem->FindIsDirectory(handle) ? FileType_Directory : FileType_File;
		}

		set_amxstring_utf8(amx, params[2], entry, strlen(entry), params[3]);
	}
	else
	{
		CDirectory* handle = p->handle;

		if (!handle)
		{
			return 0;
		}

		handle->NextEntry();

		if (!handle->MoreFiles())
		{
			return 0;
		}

		if (numParams >= 4)
		{
			cell* fileType = get_amxaddr(amx, params[4]);
			*fileType = handle->IsEntryDirectory() ? FileType_Directory : FileType_File;
		}

		const char* entry = handle->GetEntryName();

		set_amxstring_utf8(amx, params[2], entry, strlen(entry), params[3]);
	}

	return 1;
}

//native fgetc(file);
static cell AMX_NATIVE_CALL amx_fgetc(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	uint8_t val;

	if (fp->Read(&val, sizeof(val)) != sizeof(val))
	{
		return -1;
	}

	return static_cast<cell>(val);
}

//native fputc(file, data);
static cell AMX_NATIVE_CALL amx_fputc(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	uint8_t val = static_cast<uint8_t>(params[2]);

	if (fp->Write(&val, sizeof(val)) != sizeof(val))
	{
		return -1;
	}

	return val;
}

//native ungetc(file, data);
static cell AMX_NATIVE_CALL amx_ungetc(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	SystemFile* sysfile = fp->AsSystemFile();

	if (!sysfile)
	{
		LogError(amx, AMX_ERR_NATIVE, "Can not ungetc to file in the Valve file system");
		return  0;
	}

	return ungetc(static_cast<int>(params[2]), sysfile->handle());
}

// native rmdir(const path[]);
static cell AMX_NATIVE_CALL amx_rmdir(AMX *amx, cell *params)
{
	int length;
	const char* realpath = build_pathname("%s", get_amxstring(amx, params[1], 0, length));

	return rmdir(realpath) == 0;
}

// native mkdir(const dirname[], mode, bool:use_valve_fs = false, const valve_path_id[] = "GAMECONFIG");
static cell AMX_NATIVE_CALL amx_mkdir(AMX *amx, cell *params)
{
	int length;
	char *path = get_amxstring(amx, params[1], 0, length);

	size_t numParams = *params / sizeof(cell);

	if (numParams >= 3 && params[3] > 0)
	{
		const char* pathID = get_amxstring_null(amx, params[4], 1, length);

		if (g_FileSystem->IsDirectory(path))
		{
			return -1;
		}

		g_FileSystem->CreateDirHierarchy(path, pathID);

		if (g_FileSystem->IsDirectory(path))
		{
			return 0;
		}
	}

	const char* realpath = build_pathname("%s", path);

#if defined PLATFORM_WINDOWS

	return mkdir(realpath);
#else
	if (numParams >= 2)
	{
		return mkdir(realpath, params[2]);
	}

	return mkdir(realpath, 0755);
#endif
}

// native rename_file(const oldname[], const newname[], relative = 0);
static cell AMX_NATIVE_CALL rename_file(AMX *amx, cell *params)
{
	int length;
	char file_old_r[PLATFORM_MAX_PATH];
	char file_new_r[PLATFORM_MAX_PATH];

	const char* file_old = get_amxstring(amx, params[1], 0, length);
	const char* file_new = get_amxstring(amx, params[2], 1, length);

	if (params[0] / sizeof(cell) >= 3 && params[3] > 0)
	{
		build_pathname_r(file_old_r, sizeof(file_old_r), "%s", file_old);
		build_pathname_r(file_new_r, sizeof(file_new_r), "%s", file_new);
	}
	else
	{
		g_LibSys.PathFormat(file_old_r, sizeof(file_old_r) - 1, "%s", file_old);
		g_LibSys.PathFormat(file_new_r, sizeof(file_new_r) - 1, "%s", file_new);
	}

#if defined PLATFORM_POSIX

	return (rename(file_old_r, file_new_r) == 0);

#elif defined PLATFORM_WINDOWS

	return MoveFileA(file_old_r, file_new_r);
#endif
}

// native LoadFileForMe(const file[], buffer[], maxlength, &length = 0);
static cell LoadFileForMe(AMX *amx, cell *params)
{
	int length;
	char *file = get_amxstring(amx, params[1], 0, length);
	char path[PLATFORM_MAX_PATH];

	build_pathname_r(path, sizeof(path), "%s", file);

	byte* addr = LOAD_FILE_FOR_ME(path, &length);

	if (!addr)
	{
		return -1;
	}

	cell *buffer = get_amxaddr(amx, params[2]);
	cell maxlength = params[3];
	cell *bytes_avail = get_amxaddr(amx, params[4]);

	*bytes_avail = length;

	cell count;

	for (count = 0; count < length && count < maxlength; count++)
	{
		buffer[count] = addr[count];
	}

	FREE_FILE(addr);

	return count;
}

// native fflush(file);
static cell AMX_NATIVE_CALL amx_fflush(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (fp)
	{
		return fp->Flush();
	}

	return -1;
}

// native GetFileTime(const file[], FileTimeType:tmode);
static cell AMX_NATIVE_CALL GetFileTime(AMX *amx, cell *params)
{
	int length;
	const char* file = get_amxstring(amx, params[1], 0, length);

	time_t time_val;

	if (!g_LibSys.FileTime(build_pathname("%s", file), static_cast<FileTimeType>(params[2]), &time_val))
	{
		return -1;
	}

	return static_cast<cell>(time_val);
}

#define FPERM_U_READ       0x0100   /* User can read.    */
#define FPERM_U_WRITE      0x0080   /* User can write.   */
#define FPERM_U_EXEC       0x0040   /* User can exec.    */
#define FPERM_G_READ       0x0020   /* Group can read.   */
#define FPERM_G_WRITE      0x0010   /* Group can write.  */
#define FPERM_G_EXEC       0x0008   /* Group can exec.   */
#define FPERM_O_READ       0x0004   /* Anyone can read.  */
#define FPERM_O_WRITE      0x0002   /* Anyone can write. */
#define FPERM_O_EXEC       0x0001   /* Anyone can exec.  */

// native bool:SetFilePermissions(const path[], int mode);
static cell SetFilePermissions(AMX *amx, cell *params)
{
	int length;
	const char* realpath = build_pathname(get_amxstring(amx, params[1], 0, length));

#if defined PLATFORM_WINDOWS

	int mask = 0;

	if (params[2] & (FPERM_U_WRITE | FPERM_G_WRITE | FPERM_O_WRITE))
	{
		mask |= _S_IWRITE;
	}

	if (params[2] & (FPERM_U_READ | FPERM_G_READ | FPERM_O_READ | FPERM_U_EXEC | FPERM_G_EXEC | FPERM_O_EXEC))
	{
		mask |= _S_IREAD;
	}

	return _chmod(realpath, mask) == 0;
#else
	return chmod(realpath, params[2]) == 0;
#endif
}

template <typename T>
static cell File_ReadTyped(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	cell* data = get_amxaddr(amx, params[2]);

	T value;

	if (fp->Read(&value, sizeof(value)) != sizeof(value))
	{
		return 0;
	}

	*data = value;
	return 1;
}

template <typename T>
static cell File_WriteTyped(AMX *amx, cell *params)
{
	FileObject* fp = reinterpret_cast<FileObject*>(params[1]);

	if (!fp)
	{
		return 0;
	}

	T value = static_cast<T>(params[2]);

	return !!(fp->Write(&value, sizeof(value)) == sizeof(value));
}

AMX_NATIVE_INFO file_Natives[] =
{
	{"read_dir",		read_dir},
	{"read_file",		read_file},
	{"write_file",		write_file},
	{"rename_file",		rename_file},
	{"delete_file",		delete_file},
	{"unlink",			delete_file},

	{"file_exists",		file_exists},
	{"file_size",		file_size},
	{"filesize",		amx_filesize},

	{"fopen",			amx_fopen},
	{"fclose",			amx_fclose},

	{"fread",			amx_fread},
	{"fread_blocks",	amx_fread_blocks},
	{"fread_raw",		amx_fread_raw},

	{"fwrite",			amx_fwrite},
	{"fwrite_blocks",	amx_fwrite_blocks},
	{"fwrite_raw",		amx_fwrite_raw},

	{"feof",			amx_feof},
	{"fprintf",			amx_fprintf},
	{"fgets",			amx_fgets},
	{"fseek",			amx_fseek},
	{"ftell",			amx_ftell},
	{"fgetc",			amx_fgetc},
	{"fputc",			amx_fputc},
	{"fungetc",			amx_ungetc},
	{"fputs",			amx_fputs},
	{"fflush",			amx_fflush},

	{"build_pathname",	amx_build_pathname},

	{"dir_exists",		dir_exists},
	{"open_dir",		amx_open_dir},
	{"close_dir",		amx_close_dir},
	{"next_file",		amx_get_dir},
	{"rmdir",			amx_rmdir},
	{"mkdir",			amx_mkdir},

	{"LoadFileForMe",		LoadFileForMe},
	{"GetFileTime",			GetFileTime},
	{"SetFilePermissions",	SetFilePermissions},
	{"FileReadInt8",		File_ReadTyped<int8_t>},
	{"FileReadUint8",		File_ReadTyped<uint8_t>},
	{"FileReadInt16",		File_ReadTyped<int16_t>},
	{"FileReadUint16",		File_ReadTyped<uint16_t>},
	{"FileReadInt32",		File_ReadTyped<int32_t>},
	{"FileWriteInt8",		File_WriteTyped<int8_t>},
	{"FileWriteInt16",		File_WriteTyped<int16_t>},
	{"FileWriteInt32",		File_WriteTyped<int32_t>},

	{NULL,				NULL}
};
