// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_CFILESYSTEM_H_
#define _INCLUDE_CFILESYSTEM_H_

#include <FileSystem.h> // IFileSystem, FileSystemSeek_t, FileHandle_t (HLSDK)
#include <stdio.h>      // FILE*

extern IFileSystem* g_FileSystem;

class ValveFile;
class SystemFile;

class FileObject
{
	public:

		virtual ~FileObject() {};

		virtual size_t Read(void* pOut, size_t size) = 0;
		virtual char* ReadLine(char* pOut, size_t size) = 0;
		virtual size_t Write(const void* pData, size_t size) = 0;

		virtual bool Seek(int pos, int seek_type) = 0;
		virtual int Tell() = 0;
		virtual int Flush() = 0;

		virtual bool HasError() = 0;

		virtual bool EndOfFile() = 0;
		virtual void Close() = 0;

		virtual ValveFile *AsValveFile()
		{
			return nullptr;
		}

		virtual SystemFile *AsSystemFile()
		{
			return nullptr;
		}
};

class ValveFile : public FileObject
{
	public:

		ValveFile(FileHandle_t handle) : handle_(handle) {}

		~ValveFile()
		{
			Close();
		}

		static bool Exists(const char* file)
		{
			return g_FileSystem->FileExists(file);
		}

		static ValveFile* Open(const char* filename, const char* mode, const char* pathID)
		{
			FileHandle_t handle = g_FileSystem->OpenFromCacheForRead(filename, mode, pathID);

			if (!handle)
			{
				return nullptr;
			}

			return new ValveFile(handle);
		}

		static bool Delete(const char* filename, const char* pathID)
		{
			if (!Exists(filename))
			{
				return false;
			}

			g_FileSystem->RemoveFile(filename, pathID);

			if (Exists(filename))
			{
				return false;
			}

			return true;
		}

		size_t Read(void* pOut, size_t size) override
		{
			return static_cast<size_t>(g_FileSystem->Read(pOut, size, handle_));
		}

		char* ReadLine(char* pOut, size_t size) override
		{
			return g_FileSystem->ReadLine(pOut, size, handle_);
		}

		size_t Write(const void* pData, size_t size) override
		{
			return static_cast<size_t>(g_FileSystem->Write(pData, size, handle_));
		}

		bool Seek(int pos, int seek_type) override
		{
			g_FileSystem->Seek(handle_, pos, static_cast<FileSystemSeek_t>(seek_type));
			return !HasError();
		}

		int Tell() override
		{
			return g_FileSystem->Tell(handle_);
		}

		bool HasError() override
		{
			return !handle_ || !g_FileSystem->IsOk(handle_);
		}

		int Flush() override
		{
			g_FileSystem->Flush(handle_);
			return 0;
		}

		bool EndOfFile() override
		{
			return g_FileSystem->EndOfFile(handle_);
		}

		void Close() override
		{
			if (handle_)
			{
				g_FileSystem->Close(handle_);
				handle_ = nullptr;;
			}
		}

		virtual ValveFile* AsValveFile()
		{
			return this;
		}

		FileHandle_t handle() const
		{
			return handle_;
		}

	private:

		FileHandle_t handle_;
};


class SystemFile : public FileObject
{
	public:

		SystemFile(FILE* fp) : fp_(fp) {}

		~SystemFile()
		{
			Close();
		}

		static SystemFile* Open(const char* path, const char* mode)
		{
			FILE* fp = fopen(path, mode);

			if (!fp)
			{
				return nullptr;
			}

			return new SystemFile(fp);
		}

		static bool Delete(const char* path)
		{
			return unlink(path) == 0;
		}

		size_t Read(void* pOut, size_t size) override
		{
			return fread(pOut, 1, size, fp_);
		}

		char* ReadLine(char* pOut, size_t size) override
		{
			return fgets(pOut, size, fp_);
		}

		size_t Write(const void* pData, size_t size) override
		{
			return fwrite(pData, 1, size, fp_);
		}

		bool Seek(int pos, int seek_type) override
		{
			return fseek(fp_, pos, seek_type) == 0;
		}

		int Tell() override
		{
			return ftell(fp_);
		}

		bool HasError() override
		{
			return ferror(fp_) != 0;
		}

		int Flush() override
		{
			return fflush(fp_);
		}

		bool EndOfFile() override
		{
			return feof(fp_) != 0;
		}

		void Close() override
		{
			if (fp_)
			{
				fclose(fp_);
				fp_ = nullptr;
			}
		}

		virtual SystemFile* AsSystemFile()
		{
			return this;
		}

		FILE* handle() const
		{
			return fp_;
		}

	private:

		FILE* fp_;
};

#endif // _INCLUDE_CFILESYSTEM_H_
