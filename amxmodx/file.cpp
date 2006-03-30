/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

// intptr_t
#ifdef _MSC_VER
	typedef int intptr_t;
	#define _INTPTR_T_DEFINED
#endif

#ifdef __GNUC__
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#endif

// header file for unlink()
#ifdef __linux__
#include <unistd.h>
#else
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#endif

#include <extdll.h>
#include <meta_api.h>
#include "amxmodx.h"

CVector<FILE *> FileList;

class AutoFilePtr
{
	FILE *m_FP;
public:
	AutoFilePtr(FILE *fp) : m_FP(fp)
	{}
	
	~AutoFilePtr()
	{
		if (m_FP)
			fclose(m_FP);
	}
	
	operator FILE* ()
	{
		return m_FP;
	}
};

static cell AMX_NATIVE_CALL read_dir(AMX *amx, cell *params)
{
#ifdef __GNUC__
	int a;
	struct dirent *ep;
	DIR *dp;
	char* dirname = build_pathname("%s", get_amxstring(amx, params[1], 0, a));
	a = params[2];
	
	if ((dp = opendir (dirname)) == NULL)
		return 0;
	
	seekdir(dp, a);
	
	if ((ep = readdir (dp)) != NULL)
	{
		cell *length = get_amxaddr(amx, params[5]);
		*length = set_amxstring(amx, params[3], ep->d_name, params[4]);
		a = telldir(dp);
	} else
		a = 0;
	
	closedir (dp);
	
	return a;

#else
	int tmp;
	char *dirname = build_pathname("%s/*", get_amxstring(amx, params[1], 0, tmp));
	tmp = params[2];

	_finddata_t fd;
	intptr_t handle = _findfirst(dirname, &fd);
	
	if (handle < 0)
		return 0;

	++tmp;
	
	for (int i = 0; i < tmp; ++i)
	{
		if (_findnext(handle, &fd) < 0)
		{
			tmp = 0;
			break;
		}
	}
	
	// current data in fd
	cell *length = get_amxaddr(amx, params[5]);						// pointer to the outLen parameter
	*length = set_amxstring(amx, params[3], fd.name, params[4]);	// set output and outLen parameters
	_findclose(handle);

	return tmp;
#endif // __GNUC__
}

static cell AMX_NATIVE_CALL read_file(AMX *amx, cell *params) /* 5 param */
{
	int iLen;
	char* szFile = get_amxstring(amx, params[1], 0, iLen);
	FILE *fp;
	
	if ((fp =fopen(build_pathname("%s", szFile), "r")) == NULL)
	{
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	char buffor[1024];
	int i = 0, iLine = params[2];
	
	while ((i <= iLine) && fgets(buffor, 1023, fp))
		i++;
	
	fclose(fp);

	if (i > iLine)
	{
		int len = strlen(buffor);
		
		if (buffor[len - 1] == '\n')
			buffor[--len] = 0;
		
		if (buffor[len - 1] == '\r')
			buffor[--len] = 0;
		
		cell *length = get_amxaddr(amx, params[5]);
		*length = set_amxstring(amx, params[3], buffor, params[4]);
		
		return i;
	}
	
	return 0;
}

static cell AMX_NATIVE_CALL write_file(AMX *amx, cell *params) /* 3 param */
{
	int i;
	char* sFile = build_pathname("%s", get_amxstring(amx, params[1], 0, i));
	char* sText = get_amxstring(amx, params[2], 0, i);
	FILE* pFile;
	int iLine = params[3];

	// apending to the end
	if (iLine < 0)
	{
		if ((pFile = fopen(sFile, "a")) == NULL)
		{
			amx_RaiseError(amx, AMX_ERR_NATIVE);
			return 0;
		}
		
		fputs(sText, pFile);
		fputc('\n', pFile);
		fclose(pFile);
		
		return 1;
	}

	// creating a new file with a line in a middle
	if ((pFile = fopen(sFile, "r")) == NULL)
	{
		if ((pFile = fopen(sFile, "w")) == NULL)
		{
			amx_RaiseError(amx, AMX_ERR_NATIVE);
			return 0;
		}
		
		for (i = 0; i < iLine; ++i)
			fputc('\n', pFile);

		fputs(sText, pFile);
		fputc('\n', pFile);
		fclose(pFile);
		
		return 1;
	}

	// adding a new line in a middle of already existing file
	FILE* pTemp;
	char buffor[2048];

	if ((pTemp = tmpfile()) == NULL)
	{
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	for (i = 0; ; ++i)
	{
		if (i == iLine)
		{
			fgets(buffor, 2047, pFile);
			fputs(sText, pTemp);
			fputc('\n', pTemp);
		}
		else if (fgets(buffor, 2047, pFile))
		{
			fputs(buffor, pTemp);
		}
		else if (i < iLine)
		{
			fputc('\n', pTemp);
		}
		else
			break;
	}

	fclose(pFile);
	rewind(pTemp);

	// now rewrite because file can be now smaller...
	if ((pFile = fopen(sFile, "w")) == NULL)
	{
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	while (fgets(buffor, 2047, pTemp))
		fputs(buffor, pFile);

	fclose(pTemp);
	fclose(pFile);
	
	return 1;
}

static cell AMX_NATIVE_CALL delete_file(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	char* sFile = get_amxstring(amx, params[1], 0, iLen);
	
	return (unlink(build_pathname("%s", sFile)) ? 0 : 1);
}

static cell AMX_NATIVE_CALL file_exists(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	char *sFile = get_amxstring(amx, params[1], 0, iLen);
	char *file = build_pathname("%s", sFile);

#if defined WIN32 || defined _WIN32
	DWORD attr = GetFileAttributes(file);
	
	if (attr == INVALID_FILE_ATTRIBUTES)
		return 0;
	
	if (attr == FILE_ATTRIBUTE_DIRECTORY)
		return 0;
	
	return 1;
#else
	struct stat s;
	
	if (stat(file, &s) != 0)
		return 0;
	
	if (S_ISDIR(s.st_mode))
		return 0;
	
	return 1;
#endif
}

static cell AMX_NATIVE_CALL dir_exists(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	char *sFile = get_amxstring(amx, params[1], 0, iLen);
	char *file = build_pathname("%s", sFile);

	return DirExists(file) ? 1 : 0;
}

static cell AMX_NATIVE_CALL file_size(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	char* sFile = get_amxstring(amx, params[1], 0, iLen);
	AutoFilePtr fp(fopen(build_pathname("%s", sFile), "r"));
	
	if (fp != NULL)
	{
		if (params[0] < 2 || params[2] == 0)
		{
			fseek(fp, 0, SEEK_END);
			int size = ftell(fp);
			
			return size;
		}
		else if (params[2] == 1)
		{
			int a = 0,lines = 0;
			
			while (a != EOF)
			{
				++lines;
				while ((a = fgetc(fp)) != '\n' && a != EOF);
			}
			//int a, b = '\n';
			//while( (a = fgetc(fp)) != EOF ){
			//  if ( a == '\n')
			//    ++lines;
			//  b = a;
			//}
			//if ( b != '\n' )
			//  ++lines;
			return lines;
		}
		else if (params[2] == 2)
		{
			fseek(fp, -1, SEEK_END);
			
			if (fgetc(fp) == '\n')
				return 1;
			
			return 0;
		}
	}
	
	return -1;
}

static cell AMX_NATIVE_CALL amx_fopen(AMX *amx, cell *params)
{
	int len, j = -1;
	char *file = build_pathname("%s", get_amxstring(amx, params[1], 1, len));
	char *flags = get_amxstring(amx, params[2], 0, len);

	FILE *fp = fopen(file, flags);

	return (cell)fp;
}

static cell AMX_NATIVE_CALL amx_fwrite_blocks(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	cell *addr = get_amxaddr(amx, params[2]);
	size_t blocks = params[3];
	size_t btmp = blocks;
	cell mode = params[4];
	switch (mode)
	{
	case 1:
		{
			char *a = new char[blocks];
			char *ptr = a;
			while (btmp--)
				*a++ = static_cast<char>(*addr++);
			size_t res = fwrite(a, sizeof(char), blocks, fp);
			delete [] a;
			return res;
		}
	case 2:
		{
			short *a = new short[blocks];
			short *ptr = a;
			while (btmp--)
				*a++ = static_cast<short>(*addr++);
			size_t res = fwrite(a, sizeof(short), blocks, fp);
			delete [] a;
			return res;
		}
	case 4:
		{
			int *a = new int[blocks];
			int *ptr = a;
			while (btmp--)
				*a++ = static_cast<int>(*addr++);
			size_t res = fwrite(a, sizeof(int), blocks, fp);
			delete [] a;
			return res;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL amx_fwrite(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	size_t mode = params[3];
	switch (mode)
	{
	case 1:
		{
			char a = static_cast<char>(params[2]);
			return fwrite(&a, sizeof(char), 1, fp);
		}
	case 2:
		{
			short b = static_cast<short>(params[2]);
			return fwrite(&b, sizeof(short), 1, fp);
		}
	case 4:
		{
			int c = static_cast<int>(params[2]);
			return fwrite(&c, sizeof(short), 1, fp);
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL amx_fwrite_raw(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	cell *addr = get_amxaddr(amx, params[2]);
	return fwrite(addr, params[3], params[4], fp);
}

static cell AMX_NATIVE_CALL amx_fread_raw(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	cell *addr = get_amxaddr(amx, params[2]);
	size_t size = static_cast<cell>(params[3]);
	size_t blocks = static_cast<cell>(params[4]);
    
	return fread(addr, size, blocks, fp);
}

static cell AMX_NATIVE_CALL amx_fread(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	cell *addr = get_amxaddr(amx, params[2]);
	switch (params[3])
	{
	case 1:	//char
		{
			char a;
			size_t res = fread(&a, sizeof(char), 1, fp);
			*addr = static_cast<cell>(a);
			return res;
		}
	case 2:	//short
		{
			short a;
			size_t res = fread(&a, sizeof(short), 1, fp);
			*addr = static_cast<cell>(a);
			return res;
		}
	case 4:	//int
	default:
		{
			int a;
			size_t res = fread(&a, sizeof(int), 1, fp);
			*addr = static_cast<cell>(a);
			return res;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL amx_fread_blocks(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	cell *addr = get_amxaddr(amx, params[2]);
	size_t blocks = params[3];
	switch (params[3])
	{
	case 1:	//char
		{
			char *a = new char[blocks];
			char *ptr = a;
			size_t res = fread(a, sizeof(char), blocks, fp);
			while (blocks--)
				*addr++ = static_cast<cell>(*ptr++);
			delete [] a;
			return res;
		}
	case 2:	//short
		{
			short *a = new short[blocks];
			short *ptr = a;
			size_t res = fread(a, sizeof(short), blocks, fp);
			while (blocks--)
				*addr++ = static_cast<cell>(*ptr++);
			delete [] a;
			return res;
		}
	case 4:	//int
	default:
		{
			int *a = new int[blocks];
			int *ptr = a;
			size_t res = fread(a, sizeof(int), blocks, fp);
			while (blocks--)
				*addr++ = static_cast<cell>(*ptr++);
			delete [] a;
			return res;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL amx_fgets(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	static char buffer[4096];
	buffer[0] = '\0';
	fgets(buffer, sizeof(buffer)-1, fp);
	return set_amxstring(amx, params[2], buffer, params[3]);
}

static cell AMX_NATIVE_CALL amx_fseek(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	return fseek(fp, params[2], params[3]);
}

static cell AMX_NATIVE_CALL amx_ftell(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	return ftell(fp);
}

static cell AMX_NATIVE_CALL amx_fprintf(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	int len;
	char *str = format_amxstring(amx, params, 2, len);
	return fprintf(fp, "%s", str);
}

static cell AMX_NATIVE_CALL amx_feof(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 1;

	return feof(fp);
}

static cell AMX_NATIVE_CALL amx_fclose(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 1;

	fclose(fp);

	return 1;
}

static cell AMX_NATIVE_CALL amx_filesize(AMX *amx, cell *params)
{
	int len;
	char *file = build_pathname("%s", format_amxstring(amx, params, 1, len));
	long size;
	
	AutoFilePtr fp(fopen(file, "rb"));
	
	if (fp)
	{
		fseek(fp, 0, SEEK_END);
		size = ftell(fp);
		
		return size;
	}
	
	return -1;
}

static cell AMX_NATIVE_CALL amx_build_pathname(AMX *amx, cell *params)
{
	int len;
	char *szPath = get_amxstring(amx, params[1], 0, len);
	
	return set_amxstring(amx, params[2], build_pathname("%s", szPath), params[3]);
}

static cell AMX_NATIVE_CALL amx_open_dir(AMX *amx, cell *params)
{
	int len;
	char *path = get_amxstring(amx, params[1], 0, len);

#if defined WIN32 || defined _WIN32
	char *dirname = build_pathname("%s\\*", path);
	
	WIN32_FIND_DATA fd;
	HANDLE hFile = FindFirstFile(dirname, &fd);
	
	if (hFile == INVALID_HANDLE_VALUE)
		return 0;
	
	set_amxstring(amx, params[2], fd.cFileName, params[3]);
	
	return (DWORD)hFile;
#else
	char *dirname = build_pathname("%s", path);
	DIR *dp = opendir(dirname);
	
	if (!dp)
		return NULL;
	struct dirent *ep = readdir(dp);
	
	if (!ep)
	{
		closedir(dp);
		return NULL;
	}
	
	set_amxstring(amx, params[2], ep->d_name, params[3]);
	
	return (cell)dp;
#endif
}

static cell AMX_NATIVE_CALL amx_close_dir(AMX *amx, cell *params)
{
#if defined WIN32 || defined _WIN32
	HANDLE hFile = (HANDLE)((DWORD)params[1]);
	
	if (hFile == INVALID_HANDLE_VALUE || hFile == NULL)
		return 0;
	
	FindClose(hFile);
	
	return 1;
#else
	DIR *dp = (DIR *)params[1];
	
	if (!dp)
		return 0;
	
	closedir(dp);
	return 1;
#endif
}

static cell AMX_NATIVE_CALL amx_get_dir(AMX *amx, cell *params)
{
#if defined WIN32 || defined _WIN32
	HANDLE hFile = (HANDLE)((DWORD)params[1]);
	
	if (hFile == INVALID_HANDLE_VALUE || hFile == NULL)
		return 0;
	
	WIN32_FIND_DATA fd;
	
	if (!FindNextFile(hFile, &fd))
		return 0;
	
	set_amxstring(amx, params[2], fd.cFileName, params[3]);
	
	return 1;
#else
	DIR *dp = (DIR *)params[1];
	
	if (!dp)
		return 0;
	struct dirent *ep = readdir(dp);
	
	if (!ep)
		return 0;
	
	set_amxstring(amx, params[2], ep->d_name, params[3]);
	
	return 1;
#endif
}

//native fgetc( file );
static cell AMX_NATIVE_CALL amx_fgetc(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	return fgetc(fp);
}

//native fputc( file, data );
static cell AMX_NATIVE_CALL amx_fputc(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	return fputc(static_cast<int>(params[2]), fp);
}

//native ungetc( file, data );
static cell AMX_NATIVE_CALL amx_ungetc(AMX *amx, cell *params)
{
	FILE *fp = (FILE *)params[1];

	if (!fp)
		return 0;

	return ungetc(static_cast<int>(params[2]), fp);
}

#if defined __linux__
#define _rmdir rmdir
#endif

static cell AMX_NATIVE_CALL amx_rmdir(AMX *amx, cell *params)
{
	int len;
	char* sFile = build_pathname("%s", get_amxstring(amx, params[1], 0, len));

	if (_rmdir(sFile) != 0)
		return 0;

	return 1;
}

AMX_NATIVE_INFO file_Natives[] =
{
	{"delete_file",		delete_file},
	{"file_exists",		file_exists},
	{"file_size",		file_size},
	{"read_dir",		read_dir},
	{"read_file",		read_file},
	{"write_file",		write_file},
	//new, sane file natives
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
	{"filesize",		amx_filesize},
	{"unlink",			delete_file},
	{"build_pathname",	amx_build_pathname},
	{"dir_exists",		dir_exists},
	{"open_dir",		amx_open_dir},
	{"close_dir",		amx_close_dir},
	{"next_file",		amx_get_dir},
	{"fgetc",			amx_fgetc},
	{"fputc",			amx_fputc},
	{"fungetc",			amx_ungetc},
	{"rmdir",			amx_rmdir},
	{NULL,				NULL}
};
