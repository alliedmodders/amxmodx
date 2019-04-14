// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <stdio.h>
#if defined(__linux__) | defined (__APPLE__)
#include <unistd.h>
#else
#include <fcntl.h>
#include <io.h>
#endif
#include <stdlib.h>
#include "zlib/zlib.h"
#include "amx.h"
#include "amxdbg.h"
#include "amxxpc.h"
#include "Binary.h"

#ifdef _MSC_VER
	// MSVC8 - replace POSIX functions with ISO C++ conformant ones as they are deprecated
	#if _MSC_VER >= 1400
		#define unlink _unlink	
	#endif
#endif

void ReadFileIntoPl(abl *pl, FILE *fp);
bool CompressPl(abl *pl);
void Pl2Bh(abl *pl, BinPlugin *bh);
void WriteBh(BinaryWriter *bw, BinPlugin *bh);

#if defined(EMSCRIPTEN)
extern "C" void Compile32(int argc, char **argv);
extern "C" int pc_printf(const char *message,...);
#else
static PRINTF pc_printf = NULL;
#endif

int main(int argc, char **argv)
{
	struct abl pl32;

#if defined(EMSCRIPTEN)
        COMPILER sc32 = (COMPILER)Compile32;
#else
# if defined(__linux__)
	HINSTANCE lib = NULL;
	if (FileExists("./amxxpc32.so"))
		lib = dlmount("./amxxpc32.so");
	else
		lib = dlmount("amxxpc32.so");
# elif defined(__APPLE__)
	HINSTANCE lib = dlmount("amxxpc32.dylib");
# else
	HINSTANCE lib = dlmount("amxxpc32.dll");
# endif
	if (!lib)
	{
# if defined(__linux__) || defined(__APPLE__)
		printf("compiler failed to instantiate: %s\n", dlerror());
# else
		printf("compiler failed to instantiate: %d\n", GetLastError());
# endif
		exit(EXIT_FAILURE);
	}

	COMPILER sc32 = (COMPILER)dlsym(lib, "Compile32");
	pc_printf = (PRINTF)dlsym(lib, "pc_printf");
#endif //EMSCRIPTEN

	if (!sc32 || !pc_printf)
	{
#if defined(__linux__) || defined(__APPLE__)
		printf("compiler failed to link: %p.\n",sc32);
#else
		printf("compiler failed to link: %d.\n", GetLastError());
#endif
		exit(EXIT_FAILURE);
	}

	pc_printf("AMX Mod X Compiler %s\n", AMXX_VERSION);
	pc_printf("Copyright (c) 1997-2006 ITB CompuPhase\n");
        pc_printf("Copyright (c) 2004-2013 AMX Mod X Team\n\n");
	
	if (argc < 2)
	{
		pc_printf("Usage: <file.sma> [options]\n");
		pc_printf("Use -? or --help to see full options\n\n");
		getchar();
		exit(EXIT_FAILURE);
	}

	if (!strcmp(argv[1], "-?") || !strcmp(argv[1], "--help"))
	{
		show_help();
		pc_printf("Press any key to continue.\n");
		getchar();
		exit(EXIT_SUCCESS);
	}

	sc32(argc, argv);

	char *file = FindFileName(argc, argv);

	if (file == NULL)
	{
		pc_printf("Could not locate the output file.\n");
		exit(EXIT_FAILURE);
	} else if (strstr(file, ".asm")) {
		pc_printf("Assembler output succeeded.\n");
		exit(EXIT_SUCCESS);
	} else {
		FILE *fp = fopen(file, "rb");
		if (fp == NULL)
		{
			pc_printf("Could not locate output file %s (compile failed).\n", file);
			exit(EXIT_FAILURE);
		}
		ReadFileIntoPl(&pl32, fp);
		pl32.cellsize = 4;
		fclose(fp);
	}

	unlink(file);

	/////////////
	// COMPRSSION
	/////////////

	CompressPl(&pl32);

	char *newfile = new char[strlen(file)+3];
	strcpy(newfile, file);
	if (!strstr(file, ".amxx") && !strstr(file, ".AMXX"))
		strcat(newfile, "x");

	FILE *fp = fopen(newfile, "wb");
	if (!fp)
	{
		pc_printf("Error trying to write file %s.\n", newfile);
		exit(EXIT_FAILURE);
	}

	BinPlugin bh32;
	
	Pl2Bh(&pl32, &bh32);

	try
	{

	static const int kEntries = 1;

	//entry is 4 ints and a byte
	static const int kEntrySize = (sizeof(int32_t) * 4) + sizeof(int8_t);

	BinaryWriter bw(fp);

	bw.WriteUInt32(MAGIC_HEADER2);
	bw.WriteUInt16(MAGIC_VERSION);
	bw.WriteUInt8(kEntries);

	//base header
	int baseaddr = sizeof(int32_t) + sizeof(int16_t) + sizeof(int8_t);
	//extend this by the entries we have
	baseaddr += kEntrySize * kEntries;

	bh32.offs = baseaddr;
	
	WriteBh(&bw, &bh32);
	bw.WriteChars(pl32.cmp, pl32.cmpsize);
	} catch (...) {
		fclose(fp);
		unlink(file);
		pc_printf("Error, failed to write binary\n");
#if !defined EMSCRIPTEN
		dlclose(lib);
#endif
		exit(EXIT_FAILURE);
	}

	fclose(fp);

	unlink(file);
	
	/*
	Without "Done" message "Compile and start Half-Life"
	and "Compile and upload" buttons in AMXX-Studio doesn't work.
	*/
	pc_printf("Done.\n");
#if !defined EMSCRIPTEN
	dlclose(lib);
#endif

	exit(EXIT_SUCCESS);
}

void WriteBh(BinaryWriter *bw, BinPlugin *bh)
{
	bw->WriteUInt8(bh->cellsize);
	bw->WriteUInt32(bh->disksize);
	bw->WriteUInt32(bh->imagesize);
	bw->WriteUInt32(bh->memsize);
	bw->WriteUInt32(bh->offs);
}

void Pl2Bh(abl *pl, BinPlugin *bh)
{
	bh->cellsize = pl->cellsize;
	bh->disksize = pl->cmpsize;
	bh->imagesize = pl->size;
	bh->memsize = pl->stp;
}

bool CompressPl(abl *pl)
{
	pl->cmpsize = compressBound(pl->size);
	pl->cmp = new char[pl->cmpsize];

	int err = compress((Bytef *)(pl->cmp), (uLongf *)&(pl->cmpsize), (const Bytef *)(pl->data), pl->size);

	delete [] pl->data;
	pl->data = NULL;

	if (err != Z_OK)
	{
		pc_printf("internal error - compression failed on first pass: %d\n", err);
		exit(EXIT_FAILURE);
	}

	return true;
}

//As of Small 3.0, there's extra debug info in the file we need to get out.
//Sadly this is placed somewhere really inconvenient and I'm mad.
void ReadFileIntoPl(abl *pl, FILE *fp)
{
	AMX_HEADER hdr;
	AMX_DBG_HDR dbg;
	fread(&hdr, sizeof(hdr), 1, fp);
	amx_Align32((uint32_t *)&hdr.stp);
	amx_Align32((uint32_t *)&hdr.size);
	pl->stp = hdr.stp;
	int size = hdr.size;
	if (hdr.flags & AMX_FLAG_DEBUG)
	{
		fseek(fp, hdr.size, SEEK_SET);
		fread(&dbg, sizeof(dbg), 1, fp);
		size += dbg.size;
	}
	pl->size = size;
	pl->data = new char[size];
	rewind(fp);
	fread(pl->data, 1, size, fp);
}

//we get the full name of the file here
//our job is to a] switch the .sma extension to .amx
// and to b] strip everything but the trailing name
char *swiext(const char *file, const char *ext, int isO)
{
	int i = 0, pos = -1, j = 0;
	int fileLen = strlen(file);
	int extLen = strlen(ext);
	int odirFlag = -1;

	for (i=fileLen-1; i>=0; i--)
	{
		if (file[i] == '.' && pos == -1)
		{
			pos = i+1;
		}
		if ((file[i] == '/' || file[i] == '\\') && !isO)
		{
			odirFlag = i+1;
			//realign pos - we've just stripped fileLen-i chars
			pos -= i + 1;
			break;
		}
	}

	char *newbuffer = new char[fileLen+strlen(ext)+2];
	fileLen += strlen(ext);
	if (odirFlag == -1)
	{
		strcpy(newbuffer, file);
	} else {
		strcpy(newbuffer, &(file[odirFlag]));
	}

	if (pos > -1)
	{
		for (i=pos; i<fileLen; i++)
		{
			if (j < extLen)
				newbuffer[i] = ext[j++];
			else
				break;
		}
		newbuffer[i] = '\0';
	} else {
		strcat(newbuffer, ".");
		strcat(newbuffer, ext);
	}

	return newbuffer;
}

char *FindFileName(int argc, char **argv)
{
	int i = 0;
	int save = -1;
	for (i=1; i<argc; i++)
	{
		if (argv[i][0] == '-' && argv[i][1] == 'o')
		{
			if (argv[i][2] == ' ' || argv[i][2] == '\0')
			{
				if (i == argc-1)
					return NULL;
				return swiext(&argv[i+1][2], "amx", 1);
			} else {
				return swiext(&(argv[i][2]), "amx", 1);
			}
		}
		if (argv[i][0] != '-')
		{
			save = i;
		}
	}

	if (save>0)
	{
		return swiext(argv[save], "amx", 0);
	}

	return NULL;
}

void show_help()
{
	printf("Options:\n");
	printf("\t-A<num>  alignment in bytes of the data segment and the stack\n");
	printf("\t-a       output assembler code\n");
	printf("\t-C[+/-]  compact encoding for output file (default=-)\n");
	printf("\t-c<name> codepage name or number; e.g. 1252 for Windows Latin-1\n");
	printf("\t-Dpath   active directory path\n");
	printf("\t-d0      no symbolic information, no run-time checks\n");
	printf("\t-d1      [default] run-time checks, no symbolic information\n");
	printf("\t-d2      full debug information and dynamic checking\n");
	printf("\t-d3      full debug information, dynamic checking, no optimization\n");
	printf("\t-e<name> set name of error file (quiet compile)\n");
	printf("\t-H<hwnd> window handle to send a notification message on finish\n");
	printf("\t-i<name> path for include files\n");
	printf("\t-l       create list file (preprocess only)\n");
	printf("\t-o<name> set base name of output file\n");
	printf("\t-p<name> set name of \"prefix\" file\n");
	printf("\t-r[name] write cross reference report to console or to specified file\n");
}

#if defined(__linux__) || defined(__APPLE__)
bool FileExists(const char *file)
{
	FILE *fp = fopen(file, "rb");
	if (!fp)
		return false;
	fclose(fp);
	return true;
}
#endif

