#include <stdio.h>
#ifdef __linux__
#include <unistd.h>
#else
#include <fcntl.h>
#include <io.h>
#endif
#include <stdlib.h>
#include "zlib.h"
#include "amx.h"
#include "amxdbg.h"
#include "amxxpc.h"

void ReadFileIntoPl(abl *pl, FILE *fp);

int main(int argc, char **argv)
{
	struct abl pl32;
	struct abl pl64;

#ifdef _DEBUG
	printf("debug clamp\n");
	getchar();
#endif

#ifdef __linux__
	HINSTANCE lib = dlmount("./amxxpc32.so");
#else
	HINSTANCE lib = dlmount("amxxpc32.dll");
#endif
	if (!lib)
	{
#ifdef __linux__
		printf("32bit compiler failed to instantiate: %s\n", dlerror());
#else
		printf("32bit compiler failed to instantiate: %d\n", GetLastError());
#endif
		exit(0);
	}

	COMPILER sc32 = (COMPILER)dlsym(lib, "Compile32");
	PRINTF pc_printf = (PRINTF)dlsym(lib, "pc_printf");

	if (!sc32 || !pc_printf)
	{
#ifdef __linux__
		printf("32bit compiler failed to link: %p|%p.\n",sc32, sc_printf);
#else
		printf("32bit compiler failed to link: %d.\n", GetLastError());
#endif
		exit(0);
	}

	pc_printf("Welcome to the AMX Mod X %s Compiler.\n", VERSION_STRING);
	pc_printf("Copyright (c) 1997-2005 ITB CompuPhase, AMX Mod X Team\n\n");
	
	if (argc < 2)
	{
		pc_printf("Usage: <file.sma> [options]\n");
		pc_printf("Use -? or --help to see full options\n\n");
		getchar();
		exit(0);
	}

	if (!strcmp(argv[1], "-?") || !strcmp(argv[1], "--help"))
	{
		show_help();
		pc_printf("Press any key to continue.\n");
		getchar();
		exit(0);
	}

	sc32(argc, argv);

	char *file = FindFileName(argc, argv);

	if (file == NULL)
	{
		pc_printf("Could not locate the output file.\n");
		exit(0);
	} else if (strstr(file, ".asm")) {
		pc_printf("Assembler output succeeded.\n");
		exit(0);
	} else {
		FILE *fp = fopen(file, "rb");
		if (fp == NULL)
		{
			pc_printf("Could not locate output file %s (compile failed).\n", file);
			exit(0);
		}
		ReadFileIntoPl(&pl32, fp);
		pl32.cellsize = 4;
		fclose(fp);
	}

	unlink(file);

	HINSTANCE lib64 = 0;
#ifdef __linux__
	lib64 = dlmount("./amxxpc64.so");
#else
	lib64 = dlmount("amxxpc64.dll");
#endif
	if (!lib64)
	{
		pc_printf("64bit compiler failed to instantiate.\n");
		exit(0);
	}

	COMPILER sc64 = (COMPILER)dlsym(lib64, "Compile64");

	if (!sc64)
	{
#ifdef __linux__
		pc_printf("64bit compiler failed to link: %s.\n", dlerror());
#else
		pc_printf("64bit compiler failed to link: %d.\n", GetLastError());
#endif
		exit(0);
	}

	sc64(argc, argv);

	dlclose(lib64);

	if (file == NULL)
	{
		pc_printf("Could not locate the output file on second pass.\n");
		exit(0);
	} else {
		FILE *fp = fopen(file, "rb");
		if (fp == NULL)
		{
			pc_printf("Could not locate output file on second pass (compile failed).\n");
			exit(0);
		}
		ReadFileIntoPl(&pl64, fp);
		pl64.cellsize = 8;
		fclose(fp);
	}

	/////////////
	// COMPRSSION
	/////////////

	int err;

	pl32.cmpsize = compressBound(pl32.size);
	pl32.cmp = new char[pl32.cmpsize];
	err = compress((Bytef *)pl32.cmp, (uLongf *)&(pl32.cmpsize), (const Bytef*)pl32.data, pl32.size);
	
	if (err != Z_OK)
	{
		pc_printf("internal error - compression failed on first pass: %d\n", err);
		exit(0);
	}

	pl64.cmpsize = compressBound(pl64.size);
	pl64.cmp = new char[pl64.cmpsize];
	err = compress((Bytef *)pl64.cmp, (uLongf *)&(pl64.cmpsize), (const Bytef*)pl64.data, pl64.size);
	
	if (err != Z_OK)
	{
		pc_printf("internal error - compression failed on second pass: %d\n", err);
		exit(0);
	}

	char *newfile = new char[strlen(file)+3];
	strcpy(newfile, file);
	if (!strstr(file, ".amxx") && !strstr(file, ".AMXX"))
		strcat(newfile, "x");

	FILE *fp = fopen(newfile, "wb");
	if (!fp)
	{
		pc_printf("Error trying to write file %s.\n", newfile);
		exit(0);
	}

	//magic + archn
	int hdrsize = sizeof(long) + sizeof(char);
	int entry = sizeof(long) + sizeof(long) + sizeof(char);
	int offset1 = hdrsize + (entry * 2);
	int offset2 = offset1 + pl32.cmpsize;

	int magic = MAGIC_HEADER;
	fwrite((void *)&magic, sizeof(int), 1, fp);
	char n = 2;
	fwrite((void *)&n, sizeof(char), 1, fp);
	
	fwrite((void *)&(pl32.cellsize), sizeof(char), 1, fp);
	fwrite((void *)&(pl32.stp), sizeof(long), 1, fp);
	fwrite((void *)&(offset1), sizeof(long), 1, fp);
	fwrite((void *)&(pl64.cellsize), sizeof(char), 1, fp);
	fwrite((void *)&(pl64.stp), sizeof(long), 1, fp);
	fwrite((void *)&(offset2), sizeof(long), 1, fp);
	fwrite(pl32.cmp, sizeof(char), pl32.cmpsize, fp);
	fwrite(pl64.cmp, sizeof(char), pl64.cmpsize, fp);

	fclose(fp);

	unlink(file);

	pc_printf("Done.\n");

	dlclose(lib);

	exit(0);
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
	int max = 0, odirFlag = -1;

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
