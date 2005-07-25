#ifndef _AMXXSC_INCLUDE_H
#define _AMXXSC_INCLUDE_H

#define VERSION_STRING		"1.50-300"
#define VERSION				03000
#define MAGIC_HEADER		0x414D5842

#ifdef __linux__
# include <dlfcn.h>
#else
# include <windows.h>
#endif

#include <string.h>

#ifdef __linux__
# define dlmount(x)		dlopen(x, RTLD_NOW)
  typedef void*			HINSTANCE;
#else
# define dlsym(x, s)	GetProcAddress(x, s)
# define dlmount(x)		LoadLibrary(x)
# define dlclose(x)		FreeLibrary(x)
#endif

#include "zlib.h"

typedef int	(*COMPILER)(int argc, char **argv);
typedef int (*PRINTF)(const char *message, ...);

char *FindFileName(int argc, char **argv);
char *swiext(const char *file, const char *ext);
void show_help();


struct ablhdr
{
	int magic;
	char size;
};

struct abl
{
	long stp;
	char cellsize;
	int size;
	long cmpsize;
	char *data;
	char *cmp;
};

#endif //_AMXXSC_INCLUDE_H
