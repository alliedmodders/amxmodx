#ifndef _JUDYINC_INCLUDED
#define _JUDYINC_INCLUDED

#include "osdefs.h"
//#include <extdll.h>

#ifndef __linux__
#define JU_WIN
#endif

#ifdef __WIN32__
#define JU_WIN
#endif

#define JUDYERROR_NOTEST 1
#include "Judy.h"

#ifdef __GNUC__
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#endif

#ifdef __linux__
#include <unistd.h>
#else
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#endif

#include <stdio.h>
#include <string.h>

#include "amxxmodule.h"

#include "JudyVar.h"
#include "JudyVec.h"
#include "JudyEx.h"

#include "Capsule.h"

#endif
