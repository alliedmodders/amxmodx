//========= Copyright © 1996-2002, Valve LLC, All rights reserved. ============
//
// Purpose: 
//
// $NoKeywords: $
//=============================================================================

#if !defined( SYS_DEDH )
#define SYS_DEDH
#ifdef _WIN32
#ifndef __MINGW32__
#pragma once
#endif /* not __MINGW32__ */
#endif

#if defined _MSC_VER && _MSC_VER >= 1400
	#ifndef _CRT_SECURE_NO_DEPRECATE
		#define _CRT_SECURE_NO_DEPRECATE
	#endif

	#pragma warning(disable: 4996) // deprecated functions
#endif

typedef void (*SleepType)(int);
long Sys_LoadLibrary( char *lib );
void Sys_FreeLibrary( long library );
void *Sys_GetProcAddress( long library, const char *name );
void Sys_Printf(char *fmt, ...);
void Sys_ErrorMessage( int level, const char *msg );

#endif // SYS_DEDH
