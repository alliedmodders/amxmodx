//========= Copyright © 1996-2002, Valve LLC, All rights reserved. ============
//
// Purpose: 
//
// $NoKeywords: $
//=============================================================================

#if !defined ( PROCINFOH )
#define PROCINFOH
#ifdef _WIN32
#ifndef __MINGW32__
#pragma once
#endif /* not __MINGW32__ */
#endif

int		PROC_GetSpeed( void );
int		PROC_IsMMX( void );

#endif // PROCINFOH
