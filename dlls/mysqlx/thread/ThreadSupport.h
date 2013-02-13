#ifndef _INCLUDE_SOURCEMOD_THREAD_SUPPORT_H
#define _INCLUDE_SOURCEMOD_THREAD_SUPPORT_H

#if defined __linux__ || defined __APPLE__
#include "PosixThreads.h"
#elif defined WIN32
#include "WinThreads.h"
#endif

#endif //_INCLUDE_SOURCEMOD_THREAD_SUPPORT_H
