#pragma once

#include <eiface.h>

#ifndef DLLEXPORT
#ifdef _WIN32
#define DLLEXPORT	__declspec(dllexport)
#else
#define DLLEXPORT	__attribute__((visibility("default")))
#endif // _WIN32
#endif // DLLEXPORT

#ifndef C_DLLEXPORT
#define C_DLLEXPORT extern "C" DLLEXPORT
#endif

#ifndef _WIN32
#define WINAPI
#endif

typedef void (*FN_GAMEINIT)();

// Typedefs for these are provided in SDK engine/eiface.h, but I didn't
// like the names (APIFUNCTION, APIFUNCTION2, NEW_DLL_FUNCTIONS_FN).
typedef int (*GETENTITYAPI_FN)(DLL_FUNCTIONS* pFunctionTable, int interfaceVersion);
typedef int (*GETENTITYAPI2_FN)(DLL_FUNCTIONS* pFunctionTable, int* interfaceVersion);
typedef int (*GETNEWDLLFUNCTIONS_FN)(NEW_DLL_FUNCTIONS* pFunctionTable, int* interfaceVersion);

C_DLLEXPORT int GetEntityAPI(DLL_FUNCTIONS* pFunctionTable, int interfaceVersion);
C_DLLEXPORT int GetEntityAPI2(DLL_FUNCTIONS* pFunctionTable, int* interfaceVersion);
C_DLLEXPORT int GetNewDLLFunctions(NEW_DLL_FUNCTIONS* pNewFunctionTable, int* interfaceVersion);

#ifdef METAMOD_CORE
void compile_gamedll_callbacks();
void disable_clientcommand_fwd();
#endif
