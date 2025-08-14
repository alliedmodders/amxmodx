#pragma once

// Our GiveFnptrsToDll, called by engine.
typedef void (WINAPI *GIVE_ENGINE_FUNCTIONS_FN)(enginefuncs_t *pengfuncsFromEngine, globalvars_t *pGlobals);
C_DLLEXPORT void WINAPI GiveFnptrsToDll(enginefuncs_t *pengfuncsFromEngine, globalvars_t *pGlobals);
