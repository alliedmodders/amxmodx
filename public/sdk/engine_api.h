#pragma once

struct enginefuncs_s;

// Plugin's GetEngineFunctions, called by metamod.
typedef int (*GET_ENGINE_FUNCTIONS_FN)(enginefuncs_s* pengfuncsFromEngine, int* interfaceVersion);

// According to SDK engine/eiface.h:
// ONLY ADD NEW FUNCTIONS TO THE END OF THIS STRUCT. INTERFACE VERSION IS FROZEN AT 138
#define ENGINE_INTERFACE_VERSION 138

// Protect against other projects which use this include file but use the
// normal enginefuncs_t type for their meta_engfuncs.
#ifdef METAMOD_CORE
extern enginefuncs_t g_meta_engfuncs;

void compile_engine_callbacks();
#else
	extern enginefuncs_t meta_engfuncs;
#endif
