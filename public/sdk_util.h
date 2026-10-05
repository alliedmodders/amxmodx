#pragma once

// We're not including the DBG_EntOfVars and DBG_AssertFunction routines
// mentioned in the SDK util.h, so we're going to unset DEBUG here so that
// we don't get "unresolved symbol" errors.
#ifdef DEBUG
#undef DEBUG
#endif

// Inlcude local enginecallbacks wrapper *first* so that the g_engfuncs
// type is correct and the <enginecallback.h> header protection is already
// defined.
#include "enginecallbacks.h"
#include <util.h>

char* UTIL_VarArgs(const char* format, ...);
void UTIL_HudMessage(edict_t* pEntity, const hudtextparms_t& textparms, const char* pMessage);
