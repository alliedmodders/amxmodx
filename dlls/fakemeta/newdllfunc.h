#ifndef _NEWDLLFUNC_INCLUDE_H
#define _NEWDLLFUNC_INCLUDE_H

#include "fakemeta_amxx.h"

enum {
	NEWDLLFunc_OnFreeEntPrivateData,	// void  )			(edict_t *pEnt);
	NEWDLLFunc_GameShutdown,			// void  )			(void);
	NEWDLLFunc_ShouldCollide			// int  )			(edict_t *pentTouched, edict_t *pentOther);
};

#endif //_NEWDLLFUNC_INCLUDE_H

