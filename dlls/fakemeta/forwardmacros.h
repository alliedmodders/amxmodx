#ifndef FORWARDMACROS_H
#define FORWARDMACROS_H

#define SIMPLE_CONSTSTRING_HOOK_VOID(call) \
	const char* call () \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i))); \
		RETURN_META_VALUE(mswi(lastFmRes), mlStringResult); \
	} \
	const char* call##_post () \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i))); \
		RETURN_META_VALUE(MRES_IGNORED, mlStringResult); \
	}

		

#define SIMPLE_INT_HOOK_STRING(call) \
	int call (char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s)); \
		RETURN_META_VALUE(mswi(lastFmRes), mlCellResult); \
	} \
	int call##_post (char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	} 

#define SIMPLE_INT_HOOK_CONSTSTRING(call) \
	int call (const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s)); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post (const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	}
#define SIMPLE_CHAR_HOOK_STRING(call) \
	char call (char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s)); \
		RETURN_META_VALUE(mswi(lastFmRes), (char)mlCellResult); \
	} \
	char call##_post (char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s)); \
		RETURN_META_VALUE(MRES_IGNORED, (char)mlCellResult); \
	}
#define SIMPLE_VOID_HOOK_CONSTSTRING(call) \
	void call (const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s)); \
		RETURN_META(MRES_IGNORED); \
	}


#define SIMPLE_VOID_HOOK_EDICT(call) \
	void call (edict_t *ent) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(ent))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *ent) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(ent))); \
		RETURN_META(MRES_IGNORED); \
	} 
#define SIMPLE_EDICT_HOOK_VOID(call) \
	edict_t* call () \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i))); \
		RETURN_META_VALUE(mswi(lastFmRes),INDEXENT2((int)mlCellResult)); \
	} \
	edict_t* call##_post () \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i))); \
		RETURN_META_VALUE(MRES_IGNORED,INDEXENT2((int)mlCellResult)); \
	} 
#define SIMPLE_EDICT_HOOK_INT(call) \
	edict_t* call (int v) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),v)); \
		RETURN_META_VALUE(mswi(lastFmRes),INDEXENT2((int)mlCellResult)); \
	} \
	edict_t* call##_post (int v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),v)); \
		RETURN_META_VALUE(MRES_IGNORED,INDEXENT2((int)mlCellResult)); \
	} 

#define SIMPLE_VOID_HOOK_EDICT_EDICT(call) \
	void call (edict_t *ent,edict_t *entb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(ent), ENTINDEX(entb))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *ent,edict_t *entb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(ent), ENTINDEX(entb))); \
		RETURN_META(MRES_IGNORED); \
	} 

#define SIMPLE_VOID_HOOK_VOID(call) \
	void call (void) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (void) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i))); \
		RETURN_META(MRES_IGNORED); \
	} 

#define SIMPLE_INT_HOOK_EDICT(call) \
	int call (edict_t *pent) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(pent))); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post (edict_t *pent) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(pent))); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	}

#define SIMPLE_INT_HOOK_INT(call) \
	int call (int v) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v)); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post (int v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	}

#define SIMPLE_INT_HOOK_VOID(call) \
	int call () \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i))); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post () \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i))); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	}










#define ENGHOOK(pfnCall) \
	if (post) \
	{ \
		if (engtable->pfn##pfnCall == NULL) \
			engtable->pfn##pfnCall = pfnCall##_post; \
	} \
	else \
	{ \
		if (engtable->pfn##pfnCall == NULL) \
			engtable->pfn##pfnCall = pfnCall; \
	} 

#define DLLHOOK(pfnCall) \
	if (post) \
	{ \
		if (dlltable->pfn##pfnCall == NULL) \
			dlltable->pfn##pfnCall = pfnCall##_post; \
	} \
	else \
	{ \
		if (dlltable->pfn##pfnCall == NULL) \
			dlltable->pfn##pfnCall = pfnCall; \
	} 

#define FM_ENG_HANDLE(pfnCall, pfnArgs) \
	register unsigned int i = 0; \
	clfm(); \
	int fmres = FMRES_IGNORED; \
	for (i=0; i<Engine[pfnCall].size(); i++) \
	{ \
		fmres = MF_ExecuteForward pfnArgs; \
		if (fmres >= lastFmRes) { \
			if (retType == FMV_STRING) \
				mlStringResult = mStringResult; \
			else if (retType == FMV_CELL) \
				mlCellResult = mCellResult; \
			else if (retType == FMV_FLOAT) \
				mlFloatResult = mFloatResult; \
			lastFmRes = fmres; \
		} \
	}
#define FM_ENG_HANDLE_POST(pfnCall, pfnArgs) \
	register unsigned int i = 0; \
	clfm(); \
	int fmres = FMRES_IGNORED; \
	for (i=0; i<EnginePost[pfnCall].size(); i++) \
	{ \
		fmres = MF_ExecuteForward pfnArgs; \
		if (fmres >= lastFmRes) { \
			if (retType == FMV_STRING) \
				mlStringResult = mStringResult; \
			else if (retType == FMV_CELL) \
				mlCellResult = mCellResult; \
			else if (retType == FMV_FLOAT) \
				mlFloatResult = mFloatResult; \
			lastFmRes = fmres; \
		} \
	}



#endif // FORWARDMACROS_H