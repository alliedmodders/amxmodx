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

#define SIMPLE_CONSTSTRING_HOOK_INT(call) \
	const char* call (int v) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),v)); \
		RETURN_META_VALUE(mswi(lastFmRes), mlStringResult); \
	} \
	const char* call##_post (int v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),v)); \
		RETURN_META_VALUE(MRES_IGNORED, mlStringResult); \
	}

#define SIMPLE_CONSTSTRING_HOOK_CONSTEDICT(call) \
	const char* call (const edict_t *e) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX((edict_t*)e))); \
		RETURN_META_VALUE(mswi(lastFmRes), mlStringResult); \
	} \
	const char* call##_post (const edict_t *e) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX((edict_t*)e))); \
		RETURN_META_VALUE(MRES_IGNORED, mlStringResult); \
	}
#define SIMPLE_CONSTSTRING_HOOK_CONSTEDICT_CONSTSTRING(call) \
	const char* call (const edict_t *e, const char *c) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX((edict_t*)e),c)); \
		RETURN_META_VALUE(mswi(lastFmRes), mlStringResult); \
	} \
	const char* call##_post (const edict_t *e, const char *c) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX((edict_t*)e),c)); \
		RETURN_META_VALUE(MRES_IGNORED, mlStringResult); \
	}
#define SIMPLE_VOID_HOOK_CONSTEDICT_INT_INT_INT_INT(call) \
	void call (const edict_t *e, int v, int vb, int vc, int vd) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX((edict_t*)e),v,vb,vc,vd)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const edict_t *e, int v, int vb, int vc, int vd) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX((edict_t*)e),v,vb,vc,vd)); \
		RETURN_META(MRES_IGNORED); \
	}
#define SIMPLE_VOID_HOOK_INT_STRING_STRING_STRING(call) \
	void call (int v,char *c, char *cb, char *cc) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),v,c,cb,cc)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v, char *c, char *cb, char *cc) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),v,c,cb,cc)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_STRING_STRING_STRING(call) \
	void call (char *c, char *cb, char *cc) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),c,cb,cc)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (char *c, char *cb, char *cc) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),c,cb,cc)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_STRING_HOOK_STRING_STRING(call) \
	char* call (char *c, char *cb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),c,cb)); \
		RETURN_META_VALUE(mswi(lastFmRes), (char*)mlStringResult); \
	} \
	char* call##_post (char *c, char *cb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),c,cb)); \
		RETURN_META_VALUE(MRES_IGNORED, (char*)mlStringResult); \
	}
#define SIMPLE_CONSTSTRING_HOOK_EDICT(call) \
	const char* call (edict_t *e) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX(e))); \
		RETURN_META_VALUE(mswi(lastFmRes), mlStringResult); \
	} \
	const char* call##_post (edict_t *e) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX(e))); \
		RETURN_META_VALUE(MRES_IGNORED, mlStringResult); \
	}
#define SIMPLE_VOID_HOOK_CONSTEDICT_CONSTSTRING_CONSTSTRING(call) \
	void call (const edict_t *e, const char *c, const char *cb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX((edict_t*)e),c,cb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const edict_t *e, const char *c, const char *cb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX((edict_t*)e),c,cb)); \
		RETURN_META(MRES_IGNORED); \
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

#define SIMPLE_EDICT_HOOK_CONSTSTRING(call) \
	edict_t* call (const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s)); \
		RETURN_META_VALUE(mswi(lastFmRes), INDEXENT2((int)mlCellResult)); \
	} \
	edict_t* call##_post (const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s)); \
		RETURN_META_VALUE(MRES_IGNORED, INDEXENT2((int)mlCellResult)); \
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

#define SIMPLE_VOID_HOOK_STRING_STRING(call) \
	void call (char *s, char *sb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s, sb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (char *s, char *sb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s, sb)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_INT_CONSTSTRING(call) \
	void call (int v, const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), v, s)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v,const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), v, s)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_CONSTSTRING_FLOAT(call) \
	void call (const char *s, float f) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s, f)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const char *s, float f) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s, f)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_INT_HOOK_CONSTSTRING_INT(call) \
	int call (const char *s, int v) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s, v)); \
		RETURN_META_VALUE(mswi(lastFmRes),(int)mlCellResult); \
	} \
	int call##_post (const char *s, int v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s, v)); \
		RETURN_META_VALUE(MRES_IGNORED,(int)mlCellResult); \
	}

#define SIMPLE_VOID_HOOK_CONSTSTRING_CONSTSTRING(call) \
	void call (const char *s,const char *sb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), s, sb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const char *s, const char *sb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), s, sb)); \
		RETURN_META(MRES_IGNORED); \
	}


#define SIMPLE_USHORT_HOOK_INT_CONSTSTRING(call) \
	unsigned short call (int v, const char *s) \
	{ \
	printf("Addr: %d\n",s); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), v, s)); \
		RETURN_META_VALUE(mswi(lastFmRes),(unsigned short)mlCellResult); \
	} \
	unsigned short call##_post (int v,const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), v, s)); \
		RETURN_META_VALUE(MRES_IGNORED,(unsigned short)mlCellResult); \
	}

#define SIMPLE_VOID_HOOK_INT_STRING(call) \
	void call (int v, char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), v, s)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v,char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), v, s)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_INT(call) \
	void call (int v) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), v)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), v)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_FLOAT(call) \
	void call (float v) \
	{ \
		PREPARE_FLOAT(v); \
		printf("[before] v is now: %f\n",v); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), c_v)); \
		BYREF_FLOAT(v); \
		printf("[after] v is now: %f\n",v); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (float v) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), v)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_CONSTEDICT_FLOAT(call) \
	void call (const edict_t *ent, float blah) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX((edict_t*)ent), amx_ftoc(blah))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const edict_t *ent, float blah) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX((edict_t*)ent), amx_ftoc(blah))); \
		RETURN_META(MRES_IGNORED); \
	} 
#define SIMPLE_VOID_HOOK_CONSTEDICT_FLOAT_FLOAT(call) \
	void call (const edict_t *ent, float blah, float blahb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX((edict_t*)ent), amx_ftoc(blah), amx_ftoc(blahb))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const edict_t *ent, float blah, float blahb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX((edict_t*)ent), amx_ftoc(blah), amx_ftoc(blahb))); \
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

#define SIMPLE_EDICT_HOOK_EDICT(call) \
	edict_t* call (edict_t *e) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),ENTINDEX(e))); \
		RETURN_META_VALUE(mswi(lastFmRes),INDEXENT2((int)mlCellResult)); \
	} \
	edict_t* call##_post (edict_t *e) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),ENTINDEX(e))); \
		RETURN_META_VALUE(MRES_IGNORED,INDEXENT2((int)mlCellResult)); \
	} 

#define SIMPLE_EDICT_HOOK_EDICT_CONSTVECT_FLOAT(call) \
	edict_t* call (edict_t *ed, const float *vec, float fla) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(ed), p_vec, fla)); \
		RETURN_META_VALUE(mswi(lastFmRes), INDEXENT2((int)mlCellResult)); \
	} \
	edict_t* call##_post (edict_t *ed, const float *vec, float fla) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(ed), p_vec, fla)); \
		RETURN_META_VALUE(MRES_IGNORED, INDEXENT2((int)mlCellResult)); \
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

#define SIMPLE_VOID_HOOK_CONSTEDICT_CONSTEDICT(call) \
	void call (const edict_t *ent,const edict_t *entb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX((edict_t*)ent), ENTINDEX((edict_t*)entb))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const edict_t *ent,const edict_t *entb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX((edict_t*)ent), ENTINDEX((edict_t*)entb))); \
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
#define SIMPLE_UINT_HOOK_EDICT(call) \
	unsigned int call (edict_t *pent) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(pent))); \
		RETURN_META_VALUE(mswi(lastFmRes), (unsigned int)mlCellResult); \
	} \
	unsigned int call##_post (edict_t *pent) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(pent))); \
		RETURN_META_VALUE(MRES_IGNORED, (unsigned int)mlCellResult); \
	}

#define SIMPLE_INT_HOOK_CONSTEDICT(call) \
	int call (const edict_t *pent) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX((edict_t*)pent))); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post (const edict_t *pent) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX((edict_t*)pent))); \
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

#define SIMPLE_VOID_HOOK_INT_INT(call) \
	void call (int v, int vb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v, vb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v, int vb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v, vb)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_BOOL_HOOK_INT_INT(call) \
	qboolean call (int v, int vb) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v, vb)); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult > 0 ? 1 : 0); \
	} \
	qboolean call##_post (int v, int vb) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v, vb)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult > 0 ? 1 : 0); \
	}

#define SIMPLE_BOOL_HOOK_INT_INT_BOOL(call) \
	qboolean call (int v, int vb, qboolean bah) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v, vb, bah > 0 ? 1 : 0)); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult > 0 ? 1 : 0); \
	} \
	qboolean call##_post (int v, int vb, qboolean bah) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v, vb, bah > 0 ? 1 : 0)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult > 0 ? 1 : 0); \
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
#define SIMPLE_FLOAT_HOOK_VOID(call) \
	float call () \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i))); \
		RETURN_META_VALUE(mswi(lastFmRes), (float)mFloatResult); \
	} \
	float call##_post () \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i))); \
		RETURN_META_VALUE(MRES_IGNORED, (float)mFloatResult); \
	}


#define SIMPLE_INT_HOOK_CONSTVECT(call) \
	int call (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  p_vec)); \
		RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult); \
	} \
	int call##_post (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  p_vec)); \
		RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult); \
	}

#define SIMPLE_VOID_HOOK_EDICT_CONSTVECT(call) \
	void call (edict_t *e, const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(e), p_vec)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *e, const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(e), p_vec)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_EDICT_FLOAT_VECT(call) \
	void call (edict_t *e, float f, float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(e), f, p_vec)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *e, float f, float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(e), f, p_vec)); \
		RETURN_META(MRES_IGNORED); \
	}


#define SIMPLE_VOID_HOOK_CONSTVECT(call) \
	void call (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  p_vec)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  p_vec)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_CONSTVECT_VECT_VECT_VECT(call) \
	void call (const float *vec, float *vecb, float *vecc, float *vecd) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		PREPARE_VECTOR(vecc); \
		PREPARE_VECTOR(vecd); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  p_vec, p_vecb, p_vecc, p_vecd)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const float *vec, float *vecb, float *vecc, float *vecd) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		PREPARE_VECTOR(vecc); \
		PREPARE_VECTOR(vecd); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  p_vec, p_vecb, p_vecc, p_vecd)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_EDICT_CONSTVECT_CONSTVECT(call) \
	void call (edict_t *e, const float *vec, const float *vecb) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i), ENTINDEX(e), p_vec, p_vecb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *e, const float *vec, const float *vecb) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i), ENTINDEX(e), p_vec, p_vecb)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_FLOAT_HOOK_CONSTVECT(call) \
	float call (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  p_vec)); \
		RETURN_META_VALUE(mswi(lastFmRes),mlFloatResult); \
	} \
	float call##_post (const float *vec) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  p_vec)); \
		RETURN_META_VALUE(MRES_IGNORED,mlFloatResult); \
	}
#define SIMPLE_FLOAT_HOOK_CONSTSTRING(call) \
	float call (const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  s)); \
		RETURN_META_VALUE(mswi(lastFmRes),mlFloatResult); \
	} \
	float call##_post (const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  s)); \
		RETURN_META_VALUE(MRES_IGNORED,mlFloatResult); \
	}

#define SIMPLE_CONSTSTRING_HOOK_CONSTSTRING(call) \
	const char* call (const char *s) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  s)); \
		RETURN_META_VALUE(mswi(lastFmRes),mlStringResult); \
	} \
	const char* call##_post (const char *s) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  s)); \
		RETURN_META_VALUE(MRES_IGNORED,mlStringResult); \
	}

#define SIMPLE_VOID_HOOK_INT_INT_CONSTVECT_EDICT(call) \
	void call (int v, int vb, const float *vec, edict_t *e) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v, vb, p_vec, ENTINDEX(e))); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v, int vb, const float *vec, edict_t *e) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v, vb, p_vec, ENTINDEX(e))); \
		RETURN_META(MRES_IGNORED); \
	}
#define SIMPLE_BOOL_HOOK_EDICT_CONSTSTRING_CONSTSTRING_STRING128(call) \
	qboolean call (edict_t *e, const char *sza, const char *szb, char blah[128]) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(e), sza, szb, blah)); \
		RETURN_META_VALUE(mswi(lastFmRes),(int)mlCellResult > 0 ? 0 : 1); \
	} \
	qboolean call##_post (edict_t *e, const char *sza, const char *szb, char blah[128]) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(e), sza, szb, blah)); \
		RETURN_META_VALUE(MRES_IGNORED,(int)mlCellResult > 0 ? 0 : 1); \
	}
#define SIMPLE_VOID_HOOK_EDICT_INT_CONSTSTRING_FLOAT_FLOAT_INT_INT(call) \
	void call (edict_t *e, int v, const char *sz, float f, float fb, int vb, int vc) \
	{ \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(e), v, sz, f, fb, vb, vc)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *e, int v, const char *sz, float f, float fb, int vb, int vc) \
	{ \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(e), v, sz, f, fb, vb, vc)); \
		RETURN_META(MRES_IGNORED); \
	}

#define SIMPLE_VOID_HOOK_EDICT_VECT_CONSTSTRING_FLOAT_FLOAT_INT_INT(call) \
	void call (edict_t *e, float *vec, const char *sz, float f, float fb, int vb, int vc) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  ENTINDEX(e), p_vec, sz, f, fb, vb, vc)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (edict_t *e, float *vec, const char *sz, float f, float fb, int vb, int vc) \
	{ \
		PREPARE_VECTOR(vec); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  ENTINDEX(e), p_vec, sz, f, fb, vb, vc)); \
		RETURN_META(MRES_IGNORED); \
	}
//int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2 );
#define HOOK_PLAYBACK_EVENT(call) \
	void call (int v, const edict_t *e, unsigned short eb, float f, float *vec, float *vecb, float fb, float fc, int vb, int vc, int vd, int ve) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  v, ENTINDEX((edict_t*)e), eb, f, p_vec, p_vecb, fb, fc, vb, vc, vd, ve)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (int v, const edict_t *e, unsigned short eb, float f, float *vec, float *vecb, float fb, float fc, int vb, int vc, int vd, int ve) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  v, ENTINDEX((edict_t*)e), eb, f, p_vec, p_vecb, fb, fc, vb, vc, vd, ve)); \
		RETURN_META(MRES_IGNORED); \
	} 


#define SIMPLE_VOID_HOOK_CONSTVECT_CONSTVECT_FLOAT_FLOAT(call) \
	void call (const float *vec,const float *vecb, float fla, float flb) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE(FM_##call, (Engine[FM_##call].at(i),  p_vec, p_vecb, fla, flb)); \
		RETURN_META(mswi(lastFmRes)); \
	} \
	void call##_post (const float *vec,const float *vecb, float fla, float flb) \
	{ \
		PREPARE_VECTOR(vec); \
		PREPARE_VECTOR(vecb); \
		FM_ENG_HANDLE_POST(FM_##call, (EnginePost[FM_##call].at(i),  vec, vecb, fla, flb)); \
		RETURN_META(MRES_IGNORED); \
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

#define PREPARE_VECTOR(vector_name) \
	cell vector_name##_cell[3] = {amx_ftoc(vector_name[0]), amx_ftoc(vector_name[1]), amx_ftoc(vector_name[2])}; \
	cell p_##vector_name = MF_PrepareCellArray(vector_name##_cell, 3) \

#define PREPARE_FLOAT(float_name) \
	cell c_##float_name = amx_ftoc(float_name);

#define BYREF_FLOAT(float_name) \
	float_name = amx_ctof(c_##float_name);


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