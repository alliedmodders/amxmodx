#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <extdll.h>
#include "sdk/amxxmodule.h"



#include "CVector.h"
#include "CString.h"
#include "sh_stack.h"

#include "hook.h"
#include "forward.h"

#include "ham_const.h"	
#include "ham_utils.h"

#include "DataHandler.h"



extern bool gDoForwards;

#define PUSH_VOID() ReturnStack.push(new Data(RET_VOID, NULL));				OrigReturnStack.push(new Data(RET_VOID, NULL));
#define PUSH_INT() ReturnStack.push(new Data(RET_INTEGER, (void *)&ret));	OrigReturnStack.push(new Data(RET_INTEGER, (void *)&origret));
#define PUSH_FLOAT() ReturnStack.push(new Data(RET_FLOAT, (void *)&ret));	OrigReturnStack.push(new Data(RET_FLOAT, (void *)&origret));
#define PUSH_VECTOR() ReturnStack.push(new Data(RET_VECTOR, (void *)&ret)); OrigReturnStack.push(new Data(RET_VECTOR, (void *)&origret));
#define PUSH_CBASE() ReturnStack.push(new Data(RET_CBASE, (void *)&ret));	OrigReturnStack.push(new Data(RET_CBASE, (void *)&origret));
#define PUSH_STRING() ReturnStack.push(new Data(RET_STRING, (void *)&ret)); OrigReturnStack.push(new Data(RET_STRING, (void *)&origret));

#define POP() delete ReturnStack.front(); ReturnStack.pop(); delete OrigReturnStack.front(); OrigReturnStack.pop();

#define PRE_START()																\
	bool DoForwards=gDoForwards;												\
	gDoForwards=true;															\
	int result=HAM_UNSET;														\
	int thisresult=HAM_UNSET;													\
	int iThis=PrivateToIndex(pthis);											\
	if (DoForwards)																\
	{																			\
		CVector<Forward*>::iterator end=hook->pre.end();						\
		for (CVector<Forward*>::iterator i=hook->pre.begin(); i!=end; i++)		\
		{																		\
			if ((*i)->state == FSTATE_OK)										\
			{																	\
				thisresult=MF_ExecuteForward((*i)->id,iThis

#define PRE_END()												\
				);												\
			}													\
			if (thisresult > result)							\
			{													\
				result=thisresult;								\
			}													\
		}														\
	}															\
	if (result < HAM_SUPERCEDE)									\
	{

#define POST_START()														\
	}																		\
	if (DoForwards)															\
	{																		\
		CVector<Forward*>::iterator end=hook->post.end();					\
		for (CVector<Forward*>::iterator i=hook->post.begin(); i!=end; i++)	\
		{																	\
			if ((*i)->state == FSTATE_OK)									\
			{																\
				MF_ExecuteForward((*i)->id,iThis

#define POST_END()														\
				);														\
			}															\
		}																\
	}


#define CHECK_RETURN()													\
		if (thisresult < HAM_OVERRIDE)									\
		{																\
			return origret;												\
		}																

#define CHECK_RETURN_STR()												\
		if (thisresult < HAM_OVERRIDE)									\
		{																\
			return origret.c_str();										\
		}


void Hook_Void_Void(Hook *hook, void *pthis)
{
	PUSH_VOID()
	PRE_START()
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*,int)>(hook->func)(pthis,0);
#elif defined __linux__
	reinterpret_cast<void (*)(void*)>(hook->func)(pthis);
#endif

	POST_START()
	POST_END()
	POP()
}
int Hook_Int_Void(Hook *hook, void *pthis)
{
	int ret=0;
	int origret=0;

	PUSH_INT()
	PRE_START()
	PRE_END()
	
	
#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*,int)>(hook->func)(pthis,0);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*)>(hook->func)(pthis);
#endif

	POST_START()
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

void Hook_Void_Entvar(Hook *hook, void *pthis, entvars_t *entvar)
{
	PUSH_VOID()
	int iOther=EntvarToIndex(entvar);

	PRE_START()
		, iOther
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, entvars_t *)>(hook->func)(pthis, 0, entvar);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, entvars_t *)>(hook->func)(pthis, entvar);
#endif

	POST_START()
		, iOther
	POST_END()

	POP()

}

void Hook_Void_Cbase(Hook *hook, void *pthis, void *other)
{
	PUSH_VOID()
	int iOther=PrivateToIndex(other);

	PRE_START()
		, iOther
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, void *)>(hook->func)(pthis, 0, other);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, void *)>(hook->func)(pthis, other);
#endif

	POST_START()
		, iOther
	POST_END()

	POP()
}

int Hook_Int_Float_Int(Hook *hook, void *pthis, float f1, int i1)
{
	int ret=0;
	int origret=0;
	PUSH_INT()
	PRE_START()
		, f1, i1
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, float, int)>(hook->func)(pthis, 0, f1, i1);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, float, int)>(hook->func)(pthis, f1, i1);
#endif

	POST_START()
		, f1, i1
	POST_END()

	POP();
	CHECK_RETURN();
	return ret;

}
void Hook_Void_Entvar_Int(Hook *hook, void *pthis, entvars_t *ev1, int i1)
{
	PUSH_VOID()
	int iOther=EntvarToIndex(ev1);

	PRE_START()
		, iOther, i1
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, entvars_t *, int)>(hook->func)(pthis, 0, ev1, i1);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, entvars_t *, int)>(hook->func)(pthis, ev1, i1);
#endif

	POST_START()
		, iOther, i1
	POST_END()
	POP()
}

int Hook_Int_Cbase(Hook *hook, void *pthis, void *cb1)
{
	int ret=0;
	int origret=0;

	PUSH_INT()

	int iOther=PrivateToIndex(cb1);
	PRE_START()
		, iOther
	PRE_END()
#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, void *)>(hook->func)(pthis, 0, cb1);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, void *)>(hook->func)(pthis, cb1);
#endif

	POST_START()
		, iOther
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

void Hook_Void_Int_Int(Hook *hook, void *pthis, int i1, int i2)
{
	PUSH_VOID()
	PRE_START()
		,i1, i2
	PRE_END()
#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, int, int)>(hook->func)(pthis, 0, i1, i2);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, int, int)>(hook->func)(pthis, i1, i2);
#endif

	POST_START()
		,i1, i2
	POST_END()
	POP()
}

int Hook_Int_Int_Str_Int(Hook *hook, void *pthis, int i1, const char *sz1, int i2)
{
	int ret=0;
	int origret=0;
	PUSH_INT()
	PRE_START()
		,i1, sz1, i2
	PRE_END()
#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, int, const char *, int)>(hook->func)(pthis, 0, i1, sz1, i2);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, int, const char *, int)>(hook->func)(pthis, i1, sz1, i2);
#endif

	POST_START()
		,i1, sz1, i2
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

int Hook_Int_Int(Hook *hook, void *pthis, int i1)
{
	int ret=0;
	int origret=0;
	PUSH_INT()
	
	PRE_START()
		,i1
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, int)>(hook->func)(pthis, 0, i1);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, int)>(hook->func)(pthis, i1);
#endif

	POST_START()
		,i1
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

int Hook_Int_Entvar(Hook *hook, void *pthis, entvars_t *ev1)
{
	int ret=0;
	int origret=0;

	PUSH_INT()
	int iOther=EntvarToIndex(ev1);

	PRE_START()
		,iOther
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, entvars_t *)>(hook->func)(pthis, 0, ev1);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, entvars_t *)>(hook->func)(pthis, ev1);
#endif

	POST_START()
		, iOther
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}



// Takedamage
int Hook_Int_Entvar_Entvar_Float_Int(Hook *hook, void *pthis, entvars_t *inflictor, entvars_t *attacker, float damage, int damagebits)
{
	int ret=0;
	int origret=0;
	PUSH_INT()
	int iInflictor=EntvarToIndex(inflictor);
	int iAttacker=EntvarToIndex(attacker);
	

	PRE_START()
		,iInflictor, iAttacker, damage, damagebits
	PRE_END()
	
	
#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, entvars_t *, entvars_t *, float, int)>(hook->func)(pthis, 0, inflictor, attacker, damage, damagebits);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, entvars_t *, entvars_t *, float, int)>(hook->func)(pthis, inflictor, attacker, damage, damagebits);
#endif

	POST_START()
		,iInflictor, iAttacker, damage, damagebits
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

void Hook_Void_Int(Hook *hook, void *pthis, int i1)
{
	PUSH_VOID()
	PRE_START()
		, i1
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, int)>(hook->func)(pthis, 0, i1);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, int)>(hook->func)(pthis, i1);
#endif

	POST_START()
		,i1
	POST_END()

	POP()
}

void Hook_Void_Cbase_Cbase_Int_Float(Hook *hook, void *pthis, void *cb1, void *cb2, int i1, float f1)
{
	PUSH_VOID()
	int iCaller=PrivateToIndex(cb1);
	int iActivator=PrivateToIndex(cb2);
	

	PRE_START()
		,iCaller, iActivator, i1, f1
	PRE_END()
	
	
#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, void *, void *, int, float)>(hook->func)(pthis, 0, cb1, cb2, i1, f1);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, void *, void *, int, float)>(hook->func)(pthis, cb1, cb2, i1, f1);
#endif

	POST_START()
		,iCaller, iActivator, i1, f1
	POST_END()

	POP()
}

void Hook_Void_Entvar_Float_Vector_Trace_Int(Hook *hook, void *pthis, entvars_t *ev1, float f1, Vector v1, TraceResult *tr1, int i1)
{
	PUSH_VOID()
	int iev1=EntvarToIndex(ev1);
	cell cvec[3];
	cvec[0]=amx_ftoc2(v1.x);
	cvec[1]=amx_ftoc2(v1.y);
	cvec[2]=amx_ftoc2(v1.z);

	PRE_START()
		,iev1, f1, MF_PrepareCellArrayA(cvec, 3, false), tr1, i1
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, entvars_t *, float, Vector, TraceResult *, int)>(hook->func)(pthis, 0, ev1, f1, v1, tr1, i1);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, entvars_t *, float, Vector, TraceResult *, int)>(hook->func)(pthis, ev1, f1, v1, tr1, i1);
#endif

	POST_START()
		, iev1, f1, MF_PrepareCellArrayA(cvec, 3, false), tr1, i1
	POST_END()

	POP()
}

void Hook_Void_Float_Vector_TraceResult_Int(Hook *hook, void *pthis, float f1, Vector v1, TraceResult *tr1, int i1)
{
	PUSH_VOID()
	cell cvec[3];
	cvec[0]=amx_ftoc2(v1.x);
	cvec[1]=amx_ftoc2(v1.y);
	cvec[2]=amx_ftoc2(v1.z);

	PRE_START()
		, f1, MF_PrepareCellArrayA(cvec, 3, false), tr1, i1
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void*, int, float, Vector, TraceResult *, int)>(hook->func)(pthis, 0, f1, v1, tr1, i1);
#elif defined __linux__
	reinterpret_cast<void (*)(void*, float, Vector, TraceResult *, int)>(hook->func)(pthis, f1, v1, tr1, i1);
#endif

	POST_START()
		, f1, MF_PrepareCellArrayA(cvec, 3, false), tr1, i1
	POST_END()
	POP()
}
const char *Hook_Str_Void(Hook *hook, void *pthis)
{
	String ret;
	String origret;
	PUSH_STRING()
	PRE_START()
	PRE_END()

#if defined _WIN32
	origret.assign(reinterpret_cast<const char *(__fastcall*)(void*, int)>(hook->func)(pthis, 0));
#elif defined __linux__
	origret.assign(reinterpret_cast<const char *(*)(void*)>(hook->func)(pthis));
#endif

	POST_START()
	POST_END()

	POP()
	CHECK_RETURN_STR();
	return ret.c_str();

}

void *Hook_Cbase_Void(Hook *hook, void *pthis)
{
	void *ret=NULL;
	void *origret=NULL;
	PUSH_CBASE()
	PRE_START()
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<void *(__fastcall*)(void*, int)>(hook->func)(pthis, 0);
#elif defined __linux__
	origret=reinterpret_cast<void *(*)(void*)>(hook->func)(pthis);
#endif

	POST_START()
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;

}


Vector Hook_Vector_Void(Hook *hook, void *pthis)
{
	Vector ret;
	Vector origret;

	PUSH_VECTOR()

	memset(&ret, 0x0, sizeof(Vector));
	memset(&origret, 0x0, sizeof(Vector));

	PRE_START()
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<Vector (__fastcall*)(void*, int)>(hook->func)(pthis, 0);
#elif defined __linux__
	origret=reinterpret_cast<Vector (*)(void*)>(hook->func)(pthis);
#endif

	POST_START()
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;

}

Vector Hook_Vector_pVector(Hook *hook, void *pthis, Vector *v1)
{
	Vector ret;
	Vector origret;

	PUSH_VECTOR()

	memset(&ret, 0x0, sizeof(Vector));
	memset(&origret, 0x0, sizeof(Vector));

	cell cv1[3];
	cv1[0]=amx_ftoc2(v1->x);
	cv1[1]=amx_ftoc2(v1->y);
	cv1[2]=amx_ftoc2(v1->z);

	PRE_START()
		, MF_PrepareCellArrayA(cv1, 3, false)
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<Vector (__fastcall*)(void*, int, Vector *)>(hook->func)(pthis, 0, v1);
#elif defined __linux__
	origret=reinterpret_cast<Vector (*)(void*, Vector *)>(hook->func)(pthis, v1);
#endif

	POST_START()
		, MF_PrepareCellArrayA(cv1, 3, false)
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;

}

int Hook_Int_pVector(Hook *hook, void *pthis, Vector *v1)
{
	int ret=0;
	int origret=0;
	PUSH_INT()

	cell cv1[3];

	cv1[0]=amx_ftoc2(v1->x);
	cv1[1]=amx_ftoc2(v1->y);
	cv1[2]=amx_ftoc2(v1->z);

	PRE_START()
		, MF_PrepareCellArrayA(cv1, 3, false)
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void*, int, Vector *)>(hook->func)(pthis, 0, v1);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void*, Vector *)>(hook->func)(pthis, v1);
#endif

	POST_START()
		, MF_PrepareCellArrayA(cv1, 3, false)
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;

}

void Hook_Void_Entvar_Float_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1, float f2)
{
	PUSH_VOID()
	cell cev1=EntvarToIndex(ev1);

	PRE_START()
		, cev1, f1, f2
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void *, int, entvars_t *, float, float)>(hook->func)(pthis, 0, ev1, f1, f2);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *, float, float)>(hook->func)(pthis, ev1, f1, f2);
#endif

	POST_START()
		, cev1, f1, f2
	POST_END()

	POP()
}

int Hook_Int_pFloat_pFloat(Hook *hook, void *pthis, float *f1, float *f2)
{
	int ret=0;
	int origret=0;
	PUSH_INT()

	cell cf1=amx_ftoc2(f1 == NULL ? 0.0 : *f1);
	cell cf2=amx_ftoc2(f2 == NULL ? 0.0 : *f2);

	PRE_START()
		, cf1, cf2
	PRE_END()

#if defined _WIN32
	origret=reinterpret_cast<int (__fastcall*)(void *, int, float *, float *)>(hook->func)(pthis, 0, f1, f2);
#elif defined __linux__
	origret=reinterpret_cast<int (*)(void *, float *, float *)>(hook->func)(pthis, f1, f2);
#endif

	POST_START()
		, cf1, cf2
	POST_END()

	POP()
	CHECK_RETURN()
	return ret;
}

void Hook_Void_Entvar_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1)
{
	PUSH_VOID()
	cell cev1=EntvarToIndex(ev1);

	PRE_START()
		, cev1, f1
	PRE_END()

#if defined _WIN32
	reinterpret_cast<void (__fastcall*)(void *, int, entvars_t *, float)>(hook->func)(pthis, 0, ev1, f1);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *, float)>(hook->func)(pthis, ev1, f1);
#endif

	POST_START()
		, cev1, f1
	POST_END()
	POP()
}

