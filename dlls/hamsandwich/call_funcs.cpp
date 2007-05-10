#include "sdk/amxxmodule.h"

#include "offsets.h"
#include "ham_utils.h"
#include "hooklist.h"

#include "CVector.h"
#include "forward.h"
#include "hook.h"

extern CVector<Hook *> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

void FailPlugin(AMX *amx, int id, int err, const char *reason);

extern bool gDoForwards;

inline void *GetFunction(void *pthis, int id, bool &istramp)
{
	istramp=false;
	void *func=GetVTableEntry(pthis, hooklist[id].vtid, Offsets.GetBase());

	// Check to see if it's a trampoline
	CVector<Hook *>::iterator end=hooks[id].end();

	for (CVector<Hook *>::iterator i=hooks[id].begin();
		 i!=end;
		 ++i)
	{
		if (func==(*i)->tramp)
		{
			istramp=true;
			return func;
		}
	}

	return func;
}
inline void *_GetFunction(void *pthis, int id)
{
	void **vtbl=GetVTable(pthis, Offsets.GetBase());

	int **ivtbl=(int **)vtbl;
	void *func=ivtbl[hooklist[id].vtid];

	// Iterate through the hooks for the id, see if the function is found
	CVector<Hook *>::iterator end=hooks[id].end();

	for (CVector<Hook *>::iterator i=hooks[id].begin();
		 i!=end;
		 ++i)
	{
		// If the function points to a trampoline, then return the original
		// function.
		if (func==(*i)->tramp)
		{
			printf("Func=0x%08X\n",reinterpret_cast<unsigned int>((*i)->func));
			return (*i)->func;
		}
	}

	// this is an original function
	printf("Func=0x%08X\n",reinterpret_cast<unsigned int>(func));
	return func;
}

#define SETUP(NUMARGS)					\
	if (((NUMARGS + 2) * sizeof(cell)) > (unsigned)params[0])	\
	{									\
		MF_LogError(amx, AMX_ERR_NATIVE, "Bad arg count.  Expected %d, got %d.", NUMARGS + 2, params[0] / sizeof(cell));	\
		return 0;						\
	}									\
	int func=params[1];					\
	int id=params[2];					\
	CHECK_FUNCTION(func);				\
	CHECK_ENTITY(id);					\
	void *pv=IndexToPrivate(id);		\
	bool istramp;						\
	void *__func=GetFunction(pv, func, istramp);	\
	if (!istramp && !gDoForwards)		\
	{									\
		gDoForwards=true;				\
	}


cell Call_Void_Void(AMX *amx, cell *params)
{
	SETUP(0);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined __linux__
	reinterpret_cast<void (*)(void *)>(__func)(pv);
#endif
	return 1;
}

cell Call_Int_Void(AMX *amx, cell *params)
{
	SETUP(0);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *)>(__func)(pv);
#endif
}

cell Call_Void_Entvar(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	entvars_t *ev1=&(INDEXENT_NEW(id3)->v);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, entvars_t *)>(__func)(pv, 0, ev1);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *)>(__func)(pv, ev1);
#endif
	return 1;
}


cell Call_Void_Cbase(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	void *pv1=(INDEXENT_NEW(id3)->pvPrivateData);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, void *)>(__func)(pv, 0, pv1);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, void *)>(__func)(pv, pv1);
#endif
	return 1;
}

cell Call_Int_Float_Int(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ftoc2(*MF_GetAmxAddr(amx, params[3]));
	int i4=*MF_GetAmxAddr(amx, params[4]);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void*, int, float, int)>(__func)(pv, 0, f3, i4);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, float, int)>(__func)(pv, f3, i4);
#endif
}

	
cell Call_Void_Entvar_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);

	CHECK_ENTITY(id3);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, entvars_t *, int)>(__func)(pv, 0, ev3, i4);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *, int)>(__func)(pv, ev3, i4);
#endif
	return 1;
}


cell Call_Int_Cbase(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	void *pv1=(INDEXENT_NEW(id3)->pvPrivateData);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void*, int, void *)>(__func)(pv, 0, pv1);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, void *)>(__func)(pv, pv1);
#endif
}

cell Call_Void_Int_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, int, int)>(__func)(pv, 0, i3, i4);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, int, int)>(__func)(pv, i3, i4);
#endif
	return 1;
}

cell Call_Int_Int_Str_Int(AMX *amx, cell *params)
{
	
	SETUP(3);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	char *sz4=MF_GetAmxString(amx, params[4], 0, NULL);
	int i5=*MF_GetAmxAddr(amx, params[5]);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void*, int, int, const char *, int)>(__func)(pv, 0, i3, sz4, i5);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, int, const char *, int)>(__func)(pv, i3, sz4, i5);
#endif
}

cell Call_Int_Int(AMX *amx, cell *params)
{
	SETUP(1);

	int i3=*MF_GetAmxAddr(amx, params[3]);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void*, int, int)>(__func)(pv, 0, i3);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, int)>(__func)(pv, i3);
#endif
}

cell Call_Int_Entvar(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *)>(__func)(pv, 0, ev3);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, entvars_t *)>(__func)(pv, ev3);
#endif
}

cell Call_Int_Entvar_Entvar_Float_Int(AMX *amx, cell *params)
{
	SETUP(4);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof2(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);
	entvars_t *ev4=&(INDEXENT_NEW(id4)->v);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *, entvars_t *, float, int)>(__func)(pv, 0, ev3, ev4, f5, i6);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, entvars_t *, entvars_t *, float, int)>(__func)(pv, ev3, ev4, f5, i6);
#endif
}

cell Call_Void_Int(AMX *amx, cell *params)
{
	SETUP(1);

	int i3=*MF_GetAmxAddr(amx, params[3]);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void *, int, int)>(__func)(pv, 0, i3);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, int)>(__func)(pv, i3);
#endif

	return 1;
}

cell Call_Void_Cbase_Cbase_Int_Float(AMX *amx, cell *params)
{
	SETUP(4);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	void *p3=IndexToPrivate(id3);
	void *p4=IndexToPrivate(id4);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void *, int, void *, void *, int, float)>(__func)(pv, 0, p3, p4, i5, f6);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, void *, void *, int, float)>(__func)(pv, p3, p4, i5, f6);
#endif

	return 1;
}

cell Call_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, cell *params)
{
	SETUP(5);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof2(*MF_GetAmxAddr(amx, params[4]));
	Vector v5;
	TraceResult *tr6=reinterpret_cast<TraceResult *>(*MF_GetAmxAddr(amx, params[6]));
	int i7=*MF_GetAmxAddr(amx, params[7]);

	float *fl5=(float *)MF_GetAmxAddr(amx, params[5]);
	v5.x=fl5[0];
	v5.y=fl5[1];
	v5.z=fl5[2];

	if (tr6==NULL)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null traceresult provided.");

		return 0;
	}

	CHECK_ENTITY(id3);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);
#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, float, Vector, TraceResult *, int)>(__func)(pv, 0, ev3, f4, v5, tr6, i7);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *, float, Vector, TraceResult *, int)>(__func)(pv, ev3, f4, v5, tr6, i7);
#endif

	return 1;
}

cell Call_Void_Float_Vector_Trace_Int(AMX *amx, cell *params)
{
	SETUP(4);

	float f3=amx_ctof2(*MF_GetAmxAddr(amx, params[3]));
	Vector v4;
	TraceResult *tr5=reinterpret_cast<TraceResult *>(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);

	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	if (tr5==NULL)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null traceresult provided.");

		return 0;
	}

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void *, int, float, Vector, TraceResult *, int)>(__func)(pv, 0, f3, v4, tr5, i6);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, float, Vector, TraceResult *, int)>(__func)(pv, f3, v4, tr5, i6);
#endif

	return 1;
}

cell Call_Str_Void(AMX *amx, cell *params)
{
	SETUP(2);

#ifdef _WIN32
	char *v=reinterpret_cast<char *(__fastcall *)(void *, int)>(__func)(pv, 0);
#elif defined __linux__
	char *v=reinterpret_cast<char *(*)(void *)>(__func)(pv);
#endif
	return MF_SetAmxString(amx, params[3], v == NULL ? "" : v, *MF_GetAmxAddr(amx, params[4]));

}

cell Call_Cbase_Void(AMX *amx, cell *params)
{
	SETUP(0);
#ifdef _WIN32
	void *ret=reinterpret_cast<void *(__fastcall *)(void *, int)>(__func)(pv, 0);
#elif defined __linux__
	void *ret=reinterpret_cast<void *(*)(void *)>(__func)(pv);
#endif
	return PrivateToIndex(ret);
}

cell Call_Vector_Void(AMX *amx, cell *params)
{
	SETUP(1);
#ifdef _WIN32
	Vector ret=reinterpret_cast<Vector (__fastcall *)(void *, int)>(__func)(pv, 0);
#elif defined __linux__
	Vector ret=reinterpret_cast<Vector (*)(void *)>(__func)(pv);
#endif
	float *out=(float *)MF_GetAmxAddr(amx, params[3]);
	out[0]=ret.x;
	out[1]=ret.y;
	out[2]=ret.z;

	return 1;
}

cell Call_Vector_pVector(AMX *amx, cell *params)
{
	SETUP(2);

	Vector v3;
	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

#ifdef _WIN32
	Vector ret=reinterpret_cast<Vector (__fastcall *)(void *, int, Vector*)>(__func)(pv, 0, &v3);
#elif defined __linux__
	Vector ret=reinterpret_cast<Vector (*)(void *, Vector*)>(__func)(pv, &v3);
#endif
	float *out=(float *)MF_GetAmxAddr(amx, params[4]);
	out[0]=ret.x;
	out[1]=ret.y;
	out[2]=ret.z;

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	return 1;
}

cell Call_Int_pVector(AMX *amx, cell *params)
{
	SETUP(1);

	Vector v3;
	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

#ifdef _WIN32
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector*)>(__func)(pv, 0, &v3);
#elif defined __linux__
	int ret=reinterpret_cast<int (*)(void *, Vector*)>(__func)(pv, &v3);
#endif

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	return ret;
}

cell Call_Void_Entvar_Float_Float(AMX *amx, cell *params)
{
	SETUP(3);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof2(*MF_GetAmxAddr(amx, params[4]));
	float f5=amx_ctof2(*MF_GetAmxAddr(amx, params[5]));

	CHECK_ENTITY(id3);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, float, float)>(__func)(pv, 0, ev3, f4, f5);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, entvars_t *, float, float)>(__func)(pv, ev3, f4, f5);
#endif

	return 1;
}

cell Call_Int_pFloat_pFloat(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof2(*MF_GetAmxAddr(amx, params[3]));
	float f4=amx_ctof2(*MF_GetAmxAddr(amx, params[4]));

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void *, int, float*, float*)>(__func)(pv, 0, &f3, &f4);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, float*, float*)>(__func)(pv, &f3, &f4);
#endif

}

cell Call_Void_Entvar_Float(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof2(*MF_GetAmxAddr(amx, params[4]));

	CHECK_ENTITY(id3);

	entvars_t *ev3=&(INDEXENT_NEW(id3)->v);

#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t*, float)>(__func)(pv, 0, ev3, f4);
#elif defined __linux__
	return reinterpret_cast<int (*)(void *, entvars_t*, float)>(__func)(pv, ev3, f4);
#endif
}

cell Call_Void_Int_Int_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);

#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, int, int, int)>(__func)(pv, 0, i3, i4, i5);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, int, int, int)>(__func)(pv, i3, i4, i5);
#endif
	return 1;
}

cell Call_Void_ItemInfo(AMX *amx, cell *params)
{
	SETUP(1);

	void *ptr=reinterpret_cast<void *>(*MF_GetAmxAddr(amx, params[3]));

	if (ptr==0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null ItemInfo handle!");
		return 0;
	}
#ifdef _WIN32
	reinterpret_cast<void (__fastcall *)(void*, int, void *)>(__func)(pv, 0, ptr);
#elif defined __linux__
	reinterpret_cast<void (*)(void *, void *)>(__func)(pv, ptr);
#endif
	return 1;
}
