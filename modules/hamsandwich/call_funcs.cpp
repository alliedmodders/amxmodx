// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include "amxxmodule.h"

#include "offsets.h"
#include "ham_utils.h"
#include "hooklist.h"
#include "forward.h"
#include "hook.h"
#include <amtl/am-vector.h>
#include <amtl/am-string.h>

extern ke::Vector<Hook *> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

void FailPlugin(AMX *amx, int id, int err, const char *reason);

extern bool gDoForwards;

inline void *GetFunction(void *pthis, int id, bool &istramp)
{
	istramp=false;
	void *func=GetVTableEntry(pthis, hooklist[id].vtid, Offsets.GetBase());

	// Check to see if it's a trampoline
	for (size_t i = 0; i < hooks[id].length(); ++i)
	{
		if (func == hooks[id].at(i)->tramp)
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
	for (size_t i = 0; i < hooks[id].length(); ++i)
	{
		// If the function points to a trampoline, then return the original
		// function.
		if (func == hooks[id].at(i)->tramp)
		{
			return hooks[id].at(i)->func;
		}
	}

	// this is an original function
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
	void *pv=TypeConversion.id_to_cbase(id);		\
	bool istramp;						\
	void *__func=GetFunction(pv, func, istramp);	\
	if (!istramp && !gDoForwards)		\
	{									\
		gDoForwards=true;				\
	}


cell Call_Void_Void(AMX *amx, cell *params)
{
	SETUP(0);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *)>(__func)(pv);
#endif
	return 1;
}

cell Call_Int_Void(AMX *amx, cell *params)
{
	SETUP(0);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *)>(__func)(pv);
#endif
}

cell Call_Void_Entvar(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	entvars_t *ev1 = TypeConversion.id_to_entvars(id3);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, entvars_t *)>(__func)(pv, 0, ev1);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *)>(__func)(pv, ev1);
#endif
	return 1;
}


cell Call_Void_Cbase(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	void *pv1 = TypeConversion.id_to_cbase(id3);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *)>(__func)(pv, 0, pv1);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *)>(__func)(pv, pv1);
#endif
	return 1;
}

cell Call_Int_Float_Int(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	int i4=*MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, float, int)>(__func)(pv, 0, f3, i4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, float, int)>(__func)(pv, f3, i4);
#endif
}

cell Call_Int_Float_Int_Int(AMX *amx, cell *params)
{
	SETUP(3);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	int i4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, float, int, int)>(__func)(pv, 0, f3, i4, i5);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, float, int, int)>(__func)(pv, f3, i4, i5);
#endif
}
	
cell Call_Void_Entvar_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);

	CHECK_ENTITY(id3);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, entvars_t *, int)>(__func)(pv, 0, ev3, i4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, int)>(__func)(pv, ev3, i4);
#endif
	return 1;
}

cell Call_Void_Entvar_Entvar_Int(AMX *amx, cell *params)
{
	SETUP(3);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, entvars_t *, entvars_t *, int)>(__func)(pv, 0, ev3, ev4, i5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, entvars_t *, int)>(__func)(pv, ev3, ev4, i5);
#endif
	return 1;
}


cell Call_Int_Cbase(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	void *pv1 = TypeConversion.id_to_cbase(id3);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, void *)>(__func)(pv, 0, pv1);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, void *)>(__func)(pv, pv1);
#endif
}

cell Call_Void_Int_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, int, int)>(__func)(pv, 0, i3, i4);
#elif defined(__linux__) || defined(__APPLE__)
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

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, int, const char *, int)>(__func)(pv, 0, i3, sz4, i5);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, int, const char *, int)>(__func)(pv, i3, sz4, i5);
#endif
}

cell Call_Int_Int_Str_Int_Int(AMX *amx, cell *params)
{
	SETUP(4);

	int i3 = *MF_GetAmxAddr(amx, params[3]);
	char *sz4 = MF_GetAmxString(amx, params[4], 0, NULL);
	int i5 = *MF_GetAmxAddr(amx, params[5]);
	int i6 = *MF_GetAmxAddr(amx, params[6]);

#if defined(_WIN32)
	return reinterpret_cast<int(__fastcall *)(void*, int, int, const char *, int, int)>(__func)(pv, 0, i3, sz4, i5, i6);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int(*)(void *, int, const char *, int, int)>(__func)(pv, i3, sz4, i5, i6);
#endif
}

cell Call_Int_Int(AMX *amx, cell *params)
{
	SETUP(1);

	int i3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, int)>(__func)(pv, 0, i3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, int)>(__func)(pv, i3);
#endif
}

cell Call_Int_Entvar(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *)>(__func)(pv, 0, ev3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, entvars_t *)>(__func)(pv, ev3);
#endif
}

cell Call_Int_Entvar_Entvar_Float_Int(AMX *amx, cell *params)
{
	SETUP(4);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *, entvars_t *, float, int)>(__func)(pv, 0, ev3, ev4, f5, i6);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, entvars_t *, entvars_t *, float, int)>(__func)(pv, ev3, ev4, f5, i6);
#endif
}

cell Call_Int_Entvar_Entvar_Float_Float_Int(AMX *amx, cell *params)
{
	SETUP(5);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));
	int i7=*MF_GetAmxAddr(amx, params[7]);

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *, entvars_t *, float, float, int)>(__func)(pv, 0, ev3, ev4, f5, f6, i7);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, entvars_t *, entvars_t *, float, float, int)>(__func)(pv, ev3, ev4, f5, f6, i7);
#endif
}

cell Call_Void_Int(AMX *amx, cell *params)
{
	SETUP(1);

	int i3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, int)>(__func)(pv, 0, i3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, int)>(__func)(pv, i3);
#endif

	return 1;
}

cell Call_Vector_Float_Cbase_Int(AMX *amx, cell *params)
{
	SETUP(4);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	int id4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);

	CHECK_ENTITY(id4);

	void *p4 = TypeConversion.id_to_cbase(id4);

#if defined(_WIN32)
	Vector ret;
	reinterpret_cast<void(__fastcall *)(void *, int, Vector*, float, void *, int)>(__func)(pv, 0, &ret, f3, p4, i5);
#elif defined(__linux__) || defined(__APPLE__)
	Vector ret = reinterpret_cast<Vector(*)(void *, float, void *, int)>(__func)(pv, f3, p4, i5);
#endif

	float *out = (float *)MF_GetAmxAddr(amx, params[6]);
	out[0] = ret.x;
	out[1] = ret.y;
	out[2] = ret.z;

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

	void *p3 = TypeConversion.id_to_cbase(id3);
	void *p4 = TypeConversion.id_to_cbase(id4);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, void *, void *, int, float)>(__func)(pv, 0, p3, p4, i5, f6);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *, void *, int, float)>(__func)(pv, p3, p4, i5, f6);
#endif

	return 1;
}

cell Call_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, cell *params)
{
	SETUP(5);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));
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

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, float, Vector, TraceResult *, int)>(__func)(pv, 0, ev3, f4, v5, tr6, i7);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, float, Vector, TraceResult *, int)>(__func)(pv, ev3, f4, v5, tr6, i7);
#endif

	return 1;
}

cell Call_Void_Float_Vector_Trace_Int(AMX *amx, cell *params)
{
	SETUP(4);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
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

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, float, Vector, TraceResult *, int)>(__func)(pv, 0, f3, v4, tr5, i6);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, float, Vector, TraceResult *, int)>(__func)(pv, f3, v4, tr5, i6);
#endif

	return 1;
}

cell Call_Str_Void(AMX *amx, cell *params)
{
	SETUP(2);

#if defined(_WIN32)
	char *v=reinterpret_cast<char *(__fastcall *)(void *, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	char *v=reinterpret_cast<char *(*)(void *)>(__func)(pv);
#endif
	return MF_SetAmxString(amx, params[3], v == NULL ? "" : v, *MF_GetAmxAddr(amx, params[4]));
}

cell Call_Cbase_Void(AMX *amx, cell *params)
{
	SETUP(0);
#if defined(_WIN32)
	void *ret=reinterpret_cast<void *(__fastcall *)(void *, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	void *ret=reinterpret_cast<void *(*)(void *)>(__func)(pv);
#endif
	return TypeConversion.cbase_to_id(ret);
}

cell Call_Float_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int i3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	float ret=reinterpret_cast<float (__fastcall *)(void *, int, int)>(__func)(pv, 0, i3);
#elif defined(__linux__) || defined(__APPLE__)
	float ret=reinterpret_cast<float (*)(void *, int)>(__func)(pv, i3);
#endif
	*MF_GetAmxAddr(amx, params[4])=amx_ftoc(ret);

	return 1;
}

cell Call_Vector_Void(AMX *amx, cell *params)
{
	SETUP(1);
#if defined(_WIN32)
	Vector ret;
	reinterpret_cast<void (__fastcall *)(void *, int,Vector*)>(__func)(pv, 0,&ret);
#elif defined(__linux__) || defined(__APPLE__)
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

#if defined(_WIN32)
	Vector ret;
	reinterpret_cast<void (__fastcall *)(void *, int, Vector*, Vector*)>(__func)(pv, 0, &ret, &v3);
#elif defined(__linux__) || defined(__APPLE__)
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

#if defined(_WIN32)
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector*)>(__func)(pv, 0, &v3);
#elif defined(__linux__) || defined(__APPLE__)
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
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));

	CHECK_ENTITY(id3);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, float, float)>(__func)(pv, 0, ev3, f4, f5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, float, float)>(__func)(pv, ev3, f4, f5);
#endif

	return 1;
}

cell Call_Void_pFloat_pFloat(AMX *amx, cell *params)
{
	SETUP(2);

	float f3;
	float f4;

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, float*, float*)>(__func)(pv, 0, &f3, &f4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, float*, float*)>(__func)(pv, &f3, &f4);
#endif

	*MF_GetAmxAddr(amx, params[3]) = amx_ftoc(f3);
	*MF_GetAmxAddr(amx, params[4]) = amx_ftoc(f4);

	return 1;
}

cell Call_Void_Entvar_Float(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));

	CHECK_ENTITY(id3);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t*, float)>(__func)(pv, 0, ev3, f4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, entvars_t*, float)>(__func)(pv, ev3, f4);
#endif
}

cell Call_Void_Int_Int_Int(AMX *amx, cell *params)
{
	SETUP(3);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);
	int i5=*MF_GetAmxAddr(amx, params[5]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, int, int, int)>(__func)(pv, 0, i3, i4, i5);
#elif defined(__linux__) || defined(__APPLE__)
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
#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *)>(__func)(pv, 0, ptr);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *)>(__func)(pv, ptr);
#endif
	return 1;
}

cell Call_Float_Void(AMX *amx, cell *params)
{
	SETUP(1);

#if defined(_WIN32)
	float ret=reinterpret_cast<float (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	float ret=reinterpret_cast<float (*)(void *)>(__func)(pv);
#endif
	*MF_GetAmxAddr(amx, params[3])=amx_ftoc(ret);

	return 1;
}

cell Call_Void_Float_Int(AMX* amx, cell* params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	int i4 = *MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, float, char)>(__func)(pv, 0, f3, i4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void*, float, char)>(__func)(pv, f3, i4);
#endif

	return 1;
}

cell Call_Float_Float_Cbase(AMX* amx, cell* params)
{
	SETUP(3);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	int id4=*MF_GetAmxAddr(amx, params[4]);
	CHECK_ENTITY(id4);
	void *p4 = TypeConversion.id_to_cbase(id4);

#if defined(_WIN32)
	float ret = reinterpret_cast<float(__fastcall *)(void*, int, float, void*)>(__func)(pv, 0, f3, p4);
#elif defined(__linux__) || defined(__APPLE__)
	float ret = reinterpret_cast<float (*)(void*, float, void*)>(__func)(pv, f3, p4);
#endif
	*MF_GetAmxAddr(amx, params[5]) = amx_ftoc(ret);

	return 1;
}

cell Call_Void_Float(AMX* amx, cell* params)
{
	SETUP(1);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, float)>(__func)(pv, 0, f3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void*, float)>(__func)(pv, f3);
#endif
	return 1;
}

cell Call_Void_Float_Float_Float_Int(AMX* amx, cell* params)
{
	SETUP(4);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, float, float, float, int)>(__func)(pv, 0, f3, f4, f5, i6);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void*, float, float, float, int)>(__func)(pv, f3, f4, f5, i6);
#endif
	return 1;
}

cell Call_Vector_Float(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));

#if defined(_WIN32)
	Vector ret;
	reinterpret_cast<void (__fastcall *)(void *, int, Vector*, float)>(__func)(pv, 0, &ret, f3);
#elif defined(__linux__) || defined(__APPLE__)
	Vector ret = reinterpret_cast<Vector(*)(void *, float)>(__func)(pv, f3);
#endif
	float *out=(float *)MF_GetAmxAddr(amx, params[4]);
	out[0]=ret.x;
	out[1]=ret.y;
	out[2]=ret.z;

	return 1;
}

cell Call_Void_Float_Cbase(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3])); 
	int id4=*MF_GetAmxAddr(amx, params[4]);

	CHECK_ENTITY(id4);

	void *p4 = TypeConversion.id_to_cbase(id4);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, float, void *)>(__func)(pv, 0, f3, p4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, float, void *)>(__func)(pv, f3, p4);
#endif

	return 1;
}

cell Call_Int_Float_Float(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, float, float)>(__func)(pv, 0, f3, f4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, float, float)>(__func)(pv, f3, f4);
#endif
}

cell Call_Int_Float(AMX *amx, cell *params)
{
	SETUP(1);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, float)>(__func)(pv, 0, f3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, float)>(__func)(pv, f3);
#endif
}

cell Call_Int_Int_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, int, int)>(__func)(pv, 0, i3, i4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, int, int)>(__func)(pv, i3, i4);
#endif
}

cell Call_Void_Str_Float_Float_Float(AMX *amx, cell *params)
{
	SETUP(4);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *, float, float, float)>(__func)(pv, 0, sz3, f4, f5, f6);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *, float, float, float)>(__func)(pv, sz3, f4, f5, f6);
#endif

	return 1;
}

cell Call_Void_Str_Float_Float_Float_Int_Cbase(AMX *amx, cell *params)
{
	SETUP(6);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));
	int i7=*MF_GetAmxAddr(amx, params[7]);
	int id8=*MF_GetAmxAddr(amx, params[8]);

	CHECK_ENTITY(id8);

	void *p8 = TypeConversion.id_to_cbase(id8);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *, float, float, float, int, void *)>(__func)(pv, 0, sz3, f4, f5, f6, i7, p8);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *, float, float, float, int, void *)>(__func)(pv, sz3, f4, f5, f6, i7, p8);
#endif

	return 1;
}

cell Call_Int_Vector_Vector_Float_Float(AMX *amx, cell *params)
{
	SETUP(4);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, Vector, Vector, float, float)>(__func)(pv, 0, v3, v4, f5, f6);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, Vector, Vector, float, float)>(__func)(pv, v3, v4, f5, f6);
#endif
}

cell Call_Int_Short(AMX *amx, cell *params)
{
	SETUP(1);

	short s3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, short)>(__func)(pv, 0, s3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, short)>(__func)(pv, s3);
#endif
}

cell Call_Void_Entvar_Entvar_Float_Int_Int(AMX *amx, cell *params)
{
	SETUP(5);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);
	int i7=*MF_GetAmxAddr(amx, params[7]);

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, entvars_t *, float, int, int)>(__func)(pv, 0, ev3, ev4, f5, i6, i7);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, entvars_t *, float, int, int)>(__func)(pv, ev3, ev4, f5, i6, i7);
#endif

	return 1;
}

cell Call_Void_Vector_Entvar_Entvar_Float_Int_Int(AMX *amx, cell *params)
{
	SETUP(6);

	Vector v3;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	int id4=*MF_GetAmxAddr(amx, params[4]);
	int id5=*MF_GetAmxAddr(amx, params[5]);
	float f6=amx_ctof(*MF_GetAmxAddr(amx, params[6]));
	int i7=*MF_GetAmxAddr(amx, params[7]);
	int i8=*MF_GetAmxAddr(amx, params[8]);

	CHECK_ENTITY(id4);
	CHECK_ENTITY(id5);

	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);
	entvars_t *ev5 = TypeConversion.id_to_entvars(id5);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, Vector, entvars_t *, entvars_t *, float, int, int)>(__func)(pv, 0, v3, ev4, ev5, f6, i7, i8);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, Vector, entvars_t *, entvars_t *, float, int, int)>(__func)(pv, v3, ev4, ev5, f6, i7, i8);
#endif

	return 1;
}

cell Call_Float_Int_Float(AMX *amx, cell *params)
{
	SETUP(3);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));

#if defined(_WIN32)
	float ret=reinterpret_cast<float (__fastcall *)(void *, int, int, float)>(__func)(pv, 0, i3, f4);
#elif defined(__linux__) || defined(__APPLE__)
	float ret=reinterpret_cast<float (*)(void *, int, float)>(__func)(pv, i3, f4);
#endif
	*MF_GetAmxAddr(amx, params[5])=amx_ftoc(ret);

	return 1;	
}

cell Call_Int_Str(AMX *amx, cell *params)
{
	SETUP(1);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, const char *)>(__func)(pv, 0, sz3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, const char *)>(__func)(pv, sz3);
#endif
}

cell Call_Void_Edict(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);

	edict_t *ed3 = TypeConversion.id_to_edict(id3);

#if defined(_WIN32)
	reinterpret_cast<int (__fastcall *)(void*, int, edict_t *)>(__func)(pv, 0, ed3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<int (*)(void *, edict_t *)>(__func)(pv, ed3);
#endif

	return 1;
}

cell Call_Void_Int_Str_Bool(AMX *amx, cell *params)
{
	SETUP(4);

	char* sz4 = new char[48];
	int i3=*MF_GetAmxAddr(amx, params[3]);
	bool b5=*MF_GetAmxAddr(amx, params[5]) ? true : false;

#if defined(_WIN32)
	reinterpret_cast<void(__fastcall *)(void*, int, int, char *, bool)>(__func)(pv, 0, i3, sz4, b5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, int, char *, bool)>(__func)(pv, i3, sz4, b5);
#endif

	MF_SetAmxString(amx, params[4], sz4 ? sz4 : "", *MF_GetAmxAddr(amx, params[6]));
	delete [] sz4;
	return 1;
}

cell Call_Void_Vector_Vector(AMX *amx, cell *params)
{
	SETUP(2);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, Vector, Vector)>(__func)(pv, 0, v3, v4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, Vector, Vector)>(__func)(pv, v3, v4);
#endif

	return 1;
}

cell Call_Void_Str_Bool(AMX *amx, cell *params)
{
	SETUP(2);

	const char *sz3=STRING(ALLOC_STRING(MF_GetAmxString(amx, params[3], 0, NULL)));
	bool b4=*MF_GetAmxAddr(amx, params[4]) ? true : false;

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *, bool)>(__func)(pv, 0, sz3, b4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *, bool)>(__func)(pv, sz3, b4);
#endif

	return 1;
}

cell Call_Int_Str_Str_Int_Str_Int_Int(AMX* amx, cell* params)
{
	SETUP(6);

	const char *sz3=STRING(ALLOC_STRING(MF_GetAmxString(amx, params[3], 0, NULL)));
	const char *sz4=STRING(ALLOC_STRING(MF_GetAmxString(amx, params[4], 1, NULL)));
	const char *sz6=STRING(ALLOC_STRING(MF_GetAmxString(amx, params[6], 2, NULL)));

	int i5=*MF_GetAmxAddr(amx, params[5]);
	int i7=*MF_GetAmxAddr(amx, params[7]);
	int i8=*MF_GetAmxAddr(amx, params[8]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, const char *, const char *, int, const char *, int, int)>(__func)(pv, 0, sz3, sz4, i5, sz6, i7, i8);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, const char *, const char *, int, const char *, int, int)>(__func)(pv, sz3, sz4, i5, sz6, i7, i8);
#endif
}

cell Call_Int_Int_Int_Float_Int(AMX* amx, cell* params)
{
	SETUP(4);

	int i3=*MF_GetAmxAddr(amx, params[3]);
	int i4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));
	int i6=*MF_GetAmxAddr(amx, params[6]);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, int, int, float, int)>(__func)(pv, 0, i3, i4, f5, i6);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, int, int, float, int)>(__func)(pv, i3, i4, f5, i6);
#endif
}

cell Call_Void_Str_Int(AMX *amx, cell *params)
{
	SETUP(2);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	int i4=*MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *, int)>(__func)(pv, 0, sz3, i4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *, int)>(__func)(pv, sz3, i4);
#endif

	return 1;
}

cell Call_Void_Cbase_Int(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);

	void *p8 = TypeConversion.id_to_cbase(id3);

	int i4=*MF_GetAmxAddr(amx, params[4]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *, int)>(__func)(pv, 0, p8, i4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *, int)>(__func)(pv, p8, i4);
#endif

	return 1;
}

cell Call_Void_Str(AMX *amx, cell *params)
{
	SETUP(1);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *)>(__func)(pv, 0, sz3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *)>(__func)(pv, sz3);
#endif

	return 1;
}

cell Call_Void_Vector(AMX *amx, cell *params)
{
	SETUP(1);

	Vector v3;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, Vector)>(__func)(pv, 0, v3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, Vector)>(__func)(pv, v3);
#endif

	return 1;
}

cell Call_Int_Str_Vector_Str(AMX* amx, cell* params)
{
	SETUP(3);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	char *sz5=MF_GetAmxString(amx, params[5], 1, NULL);

	Vector v4;

	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, const char *, Vector,  const char *)>(__func)(pv, 0, sz3, v4, sz5);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, const char *, Vector,  const char *)>(__func)(pv, sz3, v4, sz5);
#endif
}

cell Call_Int_Str_Str(AMX* amx, cell* params)
{
	SETUP(2);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	char *sz4=MF_GetAmxString(amx, params[4], 1, NULL);

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, const char *, const char *)>(__func)(pv, 0, sz3, sz4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, const char *, const char *)>(__func)(pv, sz3, sz4);
#endif
}

cell Call_Void_Float_Float(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));
	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, float, float)>(__func)(pv, 0, f3, f4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, float, float)>(__func)(pv, f3, f4);
#endif

	return 1;
}

cell Call_Void_Str_Str_Int(AMX *amx, cell *params)
{
	SETUP(3);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);
	char *sz4=MF_GetAmxString(amx, params[4], 1, NULL);
	int i5=*MF_GetAmxAddr(amx, params[5]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, const char *, const char *, int)>(__func)(pv, 0, sz3, sz4, i5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, const char *, const char *, int)>(__func)(pv, sz3, sz4, i5);
#endif

	return 1;
}

cell Call_Int_pVector_pVector_Cbase_pFloat(AMX *amx, cell *params)
{
	SETUP(4);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);

	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	int id5=*MF_GetAmxAddr(amx, params[5]);
	CHECK_ENTITY(id5);
	void *p5 = TypeConversion.id_to_cbase(id5);

	float f6;

#if defined(_WIN32)
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector*, Vector*, void*, float*)>(__func)(pv, 0, &v3, &v4, p5, &f6);
#elif defined(__linux__) || defined(__APPLE__)
	int ret=reinterpret_cast<int (*)(void *, Vector*, Vector*, void*, float*)>(__func)(pv, &v3, &v4, p5, &f6);
#endif

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	*MF_GetAmxAddr(amx, params[6]) = amx_ftoc(f6);

	return ret;
}

cell Call_Void_Cbase_pVector_Float(AMX *amx, cell *params)
{
	SETUP(3);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);

	void *i3 = TypeConversion.id_to_cbase(id3);

	Vector v4;
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *, Vector *, float)>(__func)(pv, 0, i3, &v4, f5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *, Vector *, float)>(__func)(pv, i3, &v4, f5);
#endif

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	return 1;
}

cell Call_Int_pVector_pVector_Float_Cbase_pVector(AMX *amx, cell *params)
{
	SETUP(5);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);

	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));

	int id6=*MF_GetAmxAddr(amx, params[6]);
	CHECK_ENTITY(id6);
	void *p6 = TypeConversion.id_to_cbase(id6);

	Vector v7;
	float *fl7=(float *)MF_GetAmxAddr(amx, params[7]);

	v7.x=fl7[0];
	v7.y=fl7[1];
	v7.z=fl7[2];

#if defined(_WIN32)
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector*, Vector*, float, void*, Vector*)>(__func)(pv, 0, &v3, &v4, f5, p6, &v7);
#elif defined(__linux__) || defined(__APPLE__)
	int ret=reinterpret_cast<int (*)(void *, Vector*, Vector*, float, void*, Vector*)>(__func)(pv, &v3, &v4, f5, p6, &v7);
#endif

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	fl7[0]=v7.x;
	fl7[1]=v7.y;
	fl7[2]=v7.z;

	return ret;
}

cell Call_Int_Cbase_Bool(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	void *pv1 = TypeConversion.id_to_cbase(id3);

	bool b4=*MF_GetAmxAddr(amx, params[4]) ? true : false;

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void*, int, void *, bool)>(__func)(pv, 0, pv1, b4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, void *, bool)>(__func)(pv, pv1, b4);
#endif
}

cell Call_Int_Vector_Vector(AMX *amx, cell *params)
{
	SETUP(2);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, Vector, Vector)>(__func)(pv, 0, v3, v4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, Vector, Vector)>(__func)(pv, v3, v4);
#endif
}

cell Call_Int_Entvar_Float(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY(id3);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);

	float f4=amx_ctof(*MF_GetAmxAddr(amx, params[4]));

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, entvars_t *, float)>(__func)(pv, 0, ev3, f4);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, entvars_t *, float)>(__func)(pv, ev3, f4);
#endif
}

cell Call_Float_Float(AMX *amx, cell *params)
{
	SETUP(2);

	float f3=amx_ctof(*MF_GetAmxAddr(amx, params[3]));

#if defined(_WIN32)
	float ret=reinterpret_cast<float (__fastcall *)(void *, int, float)>(__func)(pv, 0, f3);
#elif defined(__linux__) || defined(__APPLE__)
	float ret=reinterpret_cast<float (*)(void *, float)>(__func)(pv, f3);
#endif
	*MF_GetAmxAddr(amx, params[4])=amx_ftoc(ret);

	return 1;
}

cell Call_Void_Entvar_Entvar_Float(AMX *amx, cell *params)
{
	SETUP(3);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	int id4=*MF_GetAmxAddr(amx, params[4]);
	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));

	CHECK_ENTITY(id3);
	CHECK_ENTITY(id4);

	entvars_t *ev3 = TypeConversion.id_to_entvars(id3);
	entvars_t *ev4 = TypeConversion.id_to_entvars(id4);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, entvars_t *, entvars_t *, float)>(__func)(pv, 0, ev3, ev4, f5);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, entvars_t *, entvars_t *, float)>(__func)(pv, ev3, ev4, f5);
#endif

	return 1;
}

cell Call_Bool_Void(AMX *amx, cell *params)
{
	SETUP(0);

#if defined(_WIN32)
	return reinterpret_cast<bool (__fastcall *)(void*, int)>(__func)(pv, 0);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<bool (*)(void *)>(__func)(pv);
#endif
}

cell Call_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool(AMX *amx, cell *params)
{
	SETUP(7);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);

	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	float f5=amx_ctof(*MF_GetAmxAddr(amx, params[5]));

	int id6=*MF_GetAmxAddr(amx, params[6]);
	CHECK_ENTITY(id6);
	void *p6 = TypeConversion.id_to_cbase(id6);

	Vector v7;
	float *fl7=(float *)MF_GetAmxAddr(amx, params[7]);

	v7.x=fl7[0];
	v7.y=fl7[1];
	v7.z=fl7[2];

	Vector v8;
	float *fl8=(float *)MF_GetAmxAddr(amx, params[8]);

	v8.x=fl8[0];
	v8.y=fl8[1];
	v8.z=fl8[2];

	bool b9=*MF_GetAmxAddr(amx, params[9]) > 0;

#if defined(_WIN32)
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector*, Vector*, float, void*, Vector*, Vector*, bool)>(__func)(pv, 0, &v3, &v4, f5, p6, &v7, &v8, b9);
#elif defined(__linux__) || defined(__APPLE__)
	int ret=reinterpret_cast<int (*)(void *, Vector*, Vector*, float, void*, Vector*, Vector*, bool)>(__func)(pv, &v3, &v4, f5, p6, &v7, &v8, b9);
#endif

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	fl7[0]=v7.x;
	fl7[1]=v7.y;
	fl7[2]=v7.z;

	fl8[0]=v8.x;
	fl8[1]=v8.y;
	fl8[2]=v8.z;

	return ret;
}

cell Call_Int_Vector_Cbase(AMX *amx, cell *params)
{
	SETUP(2);

	Vector v3;
	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	int id4=*MF_GetAmxAddr(amx, params[4]);
	CHECK_ENTITY(id4);
	void *p4 = TypeConversion.id_to_cbase(id4);

#if defined(_WIN32)
	int ret=reinterpret_cast<int (__fastcall *)(void *, int, Vector, void*)>(__func)(pv, 0, v3, p4);
#elif defined(__linux__) || defined(__APPLE__)
	int ret=reinterpret_cast<int (*)(void *, Vector, void*)>(__func)(pv, v3, p4);
#endif

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	return ret;
}

cell Call_Int_Vector(AMX *amx, cell *params)
{
	SETUP(1);

	Vector v3;
	Vector v4;

	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

#if defined(_WIN32)
	return reinterpret_cast<int (__fastcall *)(void *, int, Vector)>(__func)(pv, 0, v3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<int (*)(void *, Vector)>(__func)(pv, v3);
#endif
}

cell Call_Int_Cbase_pVector(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);
	void *pv1 = TypeConversion.id_to_cbase(id3);

	Vector v4;
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

#if defined(_WIN32)
	int ret = reinterpret_cast<int (__fastcall *)(void*, int, void *, Vector *)>(__func)(pv, 0, pv1, &v4);
#elif defined(__linux__) || defined(__APPLE__)
	int ret = reinterpret_cast<int (*)(void *, void *, Vector *)>(__func)(pv, pv1, &v4);
#endif

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	return ret;
}

cell Call_Void_Bool(AMX *amx, cell *params)
{
	SETUP(1);

	bool b3=*MF_GetAmxAddr(amx, params[3]) ? true : false;

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, bool)>(__func)(pv, 0, b3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, bool)>(__func)(pv, b3);
#endif

	return 1;
}

cell Call_Bool_Cbase(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);
	void *pv1 = TypeConversion.id_to_cbase(id3);

#if defined(_WIN32)
	return reinterpret_cast<bool (__fastcall *)(void*, int, void*)>(__func)(pv, 0, pv1);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<bool (*)(void *, void*)>(__func)(pv, pv1);
#endif
}

cell Call_Bool_Int(AMX *amx, cell *params)
{
	SETUP(1);

	int id3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	return reinterpret_cast<bool (__fastcall *)(void*, int, int)>(__func)(pv, 0, id3);
#elif defined(__linux__) || defined(__APPLE__)
	return reinterpret_cast<bool (*)(void *, int)>(__func)(pv, id3);
#endif
}

cell Call_Void_Cbase_Float(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);

	void *p8 = TypeConversion.id_to_cbase(id3);
	float f4 = amx_ctof(*MF_GetAmxAddr(amx, params[4]));

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *, float)>(__func)(pv, 0, p8, f4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *, float)>(__func)(pv, p8, f4);
#endif

	return 1;
}

cell Call_Void_Cbase_Bool(AMX *amx, cell *params)
{
	SETUP(2);

	int id3=*MF_GetAmxAddr(amx, params[3]);
	CHECK_ENTITY(id3);

	void *p8 = TypeConversion.id_to_cbase(id3);
	bool b4 = *MF_GetAmxAddr(amx, params[4]) ? true : false;

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void*, int, void *, bool)>(__func)(pv, 0, p8, b4);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, void *, bool)>(__func)(pv, p8, b4);
#endif

	return 1;
}

cell Call_Vector_Vector_Vector_Vector(AMX *amx, cell *params)
{
	SETUP(4);

	Vector v3;
	float *fl3=(float *)MF_GetAmxAddr(amx, params[3]);
	v3.x=fl3[0];
	v3.y=fl3[1];
	v3.z=fl3[2];

	Vector v4;
	float *fl4=(float *)MF_GetAmxAddr(amx, params[4]);
	v4.x=fl4[0];
	v4.y=fl4[1];
	v4.z=fl4[2];

	Vector v5;
	float *fl5=(float *)MF_GetAmxAddr(amx, params[5]);
	v5.x=fl5[0];
	v5.y=fl5[1];
	v5.z=fl5[2];

#if defined(_WIN32)
	Vector ret;
	reinterpret_cast<void (__fastcall *)(void *, int, Vector*, Vector, Vector, Vector)>(__func)(pv, 0, &ret, v3, v4, v5);
#elif defined(__linux__) || defined(__APPLE__)
	Vector ret=reinterpret_cast<Vector (*)(void *, Vector, Vector, Vector)>(__func)(pv, v3, v4, v5);
#endif
	float *out=(float *)MF_GetAmxAddr(amx, params[6]);
	out[0]=ret.x;
	out[1]=ret.y;
	out[2]=ret.z;

	fl3[0]=v3.x;
	fl3[1]=v3.y;
	fl3[2]=v3.z;

	fl4[0]=v4.x;
	fl4[1]=v4.y;
	fl4[2]=v4.z;

	fl5[0]=v5.x;
	fl5[1]=v5.y;
	fl5[2]=v5.z;

	return 1;
} 

cell Call_Str_Str(AMX *amx, cell *params)
{
	SETUP(3);

	char *sz3=MF_GetAmxString(amx, params[3], 0, NULL);

#if defined(_WIN32)
	char *v=reinterpret_cast<char *(__fastcall *)(void *, int, const char*)>(__func)(pv, 0, sz3);
#elif defined(__linux__) || defined(__APPLE__)
	char *v=reinterpret_cast<char *(*)(void *, const char *)>(__func)(pv, sz3);
#endif
	return MF_SetAmxString(amx, params[4], v == NULL ? "" : v, *MF_GetAmxAddr(amx, params[5]));
}

cell Call_Void_Short(AMX *amx, cell *params)
{
	SETUP(1);

	short i3=*MF_GetAmxAddr(amx, params[3]);

#if defined(_WIN32)
	reinterpret_cast<void (__fastcall *)(void *, int, short)>(__func)(pv, 0, i3);
#elif defined(__linux__) || defined(__APPLE__)
	reinterpret_cast<void (*)(void *, short)>(__func)(pv, i3);
#endif

	return 1;
}


cell Call_Deprecated(AMX *amx, cell *params)
{
	MF_LogError(amx, AMX_ERR_NATIVE, "Ham function is deprecated.");

	return 0;
}
