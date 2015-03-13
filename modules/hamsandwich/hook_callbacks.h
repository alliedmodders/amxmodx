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

#ifndef HOOK_CALLBACKS_H
#define HOOK_CALLBACKS_H

// RT_<TYPE> is true if the function returns void, false otherwise 
//				(it also would be true for large return functions such as Vector)
// RB_<TYPE> is true if the function returns an object and requires a pointer to a buffer as its first parameter in order to store its value 
// PC_<TYPE> is how many dwords get passed to the function (minus the "this" pointer)
//				(it is one larger for large return functions such as Vector)

const bool RT_Void_Void = true;
const bool RB_Void_Void = false;
const int PC_Void_Void = 0;
void Hook_Void_Void(Hook *hook, void *pthis);

const bool RT_Int_Void = false;
const bool RB_Int_Void = false;
const int PC_Int_Void = 0;
int  Hook_Int_Void(Hook *hook, void *pthis);

const bool RT_Void_Entvar = true;
const bool RB_Void_Entvar = false;
const int PC_Void_Entvar = 1;
void Hook_Void_Entvar(Hook *hook, void *pthis, entvars_t *entvar);

const bool RT_Void_Cbase = true;
const bool RB_Void_Cbase = false;
const int PC_Void_Cbase = 1;
void Hook_Void_Cbase(Hook *hook, void *pthis, void *other);

const bool RT_Int_Float_Int = false;
const bool RB_Int_Float_Int = false;
const int PC_Int_Float_Int = 2;
int  Hook_Int_Float_Int(Hook *hook, void *pthis, float f1, int i1);
	
const bool RT_Int_Float_Int_Int = false;
const bool RB_Int_Float_Int_Int = false;
const int PC_Int_Float_Int_Int = 3;
int  Hook_Int_Float_Int_Int(Hook *hook, void *pthis, float f1, int i1, int i2);

const bool RT_Void_Entvar_Int = true;
const bool RB_Void_Entvar_Int = false;
const int PC_Void_Entvar_Int = 2;
void Hook_Void_Entvar_Int(Hook *hook, void *ptis, entvars_t *ev1, int i1);

const bool RT_Void_Entvar_Entvar_Int = true;
const bool RB_Void_Entvar_Entvar_Int = false;
const int PC_Void_Entvar_Entvar_Int = 3;
void Hook_Void_Entvar_Entvar_Int(Hook *hook, void *ptis, entvars_t *ev1, entvars_t *ev2, int i1);

const bool RT_Int_Cbase = false;
const bool RB_Int_Cbase = false;
const int PC_Int_Cbase = 1;
int  Hook_Int_Cbase(Hook *hook, void *pthis, void *cb1);

const bool RT_Void_Int_Int = true;
const bool RB_Void_Int_Int = false;
const int PC_Void_Int_Int = 2;
void Hook_Void_Int_Int(Hook *hook, void *pthis, int i1, int i2);

const bool RT_Int_Int_Str_Int = false;
const bool RB_Int_Int_Str_Int = false;
const int PC_Int_Int_Str_Int = 3;
int  Hook_Int_Int_Str_Int(Hook *hook, void *pthis, int i1, const char *sz1,
						  int i2);

const bool RT_Int_Int_Str_Int_Int = false;
const bool RB_Int_Int_Str_Int_Int = false;
const int PC_Int_Int_Str_Int_Int = 4;
int  Hook_Int_Int_Str_Int_Int(Hook *hook, void *pthis, int i1, const char *sz1, int i2, int i3);

const bool RT_Int_Int = false;
const bool RB_Int_Int = false;
const int PC_Int_Int = 1;
int  Hook_Int_Int(Hook *hook, void *pthis, int i1);

const bool RT_Int_Entvar = false;
const bool RB_Int_Entvar = false;
const int PC_Int_Entvar = 1;
int  Hook_Int_Entvar(Hook *hook, void *pthis, entvars_t *ev1);

const bool RT_Int_Entvar_Entvar_Float_Int = false;
const bool RB_Int_Entvar_Entvar_Float_Int = false;
const int PC_Int_Entvar_Entvar_Float_Int = 4;
int  Hook_Int_Entvar_Entvar_Float_Int(Hook *hook, void *pthis, 
									  entvars_t *inflictor, 
									  entvars_t *attacker, float damage, 
									  int damagebits);

const bool RT_Int_Entvar_Entvar_Float_Float_Int = false;
const bool RB_Int_Entvar_Entvar_Float_Float_Int = false;
const int PC_Int_Entvar_Entvar_Float_Float_Int = 5;
int  Hook_Int_Entvar_Entvar_Float_Float_Int(Hook *hook, void *pthis, 
									  entvars_t *inflictor, 
									  entvars_t *attacker, float damage, 
									  float unknown, int damagebits);

const bool RT_Void_Int = true;
const bool RB_Void_Int = false;
const int PC_Void_Int = 1;
void Hook_Void_Int(Hook *hook, void *pthis, int i1);

const bool RT_Void_Cbase_Cbase_Int_Float = true;
const bool RB_Void_Cbase_Cbase_Int_Float = false;
const int PC_Void_Cbase_Cbase_Int_Float = 4;
void Hook_Void_Cbase_Cbase_Int_Float(Hook *hook, void *pthis, void *cb1, 
									 void *cb2, int i1, float f1);

const bool RT_Vector_Float_Cbase_Int = true;
const bool RB_Vector_Float_Cbase_Int = true;
const int PC_Vector_Float_Cbase_Int = 4;
#if defined(_WIN32)
void Hook_Vector_Float_Cbase_Int(Hook *hook, void *pthis, Vector *out, float f1, void *cb, int i1);
#elif defined(__linux__) || defined(__APPLE__)
void Hook_Vector_Float_Cbase_Int(Hook *hook, Vector *out, void *pthis, float f1, void *cb, int i1);
#endif

const bool RT_Void_Entvar_Float_Vector_Trace_Int = true;
const bool RB_Void_Entvar_Float_Vector_Trace_Int = false;
const int PC_Void_Entvar_Float_Vector_Trace_Int = 7;
void Hook_Void_Entvar_Float_Vector_Trace_Int(Hook *hook, void *pthis, 
											 entvars_t *ev1, float f1, 
											 Vector v1, TraceResult *tr1, 
											 int i1);

const bool RT_Void_Float_Vector_Trace_Int = true;
const bool RB_Void_Float_Vector_Trace_Int = false;
const int PC_Void_Float_Vector_Trace_Int = 6;
void Hook_Void_Float_Vector_Trace_Int(Hook *hook, void *pthis, float f1,
											Vector v1, TraceResult *tr1, 
											int i1);

const bool RT_Str_Void = false;
const bool RB_Str_Void = false;
const int PC_Str_Void = 0;
const char *Hook_Str_Void(Hook *hook, void *pthis);

const bool RT_Cbase_Void = false;
const bool RB_Cbase_Void = false;
const int PC_Cbase_Void = 0;
void *Hook_Cbase_Void(Hook *hook, void *pthis);

// HACK: I'm too lazy to fix up trampoline generator to deal with
//       special return values.  this is so much easier.
const bool RT_Vector_Void = true;
const bool RB_Vector_Void = true;
const int PC_Vector_Void = 1;
#if defined(_WIN32)
void Hook_Vector_Void(Hook *hook, void *pthis, Vector *out);
#elif defined(__linux__) || defined(__APPLE__)
void Hook_Vector_Void(Hook *hook, Vector *out, void *pthis);
#endif

const bool RT_Vector_pVector = true;
const bool RB_Vector_pVector = true;
const int PC_Vector_pVector = 2;
#if defined(_WIN32)
void Hook_Vector_pVector(Hook *hook, void *pthis, Vector *out, Vector *v1);
#elif defined(__linux__) || defined(__APPLE__)
void Hook_Vector_pVector(Hook *hook, Vector *out, void *pthis, Vector *v1);
#endif

const bool RT_Int_pVector = false;
const bool RB_Int_pVector = false;
const int PC_Int_pVector = 1;
int Hook_Int_pVector(Hook *hook, void *pthis, Vector *v1);

const bool RT_Void_Entvar_Float_Float = true;
const bool RB_Void_Entvar_Float_Float = false;
const int PC_Void_Entvar_Float_Float = 3;
void Hook_Void_Entvar_Float_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1, float f2);

const bool RT_Void_pFloat_pFloat = true;
const bool RB_Void_pFloat_pFloat = false;
const int PC_Void_pFloat_pFloat = 2;
void Hook_Void_pFloat_pFloat(Hook *hook, void *pthis, float *f1, float *f2);

const bool RT_Void_Entvar_Float = true;
const bool RB_Void_Entvar_Float = false;
const int PC_Void_Entvar_Float = 2;
void Hook_Void_Entvar_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1);


const bool RT_Void_Int_Int_Int = true;
const bool RB_Void_Int_Int_Int = false;
const int PC_Void_Int_Int_Int = 3;
void Hook_Void_Int_Int_Int(Hook *hook, void *pthis, int i1, int i2, int i3);

const bool RT_Void_ItemInfo = true;
const bool RB_Void_ItemInfo = false;
const int PC_Void_ItemInfo = 1;
void Hook_Void_ItemInfo(Hook *hook, void *pthis, void *iteminfo);


const bool RT_Float_Void = false;
const bool RB_Float_Void = false;
const int PC_Float_Void = 0;
float Hook_Float_Void(Hook *hook, void *pthis);

const bool RT_Float_Int = false;
const bool RB_Float_Int = false;
const int PC_Float_Int = 1;
float Hook_Float_Int(Hook *hook, void *pthis, int i1);

const bool RT_Void_Float_Int = true;
const bool RB_Void_Float_Int = false;
const int PC_Void_Float_Int = 2;
void Hook_Void_Float_Int(Hook *hook, void *pthis, float f1, int i1);

const bool RT_Float_Float_Cbase = true;
const bool RB_Float_Float_Cbase = false;
const int PC_Float_Float_Cbase = 2;
float Hook_Float_Float_Cbase(Hook *hook, void *pthis, float f1, void *cb1);

const bool RT_Void_Float = true;
const bool RB_Void_Float = false;
const int PC_Void_Float = 1;
void Hook_Void_Float(Hook *hook, void *pthis, float f1);

const bool RT_Void_Float_Float_Float_Int = true;
const bool RB_Void_Float_Float_Float_Int = false;
const int PC_Void_Float_Float_Float_Int = 4;
void Hook_Void_Float_Float_Float_Int(Hook *hook, void *pthis, float f1, float f2, float f3, int i1);

const bool RT_Vector_Float = true;
const bool RB_Vector_Float = true;
const int PC_Vector_Float = 2;
#if defined(_WIN32)
void Hook_Vector_Float(Hook *hook, void *pthis, Vector *out, float f1);
#elif defined(__linux__) || defined(__APPLE__)
void Hook_Vector_Float(Hook *hook, Vector *out, void *pthis, float f1);
#endif

const bool RT_Void_Float_Cbase = true;
const bool RB_Void_Float_Cbase = false;
const int PC_Void_Float_Cbase = 2;
void Hook_Void_Float_Cbase(Hook *hook, void *pthis, float f1, void *cb);

const bool RT_Int_Float_Float = false;
const bool RB_Int_Float_Float = false;
const int PC_Int_Float_Float = 2;
int Hook_Int_Float_Float(Hook *hook, void *pthis, float f1, float f2);

const bool RT_Int_Float = false;
const bool RB_Int_Float = false;
const int PC_Int_Float = 1;
int Hook_Int_Float(Hook *hook, void *pthis, float f1);

const bool RT_Int_Int_Int = false;
const bool RB_Int_Int_Int = false;
const int PC_Int_Int_Int = 2;
int Hook_Int_Int_Int(Hook *hook, void *pthis, int i1, int i2);

const bool RT_Void_Str_Float_Float_Float = true;
const bool RB_Void_Str_Float_Float_Float = false;
const int PC_Void_Str_Float_Float_Float = 4;
void Hook_Void_Str_Float_Float_Float(Hook *hook, void *pthis, const char *sz1, float f1, float f2, float f3);

const bool RT_Void_Str_Float_Float_Float_Int_Cbase = true;
const bool RB_Void_Str_Float_Float_Float_Int_Cbase = false;
const int PC_Void_Str_Float_Float_Float_Int_Cbase = 6;
void Hook_Void_Str_Float_Float_Float_Int_Cbase(Hook *hook, void *pthis, const char *sz1, float f1, float f2, float f3, int i1, void *cb);

const bool RT_Int_Vector_Vector_Float_Float= false;
const bool RB_Int_Vector_Vector_Float_Float = false;
const int PC_Int_Vector_Vector_Float_Float = 8;
int Hook_Int_Vector_Vector_Float_Float(Hook *hook, void *pthis, Vector v1, Vector v2, float f1, float f2);

const bool RT_Int_Short = false;
const bool RB_Int_Short = false;
const int PC_Int_Short = 1;
int Hook_Int_Short(Hook *hook, void *pthis, short s1);

const bool RT_Void_Entvar_Entvar_Float_Int_Int = true;
const bool RB_Void_Entvar_Entvar_Float_Int_Int = false;
const int PC_Void_Entvar_Entvar_Float_Int_Int = 5;
void  Hook_Void_Entvar_Entvar_Float_Int_Int(Hook *hook, void *pthis, 
	entvars_t *inflictor, 
	entvars_t *attacker, float damage, 
	int classignore, int damagebits);

const bool RT_Void_Vector_Entvar_Entvar_Float_Int_Int = true;
const bool RB_Void_Vector_Entvar_Entvar_Float_Int_Int = false;
const int PC_Void_Vector_Entvar_Entvar_Float_Int_Int = 8;
void  Hook_Void_Vector_Entvar_Entvar_Float_Int_Int(Hook *hook, void *pthis, 
	Vector source,
	entvars_t *inflictor, 
	entvars_t *attacker, float damage, 
	int classignore, int damagebits);


const bool RT_Float_Int_Float = false;
const bool RB_Float_Int_Float = false;
const int PC_Float_Int_Float = 2;
float Hook_Float_Int_Float(Hook *hook, void *pthis, int i1, float f2);

const bool RT_Int_Str = false;
const bool RB_Int_Str = false;
const int PC_Int_Str = 1;
int Hook_Int_Str(Hook *hook, void *pthis, const char *sz1);

const bool RT_Void_Edict = true;
const bool RB_Void_Edict = false;
const int PC_Void_Edict = 1;
void Hook_Void_Edict(Hook *hook, void *pthis, edict_t *ed1 );

const bool RT_Void_Int_Str_Bool = true;
const bool RB_Void_Int_Str_Bool = false;
const int PC_Void_Int_Str_Bool = 3;
void Hook_Void_Int_Str_Bool(Hook *hook, void *pthis, int i1, const char *sz2, bool b3);

const bool RT_Void_Vector_Vector= true;
const bool RB_Void_Vector_Vector = false;
const int PC_Void_Vector_Vector = 6;
void Hook_Void_Vector_Vector(Hook *hook, void *pthis, Vector v1, Vector v2);

const bool RT_Void_Str_Bool = true;
const bool RB_Void_Str_Bool = false;
const int PC_Void_Str_Bool = 2;
void Hook_Void_Str_Bool(Hook *hook, void *pthis, const char *sz1, bool b2);

const bool RT_Int_Str_Str_Int_Str_Int_Int = false;
const bool RB_Int_Str_Str_Int_Str_Int_Int = false;
const int PC_Int_Str_Str_Int_Str_Int_Int = 6;
int Hook_Int_Str_Str_Int_Str_Int_Int(Hook *hook, void *pthis, const char *sz1, const char *sz2, int i1, const char *sz3, int i2, int i3);

const bool RT_Int_Int_Int_Float_Int = false;
const bool RB_Int_Int_Int_Float_Int = false;
const int PC_Int_Int_Int_Float_Int = 4;
int Hook_Int_Int_Int_Float_Int(Hook *hook, void *pthis, int i1, int i2, float f1, int i3);

const bool RT_Void_Str_Int = true;
const bool RB_Void_Str_Int = false;
const int PC_Void_Str_Int = 2;
void Hook_Void_Str_Int(Hook *hook, void *pthis, const char *sz1, int i2);

const bool RT_Void_Cbase_Int = true;
const bool RB_Void_Cbase_Int = false;
const int PC_Void_Cbase_Int = 2;
void Hook_Void_Cbase_Int(Hook *hook, void *pthis, void *p1, int i1);

const bool RT_Void_Str = true;
const bool RB_Void_Str = false;
const int PC_Void_Str = 1;
void Hook_Void_Str(Hook *hook, void *pthis, const char *sz1);

const bool RT_Void_Vector = true;
const bool RB_Void_Vector = false;
const int PC_Void_Vector = 3;
void Hook_Void_Vector(Hook *hook, void *pthis, Vector v1);

const bool RT_Int_Str_Vector_Str = false;
const bool RB_Int_Str_Vector_Str = false;
const int PC_Int_Str_Vector_Str = 5;
int Hook_Int_Str_Vector_Str(Hook *hook, void *pthis, const char *sz1, Vector v2, const char *sz2);

const bool RT_Int_Str_Str = false;
const bool RB_Int_Str_Str = false;
const int PC_Int_Str_Str = 2;
int Hook_Int_Str_Str(Hook *hook, void *pthis, const char *sz1, const char *sz2);

const bool RT_Void_Float_Float = true;
const bool RB_Void_Float_Float = false;
const int PC_Void_Float_Float = 2;
void Hook_Void_Float_Float(Hook *hook, void *pthis, float f1, float f2);

const bool RT_Void_Str_Str_Int = true;
const bool RB_Void_Str_Str_Int = false;
const int PC_Void_Str_Str_Int = 3;
void Hook_Void_Str_Str_Int(Hook *hook, void *pthis, const char *sz1, const char *sz2, int i3);

const bool RT_Int_pVector_pVector_Cbase_pFloat = false;
const bool RB_Int_pVector_pVector_Cbase_pFloat = false;
const int PC_Int_pVector_pVector_Cbase_pFloat = 4;
int Hook_Int_pVector_pVector_Cbase_pFloat(Hook *hook, void *pthis, Vector *v1, Vector *v2, void* cb, float* fl);

const bool RT_Void_Cbase_pVector_Float = true;
const bool RB_Void_Cbase_pVector_Float = false;
const int PC_Void_Cbase_pVector_Float = 3;
void Hook_Void_Cbase_pVector_Float(Hook *hook, void *pthis, void *p1, Vector *v1, float fl);

const bool RT_Int_pVector_pVector_Float_Cbase_pVector = false;
const bool RB_Int_pVector_pVector_Float_Cbase_pVector = false;
const int PC_Int_pVector_pVector_Float_Cbase_pVector = 5;
int Hook_Int_pVector_pVector_Float_Cbase_pVector(Hook *hook, void *pthis, Vector *v1, Vector *v2, float fl, void* cb, Vector *v3);

const bool RT_Int_Cbase_Bool = false;
const bool RB_Int_Cbase_Bool = false;
const int PC_Int_Cbase_Bool = 2;
int Hook_Int_Cbase_Bool(Hook *hook, void *pthis, void *cb1, bool b1);

const bool RT_Int_Vector_Vector = false;
const bool RB_Int_Vector_Vector = false;
const int PC_Int_Vector_Vector = 6;
int Hook_Int_Vector_Vector(Hook *hook, void *pthis, Vector v1, Vector v2);

const bool RT_Int_Entvar_Float = false;
const bool RB_Int_Entvar_Float = false;
const int PC_Int_Entvar_Float = 2;
int Hook_Int_Entvar_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1);

const bool RT_Float_Float = false;
const bool RB_Float_Float = false;
const int PC_Float_Float = 1;
float Hook_Float_Float(Hook *hook, void *pthis, float f1);

const bool RT_Void_Entvar_Entvar_Float = true;
const bool RB_Void_Entvar_Entvar_Float = false;
const int PC_Void_Entvar_Entvar_Float = 3;
void Hook_Void_Entvar_Entvar_Float(Hook *hook, void *pthis, entvars_t *attacker, entvars_t *inflictor, float damage);

const bool RT_Bool_Void = false;
const bool RB_Bool_Void = false;
const int PC_Bool_Void = 0;
bool Hook_Bool_Void(Hook *hook, void *pthis);

const bool RT_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool = false;
const bool RB_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool = false;
const int PC_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool = 7;
int Hook_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool(Hook *hook, void *pthis, Vector *v1, Vector *v2, float fl, void* cb, Vector *v3, Vector *v4, bool b1);

const bool RT_Int_Vector_Cbase = false;
const bool RB_Int_Vector_Cbase = false;
const int PC_Int_Vector_Cbase = 4;
int Hook_Int_Vector_Cbase(Hook *hook, void *pthis, Vector v1, void *cb);

const bool RT_Int_Vector= false;
const bool RB_Int_Vector = false;
const int PC_Int_Vector = 3;
int Hook_Int_Vector(Hook *hook, void *pthis, Vector v1);

const bool RT_Int_Cbase_pVector = false;
const bool RB_Int_Cbase_pVector = false;
const int PC_Int_Cbase_pVector = 2;
int  Hook_Int_Cbase_pVector(Hook *hook, void *pthis, void *cb1, Vector *v1);

const bool RT_Void_Bool = true;
const bool RB_Void_Bool = false;
const int PC_Void_Bool = 1;
void Hook_Void_Bool(Hook *hook, void *pthis, bool b1);

const bool RT_Bool_Cbase = false;
const bool RB_Bool_Cbase = false;
const int PC_Bool_Cbase = 1;
bool Hook_Bool_Cbase(Hook *hook, void *pthis, void *cb);

const bool RT_Bool_Int = false;
const bool RB_Bool_Int = false;
const int PC_Bool_Int = 1;
bool Hook_Bool_Int(Hook *hook, void *pthis, int i1);

const bool RT_Void_Cbase_Float = true;
const bool RB_Void_Cbase_Float = false;
const int PC_Void_Cbase_Float = 2;
void Hook_Void_Cbase_Float(Hook *hook, void *pthis, void *p1, float f1);

const bool RT_Void_Cbase_Bool = true;
const bool RB_Void_Cbase_Bool = false;
const int PC_Void_Cbase_Bool = 2;
void Hook_Void_Cbase_Bool(Hook *hook, void *pthis, void *p1, bool b1);

const bool RT_Vector_Vector_Vector_Vector = true;
const bool RB_Vector_Vector_Vector_Vector = true;
const int PC_Vector_Vector_Vector_Vector = 10;
#if defined(_WIN32)
void Hook_Vector_Vector_Vector_Vector(Hook *hook, void *pthis, Vector *out, Vector v1, Vector v2, Vector v3);
#elif defined(__linux__) || defined(__APPLE__)
void Hook_Vector_Vector_Vector_Vector(Hook *hook, Vector *out, void *pthis, Vector v1, Vector v2, Vector v3);
#endif

const bool RT_Str_Str = false;
const bool RB_Str_Str = false;
const int PC_Str_Str = 1;
const char *Hook_Str_Str(Hook *hook, void *pthis, const char* str);

const bool RT_Void_Short = true;
const bool RB_Void_Short = false;
const int PC_Void_Short = 1;
void Hook_Void_Short(Hook *hook, void *pthis, short i1);


const bool RT_Deprecated = true;
const bool RB_Deprecated = false;
const int PC_Deprecated = 0;
void Hook_Deprecated(Hook* hook);
#endif
