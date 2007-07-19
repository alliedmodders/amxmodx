/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#ifndef HOOK_CALLBACKS_H
#define HOOK_CALLBACKS_H

// RT_<TYPE> is true if the function returns void, false otherwise 
//				(it also would be true for large return functions such as Vector)
// PC_<TYPE> is how many dwords get passed to the function (minus the "this" pointer)
//				(it is one larger for large return functions such as Vector)

const bool RT_Void_Void = true;
const int PC_Void_Void = 0;
void Hook_Void_Void(Hook *hook, void *pthis);

const bool RT_Int_Void = false;
const int PC_Int_Void = 0;
int  Hook_Int_Void(Hook *hook, void *pthis);

const bool RT_Void_Entvar = true;
const int PC_Void_Entvar = 1;
void Hook_Void_Entvar(Hook *hook, void *pthis, entvars_t *entvar);

const bool RT_Void_Cbase = true;
const int PC_Void_Cbase = 1;
void Hook_Void_Cbase(Hook *hook, void *pthis, void *other);

const bool RT_Int_Float_Int = false;
const int PC_Int_Float_Int = 2;
int  Hook_Int_Float_Int(Hook *hook, void *pthis, float f1, int i1);
	
const bool RT_Void_Entvar_Int = true;
const int PC_Void_Entvar_Int = 2;
void Hook_Void_Entvar_Int(Hook *hook, void *ptis, entvars_t *ev1, int i1);

const bool RT_Int_Cbase = false;
const int PC_Int_Cbase = 1;
int  Hook_Int_Cbase(Hook *hook, void *pthis, void *cb1);

const bool RT_Void_Int_Int = true;
const int PC_Void_Int_Int = 2;
void Hook_Void_Int_Int(Hook *hook, void *pthis, int i1, int i2);

const bool RT_Int_Int_Str_Int = false;
const int PC_Int_Int_Str_Int = 3;
int  Hook_Int_Int_Str_Int(Hook *hook, void *pthis, int i1, const char *sz1,
						  int i2);

const bool RT_Int_Int = false;
const int PC_Int_Int = 1;
int  Hook_Int_Int(Hook *hook, void *pthis, int i1);

const bool RT_Int_Entvar = false;
const int PC_Int_Entvar = 1;
int  Hook_Int_Entvar(Hook *hook, void *pthis, entvars_t *ev1);

const bool RT_Int_Entvar_Entvar_Float_Int = false;
const int PC_Int_Entvar_Entvar_Float_Int = 4;
int  Hook_Int_Entvar_Entvar_Float_Int(Hook *hook, void *pthis, 
									  entvars_t *inflictor, 
									  entvars_t *attacker, float damage, 
									  int damagebits);

const bool RT_Int_Entvar_Entvar_Float_Float_Int = false;
const int PC_Int_Entvar_Entvar_Float_Float_Int = 5;
int  Hook_Int_Entvar_Entvar_Float_Float_Int(Hook *hook, void *pthis, 
									  entvars_t *inflictor, 
									  entvars_t *attacker, float damage, 
									  float unknown, int damagebits);

const bool RT_Void_Int = true;
const int PC_Void_Int = 1;
void Hook_Void_Int(Hook *hook, void *pthis, int i1);

const bool RT_Void_Cbase_Cbase_Int_Float = true;
const int PC_Void_Cbase_Cbase_Int_Float = 4;
void Hook_Void_Cbase_Cbase_Int_Float(Hook *hook, void *pthis, void *cb1, 
									 void *cb2, int i1, float f1);

const bool RT_Void_Entvar_Float_Vector_Trace_Int = true;
const int PC_Void_Entvar_Float_Vector_Trace_Int = 7;
void Hook_Void_Entvar_Float_Vector_Trace_Int(Hook *hook, void *pthis, 
											 entvars_t *ev1, float f1, 
											 Vector v1, TraceResult *tr1, 
											 int i1);

const bool RT_Void_Float_Vector_Trace_Int = true;
const int PC_Void_Float_Vector_Trace_Int = 6;
void Hook_Void_Float_Vector_Trace_Int(Hook *hook, void *pthis, float f1,
											Vector v1, TraceResult *tr1, 
											int i1);

const bool RT_Str_Void = false;
const int PC_Str_Void = 0;
const char *Hook_Str_Void(Hook *hook, void *pthis);

const bool RT_Cbase_Void = false;
const int PC_Cbase_Void = 0;
void *Hook_Cbase_Void(Hook *hook, void *pthis);

// HACK: I'm too lazy to fix up trampoline generator to deal with
//       special return values.  this is so much easier.
const bool RT_Vector_Void = true;
const int PC_Vector_Void = 1;
#ifdef _WIN32
void Hook_Vector_Void(Hook *hook, void *pthis, Vector *out);
#elif defined __linux__
void Hook_Vector_Void(Hook *hook, Vector *out, void *pthis);
#endif

const bool RT_Vector_pVector = true;
const int PC_Vector_pVector = 2;
#ifdef _WIN32
void Hook_Vector_pVector(Hook *hook, void *pthis, Vector *out, Vector *v1);
#elif defined __linux__
void Hook_Vector_pVector(Hook *hook, Vector *out, void *pthis, Vector *v1);
#endif

const bool RT_Int_pVector = false;
const int PC_Int_pVector = 1;
int Hook_Int_pVector(Hook *hook, void *pthis, Vector *v1);

const bool RT_Void_Entvar_Float_Float = true;
const int PC_Void_Entvar_Float_Float = 3;
void Hook_Void_Entvar_Float_Float(Hook *hook, void *pthis, entvars_t *ev1, 
								  float f1, float f2);

const bool RT_Int_pFloat_pFloat = false;
const int PC_Int_pFloat_pFloat = 2;
int Hook_Int_pFloat_pFloat(Hook *hook, void *pthis, float *f1, 
						   float *f2);

const bool RT_Void_Entvar_Float = true;
const int PC_Void_Entvar_Float = 2;
void Hook_Void_Entvar_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1);


const bool RT_Void_Int_Int_Int = true;
const int PC_Void_Int_Int_Int = 3;
void Hook_Void_Int_Int_Int(Hook *hook, void *pthis, int i1, int i2, int i3);

const bool RT_Void_ItemInfo = true;
const int PC_Void_ItemInfo = 1;
void Hook_Void_ItemInfo(Hook *hook, void *pthis, void *iteminfo);


const bool RT_Float_Void = false;
const int PC_Float_Void = 0;
float Hook_Float_Void(Hook *hook, void *pthis);

const bool RT_Void_Float_Int = true;
const int PC_Void_Float_Int = 2;
void Hook_Void_Float_Int(Hook *hook, void *pthis, float f1, int i1);

const bool RT_Deprecated = true;
const int PC_Deprecated = 0;
void Hook_Deprecated(Hook* hook);



#endif
