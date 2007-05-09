
#ifndef HOOK_CALLBACKS_H
#define HOOK_CALLBACKS_H


void Hook_Void_Void(Hook *hook, void *pthis);

int  Hook_Int_Void(Hook *hook, void *pthis);

void Hook_Void_Entvar(Hook *hook, void *pthis, entvars_t *entvar);

void Hook_Void_Cbase(Hook *hook, void *pthis, void *other);

int  Hook_Int_Float_Int(Hook *hook, void *pthis, float f1, int i1);
	
void Hook_Void_Entvar_Int(Hook *hook, void *ptis, entvars_t *ev1, int i1);

int  Hook_Int_Cbase(Hook *hook, void *pthis, void *cb1);

void Hook_Void_Int_Int(Hook *hook, void *pthis, int i1, int i2);

int  Hook_Int_Int_Str_Int(Hook *hook, void *pthis, int i1, const char *sz1,
						  int i2);

int  Hook_Int_Int(Hook *hook, void *pthis, int i1);

int  Hook_Int_Entvar(Hook *hook, void *pthis, entvars_t *ev1);

int  Hook_Int_Entvar_Entvar_Float_Int(Hook *hook, void *pthis, 
									  entvars_t *inflictor, 
									  entvars_t *attacker, float damage, 
									  int damagebits);

void Hook_Void_Int(Hook *hook, void *pthis, int i1);

void Hook_Void_Cbase_Cbase_Int_Float(Hook *hook, void *pthis, void *cb1, 
									 void *cb2, int i1, float f1);

void Hook_Void_Entvar_Float_Vector_Trace_Int(Hook *hook, void *pthis, 
											 entvars_t *ev1, float f1, 
											 Vector v1, TraceResult *tr1, 
											 int i1);

void Hook_Void_Float_Vector_TraceResult_Int(Hook *hook, void *pthis, float f1,
											Vector v1, TraceResult *tr1, 
											int i1);

const char *Hook_Str_Void(Hook *hook, void *pthis);

void *Hook_Cbase_Void(Hook *hook, void *pthis);

void Hook_Vector_Void(Hook *hook, Vector *out, void *pthis);

void Hook_Vector_pVector(Hook *hook, Vector *out, void *pthis, Vector *v1);

int Hook_Int_pVector(Hook *hook, void *pthis, Vector *v1);

void Hook_Void_Entvar_Float_Float(Hook *hook, void *pthis, entvars_t *ev1, 
								  float f1, float f2);

int Hook_Int_pFloat_pFloat(Hook *hook, void *pthis, float *f1, 
						   float *f2);

void Hook_Void_Entvar_Float(Hook *hook, void *pthis, entvars_t *ev1, float f1);


#endif
