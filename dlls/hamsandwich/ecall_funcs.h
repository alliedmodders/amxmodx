#ifndef HOOK_ECALL_H
#define HOOK_ECALL_H


int eCall_Void_Void(AMX *amx, cell *params);

int eCall_Int_Void(AMX *amx, cell *params);

int eCall_Void_Entvar(AMX *amx, cell *params);

int eCall_Void_Cbase(AMX *amx, cell *params);

int eCall_Int_Float_Int(AMX *amx, cell *params);
	
int eCall_Void_Entvar_Int(AMX *amx, cell *params);

int eCall_Int_Cbase(AMX *amx, cell *params);

int eCall_Void_Int_Int(AMX *amx, cell *params);

int eCall_Int_Int_Str_Int(AMX *amx, cell *params);

int eCall_Int_Int(AMX *amx, cell *params);

int eCall_Int_Entvar(AMX *amx, cell *params);

int eCall_Int_Entvar_Entvar_Float_Int(AMX *amx, cell *params);

int eCall_Void_Int(AMX *amx, cell *params);

int eCall_Void_Cbase_Cbase_Int_Float(AMX *amx, cell *params);

int eCall_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, cell *params);

int eCall_Void_Float_Vector_TraceResult_Int(AMX *amx, cell *params);

int eCall_Str_Void(AMX *amx, cell *params);

int eCall_Cbase_Void(AMX *amx, cell *params);

int eCall_Vector_Void(AMX *amx, cell *params);

int eCall_Vector_pVector(AMX *amx, cell *params);

int eCall_Int_pVector(AMX *amx, cell *params);

int eCall_Void_Entvar_Float_Float(AMX *amx, cell *params);

int eCall_Int_pFloat_pFloat(AMX *amx, cell *params);

int eCall_Void_Entvar_Float(AMX *amx, cell *params);


#endif
