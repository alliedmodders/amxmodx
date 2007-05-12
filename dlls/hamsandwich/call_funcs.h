#ifndef HOOK_Call_H
#define HOOK_Call_H


cell Call_Void_Void(AMX *amx, cell *params);

cell Call_Int_Void(AMX *amx, cell *params);

cell Call_Void_Entvar(AMX *amx, cell *params);

cell Call_Void_Cbase(AMX *amx, cell *params);

cell Call_Int_Float_Int(AMX *amx, cell *params);
	
cell Call_Void_Entvar_Int(AMX *amx, cell *params);

cell Call_Int_Cbase(AMX *amx, cell *params);

cell Call_Void_Int_Int(AMX *amx, cell *params);

cell Call_Int_Int_Str_Int(AMX *amx, cell *params);

cell Call_Int_Int(AMX *amx, cell *params);

cell Call_Int_Entvar(AMX *amx, cell *params);

cell Call_Int_Entvar_Entvar_Float_Int(AMX *amx, cell *params);

cell Call_Int_Entvar_Entvar_Float_Float_Int(AMX *amx, cell *params);

cell Call_Void_Int(AMX *amx, cell *params);

cell Call_Void_Cbase_Cbase_Int_Float(AMX *amx, cell *params);

cell Call_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, cell *params);

cell Call_Void_Float_Vector_Trace_Int(AMX *amx, cell *params);

cell Call_Str_Void(AMX *amx, cell *params);

cell Call_Cbase_Void(AMX *amx, cell *params);

cell Call_Vector_Void(AMX *amx, cell *params);

cell Call_Vector_pVector(AMX *amx, cell *params);

cell Call_Int_pVector(AMX *amx, cell *params);

cell Call_Void_Entvar_Float_Float(AMX *amx, cell *params);

cell Call_Int_pFloat_pFloat(AMX *amx, cell *params);

cell Call_Void_Entvar_Float(AMX *amx, cell *params);

cell Call_Void_Int_Int_Int(AMX *amx, cell *params);

cell Call_Void_ItemInfo(AMX *amx, cell *params);

#endif
