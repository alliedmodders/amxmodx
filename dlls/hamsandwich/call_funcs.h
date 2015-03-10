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

#ifndef HOOK_Call_H
#define HOOK_Call_H


cell Call_Void_Void(AMX *amx, cell *params);

cell Call_Int_Void(AMX *amx, cell *params);

cell Call_Void_Entvar(AMX *amx, cell *params);

cell Call_Void_Cbase(AMX *amx, cell *params);

cell Call_Int_Float_Int(AMX *amx, cell *params);
	
cell Call_Int_Float_Int_Int(AMX *amx, cell *params);

cell Call_Void_Entvar_Int(AMX *amx, cell *params);

cell Call_Void_Entvar_Entvar_Int(AMX *amx, cell *params);

cell Call_Int_Cbase(AMX *amx, cell *params);

cell Call_Void_Int_Int(AMX *amx, cell *params);

cell Call_Int_Int_Str_Int(AMX *amx, cell *params);

cell Call_Int_Int_Str_Int_Int(AMX *amx, cell *params);

cell Call_Int_Int(AMX *amx, cell *params);
	
cell Call_Int_Entvar(AMX *amx, cell *params);

cell Call_Int_Entvar_Entvar_Float_Int(AMX *amx, cell *params);

cell Call_Int_Entvar_Entvar_Float_Float_Int(AMX *amx, cell *params);

cell Call_Void_Int(AMX *amx, cell *params);

cell Call_Vector_Float_Cbase_Int(AMX *amx, cell *params);

cell Call_Void_Cbase_Cbase_Int_Float(AMX *amx, cell *params);

cell Call_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, cell *params);

cell Call_Void_Float_Vector_Trace_Int(AMX *amx, cell *params);

cell Call_Str_Void(AMX *amx, cell *params);

cell Call_Cbase_Void(AMX *amx, cell *params);

cell Call_Vector_Void(AMX *amx, cell *params);

cell Call_Vector_pVector(AMX *amx, cell *params);

cell Call_Int_pVector(AMX *amx, cell *params);

cell Call_Void_Entvar_Float_Float(AMX *amx, cell *params);

cell Call_Void_pFloat_pFloat(AMX *amx, cell *params);

cell Call_Void_Entvar_Float(AMX *amx, cell *params);

cell Call_Void_Int_Int_Int(AMX *amx, cell *params);

cell Call_Void_ItemInfo(AMX *amx, cell *params);

cell Call_Float_Void(AMX *amx, cell *params);

cell Call_Void_Float_Int(AMX* amx, cell* params);

cell Call_Float_Float_Cbase(AMX* amx, cell* params);

cell Call_Void_Float(AMX* amx, cell* params);

cell Call_Void_Float_Float_Float_Int(AMX* amx, cell* params);

cell Call_Float_Int(AMX* amx, cell* params);

cell Call_Vector_Float(AMX* amx, cell* params);

cell Call_Void_Float_Cbase(AMX *amx, cell *params);

cell Call_Int_Float_Float(AMX *amx, cell *params);

cell Call_Int_Float(AMX *amx, cell *params);

cell Call_Int_Int_Int(AMX *amx, cell *params);

cell Call_Void_Str_Float_Float_Float(AMX *amx, cell *params);

cell Call_Void_Str_Float_Float_Float_Int_Cbase(AMX *amx, cell *params);

cell Call_Int_Vector_Vector_Float_Float(AMX *amx, cell *params);

cell Call_Int_Short(AMX *amx, cell *params);

cell Call_Void_Entvar_Entvar_Float_Int_Int(AMX *amx, cell *params);

cell Call_Void_Vector_Entvar_Entvar_Float_Int_Int(AMX *amx, cell *params);

cell Call_Float_Int_Float(AMX* amx, cell* params);

cell Call_Int_Str(AMX* amx, cell* params);

cell Call_Void_Edict(AMX* amx, cell* params);

cell Call_Void_Int_Str_Bool(AMX* amx, cell* params);

cell Call_Void_Vector_Vector(AMX* amx, cell* params);

cell Call_Void_Str_Bool(AMX* amx, cell* params);

cell Call_Int_Str_Str_Int_Str_Int_Int(AMX* amx, cell* params);

cell Call_Int_Int_Int_Float_Int(AMX* amx, cell* params);

cell Call_Void_Str_Int(AMX* amx, cell* params);

cell Call_Void_Cbase_Int(AMX* amx, cell* params);

cell Call_Void_Str(AMX* amx, cell* params);

cell Call_Void_Vector(AMX* amx, cell* params);

cell Call_Int_Str_Vector_Str(AMX* amx, cell* params);

cell Call_Int_Str_Str(AMX* amx, cell* params);

cell Call_Void_Float_Float(AMX* amx, cell* params);

cell Call_Void_Str_Str_Int(AMX* amx, cell* params);

cell Call_Int_pVector_pVector_Cbase_pFloat(AMX* amx, cell* params);

cell Call_Void_Cbase_pVector_Float(AMX* amx, cell* params);

cell Call_Int_pVector_pVector_Float_Cbase_pVector(AMX* amx, cell* params);

cell Call_Int_Cbase_Bool(AMX* amx, cell* params);

cell Call_Int_Vector_Vector(AMX *amx, cell *params);

cell Call_Int_Entvar_Float(AMX *amx, cell *params);

cell Call_Float_Float(AMX* amx, cell* params);

cell Call_Void_Entvar_Entvar_Float(AMX *amx, cell *params);

cell Call_Bool_Void(AMX *amx, cell *params);

cell Call_Int_pVector_pVector_Float_Cbase_pVector_pVector_Bool(AMX* amx, cell* params);

cell Call_Int_Vector_Cbase(AMX *amx, cell *params);

cell Call_Int_Vector(AMX *amx, cell *params);

cell Call_Int_Cbase_pVector(AMX *amx, cell *params);

cell Call_Void_Bool(AMX *amx, cell *params);

cell Call_Bool_Cbase(AMX *amx, cell *params);

cell Call_Bool_Int(AMX *amx, cell *params);

cell Call_Void_Cbase_Float(AMX* amx, cell* params);

cell Call_Void_Cbase_Bool(AMX* amx, cell* params);

cell Call_Vector_Vector_Vector_Vector(AMX *amx, cell *params);

cell Call_Str_Str(AMX *amx, cell *params);

cell Call_Void_Short(AMX *amx, cell *params);


cell Call_Deprecated(AMX* amx, cell* params);

#endif
