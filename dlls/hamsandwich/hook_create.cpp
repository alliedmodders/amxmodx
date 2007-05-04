#include "sdk/amxxmodule.h"

int Create_Void_Void(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_DONE);
}

int Create_Int_Void(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_DONE);
}

int Create_Void_Entvar(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}


int Create_Void_Cbase(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Int_Float_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_FLOAT, FP_CELL, FP_DONE);
}

	
int Create_Void_Entvar_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}


int Create_Int_Cbase(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Void_Int_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Int_Int_Str_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_STRING, FP_CELL, FP_DONE);
}

int Create_Int_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Int_Entvar(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Int_Entvar_Entvar_Float_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_CELL, FP_DONE);
}

int Create_Void_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Void_Cbase_Cbase_Int_Float(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_DONE);
}

int Create_Void_Entvar_Float_Vector_Trace_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Void_Float_Vector_TraceResult_Int(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_FLOAT, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Str_Void(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_DONE);
}

int Create_Cbase_Void(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_DONE);
}

int Create_Vector_Void(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_DONE);
}

int Create_Vector_pVector(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_ARRAY, FP_DONE);
}

int Create_Int_pVector(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_ARRAY, FP_DONE);
}

int Create_Void_Entvar_Float_Float(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_FLOAT, FP_FLOAT, FP_DONE);
}

int Create_Int_pFloat_pFloat(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

int Create_Void_Entvar_Float(AMX *amx, const char *func)
{
	return MF_RegisterSPForwardByName(amx, func, FP_CELL, FP_CELL, FP_FLOAT, FP_DONE);
}

