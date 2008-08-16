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
#include "amxxmodule.h"

#include "CVector.h"
#include "CString.h"
#include "sh_stack.h"
#include "DataHandler.h"

#include "ham_const.h"
#include "ham_utils.h"
#include "NEW_Util.h"

CStack< Data * > ReturnStack;
CStack< Data * > OrigReturnStack;
CStack< CVector< Data * > * > ParamStack;
CStack< int * > ReturnStatus;
#define CHECK_STACK(__STACK__)								\
	if (  ( __STACK__ ).size() <= 0)						\
	{																	\
		MF_LogError(amx, AMX_ERR_NATIVE, "%s is empty!", #__STACK__);	\
		return 0;														\
	}

#define PARSE_RETURN()										\
	if (ret==-2)											\
	{														\
		MF_LogError(amx, AMX_ERR_NATIVE, "Data pointer is NULL!");	\
	}														\
	else if (ret==-1)										\
	{														\
		MF_LogError(amx, AMX_ERR_NATIVE, "Wrong data type (data is of type %s)", returntypes[dat->GetType()]);	\
	}														\
	return ret

static const char *returntypes[] =
{
	"void",
	"integer",
	"float",
	"vector",
	"string",
	"entity",
	"entity",
	"traceresult",
	"iteminfo"
};

static cell AMX_NATIVE_CALL GetHamReturnInteger(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->GetInt(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetOrigHamReturnInteger(AMX *amx, cell *params)
{
	CHECK_STACK(OrigReturnStack);
	Data *dat=OrigReturnStack.front();

	int ret=dat->GetInt(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetHamReturnFloat(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->GetFloat(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetOrigHamReturnFloat(AMX *amx, cell *params)
{
	CHECK_STACK(OrigReturnStack);
	Data *dat=OrigReturnStack.front();

	int ret=dat->GetFloat(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetHamReturnVector(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->GetVector(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetOrigHamReturnVector(AMX *amx, cell *params)
{
	CHECK_STACK(OrigReturnStack);
	Data *dat=OrigReturnStack.front();

	int ret=dat->GetVector(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetHamReturnEntity(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->GetEntity(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetOrigHamReturnEntity(AMX *amx, cell *params)
{
	CHECK_STACK(OrigReturnStack);
	Data *dat=OrigReturnStack.front();

	int ret=dat->GetEntity(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetHamReturnString(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->GetString(MF_GetAmxAddr(amx, params[1]), params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetOrigHamReturnString(AMX *amx, cell *params)
{
	CHECK_STACK(OrigReturnStack);
	Data *dat=OrigReturnStack.front();

	int ret=dat->GetString(MF_GetAmxAddr(amx, params[1]), params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamReturnInteger(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->SetInt(&params[1]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamReturnFloat(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->SetFloat(&params[1]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamReturnVector(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->SetVector(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamReturnEntity(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->SetEntity(&params[1]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamReturnString(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStack);
	Data *dat=ReturnStack.front();

	int ret=dat->SetString(MF_GetAmxAddr(amx, params[1]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamInteger(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetInt(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamTraceResult(AMX *amx, cell *params)
{
	if (params[2] == 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null traceresult provided.");

		return 0;
	}
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetInt(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamFloat(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1] || params[1] < 1) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetFloat(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamVector(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetVector(MF_GetAmxAddr(amx, params[2]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamEntity(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetEntity(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamString(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	CVector<Data *> *vec=ParamStack.front(); 
	if (vec->size() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->size()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetString(MF_GetAmxAddr(amx, params[2]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL GetHamReturnStatus(AMX *amx, cell *params)
{
	CHECK_STACK(ReturnStatus);
	int *i=ReturnStatus.front();

	return *i;
}

AMX_NATIVE_INFO ReturnNatives[] =
{
	{ "GetHamReturnInteger",		GetHamReturnInteger },
	{ "GetHamReturnFloat",			GetHamReturnFloat },
	{ "GetHamReturnVector",			GetHamReturnVector },
	{ "GetHamReturnEntity",			GetHamReturnEntity },
	{ "GetHamReturnString",			GetHamReturnString },
	{ "GetOrigHamReturnInteger",	GetOrigHamReturnInteger },
	{ "GetOrigHamReturnFloat",		GetOrigHamReturnFloat },
	{ "GetOrigHamReturnVector",		GetOrigHamReturnVector },
	{ "GetOrigHamReturnEntity",		GetOrigHamReturnEntity },
	{ "GetOrigHamReturnString",		GetOrigHamReturnString },
	{ "SetHamReturnInteger",		SetHamReturnInteger },
	{ "SetHamReturnFloat",			SetHamReturnFloat },
	{ "SetHamReturnVector",			SetHamReturnVector },
	{ "SetHamReturnEntity",			SetHamReturnEntity },
	{ "SetHamReturnString",			SetHamReturnString },

	{ "GetHamReturnStatus",			GetHamReturnStatus },

	{ "SetHamParamInteger",			SetHamParamInteger },
	{ "SetHamParamFloat",			SetHamParamFloat },
	{ "SetHamParamVector",			SetHamParamVector },
	{ "SetHamParamEntity",			SetHamParamEntity },
	{ "SetHamParamString",			SetHamParamString },
	{ "SetHamParamTraceResult",		SetHamParamTraceResult },

	{ NULL,							NULL },
};
