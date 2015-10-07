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

#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include <sh_stack.h>
#include "DataHandler.h"

#include "ham_const.h"
#include "ham_utils.h"

CStack< Data * > ReturnStack;
CStack< Data * > OrigReturnStack;
CStack< ke::Vector< Data * > * > ParamStack;
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
	ke::Vector<Data *> *vec = ParamStack.front();
	if (vec->length() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
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
	ke::Vector<Data *> *vec = ParamStack.front();
	if (vec->length() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetInt(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamFloat(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	ke::Vector<Data *> *vec = ParamStack.front();
	if (vec->length() < (unsigned)params[1] || params[1] < 1) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetFloat(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamVector(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	ke::Vector<Data *> *vec = ParamStack.front();
	if (vec->length() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetVector(MF_GetAmxAddr(amx, params[2]));
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamEntity(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	ke::Vector<Data *> *vec = ParamStack.front();
	if (vec->length() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetEntity(&params[2]);
	PARSE_RETURN();
}
static cell AMX_NATIVE_CALL SetHamParamString(AMX *amx, cell *params)
{
	CHECK_STACK(ParamStack);
	ke::Vector<Data *> *vec=ParamStack.front(); 
	if (vec->length() < (unsigned)params[1]) 
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length()); 
		return 0; 
	} 
	Data *dat=vec->at(params[1] - 1);

	int ret=dat->SetString(MF_GetAmxAddr(amx, params[2]));
	PARSE_RETURN();
}

static cell AMX_NATIVE_CALL SetHamParamItemInfo(AMX *amx, cell *params)
{
	if (params[2] == 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null ItemInfo handle provided.");
		return 0;
	}

	CHECK_STACK(ParamStack);
	ke::Vector<Data *> *vec = ParamStack.front();

	if (vec->length() < (unsigned)params[1])
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid parameter number, got %d, expected %d", params[1], vec->length());
		return 0;
	}

	Data *dat = vec->at(params[1] - 1);

	int ret = dat->SetInt(&params[2]);
	PARSE_RETURN();
}


static cell AMX_NATIVE_CALL GetHamItemInfo(AMX *amx, cell *params)
{
	if (params[1] == 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null iteminfo handle provided.");
		return 0;
	}

	int type = params[2];

	if ((type == ItemInfo_pszAmmo1 || type == ItemInfo_pszAmmo2 || type == ItemInfo_pszName) && (*params / sizeof(cell)) != 4)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Bad arg count.  Expected %d, got %d.", 4, *params / sizeof(cell));
		return 0;
	}

	ItemInfo *pItem = reinterpret_cast<ItemInfo *>(params[1]);

	switch (type)
	{
		case ItemInfo_iSlot:
			return pItem->iSlot;

		case ItemInfo_iPosition:
			return pItem->iPosition;

		case ItemInfo_pszAmmo1:
			return MF_SetAmxString( amx, params[3], pItem->pszAmmo1 > 0 ? pItem->pszAmmo1 : "", params[4] );

		case ItemInfo_iMaxAmmo1:
			return pItem->iMaxAmmo1;

		case ItemInfo_pszAmmo2:
			return MF_SetAmxString( amx, params[3], pItem->pszAmmo2 > 0 ? pItem->pszAmmo2 : "", params[4] );

		case ItemInfo_iMaxAmmo2:
			return pItem->iMaxAmmo2;

		case ItemInfo_pszName:
			return MF_SetAmxString( amx, params[3], pItem->pszName > 0 ? pItem->pszName : "", params[4] );

		case ItemInfo_iMaxClip:
			return pItem->iMaxClip;

		case ItemInfo_iId:
			return pItem->iId;

		case ItemInfo_iFlags:
			return pItem->iFlags;

		case ItemInfo_iWeight:
			return pItem->iWeight;
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown ItemInfo type %d", type);
	return 0;
}

CStack<ItemInfo *> g_FreeIIs;

static cell AMX_NATIVE_CALL CreateHamItemInfo(AMX *amx, cell *params)
{
	ItemInfo *ii;

	if (g_FreeIIs.empty())
	{
		ii = new ItemInfo;
	}
	else 
	{
		ii = g_FreeIIs.front();
		g_FreeIIs.pop();
	}

	memset(ii, 0, sizeof(ItemInfo));

	return reinterpret_cast<cell>(ii);
}

static cell AMX_NATIVE_CALL FreeHamItemInfo(AMX *amx, cell *params)
{
	ItemInfo *ii = reinterpret_cast<ItemInfo *>(params[1]);

	if (!ii)
	{
		return 0;
	}

	g_FreeIIs.push(ii);

	return 1;
}


static cell AMX_NATIVE_CALL SetHamItemInfo(AMX *amx, cell *params)
{
	if (params[1] == 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Null iteminfo handle provided.");
		return 0;
	}

	ItemInfo *pItem = reinterpret_cast<ItemInfo *>(params[1]);
	cell *ptr = MF_GetAmxAddr(amx, params[3]);
	int iLen;

	switch (params[2])
	{
		case ItemInfo_iSlot:
			pItem->iSlot = *ptr;
			break;

		case ItemInfo_iPosition:
			pItem->iPosition = *ptr;
			break;

		case ItemInfo_pszAmmo1:
			pItem->pszAmmo1 = MF_GetAmxString(amx, params[3], 0, &iLen);
			return iLen;

		case ItemInfo_iMaxAmmo1:
			pItem->iMaxAmmo1 = *ptr;
			break;

		case ItemInfo_pszAmmo2:
			pItem->pszAmmo2 = MF_GetAmxString(amx, params[3], 0, &iLen);
			return iLen;

		case ItemInfo_iMaxAmmo2:
			pItem->iMaxAmmo2 = *ptr;
			break;

		case ItemInfo_pszName:
			pItem->pszName = MF_GetAmxString(amx, params[3], 0, &iLen);
			return iLen;

		case ItemInfo_iMaxClip:
			pItem->iMaxClip = *ptr;
			break;

		case ItemInfo_iId:
			pItem->iId = *ptr;
			break;

		case ItemInfo_iFlags:
			pItem->iFlags = *ptr;
			break;

		case ItemInfo_iWeight:
			pItem->iWeight = *ptr;
			break;

		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Unknown ItemInfo type %d", params[2]);
			return 0;
	}

	return 1;
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
	{ "SetHamParamItemInfo",		SetHamParamItemInfo },

	{ "GetHamItemInfo",				GetHamItemInfo },
	{ "SetHamItemInfo",				SetHamItemInfo },
	{ "CreateHamItemInfo",			CreateHamItemInfo },
	{ "FreeHamItemInfo",			FreeHamItemInfo },

	{ NULL,							NULL },
};
