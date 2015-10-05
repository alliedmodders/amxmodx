// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Regular Expressions Module
//

#include <string.h>
#include "pcre.h"
#include "amxxmodule.h"
#include <amtl/am-vector.h>
#include <amtl/am-utility.h>
#include "CRegEx.h"
#include "utils.h"

ke::Vector<RegEx *> PEL;

int GetPEL()
{
	for (int i=0; i<(int)PEL.length(); i++)
	{
		if (PEL[i]->isFree())
			return i;
	}

	RegEx *x = new RegEx();
	PEL.append(x);

	return (int)PEL.length() - 1;
}

// native Regex:regex_compile(const pattern[], &ret, error[], maxLen, const flags[]="");
static cell AMX_NATIVE_CALL regex_compile(AMX *amx, cell *params)
{
	int len;
	const char *regex = MF_GetAmxString(amx, params[1], 0, &len);
	const char *flags = MF_GetAmxString(amx, params[5], 1, &len);

	int id = GetPEL();
	RegEx *x = PEL[id];
	
	if (x->Compile(regex, flags) == 0)
	{
		const char *err = x->mError;
		*MF_GetAmxAddr(amx, params[2]) = x->mErrorOffset;
		MF_SetAmxString(amx, params[3], err?err:"unknown", params[4]);
		return -1;
	}
	
	return id+1;
}

// native Regex:regex_compile_ex(const pattern[], flags = 0, error[] = "", maxLen = 0, &errcode = 0);
static cell AMX_NATIVE_CALL regex_compile_ex(AMX *amx, cell *params)
{
	int len;
	const char *regex = MF_GetAmxString(amx, params[1], 0, &len);

	int id = GetPEL();
	RegEx *x = PEL[id];

	if (x->Compile(regex, params[2]) == 0)
	{
		const char *err = x->mError;
		*MF_GetAmxAddr(amx, params[5]) = x->mErrorOffset;
		MF_SetAmxString(amx, params[3], err ? err : "unknown", params[4]);
		return -1;
	}

	return id + 1;
}

cell match(AMX *amx, cell *params, bool all)
{
	int len;
	const char *str = MF_GetAmxString(amx, params[1], 0, &len);
	const char *regex = MF_GetAmxString(amx, params[2], 1, &len);

	int id = GetPEL();
	RegEx *x = PEL[id];

	char *flags = NULL;
	cell *errorCode;
	int result = 0;

	if (!all)
	{
		if (*params / sizeof(cell) >= 6) // compiled with 1.8's extra parameter
		{
			flags = MF_GetAmxString(amx, params[6], 2, &len);
		}

		result = x->Compile(regex, flags);
		errorCode = MF_GetAmxAddr(amx, params[3]);
	}
	else
	{
		result = x->Compile(regex, params[3]);
		errorCode = MF_GetAmxAddr(amx, params[6]);
	}

	if (!result)
	{
		const char *err = x->mError;
		*errorCode = x->mErrorOffset;
		MF_SetAmxString(amx, params[4], err ? err : "unknown", params[5]);
		return -1;
	}

	int e;

	if (all)
		e = x->MatchAll(str);
	else
		e = x->Match(str);

	if (e == -1)
	{
		/* there was a match error.  destroy this and move on. */
		*errorCode = x->mErrorOffset;
		x->Clear();
		return -2;
	}
	else if (e == 0) 
	{
		*errorCode = 0;
		x->Clear();
		return 0;
	}
	else 
	{
		*errorCode = x->Count();
		if (all)
			return x->Count();
	}

	return id + 1;
}

// native Regex:regex_match(const string[], const pattern[], &ret, error[], maxLen, const flags[] = "");
static cell AMX_NATIVE_CALL regex_match(AMX *amx, cell *params)
{
	return match(amx, params, false);
}

// native Regex:regex_match_all(const string[], const pattern[], flags = 0, error[] = "", maxLen = 0, &errcode = 0);
static cell AMX_NATIVE_CALL regex_match_all(AMX *amx, cell *params)
{
	return match(amx, params, true);
}

cell match_c(AMX *amx, cell *params, bool all)
{
	int id = params[2] - 1;

	if (id >= (int)PEL.length() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	int len;
	const char *str = MF_GetAmxString(amx, params[1], 0, &len);
	cell *errorCode = MF_GetAmxAddr(amx, params[3]);

	RegEx *x = PEL[id];

	int e;
	if (all)
		e = x->MatchAll(str);
	else
		e = x->Match(str);

	if (e == -1)
	{
		/* there was a match error.  move on. */
		*errorCode = x->mErrorOffset;

		/* only clear the match results, since the regex object
		may still be referenced later */
		x->ClearMatch();
		return -2;
	}
	else if (e == 0) 
	{
		*errorCode = 0;

		/* only clear the match results, since the regex object
		may still be referenced later */
		x->ClearMatch();
		return 0;
	}
	else 
	{
		*errorCode = x->Count();
		return x->Count();
	}
}

// native regex_match_c(const string[], Regex:id, &ret);
static cell AMX_NATIVE_CALL regex_match_c(AMX *amx, cell *params)
{
	return match_c(amx, params, false);
}

// native regex_match_all_c(const string[], Regex:id, &ret);
static cell AMX_NATIVE_CALL regex_match_all_c(AMX *amx, cell *params)
{
	return match_c(amx, params, true);
}

// native regex_substr(Regex:id, str_id, buffer[], maxLen);
static cell AMX_NATIVE_CALL regex_substr(AMX *amx, cell *params)
{
	int id = params[1]-1;
	if (id >= (int)PEL.length() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	RegEx *x = PEL[id];
	static char buffer[16384]; // Same as AMXX buffer.

	size_t length;
	size_t maxLength = ke::Min<size_t>(params[4], sizeof(buffer) - 1);

	const char *ret = x->GetSubstring(params[2], buffer, maxLength, &length);

	if (ret == NULL)
	{
		return 0;
	}

	if (length >= maxLength && ret[length - 1] & 1 << 7)
	{
		maxLength -= UTIL_CheckValidChar((char *)ret + length - 1);
	}

	MF_SetAmxString(amx, params[3], ret, maxLength);

	return 1;
}

static cell AMX_NATIVE_CALL regex_free(AMX *amx, cell *params)
{
	cell *c = MF_GetAmxAddr(amx, params[1]);
	int id = *c;
	*c = 0;
	id -= 1;
	if (id >= (int)PEL.length() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	RegEx *x = PEL[id];
	x->Clear();

	return 1;
}

//native regex_replace(Regex:pattern, string[], maxLen, const replace[], flags = REGEX_FORMAT_DEFAULT, &errcode = 0);
static cell AMX_NATIVE_CALL regex_replace(AMX *amx, cell *params)
{
	int id = params[1] - 1;
	if (id >= (int)PEL.length() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	int textLen, replaceLen;
	char *text = MF_GetAmxString(amx, params[2], 0, &textLen);
	const char *replace = MF_GetAmxString(amx, params[4], 1, &replaceLen);

	cell *erroCode = MF_GetAmxAddr(amx, params[6]);

	RegEx *x = PEL[id]; 
	int e = x->Replace(text, params[3] + 1, replace, replaceLen, params[5]);

	if (e == -1)
	{
		*erroCode = x->mErrorOffset;
		x->ClearMatch();
		return -2;
	}
	else if (e == 0)
	{
		*erroCode = 0;
		x->ClearMatch();
		return 0;
	}

	MF_SetAmxString(amx, params[2], text, params[3]);

	return e;
}

AMX_NATIVE_INFO regex_Natives[] = {
	{"regex_compile",			regex_compile},
	{"regex_compile_ex",		regex_compile_ex},
	{"regex_match",				regex_match},
	{"regex_match_c",			regex_match_c},
	{"regex_match_all",			regex_match_all},
	{"regex_match_all_c",		regex_match_all_c},
	{"regex_substr",			regex_substr},
	{"regex_replace",			regex_replace},
	{"regex_free",				regex_free},
	{NULL,						NULL},
};

void OnAmxxAttach()
{
	MF_AddNatives(regex_Natives);
}

void OnAmxxDetach()
{
	for (int i = 0; i<(int)PEL.length(); i++)
	{
		if (PEL[i])
		{
			delete PEL[i];
			PEL[i] = 0;
		}
	}

	PEL.clear();
}
