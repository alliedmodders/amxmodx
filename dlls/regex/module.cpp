#include <string.h>
#include "pcre.h"
#include "amxxmodule.h"
#include "CVector.h"
#include "CRegEx.h"

CVector<RegEx *> PEL;

int GetPEL()
{
	for (int i=0; i<(int)PEL.size(); i++)
	{
		if (PEL[i]->isFree())
			return i;
	}

	RegEx *x = new RegEx();
	PEL.push_back(x);

	return (int)PEL.size() - 1;
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
		cell *eOff = MF_GetAmxAddr(amx, params[2]);
		const char *err = x->mError;
		*eOff = x->mErrorOffset;
		MF_SetAmxString(amx, params[3], err?err:"unknown", params[4]);
		return -1;
	}
	
	return id+1;
}// 1.8 includes the last parameter
// Regex:regex_match(const string[], const pattern[], &ret, error[], maxLen, const flags[] = "");
static cell AMX_NATIVE_CALL regex_match(AMX *amx, cell *params)
{
	int len;
	const char *str = MF_GetAmxString(amx, params[1], 0, &len);
	const char *regex = MF_GetAmxString(amx, params[2], 1, &len);

	int id = GetPEL();
	RegEx *x = PEL[id];
	
	char* flags = NULL;
	
	if ((params[0] / sizeof(cell)) >= 6) // compiled with 1.8's extra parameter
	{
		flags = MF_GetAmxString(amx, params[6], 2, &len);
	}
	
	if (x->Compile(regex, flags) == 0)
	{
		cell *eOff = MF_GetAmxAddr(amx, params[3]);
		const char *err = x->mError;
		*eOff = x->mErrorOffset;
		MF_SetAmxString(amx, params[4], err?err:"unknown", params[5]);
		return -1;
	}

	int e = x->Match(str);
	if (e == -1)
	{
		/* there was a match error.  destroy this and move on. */
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mErrorOffset;
		x->Clear();
		return -2;
	} else if (e == 0) {
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = 0;
		x->Clear();
		return 0;
	} else {
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mSubStrings;
	}

	return id+1;
}
// native regex_match_c(const string[], Regex:id, &ret);
static cell AMX_NATIVE_CALL regex_match_c(AMX *amx, cell *params)
{
	int len;
	int id = params[2]-1;
	const char *str = MF_GetAmxString(amx, params[1], 0, &len);

	if (id >= (int)PEL.size() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}
	
	RegEx *x = PEL[id];

	int e = x->Match(str);
	if (e == -1)
	{
		/* there was a match error.  move on. */
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mErrorOffset;
		/* only clear the match results, since the regex object
		   may still be referenced later */
		x->ClearMatch();
		return -2;
	} else if (e == 0) {
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = 0;
		/* only clear the match results, since the regex object
		   may still be referenced later */
		x->ClearMatch();
		return 0;
	} else {
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mSubStrings;
		return x->mSubStrings;
	}
}

static cell AMX_NATIVE_CALL regex_substr(AMX *amx, cell *params)
{
	int id = params[1]-1;
	if (id >= (int)PEL.size() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	RegEx *x = PEL[id];
	//good idea? probably not.
	static char buffer[4096];

	const char *ret = x->GetSubstring(params[2], buffer, 4095);

	if (ret == NULL)
		return 0;

	MF_SetAmxString(amx, params[3], ret, params[4]);

	return 1;
}

static cell AMX_NATIVE_CALL regex_free(AMX *amx, cell *params)
{
	cell *c = MF_GetAmxAddr(amx, params[1]);
	int id = *c;
	*c = 0;
	id -= 1;
	if (id >= (int)PEL.size() || id < 0 || PEL[id]->isFree())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid regex handle %d", id);
		return 0;
	}

	RegEx *x = PEL[id];
	x->Clear();

	return 1;
}

AMX_NATIVE_INFO regex_Natives[] = {
	{"regex_compile",			regex_compile},
	{"regex_match",				regex_match},
	{"regex_match_c",			regex_match_c},
	{"regex_substr",			regex_substr},
	{"regex_free",				regex_free},
	{NULL,						NULL},
};

void OnAmxxAttach()
{
	MF_AddNatives(regex_Natives);
}

void OnAmxxDetach()
{
	for (int i = 0; i<(int)PEL.size(); i++)
	{
		if (PEL[i])
		{
			delete PEL[i];
			PEL[i] = 0;
		}
	}

	PEL.clear();
}
