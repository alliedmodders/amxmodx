#include <string.h>
#include "pcre.h"
#include "CVector.h"
#include "CRegEx.h"
#include "amxxmodule.h"
#include "module.h"

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

static cell AMX_NATIVE_CALL regex_match(AMX *amx, cell *params)
{
	int len;
	const char *str = MF_GetAmxString(amx, params[1], 0, &len);
	const char *regex = MF_GetAmxString(amx, params[2], 1, &len);

	int id = GetPEL();
	RegEx *x = PEL[id];
	
	if (x->Compile(regex) == 0)
	{
		cell *eOff = MF_GetAmxAddr(amx, params[3]);
		const char *err = x->mError;
		*eOff = x->mErrorOffset;
		MF_SetAmxString(amx, params[4], err?err:"unknown", params[5]);
		return 0;
	}

	int e = x->Match(str);
	if (e == -1)
	{
		/* there was a match error.  destroy this and move on. */
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mErrorOffset;
		x->Clear();
		return -1;
	} else {
		cell *res = MF_GetAmxAddr(amx, params[3]);
		*res = x->mSubStrings;
	}

	return id+1;
}

static cell AMX_NATIVE_CALL regex_substr(AMX *amx, cell *params)
{
	int id = params[1];
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

AMX_NATIVE_INFO regex_Natives[] = {
	{"regex_match",				regex_match},
	{"regex_substr",			regex_substr},
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