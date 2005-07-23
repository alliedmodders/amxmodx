#include <stdlib.h>
#include "amxxapi.h"

#ifdef __linux__
#include <unistd.h>
#else
#include <direct.h>
#endif

CVector<Vault *> Vaults;

static cell nvault_open(AMX *amx, cell *params)
{
	int len, id=-1;
	char *name = MF_GetAmxString(amx, params[1], 0, &len);
	char *file = MF_BuildPathname("%s/nvault/%s", LOCALINFO("amxx_datadir"), name);
	for (size_t i=0; i<Vaults.size(); i++)
	{
		if (!Vaults.at(i))
		{
			id = i;
		} else if (strcmp(Vaults.at(i)->GetFileName(), file) == 0) {
			return i;
		}
	}
	Vault *v = new Vault(file);
	if (id != -1)
	{
		Vaults[id] = v;
	} else {
		Vaults.push_back(v);
		id = (int)Vaults.size()-1;
	}

	return id;
}

static cell nvault_get(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id > Vaults.size() || !Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	unsigned int numParams = (*params)/sizeof(cell);
	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	const char *val = Vaults.at(id)->Find(key)->val.c_str();
	switch (numParams)
	{
	case 2:
		{
			return atoi(val);
			break;
		}
	case 3:
		{
			cell *fAddr = MF_GetAmxAddr(amx, params[3]);
			*fAddr = amx_ftoc((REAL)atof(val));
			return 1;
			break;
		}
	case 4:
		{
			len = *(MF_GetAmxAddr(amx, params[4]));
			return MF_SetAmxString(amx, params[3], val, len);
			break;
		}
	}

	return 0;
}

static cell nvault_timeget(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id > Vaults.size() || !Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	unsigned int numParams = (*params)/sizeof(cell);
	int len;
	HashTable::htNode *node;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	node = Vaults.at(id)->Find(key);
	const char *val = node->val.c_str();
	cell *t_addr = MF_GetAmxAddr(amx, params[3]);
	*t_addr = (cell)(node->stamp);
	switch (numParams)
	{
	case 3:
		{
			return atoi(val);
			break;
		}
	case 4:
		{
			cell *fAddr = MF_GetAmxAddr(amx, params[4]);
			*fAddr = amx_ftoc((REAL)atof(val));
			return 1;
			break;
		}
	case 5:
		{
			len = *(MF_GetAmxAddr(amx, params[5]));
			return MF_SetAmxString(amx, params[4], val, len);
			break;
		}
	}

	return 0;
}

static cell nvault_set(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id > Vaults.size() || !Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}

	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	char *val = MF_FormatAmxString(amx, params, 3, &len);

	Vaults.at(id)->Store(key, val);

	return 1;
}

static cell nvault_pset(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id > Vaults.size() || !Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}

	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	char *val = MF_FormatAmxString(amx, params, 3, &len);

	Vaults.at(id)->Store(key, val, false);

	return 1;
}

void OnAmxxAttach()
{
	//create the dir if it doesn't exist
#ifdef __linux__
	mkdir(MF_BuildPathname("%s/nvault", LOCALINFO("amxx_datadir")), 0700);
#else
	mkdir(MF_BuildPathname("%s/nvault", LOCALINFO("amxx_datadir")));
#endif
}
