#include <stdlib.h>
#include "amxxapi.h"
#include "NVault.h"
#include "CQueue.h"

#ifdef WIN32
#define MKDIR(p) mkdir(p)
#else
#define MKDIR(p) mkdir(p, 0755)
#endif

#ifdef __linux__
#include <unistd.h>
#else
#include <direct.h>
#endif

CVector<NVault *> g_Vaults;
CQueue<int> g_OldVaults;

VaultMngr g_VaultMngr;

static cell nvault_open(AMX *amx, cell *params)
{
	int len, id=-1;
	char *name = MF_GetAmxString(amx, params[1], 0, &len);
	char path[255], file[255];
	MF_BuildPathnameR(path, sizeof(path)-1, "%s/vault", LOCALINFO("amxx_datadir"));
	sprintf(file, "%s/%s.vault", path, name);
	for (size_t i=0; i<g_Vaults.size(); i++)
	{
		if (strcmp(g_Vaults.at(i)->GetFilename(), file) == 0) 
		{
			return i;
		}
	}
	NVault *v = new NVault(file);
	if (!v->Open())
	{
		delete v;
		return -1;
	}
	if (!g_OldVaults.empty())
	{
		id = g_OldVaults.front();
		g_OldVaults.pop();
	}
	if (id != -1)
	{
		g_Vaults[id] = v;
	} else {
		g_Vaults.push_back(v);
		id = (int)g_Vaults.size()-1;
	}

	return id;
}

static cell nvault_get(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	unsigned int numParams = (*params)/sizeof(cell);
	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	const char *val = pVault->GetValue(key);
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

static cell nvault_lookup(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	int len;
	time_t stamp;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	char *buffer = new char[params[4]+1];
	if (!pVault->GetValue(key, stamp, buffer, params[4]))
	{
		delete [] buffer;
		return 0;
	}
	MF_SetAmxString(amx, params[3], buffer, params[4]);
	cell *addr = MF_GetAmxAddr(amx, params[5]);
	addr[0] = (cell)stamp;
	delete [] buffer;
	return 1;
}

static cell nvault_set(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	char *val = MF_FormatAmxString(amx, params, 3, &len);

	pVault->SetValue(key, val);

	return 1;
}

static cell nvault_pset(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	int len;
	char *key = MF_GetAmxString(amx, params[2], 0, &len);
	char *val = MF_FormatAmxString(amx, params, 3, &len);

	pVault->SetValue(key, val, 0);

	return 1;
}

static cell nvault_close(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	pVault->Close();
	delete pVault;
	g_Vaults[id] = NULL;
	g_OldVaults.push(id);

	return 1;
}

static cell AMX_NATIVE_CALL nvault_prune(AMX *amx, cell *params)
{
	unsigned int id = params[1];
	if (id >= g_Vaults.size() || !g_Vaults.at(id))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}
	NVault *pVault = g_Vaults.at(id);
	time_t start = (time_t )params[2];
	time_t end = (time_t )params[3];

	return pVault->Prune(start, end);
}

IVaultMngr *GetVaultMngr()
{
	return static_cast<IVaultMngr *>(&g_VaultMngr);
}

void OnAmxxAttach()
{
	//create the dir if it doesn't exist
	MKDIR(MF_BuildPathname("%s/vault", LOCALINFO("amxx_datadir")));
	MF_AddNatives(nVault_natives);
	MF_RegisterFunction(GetVaultMngr, "GetVaultMngr");
}

void ServerDeactivate_Post()
{
	for (size_t i=0; i<g_Vaults.size(); i++)
	{
		if (g_Vaults[i])
			delete g_Vaults[i];
	}
	g_Vaults.clear();
	while (!g_OldVaults.empty())
		g_OldVaults.pop();
}

AMX_NATIVE_INFO nVault_natives[] = {
	{"nvault_open",				nvault_open},
	{"nvault_get",				nvault_get},
	{"nvault_lookup",			nvault_lookup},
	{"nvault_set",				nvault_set},
	{"nvault_pset",				nvault_pset},
	{"nvault_close",			nvault_close},
	{"nvault_prune",			nvault_prune},
	{NULL,				NULL},
};
