#include "amxxapi.h"

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
	if (id > Vaults.size() || !Vaults.at(i))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid vault id: %d\n", id);
		return 0;
	}


}

void OnAmxxAttach()
{
	//create the dir if it doesn't exist
#ifdef __linux__
	mkdir(MF_BuildPathname("%s/nvault", LOCALINFO("amxx_datadir")), 0700);
#else
	mkdir(MF_BuildPathname("%s/nvault", LOCALINFO("amxx_datadir"));
#endif
}
