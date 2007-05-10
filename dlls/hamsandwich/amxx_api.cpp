#include <extdll.h>
#include "sdk/amxxmodule.h"

#include "NEW_Util.h"
#include "CVector.h"
#include "forward.h"
#include "hook.h"
#include "ham_const.h"
#include "hooklist.h"
#include <assert.h>

edict_t *NEW_FirstEdict;
bool NEW_Initialized;

extern CVector<Hook*> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

extern AMX_NATIVE_INFO RegisterNatives[];
extern AMX_NATIVE_INFO ReturnNatives[];

extern hook_t hooklist[];

int ReadConfig(void);

void OnAmxxAttach(void)
{
	// Assert that the enum is aligned properly with the table
	assert(strcmp(hooklist[Ham_NS_UpdateOnRemove].name, "ns_updateonremove")==0);

	if (ReadConfig() > 0)
	{
		MF_AddNatives(RegisterNatives);
		MF_AddNatives(ReturnNatives);
	}
}

void HamCommand(void);

void OnPluginsUnloaded(void)
{

	CVector <Hook *>::iterator end;
	for (int i = 0; i < HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
	{
		end=hooks[i].end();

		for (CVector<Hook*>::iterator j=hooks[i].begin();
			 j!=end;
			 ++j)
		{
			delete (*j);
		}
		hooks[i].clear();
	}
}

void OnPluginsLoaded(void)
{
	NEW_Initialize(INDEXENT(0));
}
void OnMetaAttach(void)
{
	REG_SVR_COMMAND("ham", HamCommand);
}
