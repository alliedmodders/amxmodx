#include <extdll.h>
#include "sdk/amxxmodule.h"
#include "NEW_Util.h"

edict_t *NEW_FirstEdict;
bool NEW_Initialized;


extern AMX_NATIVE_INFO RegisterNatives[];

int ReadConfig(void);

void OnAmxxAttach(void)
{
	printf("LOLOL");
	if (ReadConfig() > 0)
	{
		MF_AddNatives(RegisterNatives);
	}
}

void HamCommand(void);

void OnPluginsLoaded(void)
{
	NEW_Initialize(INDEXENT(0));
}
void OnMetaAttach(void)
{
	REG_SVR_COMMAND("ham", HamCommand);
}
