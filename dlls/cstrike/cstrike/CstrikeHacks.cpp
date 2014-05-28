#include "CstrikeDatas.h"
#include <MemoryUtils.h>
#include "CDetour/detours.h"

#if defined(__APPLE__)
	#include <mach-o/nlist.h>
#endif

void CtrlDetour_ClientCommand(bool set);

int g_CSCliCmdFwd = -1;
int *g_UseBotArgs = NULL;
const char **g_BotArgs = NULL;
CDetour *g_ClientCommandDetour = NULL;

void InitializeHacks()
{
	CtrlDetour_ClientCommand(true);
}

void ShutdownHacks()
{
	CtrlDetour_ClientCommand(false);
}

DETOUR_DECL_STATIC1(C_ClientCommand, void, edict_t*, pEdict)
{
	if (*g_UseBotArgs)
	{
		int client = ENTINDEX(pEdict);
		const char *args = *g_BotArgs;

		if (MF_ExecuteForward(g_CSCliCmdFwd, static_cast<cell>(client), args) > 0)
		{
			return;
		}
	}

	DETOUR_STATIC_CALL(C_ClientCommand)(pEdict);
}

void CtrlDetour_ClientCommand(bool set)
{
#if defined AMD64
	#error UNSUPPORTED
#endif

	void *target = (void *)MDLL_ClientCommand;

	if (!g_UseBotArgs)
	{
#if defined(__linux__)

		g_UseBotArgs = (int *)g_MemUtils.ResolveSymbol(target, "UseBotArgs");
		g_BotArgs = (const char **)g_MemUtils.ResolveSymbol(target, "BotArgs");

#elif defined(__APPLE__)

		/* Using dlsym on OS X won't work because the symbols are hidden */
		char dll[256];
		uintptr_t base;
		g_MemUtils.GetLibraryOfAddress(target, dll, sizeof(dll), &base);      
		
		struct nlist symbols[3];
		memset(symbols, 0, sizeof(symbols));
		symbols[0].n_un.n_name = (char *)"_UseBotArgs";
		symbols[1].n_un.n_name = (char *)"_BotArgs";
		if (nlist(dll, symbols) != 0)
		{
			return;
		}
		g_UseBotArgs = (int *)(base + symbols[0].n_value);
		g_BotArgs = (const char **)(base + symbols[1].n_value);

#elif defined(WIN32)

		g_UseBotArgs = *(int **)((unsigned char *)target + CS_CLICMD_OFFS_USEBOTARGS);
		g_BotArgs = (const char **)*(const char **)((unsigned char *)target + CS_CLICMD_OFFS_BOTARGS);

#endif
	}

	if (set)
	{
		g_ClientCommandDetour = DETOUR_CREATE_STATIC_FIXED(C_ClientCommand, target);

		if (g_ClientCommandDetour != NULL)
		{
			g_ClientCommandDetour->EnableDetour();
		}
	}
	else
	{
		g_ClientCommandDetour->Destroy();
	}
}
