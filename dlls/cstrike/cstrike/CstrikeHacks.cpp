#include <assert.h>
#include <stdlib.h>
#include "cstrike.h"

#if defined(__linux__) || defined(__APPLE__)
#include <sys/mman.h>
#define	PAGE_EXECUTE_READWRITE	PROT_READ|PROT_WRITE|PROT_EXEC

#if defined(__linux)
#include <malloc.h>
#endif
#endif

#if defined(__APPLE__)
#include <mach-o/nlist.h>
#endif

/* Utils */
unsigned char *UTIL_CodeAlloc(size_t size);
void UTIL_CodeFree(unsigned char *addr);
void UTIL_MemProtect(void *addr, int length, int prot);
bool UTIL_GetLibraryOfAddress(void *memInBase, char *buffer, size_t maxlength, uintptr_t *base);

/* Detours */
void CtrlDetour_ClientCommand(bool set);
int Detour_ClientCommand(edict_t *pEdict);

int g_CSCliCmdFwd = -1;
int *g_UseBotArgs = NULL;
const char **g_BotArgs = NULL;

/* Called on startup */
void InitializeHacks()
{
	CtrlDetour_ClientCommand(true);
}

void OnPluginsLoaded()
{
	g_CSCliCmdFwd = MF_RegisterForward("CS_InternalCommand", ET_STOP, FP_CELL, FP_STRING, FP_DONE);
}

void ShutdownHacks()
{
	CtrlDetour_ClientCommand(false);
}

void CtrlDetour_ClientCommand(bool set)
{
#if defined AMD64
#error UNSUPPORTED
#endif
	static unsigned char DetourOps[] = 
	{
		0x50,									/* push eax         ; just for safety */
		0xff, 0x74, 0x24, 0x08,			/* push [esp+0x8]	; push the edict pointer */
		0xe8, 0x00, 0x00, 0x00, 0x00, /* call <gate>		; call our function */
		0x83, 0xc4, 0x08,					/* add esp, 8		; correct stack */
		0x85, 0xc0,							/* test eax, eax	; do != 0 test */
		0x74, 0x01,							/* je <cont>		; if == 0, jump to where old func is saved */
		0xc3									/* ret				; return otherwise */
	};
	static unsigned char DetourJmp = '\xE9';

	const unsigned int DetourBytes = 18;
	const unsigned int DetourCallPos = 6;
	const unsigned int DetourJmpPos = DetourBytes + CS_DETOURCOPYBYTES_CLIENTCOMMAND;
	const unsigned int DetourJmpBytes = 5;
	static unsigned char *FullDetour = NULL;

	void *target = (void *)MDLL_ClientCommand;
	unsigned char *paddr;

	if (!g_UseBotArgs)
	{
#if defined(__linux__) || defined(__APPLE__)
		/* Find the DLL */
		char dll[256];
		uintptr_t base;
		if (!UTIL_GetLibraryOfAddress(target, dll, sizeof(dll), &base))
		{
			return;
		}
#if defined(__linux__)
		void *handle = dlopen(dll, RTLD_NOW);
		if (!handle)
		{
			return;
		}
		g_UseBotArgs = (int *)dlsym(handle, "UseBotArgs");
		g_BotArgs = (const char **)dlsym(handle, "BotArgs");
		dlclose(handle);
#elif defined(__APPLE__)
		/* Using dlsym on OS X won't work because the symbols are hidden */
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
#endif
#else /* Windows */
		/* Find the bot args addresses */
		paddr = (unsigned char *)target + CS_CLICMD_OFFS_USEBOTARGS;
		g_UseBotArgs = *(int **)paddr;
		paddr = (unsigned char *)target + CS_CLICMD_OFFS_BOTARGS;
		g_BotArgs = (const char **)*(const char **)paddr;
#endif
	}

	if (set)
	{
		assert(FullDetour == NULL);
		FullDetour = UTIL_CodeAlloc(DetourBytes + CS_DETOURCOPYBYTES_CLIENTCOMMAND + DetourJmpBytes);
	
		/* Copy the main trampoline function */
		memcpy(FullDetour, DetourOps, DetourBytes);
	
		/* Copy our detour call into the trampoline */
		paddr = &FullDetour[DetourCallPos];
		*(unsigned long *)paddr = (unsigned long)Detour_ClientCommand - (unsigned long)(paddr + 4);
	
		/* Copy original bytes onto the end of the function */
	    	memcpy(&FullDetour[DetourBytes], target, CS_DETOURCOPYBYTES_CLIENTCOMMAND);
		
		/* Patch and copy the final jmp */
		paddr = &FullDetour[DetourJmpPos];
		*paddr++ = DetourJmp;
		*(unsigned long *)paddr = ((unsigned long)target + CS_DETOURCOPYBYTES_CLIENTCOMMAND)
									- (unsigned long)(paddr + 4);
	
		/* Now overwrite the target function with our trampoline */
		UTIL_MemProtect(target, CS_DETOURCOPYBYTES_CLIENTCOMMAND + 10, PAGE_EXECUTE_READWRITE);
		paddr = (unsigned char *)target;
		*paddr++ = DetourJmp;
		*(unsigned long *)paddr = (unsigned long)FullDetour - (unsigned long)(paddr + 4);
	} else {
		assert(FullDetour != NULL);

		/* Copy back the original function bytes */
		memcpy(target, &FullDetour[DetourBytes], CS_DETOURCOPYBYTES_CLIENTCOMMAND);

		/* Free memory used */
		UTIL_CodeFree(FullDetour);
		FullDetour = NULL;
	}
}

int Detour_ClientCommand(edict_t *pEdict)
{
	if (*g_UseBotArgs)
	{
		int client = ENTINDEX(pEdict);
		const char *args = *g_BotArgs;
		return MF_ExecuteForward(g_CSCliCmdFwd, (cell)client, args);
	}
	return 0;
}

unsigned char *UTIL_CodeAlloc(size_t size)
{
#if defined WIN32
	return (unsigned char *)VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#elif defined __GNUC__
#if defined __APPLE__
	unsigned char *addr = (unsigned char *)valloc(size);
#else
	unsigned char *addr = (unsigned char *)memalign(sysconf(_SC_PAGESIZE), size);
#endif
	mprotect(addr, size, PROT_READ|PROT_WRITE|PROT_EXEC);
	return addr;
#endif
}

void UTIL_CodeFree(unsigned char *addr)
{
#if defined WIN32
	VirtualFree(addr, 0, MEM_RELEASE);
#else
	free(addr);
#endif
}

void UTIL_MemProtect(void *addr, int length, int prot)
{
#if defined(__linux__) || defined(__APPLE__)
#define ALIGN(ar) ((long)ar & ~(sysconf(_SC_PAGESIZE)-1))
	void *addr2 = (void *)ALIGN(addr);
	mprotect(addr2, sysconf(_SC_PAGESIZE), prot);
#else
	DWORD old_prot;
	VirtualProtect(addr, length, prot, &old_prot);
#endif
}

bool UTIL_GetLibraryOfAddress(void *memInBase, char *buffer, size_t maxlength, uintptr_t *base)
{
#if defined(__linux__) || defined(__APPLE__)
	Dl_info info;
	if (!dladdr(memInBase, &info))
	{
		return false;
	}
	if (!info.dli_fbase || !info.dli_fname)
	{
		return false;
	}
	const char *dllpath = info.dli_fname;
	snprintf(buffer, maxlength, "%s", dllpath);
	if (base)
	{
		*base = (uintptr_t)info.dli_fbase;
	}
#else
	MEMORY_BASIC_INFORMATION mem;
	if (!VirtualQuery(memInBase, &mem, sizeof(mem)))
	{
		return false;
	}
	if (mem.AllocationBase == NULL)
	{
		return false;
	}
	HMODULE dll = (HMODULE)mem.AllocationBase;
	GetModuleFileName(dll, (LPTSTR)buffer, maxlength);
	if (base)
	{
		*base = (uintptr_t)mem.AllocationBase;
	}
#endif
	return true;
}

