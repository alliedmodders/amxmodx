#ifndef _INCLUDE_NATIVES_H
#define _INCLUDE_NATIVES_H

//only 16 for now sorry
#define CALLFUNC_MAXPARAMS 16

#define CALLFUNC_FLAG_BYREF			1
#define CALLFUNC_FLAG_BYREF_REUSED	2

#define N_CELL		1
#define	N_ARRAY		2
#define N_BYREF		3
#define	N_VARARG	4

struct regnative
{
	AMX *amx;
	String name;
	char *pfn;
	int func;
	AMX *caller;
	int style;
	cell params[CALLFUNC_MAXPARAMS];
};

extern "C" void amxx_DynaInit(void *ptr);
extern "C" void amxx_DynaMake(char *buffer, int id);
extern "C" int amxx_DynaFunc(AMX *amx, cell *params);
extern "C" int amxx_DynaCodesize();

AMX_NATIVE_INFO *BuildNativeTable();
void AddPluginLibrary(const char *name);
void ClearPluginLibraries();
bool LibraryExists(const char *name);

//I couldn't resist :)
extern AMX_NATIVE_INFO g_NativeNatives[];

#endif //_INCLUDE_NATIVES_H
