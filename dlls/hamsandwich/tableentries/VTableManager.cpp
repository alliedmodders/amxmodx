#include "sdk/amxxmodule.h"

#include "hamsandwich.h"

#include "VTableManager.h"
#include "VTableEntries.h"

#include "NEW_Util.h"

VTableManager VTMan;

NATIVEFUNC VTableManager::RegisterNatives[HAM_END_DONT_USE_ME];
NATIVEFUNC VTableManager::RegisterIDNatives[HAM_END_DONT_USE_ME];
const char *VTableManager::RegisterNames[HAM_END_DONT_USE_ME];

void RegisterThisRegister(int index,NATIVEFUNC byname, NATIVEFUNC byid)
{
	VTableManager::RegisterNatives[index]=byname;
	VTableManager::RegisterIDNatives[index]=byid;
}
void RegisterThisRegisterName(int index, const char *name)
{
	VTableManager::RegisterNames[index]=name;
}

static AMX_NATIVE_INFO registernatives[] = {
	{ "ham_register",			VTableManager::Register },
	{ "ham_registerid",			VTableManager::RegisterID },

	{ NULL,						NULL }
};
void RegisterRegisterNatives(void)
{
	MF_AddNatives(registernatives);
}
cell VTableManager::Register(AMX *amx, cell *params)
{
	int id=params[1];

	if (id<0 || id>=HAM_END_DONT_USE_ME || RegisterIDNatives[id]==NULL)
	{
		// this register is not found, fail the plugin
		int fwd=MF_RegisterSPForwardByName(amx,"__fatal_ham_error",FP_STRING,FP_DONE);
		char error[256];

		snprintf(&error[0],sizeof(error)-1,"Requested to ham_registerid a function ID that is not registered in configs/hamdata.ini, cannot continue. (Requested: %d)",id);

		MF_ExecuteForward(fwd,&error[0]);

		MF_UnregisterSPForward(fwd);
		return 0;

	}

	cell tempparams[4];

	// remove one parameter from this param count
	tempparams[0]=(params[0]-(sizeof(cell)));
	tempparams[1]=params[2];
	tempparams[2]=params[3];
	tempparams[3]=params[4];

	return RegisterNatives[id](amx,&tempparams[0]);
}
cell VTableManager::RegisterID(AMX *amx, cell *params)
{
	int id=params[1];

	if (id<0 || id>=HAM_END_DONT_USE_ME || RegisterNatives[id]==NULL)
	{
		// this register is not found, fail the plugin
		int fwd=MF_RegisterSPForwardByName(amx,"__fatal_ham_error",FP_STRING,FP_DONE);

		char error[256];

		snprintf(&error[0],sizeof(error)-1,"Requested to ham_register a function ID that is not registered in configs/hamdata.ini, cannot continue. (Requested: %d)",id);
		
		MF_ExecuteForward(fwd,&error[0]);

		MF_UnregisterSPForward(fwd);
		return 0;

	}

	cell tempparams[4];

	// remove one parameter from this param count
	tempparams[0]=(params[0]-(sizeof(cell)));
	tempparams[1]=params[2];
	tempparams[2]=params[3];
	tempparams[3]=params[4];

	return RegisterIDNatives[id](amx,&tempparams[0]);
}


void *VTableManager::InsertIntoVTable(void **vtable, int index, void *trampoline)
{
	void *func;
#if defined _WIN32
	DWORD OldFlags;
	VirtualProtect(&vtable[index],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined __linux__
	mprotect(&vtable[index],sizeof(int*),PROT_READ|PROT_WRITE);
#endif
	func=vtable[index];
	vtable[index]=trampoline;

	return func;
};


#define CLEAR_ENTRIES(Container)		\
	i=Container.size();					\
	while (i--)							\
	{									\
		Container[i]->Destroy();		\
		delete Container[i];			\
	}									\
	Container.clear()


void VTableManager::Cleanup(void)
{
	int i;

	CLEAR_ENTRIES(UseEntries);
	CLEAR_ENTRIES(TakeDamageEntries);
};



void VTableEntryBase::CreateGenericTrampoline(VTableManager *manager, void **vtable, int vtid, int id, void **outtrampoline, void **origfunc, void *callee, int paramcount, int voidcall, int thiscall)
{
	Trampolines::TrampolineMaker tramp;

	if (voidcall)
	{
		if (thiscall)
		{
			tramp.ThisVoidPrologue();
		}
		else
		{
			tramp.VoidPrologue();
		}
	}
	else
	{
		if (thiscall)
		{
			tramp.ThisReturnPrologue();
		}
		else
		{
			tramp.ReturnPrologue();
		}
	}

	while (paramcount)
	{
		tramp.PushParam(paramcount--);
	}
	if (thiscall)
	{
		tramp.PushThis();
	}
	tramp.PushNum(id);
	tramp.Call(callee);
	tramp.FreeTargetStack();
	if (voidcall)
	{
#if defined _WIN32
		tramp.VoidEpilogueAndFree();
#elif defined __linux__
		tramp.VoidEpilogue();
#endif
	}
	else
	{
#if defined _WIN32
		tramp.ReturnEpilogueAndFree();
#elif defined __linux__
		tramp.ReturnEpilogue();
#endif
	}

	void *trampoline=tramp.Finish(NULL);

	*outtrampoline=trampoline;

	*origfunc=manager->InsertIntoVTable(vtable,vtid,trampoline);
};
