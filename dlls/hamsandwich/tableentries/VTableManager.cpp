#include "sdk/amxxmodule.h"

#include "VTableManager.h"
#include "VTableEntries.h"

#include "NEW_Util.h"

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
