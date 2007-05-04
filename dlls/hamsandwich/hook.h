#ifndef HOOK_H
#define HOOK_H

#include "forward.h"
#include "Trampolines.h"

// This is just a simple container for data so I only have to add 1 extra 
// parameter to calls that get trampolined

class Hook
{
public:
	CVector<Forward *> pre;     // pre forwards
	CVector<Forward *> post;    // post forwards
	void            *func;    // original function
	void           **vtable;  // vtable of the original location
	int              entry;   // vtable entry of the function
	void            *target;  // target function being called (the hook)
	int              exec;    // 1 when this hook is in execution
	int              del;     // 1 if this hook should be destroyed after exec
	void            *tramp;   // trampoline for this hook
	char			*ent;     // ent name that's being hooked

	Hook(void **vtable_, int entry_, void *target_, bool voidcall, int paramcount, char *name) :
		func(NULL), vtable(vtable_), entry(entry_), target(target_), exec(0), del(0), tramp(NULL)
		{
			// original function is vtable[entry]
			// to not make the compiler whine, cast vtable to int **
			int **ivtable=(int **)vtable;
			func=(void *)ivtable[entry];

			// now install a trampoline
			// (int thiscall, int voidcall, int paramcount, void *extraptr)
			tramp=CreateGenericTrampoline(true, voidcall, paramcount, (void*)this, target);

			// Insert into vtable
#if defined _WIN32
			DWORD OldFlags;
			VirtualProtect(&ivtable[entry],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined __linux__
			mprotect(&ivtable[entry],sizeof(int*),PROT_READ|PROT_WRITE);
#endif
			ivtable[entry]=(int*)tramp;

			size_t len=strlen(name);
			ent=new char[len+1];

			snprintf(ent,len+1,"%s",name);
		};

	~Hook()
	{
		// Insert the original function back into the vtable
		int **ivtable=(int **)vtable;

#if defined _WIN32
		DWORD OldFlags;
		VirtualProtect(&ivtable[entry],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined __linux__
		mprotect(&ivtable[entry],sizeof(int*),PROT_READ|PROT_WRITE);
#endif

		ivtable[entry]=(int *)func;

		free(tramp);

		delete[] ent;

		CVector<Forward *>::iterator end=pre.end();

		for (CVector<Forward *>::iterator i=pre.begin();
			 i!=end;
			 ++i)
		{
			delete (*i);
		}
	}
};

#endif
