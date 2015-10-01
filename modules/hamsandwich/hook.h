// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#ifndef HOOK_H
#define HOOK_H

#include "forward.h"
#include "Trampolines.h"
#include <amtl/am-vector.h>
#include <amtl/am-string.h>

#define ALIGN(ar) ((intptr_t)ar & ~(sysconf(_SC_PAGESIZE)-1))

// This is just a simple container for data so I only have to add 1 extra 
// parameter to calls that get trampolined

class Hook
{
public:
	ke::Vector<Forward *> pre;     // pre forwards
	ke::Vector<Forward *> post;    // post forwards
	void			*func;    // original function
	void           **vtable;  // vtable of the original location
	int              entry;   // vtable entry of the function
	void            *target;  // target function being called (the hook)
	int              exec;    // 1 when this hook is in execution
	int              del;     // 1 if this hook should be destroyed after exec
	void            *tramp;   // trampoline for this hook
	char			*ent;     // ent name that's being hooked

	Hook(void **vtable_, int entry_, void *target_, bool voidcall, bool retbuf, int paramcount, char *name) :
		func(NULL), vtable(vtable_), entry(entry_), target(target_), exec(0), del(0), tramp(NULL)
		{
			// original function is vtable[entry]
			// to not make the compiler whine, cast vtable to int **
			int **ivtable=(int **)vtable;
			func=(void *)ivtable[entry];

			// now install a trampoline
			// (int thiscall, int voidcall, int paramcount, void *extraptr)
			tramp = CreateGenericTrampoline(true, voidcall, retbuf, paramcount, (void*)this, target);

			// Insert into vtable
#if defined(_WIN32)
			DWORD OldFlags;
			VirtualProtect(&ivtable[entry],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined(__linux__) || defined(__APPLE__)
			void *addr = (void *)ALIGN(&ivtable[entry]);
			mprotect(addr,sysconf(_SC_PAGESIZE),PROT_READ|PROT_WRITE);
#endif
			ivtable[entry]=(int*)tramp;

			size_t len=strlen(name);
			ent=new char[len+1];

			ke::SafeSprintf(ent, len + 1, "%s", name);
		};

	~Hook()
	{
		// Insert the original function back into the vtable
		int **ivtable=(int **)vtable;

#if defined(_WIN32)
		DWORD OldFlags;
		VirtualProtect(&ivtable[entry],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined(__linux__) || defined(__APPLE__)
		void *addr = (void *)ALIGN(&ivtable[entry]);
		mprotect(addr,sysconf(_SC_PAGESIZE),PROT_READ|PROT_WRITE);
#endif

		ivtable[entry]=(int *)func;
#if defined(_WIN32)
		VirtualFree(tramp, 0, MEM_RELEASE);
#elif defined(__linux__) || defined(__APPLE__)
		free(tramp);
#endif

		delete[] ent;

		for (size_t i = 0; i < pre.length(); ++i)
		{
			pre.at(i)->Release();
		}

		for (size_t i = 0; i < post.length(); ++i)
		{
			post.at(i)->Release();
		}

		pre.clear();
		post.clear();
	}
};

#endif
