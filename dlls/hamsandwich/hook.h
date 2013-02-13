/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#ifndef HOOK_H
#define HOOK_H

#include "forward.h"
#include "Trampolines.h"

// This is just a simple container for data so I only have to add 1 extra 
// parameter to calls that get trampolined

#define ALIGN(ar) ((intptr_t)ar & ~(sysconf(_SC_PAGESIZE)-1))

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

			snprintf(ent,len+1,"%s",name);
		};

	~Hook()
	{
		// Insert the original function back into the vtable
		int **ivtable=(int **)vtable;

#if defined(_WIN32)
		DWORD OldFlags;
		VirtualProtect(&ivtable[entry],sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined(__linux_) || defined(__APPLE__)
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

		CVector<Forward *>::iterator end=pre.end();

		for (CVector<Forward *>::iterator i=pre.begin();
			 i!=end;
			 ++i)
		{
			delete (*i);
		}
		end=post.end();
		for (CVector<Forward *>::iterator i=post.begin();
			 i!=end;
			 ++i)
		{
			delete (*i);
		}
		pre.clear();
		post.clear();
	}
};

#endif
