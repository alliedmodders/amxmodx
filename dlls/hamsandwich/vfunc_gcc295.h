/* Ham Sandwich
 *
 * by sawce
 *
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
// Calling virtual functions on binaries compiled with GCC 2.95
// 2.95 and before stores the virtual table at the end of the
// inheritable size of the base class.
// I have no idea how it does it for multiple inheritance; i don't
// really care.  Everything I'm calling does it in single inheritence.
// GCC doesn't put this on a register like MSVC does, so 
// just pass it like a normal parameter (the first one)
// For GCC 3.3 compiled binaries, set the "size" parameter to 0

#ifdef __linux__
#ifndef VFUNC_GCC295_H
#define VFUNC_GCC295_H

inline void *GetVTableEntry(void *pThis, int ventry, int size)
{
	char *pcThis=*(char **)&pThis;

	pcThis+=size;

	void **vtbl=*(void ***)pcThis;

	return vtbl[ventry];
}

// I only comment on the first call, because it's jut copy/paste after
// the rest are compacted for copy/paste ease
template <class PTypeA, class PTypeB, class PTypeC, class PTypeD>
inline void VoidVCall4(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc, PTypeD pd)
{
	// First move up past the size of the class
	char *pcThis=*(char **)&pThis;

	pcThis+=size;

	void **vtbl=*(void ***)pcThis;

	// now points to the vtable of this object

	typedef void (*fptr)(void*,PTypeA,PTypeB,PTypeC,PTypeD);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);

	function(pThis,pa,pb,pc,pd);
};
template <class RetType, class PTypeA, class PTypeB, class PTypeC, class PTypeD>
inline RetType VCall4(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc, PTypeD pd)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef RetType (*fptr)(void*,PTypeA,PTypeB,PTypeC,PTypeD);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	return function(pThis,pa,pb,pc,pd);
};
template <class PTypeA, class PTypeB, class PTypeC>
inline void VoidVCall3(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef void (*fptr)(void*,PTypeA,PTypeB,PTypeC);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	function(pThis,pa,pb,pc);
};
template <class RetType, class PTypeA, class PTypeB, class PTypeC>
inline RetType VCall3(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef RetType (*fptr)(void*,PTypeA,PTypeB,PTypeC);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	return function(pThis,pa,pb,pc);
};
template <class PTypeA, class PTypeB>
inline void VoidVCall2(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef void (*fptr)(void*,PTypeA,PTypeB);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	function(pThis,pa,pb);
};
template <class RetType, class PTypeA, class PTypeB>
inline RetType VCall2(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef RetType (*fptr)(void*,PTypeA,PTypeB);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	return function(pThis,pa,pb);
};
template <class PTypeA>
inline void VoidVCall1(void *pThis, int ventry, int size, PTypeA pa)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef void (*fptr)(void*,PTypeA);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	function(pThis,pa);
};
template <class RetType, class PTypeA>
inline RetType VCall1(void *pThis, int ventry, int size, PTypeA pa)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef RetType (*fptr)(void*,PTypeA);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	return function(pThis,pa);
};
inline void VoidVCall0(void *pThis, int ventry, int size)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef void (*fptr)(void*);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	function(pThis);
};
template <class RetType>
inline RetType VCall0(void *pThis, int ventry, int size)
{
	char *pcThis=*(char **)&pThis;
	pcThis+=size;
	int **vtbl=*(int ***)pcThis;
	typedef RetType (*fptr)(void*);
	fptr function=reinterpret_cast<fptr>(vtbl[ventry]);
	return function(pThis);
};


#endif //VFUNC_GCC295_H
#endif // __linux__
