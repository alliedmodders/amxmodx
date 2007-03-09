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

// MSVC stores vtable like normal at the front as well
// however these are thiscall functions
// i use inline assembly to call them
#ifdef _WIN32
#ifndef VFUNC_MSVC_H
#define VFUNC_MSVC_H

inline void *GetVTableEntry(void *pThis, int ventry, int size)
{
	void **vtbl=*(void ***)pThis;

	return vtbl[ventry];
}
template <class PTypeA, class PTypeB, class PTypeC, class PTypeD>
inline void VoidVCall4(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc, PTypeD pd)
{
	// vtable pointer is stored in the first dword of the object
	// reference it as an array of objects
	void **vtbl=*(void ***)pThis;

	void *func=vtbl[ventry];

	// Simulate a thiscall
	// this on ecx, all other parameters pushed normally
	_asm {
		push ecx; // save ecx
		push eax; // save eax - shouldn't be needed, but just incase

		push pd;  // push param 4
		push pc;  // push param 3
		push pb;  // push param 2
		push pa;  // push param 1

		mov ecx, pThis; // store this in ecx

		call [func]; // call function

		pop eax; // restore eax
		pop ecx; // restore ecx
	};
};
template <class RetType, class PTypeA, class PTypeB, class PTypeC, class PTypeD>
inline RetType VCall4(void *pThis, int ventry, int size, PTypeA pa, PTypeB pb, PTypeC pc, PTypeD pd)
{
	void **vtbl=*(void ***)pThis;

	void *func=vtbl[ventry];

	RetType _ret;
	_asm {
		push ecx;
		push eax;
		push pd;
		push pc;
		push pb;
		push pa;
		mov ecx, pThis;
		call [func];
		mov _ret, eax;
		pop eax;
		pop ecx;
	};

	return _ret;
};
template <class PTypeA>
inline void VoidVCall1(void *pThis, int ventry, int size, PTypeA pa)
{
	void **vtbl=*(void ***)pThis;

	void *func=vtbl[ventry];

	_asm {
		push ecx;
		push eax;
		push pa;
		mov ecx, pThis;
		call [func];
		pop eax;
		pop ecx;
	};
};
template <class RetType, class PTypeA>
inline RetType VCall1(void *pThis, int ventry, int size, PTypeA pa)
{
	void **vtbl=*(void ***)pThis;

	void *func=vtbl[ventry];

	RetType _ret;
	_asm {
		push ecx;
		push eax;
		push pa;
		mov ecx, pThis;
		call [func];
		mov _ret, eax;
		pop eax;
		pop ecx;
	};

	return _ret;
};


#endif //VFUNC_MSVC_H
#endif // _WIN32
