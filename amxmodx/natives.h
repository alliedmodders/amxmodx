/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#ifndef _INCLUDE_NATIVES_H
#define _INCLUDE_NATIVES_H

//only 16 for now sorry
#if !defined CALLFUNC_MAXPARAMS
#define CALLFUNC_MAXPARAMS 16
#endif

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
