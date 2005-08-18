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

#include "amxmodx.h"
#include "CStack.h"
#include "natives.h"

#ifdef __linux__
#include <sys/mman.h>
#endif

//Written by David "BAILOPAN" Anderson
//With the exception for param_convert, which was written by
// Julien "dJeyL" Laurent

CVector<regnative *> g_RegNatives;
CStack<regnative *> g_NativeStack;
CVector<String> g_Libraries;
static char g_errorStr[512] = {0};
static int g_errorNum = 0;
bool g_Initialized = false;

int amxx_DynaCallback(int idx, AMX *amx, cell *params)
{
	if (idx < 0 || idx >= (int)g_RegNatives.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid dynamic native called");
		return 0;
	}

	regnative *pNative = g_RegNatives[idx];
	int numParams = params[0] / sizeof(cell);

	if (numParams > CALLFUNC_MAXPARAMS)
	{
		LogError(amx, AMX_ERR_NATIVE, "Called dynanative with too many parameters (%d)", CALLFUNC_MAXPARAMS);
		return 0;
	}

	//parameter stack
	pNative->caller = amx;

	CPluginMngr::CPlugin *pPlugin = g_plugins.findPluginFast(amx);

	int err = 0;
	cell ret = 0;
	g_errorNum = 0;
	g_NativeStack.push(pNative);
	if (pNative->style == 0)
	{
		amx_Push(pNative->amx, numParams);
		amx_Push(pNative->amx, pPlugin->getId());
		for (int i=numParams; i>=1; i--)
			pNative->params[i] = params[i];
	} else if (pNative->style == 1) {
		//use dJeyL's system .. very clever!
		for (int i=numParams; i>=1; i--)
			amx_Push(pNative->amx, params[i]);
	}
	if ( (err=amx_Exec(pNative->amx, &ret, pNative->func)) != AMX_ERR_NONE)
	{
		g_NativeStack.pop();
		LogError(pNative->amx, err, "");
		return 0;
	}
	if (g_errorNum)
	{
		g_NativeStack.pop();
		LogError(amx, g_errorNum, g_errorStr);
		return ret;
	}
	g_NativeStack.pop();

	return ret;
}

AMX_NATIVE_INFO *BuildNativeTable()
{
	if (g_RegNatives.size() < 1)
		return NULL;

	AMX_NATIVE_INFO *pNatives = new AMX_NATIVE_INFO[g_RegNatives.size() + 1];

	AMX_NATIVE_INFO info;
	regnative *pNative;
	for (size_t i=0; i<g_RegNatives.size(); i++)
	{
		pNative = g_RegNatives[i];
		info.name = pNative->name.c_str();
		info.func = (AMX_NATIVE)((void *)(pNative->pfn));
		pNatives[i] = info;
	}
	pNatives[g_RegNatives.size()].name = NULL;
	pNatives[g_RegNatives.size()].func = NULL;

	//this needs to be deleted
	return pNatives;
}

static cell AMX_NATIVE_CALL log_error(AMX *amx, cell *params)
{
	int len;
	char *err = format_amxstring(amx, params, 2, len);

	_snprintf(g_errorStr, sizeof(g_errorStr), "%s", err);
	g_errorNum = params[1];

	return 1;
}

//get_string(param, dest[], len)
static cell AMX_NATIVE_CALL get_string(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	int len;
	char *str = get_amxstring(pNative->caller, pNative->params[p], 0, len);
	return set_amxstring(amx, params[2], str, params[3]);
}

//set_string(param, source[], maxlen)
static cell AMX_NATIVE_CALL set_string(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	int len;
	char *str = get_amxstring(amx, params[2], 0, len);

	return set_amxstring(pNative->caller, pNative->params[p], str, params[3]);
}

//get a byvalue parameter
//get_param(num)
static cell AMX_NATIVE_CALL get_param(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	return pNative->params[p];
}

//get_param_byref(num)
static cell AMX_NATIVE_CALL get_param_byref(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	cell *addr = get_amxaddr(pNative->caller, pNative->params[p]);

	return addr[0];
}

//set_param_byref(num, val)
static cell AMX_NATIVE_CALL set_param_byref(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	cell *addr = get_amxaddr(pNative->caller, pNative->params[p]);

	addr[0] = params[2];

	return 1;
}

//get_array(param, dest[], size)
static cell AMX_NATIVE_CALL get_array(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	cell *source = get_amxaddr(pNative->caller, pNative->params[p]);
	cell *dest = get_amxaddr(amx, params[2]);

	int size = params[3];

	while (size-->0)
		*dest = *source;

	return 1;
}

//set_array(param, source[], size)
static cell AMX_NATIVE_CALL set_array(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	int p = params[1];

	cell *dest = get_amxaddr(pNative->caller, pNative->params[p]);
	cell *source = get_amxaddr(amx, params[2]);

	int size = params[3];

	while (size-->0)
		*dest = *source;

	return 1;
}

//This is basically right from dJeyL's lib_convert function
//This awesome hack modifies the stack frame to have an address offset
// that will align to the other plugin's memory.
//I've no idea how he thought of this, but it's great.  No idea how well it works.
static cell AMX_NATIVE_CALL param_convert(AMX *amx, cell *params)
{
	if (!g_NativeStack.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Not currently in a dynamic native");
		return 0;
	}
	regnative *pNative = g_NativeStack.top();
	if (pNative->style != 1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Wrong style of dynamic native");
		return 0;
	}
	cell p = params[1];

	AMX *caller = pNative->caller;

	unsigned char *data =amx->base+(int)((AMX_HEADER *)amx->base)->dat;
	unsigned char *realdata = caller->base+(int)((AMX_HEADER *)caller->base)->dat;

	* (cell *)(data+(int)amx->frm+(p+2)*sizeof(cell)) -= (cell)data-(cell)realdata;

	return 1;
}

static cell AMX_NATIVE_CALL register_library(AMX *amx, cell *params)
{
	int len;
	char *lib = get_amxstring(amx, params[1], 0, len);

	AddPluginLibrary(lib);

	return 1;
}

//register_native(const name[], const handler[])
static cell AMX_NATIVE_CALL register_native(AMX *amx, cell *params)
{
	if (!g_Initialized)
		amxx_DynaInit((void *)(amxx_DynaCallback));

	g_Initialized = true;

	int len;
	char *name = get_amxstring(amx, params[1], 0, len);
	char *func = get_amxstring(amx, params[2], 1, len);

	int idx, err;
	if ( (err=amx_FindPublic(amx, func, &idx)) != AMX_ERR_NONE)
	{
		LogError(amx, err, "Function \"%s\" was not found", func);
		return 0;
	}

	regnative *pNative = new regnative;
	pNative->amx = amx;
	pNative->func = idx;
	
	//we'll apply a safety buffer too
	//make our function
	int size = amxx_DynaCodesize();
#ifndef __linux__
	DWORD temp;
	pNative->pfn = new char[size + 10];
	VirtualProtect(pNative->pfn, size+10, PAGE_EXECUTE_READWRITE, &temp);
#else
	pNative->pfn = (char *)memalign(sysconf(_SC_PAGESIZE), size+10);
	mprotect((void *)pNative->pfn, size+10, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif

	int id = (int)g_RegNatives.size();
	
	amxx_DynaMake(pNative->pfn, id);
	pNative->func = idx;
	pNative->style = params[3];

	g_RegNatives.push_back(pNative);

	pNative->name.assign(name);

	return 1;
}

bool LibraryExists(const char *name)
{
	for (size_t i=0; i<g_Libraries.size(); i++)
	{
		if (stricmp(g_Libraries[i].c_str(), name)==0)
			return true;
	}

	return false;
}

void AddPluginLibrary(const char *name)
{
	String f(name);
	g_Libraries.push_back(f);
}

void ClearPluginLibraries()
{
	g_Libraries.clear();

	for (size_t i=0; i<g_RegNatives.size(); i++)
	{
		delete [] g_RegNatives[i]->pfn;
		delete g_RegNatives[i];
	}
	g_RegNatives.clear();
}

AMX_NATIVE_INFO g_NativeNatives[] = {
	{"register_native",	register_native},
	{"log_error",		log_error},
	{"register_library",register_library},
	{"get_string",		get_string},
	{"set_string",		set_string},
	{"get_param",		get_param},
	{"get_param_byref",	get_param_byref},
	{"set_param_byref",	set_param_byref},
	{"get_array",		set_array},
	{"set_array",		set_array},
	//these are dummy functions for floats ;p
	{"get_param_f",		get_param},
	{"get_float_byref",	get_param_byref},
	{"set_float_byref", set_param_byref},
	{"get_array_f",		get_array},
	{"set_array_f",		set_array},
	{"param_convert",	param_convert},
	//////////////////////////
	{NULL,				NULL},
};
