// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#include <string.h>

#include "amxxmodule.h"

#include "ns.h"
#include "ns_const.h"

#include "utilfunctions.h"

#include "FastDelegate.h"
#include "GameManager.h"

extern int IsValidBuilding[AVH_USER3_MAX + 1];

using namespace fastdelegate::detail;


void *GameRules=NULL;


mBOOL dlclose_handle_invalid; // Linking errors with metamod

// void AvHBaseBuildable::StartRecycle()
static void (GenericClass::*MFP_Recycle)();

// void AvHWeldable::AddBuildTime(float)
static void (GenericClass::*MFP_WeldFinished)(float);

// AvHGameRules *GetGameRules(void)
static void *(*FP_GetGameRules)();


char *FuncBase;

/**
 * sizeof(void (detail::GenericClass::*fptr)())
 * is 8 in GCC.  Add an empty void * pointer at
 * the end to compensate.
 * Layout in GCC:
 * union { 
 *   void *address;      // When this is an address it will always be positive
 *   int   vtable_index; // When it is a vtable index it will always be odd = (vindex*2)+1
 * };
 * int delta;
 * -
 * Delta is the adjustment to the this pointer
 * For my implementations I will only need it to 0
 */
#ifdef __GNUC__
template <typename OutType>
inline void set_mfp(OutType &out, void *in)
{
	union
	{
		void    *in[2];
		OutType out;
	} mfpu;

	mfpu.in[0]=in;
	mfpu.in[1]=NULL;
	out=mfpu.out;
};
#else
template <typename OutType>
inline void set_mfp(OutType &out, void *in)
{
	out=horrible_cast<OutType>(in);
};
#endif

void MFuncs_Initialize(void)
{
	char FileName[256];
	DLHANDLE DLLBase;
#ifdef __linux__
	UTIL_Format(FileName,sizeof(FileName)-1,"%s/dlls/ns_i386.so",MF_GetModname());
#else
	UTIL_Format(FileName, sizeof(FileName)-1, "%s\\dlls\\ns.dll", MF_GetModname());
#endif

	DLLBase=DLOPEN(FileName);
	FuncBase=(char *)DLSYM(DLLBase, MAKE_OFFSET(BASE));
	DLCLOSE(DLLBase);

#define MFP(Offs) (((void *)(((char *)FuncBase)+MAKE_OFFSET(Offs))))

	set_mfp(MFP_Recycle,MFP(MEMBER_RECYCLE));
	
	set_mfp(MFP_WeldFinished,MFP(MEMBER_TRIGGER_WELDABLE));

	// This is not a member function pointer, but use MFP since it
	// uses the same address conversion as MFPs do
	FP_GetGameRules=horrible_cast<void *(*)()>(MFP(GETGAMERULES));
};

static cell AMX_NATIVE_CALL ns_recycle(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->free || Entity->pvPrivateData==NULL)
	{
		return 0;
	}

	if (Entity->v.iuser3 <= AVH_USER3_NONE || Entity->v.iuser3 >= AVH_USER3_MAX)
	{
		return 0;
	}
	if (IsValidBuilding[Entity->v.iuser3]!=1) // Not a marine structure?
	{
		return 0;
	}

	// Make sure it's a marine building, undefined stuff happens on alien structures
	(reinterpret_cast<GenericClass *>(Entity->pvPrivateData)->*(MFP_Recycle))();
	

	return 1;
};
static cell AMX_NATIVE_CALL ns_finish_weldable(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->free || Entity->pvPrivateData==NULL)
	{
		return 0;
	}
	// verify the classname since this will crash if it's the wrong class!
	if (strcmp(STRING(Entity->v.classname),"avhweldable")!=0)
	{
		return 0;
	}

	// First need to set the weldable to 100% complete
	set_private_f(Entity,MAKE_OFFSET(WELD_DONE),get_private_f(Entity,MAKE_OFFSET(WELD_TIME)));

	// Now make NS think the weldable has been welded again
	// This has to call AvHWeldable::AddBuildTime(float)
	// because AvHWeldable::TriggerFinished() does not work properly
	(reinterpret_cast<GenericClass *>(Entity->pvPrivateData)->*(MFP_WeldFinished))(100.0);
	

	return 1;
};

static cell AMX_NATIVE_CALL ns_get_teamres(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}
	if (GameRules==NULL) // GameRules not initialized yet
	{
		GameRules=(*(FP_GetGameRules))();
	}
	if (GameRules==NULL) // Still null? Get out of here
	{
		return 0;
	}
	switch(params[1])
	{
	case 1:
		{
			return amx_ftoc2(*(REAL *)((char *)GameRules+GAMERULES_TEAMA_RESOURCES));
		}
	case 2:
		{
			return amx_ftoc2(*(REAL *)((char *)GameRules+GAMERULES_TEAMB_RESOURCES));
		}
	default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "ns_get_teamres: Expected 1 for team a or 2 for team b, got %d", params[1]);
			return 0;
		}
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_set_teamres(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}
	if (GameRules==NULL) // GameRules not initialized yet
	{
		GameRules=(*(FP_GetGameRules))();
	}
	if (GameRules==NULL) // Still null? Get out of here
	{
		return 0;
	}
	switch(params[1])
	{
	case 1:
		{
			*(REAL *)((char *)GameRules+GAMERULES_TEAMA_RESOURCES)=amx_ctof2(params[2]);
			return 1;
		}
	case 2:
		{
			*(REAL *)((char *)GameRules+GAMERULES_TEAMB_RESOURCES)=amx_ctof2(params[2]);
			return 1;
		}
	default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "ns_set_teamres: Expected 1 for team a or 2 for team b, got %d", params[1]);
			return 0;
		}
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_add_teamres(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}
	if (GameRules==NULL) // GameRules not initialized yet
	{
		GameRules=(*(FP_GetGameRules))();
	}
	if (GameRules==NULL) // Still null? Get out of here
	{
		return 0;
	}
	switch(params[1])
	{
	case 1:
		{
			return amx_ftoc2(*(REAL *)((char *)GameRules+GAMERULES_TEAMA_RESOURCES)+=amx_ctof2(params[2]));
		}
	case 2:
		{
			return amx_ftoc2(*(REAL *)((char *)GameRules+GAMERULES_TEAMB_RESOURCES)+=amx_ctof2(params[2]));
		}
	default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "ns_add_teamres: Expected 1 for team a or 2 for team b, got %d", params[1]);
			return 0;
		}
	}
	return 0;
}

#ifdef DEVELOPER_BUILD
static cell AMX_NATIVE_CALL findgameinfo(AMX *amx, cell *params)
{
	void *Ret=(*(FP_GetGameRules))();
	union 
	{
		void *v;
		int i;
	}vi;
	vi.v=Ret;

	printf("GameRules=%d\n",vi.i);
	return 1;
};
#endif
AMX_NATIVE_INFO memberfunc_natives[] = {
#ifdef DEVELOPER_BUILD
	{ "getgameinfo",			findgameinfo },
#endif
	{ "ns_recycle",				ns_recycle },
	{ "ns_finish_weldable",		ns_finish_weldable },
	{ "ns_get_teamres",			ns_get_teamres },
	{ "ns_set_teamres",			ns_set_teamres },
	{ "ns_add_teamres",			ns_add_teamres },
	{ NULL,						NULL }
};

void AddNatives_MemberFunc()
{
	MF_AddNatives(memberfunc_natives);
};
