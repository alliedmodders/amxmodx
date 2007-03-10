#include "sdk/amxxmodule.h"

#include "hamsandwich.h"

#include "VTableManager.h"
#include "VTableEntries.h"

#include "vfunc_gcc295.h"
#include "vfunc_msvc.h"

#include "NEW_Util.h"

// Change these on a per-hook basis! Auto-changes all the annoying fields in the following functions
#define ThisVTable  VTableAddPlayerItem
#define ThisEntries	AddPlayerItemEntries

#define ThisKey			"addplayeritem"
#define ThisNative		"ham_addplayeritem"
#define ThisENative		"ham_eaddplayeritem"
#define ThisRegisterID	HAM_AddPlayerItem
#define ThisParamCount	1
#define ThisVoidCall	0

unsigned int	*ThisVTable::pevoffset=NULL;
unsigned int	*ThisVTable::pevset=NULL;
unsigned int	*ThisVTable::baseoffset=NULL;
unsigned int	*ThisVTable::baseset=0;
unsigned int	 ThisVTable::index=0;
unsigned int	 ThisVTable::indexset=0;

static AMX_NATIVE_INFO callnatives[] = {
	{ ThisNative,				ThisVTable::NativeCall },
	{ ThisENative,				ThisVTable::ENativeCall },
	{ NULL,						NULL }
};

/**
 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
 *
 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
 * @noreturn
 */
void ThisVTable::Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset)
{
	ThisVTable::pevoffset=poffset;
	ThisVTable::pevset=pset;

	ThisVTable::baseoffset=baseoffs;
	ThisVTable::baseset=baseset;

	ThisVTable::index=0;
	ThisVTable::indexset=0;

	RegisterConfigCallback(ThisVTable::ConfigDone);

	RegisterKeySuffix(ThisKey,ThisVTable::KeyValue);

	RegisterThisRegisterName(ThisRegisterID,ThisKey);
};

/**
 * Called when one of this table entry's keyvalues is caught in a config file.
 *
 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
 * @param data				The data this keyvalue is set to.
 * @noreturn
 */
void ThisVTable::KeyValue(const char *key, const char *data)
{
	if (strcmp(key,ThisKey)==0)
	{
		ThisVTable::index=HAM_StrToNum(data);
		ThisVTable::indexset=1;
	}
};

/**
 * Called immediately after the config file is done being parsed.  Register our natives here.
 *
 * @noreturn
 */
void ThisVTable::ConfigDone(void)
{
	if (ThisVTable::indexset && *(ThisVTable::baseset))
	{
		MF_AddNatives(callnatives);

		if (*(ThisVTable::pevset))
		{
			//MF_AddNatives(registernatives);
			RegisterThisRegister(ThisRegisterID,ThisVTable::RegisterNative,ThisVTable::RegisterIDNative);
		}
	}
};

/**
 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
 * 
 * @param amx				The AMX structure for the plugin.
 * @param params			The parameters passed from the plugin.
 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
 */
cell ThisVTable::RegisterNative(AMX *amx, cell *params)
{
	// Get the classname
	char *classname=MF_GetAmxString(amx,params[1],1,NULL);

	// create an entity, assign it the gamedll's class, hook it and destroy it
	edict_t *Entity=CREATE_ENTITY();

	CALL_GAME_ENTITY(PLID,classname,&Entity->v);

	if (Entity->pvPrivateData)
	{
		// Simulate a call to hs_register_id_takedamage
		cell tempparams[4];
		memcpy(tempparams,params,sizeof(cell)*4);
		tempparams[1]=ENTINDEX_NEW(Entity);
		ThisVTable::RegisterIDNative(amx,&tempparams[0]);
		REMOVE_ENTITY(Entity);
		return 1;
	}

	REMOVE_ENTITY(Entity);

	char *function=MF_GetAmxString(amx,params[2],0,NULL);
	// class was not found
	// throw an error alerting console that this hook did not happen
	MF_LogError(amx, AMX_ERR_NATIVE,"Failed to retrieve classtype for \"%s\", hook for \"%s\" not active.",classname,function);
	
	return 0;

};

/**
 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
 * 
 * @param amx				The AMX structure for the plugin.
 * @param params			The parameters passed from the plugin.
 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
 */
cell ThisVTable::RegisterIDNative(AMX *amx, cell *params)
{
	int funcid;
	char *function=MF_GetAmxString(amx,params[2],0,NULL);

	if (MF_AmxFindPublic(amx,function,&funcid)!=AMX_ERR_NONE)
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Can not find function \"%s\"",function);
		return 0;
	}
	edict_t *Entity=INDEXENT_NEW(params[1]);

	if (Entity->pvPrivateData)
	{
		ThisVTable::Hook(&VTMan,EdictToVTable(Entity),amx,funcid,params[0] / sizeof(cell) > 2 ? params[3] : 0);
		return 1;
	}

	// class was not found
	// throw an error alerting console that this hook did not happen
	MF_LogError(amx, AMX_ERR_NATIVE,"Failed to retrieve classtype for entity id %d, hook for \"%s\" not active.",params[1],function);
	
	return 0;

};

/**
 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
 *
 * @param amx				The AMX structure for the plugin.
 * @param params			The parameters passed from the plugin.
 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
 */
cell ThisVTable::NativeCall(AMX *amx, cell *params)
{
	// scan to see if this virtual function is a trampoline
	void *pthis=INDEXENT_NEW(params[1])->pvPrivateData;
	void *func=GetVTableEntry(pthis,ThisVTable::index,*ThisVTable::baseoffset);

	int i=0;
	int end=VTMan.ThisEntries.size();

	while (i<end)
	{
		if (VTMan.ThisEntries[i]->IsTrampoline(func))
		{
			// this function is a trampoline
			// use the original function instead
			func=VTMan.ThisEntries[i]->GetOriginalFunction();
			break;
		}
		++i;
	}
	// TODO: Inline ASM this
#ifdef _WIN32
	return reinterpret_cast<int (__fastcall *)(void *,int,void *)>(func)(
		pthis,											/*this*/
		0,												/*fastcall buffer*/
		INDEXENT_NEW(params[2])->pvPrivateData			/*item*/
		);
#else
	return reinterpret_cast<int (*)(void *,void *)>(func)(
		pthis,											/*this*/
		INDEXENT_NEW(params[2])->pvPrivateData			/*item*/
		);
#endif
};

/**
 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
 *
 * @param amx				The AMX structure for the plugin.
 * @param params			The parameters passed from the plugin.
 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
 */
cell ThisVTable::ENativeCall(AMX *amx, cell *params)
{
	return VCall1<int>(
		INDEXENT_NEW(params[1])->pvPrivateData, /*this*/
		ThisVTable::index,						/*vtable entry*/
		*(ThisVTable::baseoffset),				/*size of class*/
		INDEXENT_NEW(params[3])->pvPrivateData	/*item*/
	);
};

/**
 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
 *
 * @param manager			The VTableManager this is a child of.
 * @param vtable			The virtual table we're molesting.
 * @param outtrampoline		The trampoline that was created.
 * @param origfunc			The original function that was hooked.
 * @noreturn
 */
void ThisVTable::CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc)
{

	VTableEntryBase::CreateGenericTrampoline(manager,
		vtable,
		ThisVTable::index,
		id,
		outtrampoline,
		origfunc,
		reinterpret_cast<void *>(ThisVTable::EntryPoint),
		ThisParamCount,  // param count
		ThisVoidCall,  // voidcall
		1); // thiscall

};

/**
 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
 *
 * @param manager			The VTableManager this is a child of.
 * @param vtable			The virtual table we're molesting.
 * @param plugin			The plugin that's requesting this.
 * @param funcid			The function id of the callback.
 * @noreturn
 */
void ThisVTable::Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post)
{
	void *ptr=vtable[ThisVTable::index];

	int i=0;
	int end=manager->ThisEntries.size();
	int fwd=MF_RegisterSPForward(plugin,funcid,FP_CELL/*this*/,FP_CELL/*item*/,FP_DONE);
	while (i<end)
	{
		if (manager->ThisEntries[i]->IsTrampoline(ptr))
		{
			// this function is already hooked!

			if (post)
			{
				manager->ThisEntries[i]->AddPostForward(fwd);
			}
			else
			{
				manager->ThisEntries[i]->AddForward(fwd);
			}
			
			return;
		}

		++i;
	}
	// this function is NOT hooked
	void *tramp;
	void *func;
	ThisVTable::CreateHook(manager,vtable,manager->ThisEntries.size(),&tramp,&func);
	ThisVTable *entry=new ThisVTable;
	
	entry->Setup(&vtable[ThisVTable::index],tramp,func);

	manager->ThisEntries.push_back(entry);
	
	if (post)
	{
		entry->AddPostForward(fwd);
	}
	else
	{
		entry->AddForward(fwd);
	}

}

/**
 * Execute the command.  This is called directly from our global hook function.
 *
 * @param pthis				The "this" pointer, cast to a void.  The victim.
 * @param inflictor			Damage inflictor.
 * @param attacker			The attacker who caused the inflictor to damage the victim.
 * @param damage			How much damage was caused.
 * @param type				Damage type (usually in bitmask form).
 * @return					Unsure.  Does not appear to be used.
 */
int ThisVTable::Execute(void *pthis, void *item)
{
	int i=0;

	int end=Forwards.size();

	int result=HAM_UNSET;
	int thisresult=HAM_UNSET;

	int iThis=PrivateToIndex(pthis);
	int iItem=PrivateToIndex(item);

	while (i<end)
	{
		thisresult=MF_ExecuteForward(Forwards[i++],iThis,item);

		if (thisresult>result)
		{
			result=thisresult;
		}
	};
	int ireturn=0;

	if (result<HAM_SUPERCEDE)
	{
#if defined _WIN32
		ireturn=reinterpret_cast<int (__fastcall *)(void *,int,void *)>(function)(pthis,0,item);
#elif defined __linux__
		ireturn=reinterpret_cast<int (*)(void *,void *)>(function)(pthis,item);
#endif
	}

	i=0;

	end=PostForwards.size();
	while (i<end)
	{
		MF_ExecuteForward(PostForwards[i++],iThis,iItem);
	}


	if (result!=HAM_OVERRIDE)
		return ireturn;

	return 0;
};
HAM_CDECL int ThisVTable::EntryPoint(int id,void *pthis,void *item)
{
	return VTMan.ThisEntries[id]->Execute(pthis,item);
}
