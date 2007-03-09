#include "sdk/amxxmodule.h"

#include "hamsandwich.h"

#include "VTableManager.h"
#include "VTableEntries.h"

#include "vfunc_gcc295.h"
#include "vfunc_msvc.h"

#include "NEW_Util.h"

// Change these on a per-hook basis! Auto-changes all the annoying fields in the following functions
#define ThisVTable  VTableUse
#define ThisEntries	UseEntries
#define ThisHook	VHOOK_Use

unsigned int	*ThisVTable::pevoffset=NULL;
unsigned int	*ThisVTable::pevset=NULL;
unsigned int	*ThisVTable::baseoffset=NULL;
unsigned int	*ThisVTable::baseset=0;
unsigned int	 ThisVTable::index=0;
unsigned int	 ThisVTable::indexset=0;

static AMX_NATIVE_INFO registernatives[] = {
	{ "register_use",			ThisVTable::RegisterNative },
	{ NULL,						NULL }
};

static AMX_NATIVE_INFO callnatives[] = {
	{ "hs_use",					ThisVTable::NativeCall },
	{ "hs_euse",				ThisVTable::ENativeCall },
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

	RegisterKeySuffix("use",ThisVTable::KeyValue);
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
	if (strcmp(key,"use")==0)
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
			MF_AddNatives(registernatives);
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
	int funcid;
	char *function=MF_GetAmxString(amx,params[2],0,NULL);

	if (MF_AmxFindPublic(amx,function,&funcid)!=AMX_ERR_NONE)
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Can not find function \"%s\"",function);
		return 0;
	}
	// Get the classname
	char *classname=MF_GetAmxString(amx,params[1],1,NULL);

	// create an entity, assign it the gamedll's class, hook it and destroy it
	edict_t *Entity=CREATE_ENTITY();

	CALL_GAME_ENTITY(PLID,classname,&Entity->v);

	if (Entity->pvPrivateData)
	{
		ThisVTable::Hook(&VTMan,EdictToVTable(Entity),amx,funcid);
		REMOVE_ENTITY(Entity);
		return 1;
	}

	REMOVE_ENTITY(Entity);
	// class was not found
	// throw an error alerting console that this hook did not happen
	MF_LogError(amx, AMX_ERR_NATIVE,"Failed to retrieve classtype for \"%s\", hook for \"%s\" not active.",classname,function);
	
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
	// TODO: This
	return 0;
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
	VoidVCall4(
		INDEXENT_NEW(params[1])->pvPrivateData,		/*this*/
		ThisVTable::index,							/*vtable entry*/
		*(ThisVTable::baseoffset),					/*size of class*/
		INDEXENT_NEW(params[2])->pvPrivateData,		/*activator*/
		INDEXENT_NEW(params[3])->pvPrivateData,		/*caller*/
		params[4],									/*type*/
		amx_ctof2(params[5]));						/*value*/

	return 1;
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
		reinterpret_cast<void *>(ThisHook),
		4,  // param count
		1,  // voidcall
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
void ThisVTable::Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid)
{
	void *ptr=vtable[ThisVTable::index];

	int i=0;
	int end=manager->ThisEntries.size();
	int fwd=MF_RegisterSPForward(plugin,funcid,FP_CELL/*this*/,FP_CELL/*inflictor*/,FP_CELL/*attacker*/,FP_CELL/*damage*/,FP_CELL/*type*/,FP_DONE);
	while (i<end)
	{
		if (manager->ThisEntries[i]->IsTrampoline(ptr))
		{
			// this function is already hooked!

			manager->ThisEntries[i]->AddForward(fwd);
			
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
	
	entry->AddForward(fwd);

}

/**
 * Execute the command.  This is called directly from our global hook function.
 *
 * @param pthis				The "this" pointer, cast to a void.  The victim.
 * @param activator			Entity causing the opening.
 * @param caller			Entity controlling the caller.
 * @param type				USE_TYPE (USE_{ON,OFF,SET}
 * @param value				Use value, only seen set when USE_SET is used.
 * @noreturn
 */
void ThisVTable::Execute(void *pthis, void *activator, void *caller, int type, float value)
{
	int i=0;

	int end=Forwards.size();

	int result=HAM_UNSET;
	int thisresult=HAM_UNSET;

	int iThis=PrivateToIndex(pthis);
	int iActivator=PrivateToIndex(activator);
	int iCaller=PrivateToIndex(caller);

	while (i<end)
	{
		thisresult=MF_ExecuteForward(Forwards[i++],iThis,iActivator,iCaller,type,amx_ftoc2(value));

		if (thisresult>result)
		{
			result=thisresult;
		}
	};

	if (result>=HAM_SUPERCEDE)
	{
		return;
	}

#if defined _WIN32
	reinterpret_cast<void (__fastcall *)(void *,int,void *,void *,int,float)>(function)(pthis,0,activator,caller,type,value);
#elif defined __linux__
	reinterpret_cast<void (*)(void *,void *,void *,int,float)>(function)(pthis,activator,caller,type,value);
#endif

};
