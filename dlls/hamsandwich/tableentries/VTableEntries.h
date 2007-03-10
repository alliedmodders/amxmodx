/* Ham Sandwich
 *
 * by the AMX Mod X Dev Team
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


#ifndef VTABLEENTRIES_H
#define VTABLEENTRIES_H

#ifdef _WIN32
#define HAM_CDECL __cdecl
#else
#define HAM_CDECL __attribute__((cdecl))
#endif

#include "CVector.h"

class VTableManager;

class VTableEntryBase
{
public:
	void					 *function;		/**< The pointer to the original function that is being hooked. */
	void					**location;		/**< The location of the vtable entry that is being hooked. */
	void					 *trampoline;	/**< Our trampoline (needs to be freed when it's not hooking any more!). */
	CVector<int>			  Forwards;		/**< Vector of forwards to call for this hook.*/
	CVector<int>			  PostForwards;	/**< Vector of forwards to call for this post hook.*/

	/**
	 * Saves virtual table location, trampoline and function pointers.
	 *
	 * @param vt				Pointer to the index in the virtual table.
	 * @param tramp				Pointer to the trampoline.
	 * @param func				Pointer to the original function.
	 * @noreturn
	 */
	void Setup(void **vt,void *tramp, void *func)
	{
		location=vt;
		trampoline=tramp;
		function=func;

	};

	/**
	 * Manually called by VTableManager at destruction.  Treat this like a dtor.
	 *
	 * @see VTableManager::Cleanup()
	 * @noreturn
	 */
	void Destroy()
	{
		// restore original location
		if (location)
		{
#if defined _WIN32
			DWORD OldFlags;
			VirtualProtect(location,sizeof(int*),PAGE_READWRITE,&OldFlags);
#elif defined __linux__
			mprotect(location,sizeof(int*),PROT_READ|PROT_WRITE);
#endif

			*location=function;
		}

		// free the trampoline
		free(trampoline);

		Forwards.clear();
		PostForwards.clear();

	};
	/**
	 * Tells whether the given pointer is this entry's trampoline.
	 *
	 * @param ptr			Pointer (cast to void*) of the function in question.
	 * @return				true: Yes, the pointer in question is this entry's trampoline.
	 *						fase: No, the pointer in question is not this entry's trampoline.
	 */
	bool IsTrampoline(void *ptr)
	{
		return (ptr==trampoline);
	};

	/**
	 * Returns the pointer (cast to void*) of the original function which is being hooked.
	 *
	 * @return				Original function pointer, cast to void*.
	 */
	void *GetOriginalFunction(void)
	{
		return function;
	};

	/**
	 * Returns the location of this entry's virtual table entry itself.
	 *
	 * @return				Pointer to this entry's virtual table entry.
	 */
	void **GetLocation(void)
	{
		return location;
	};

	/**
	 * Returns a pointer to this entry's trampoline.
	 *
	 * @return				Trampoline pointer, cast to void*.
	 */
	void *GetTrampoline(void)
	{
		return trampoline;
	};
	/**
	 * Adds a forward to this entry's forward vector.
	 *
	 * @param fwd			Forward index to add.
	 * @noreturn
	 */
	void AddForward(int fwd)
	{
		Forwards.push_back(fwd);
	};

	/**
	 * Adds a forward to this entry's post forward vector.
	 *
	 * @param fwd			Forward index to add.
	 * @noreturn
	 */
	void AddPostForward(int fwd)
	{
		PostForwards.push_back(fwd);
	};

	/**
	 * Creates a generic trampoline.
	 *
	 * @param manager			The VTableManager this entry belongs to.
	 * @param vtable			Pointer to the virtual table to molest.
	 * @param vindex			VTable index to replace.
	 * @param id				The unique id of this trampoline.
	 * @param outtrampoline		Gets set to the location of the trampoline.
	 * @param origfunc			Gets set to the original function which is being hooked.
	 * @param calee				Target function this trampoline will call.
	 * @param paramcount		How many parameters this trampoline pushes.
	 * @param voidcall			Set to 1, this function does not return.  0 otherwise.
	 * @param thiscall			Set to 1, treat this function like a thiscall.
	 */
	static void CreateGenericTrampoline(VTableManager *manager, void **vtable, int vtableindex, int id, void **outtrampoline, void **origfunc, void *calee, int paramcount, int voidcall=1, int thiscall=1);

};


// int ObjectCaps(void) TODO
class VTableObjectCaps : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int Classify(void) TODO
class VTableClassify : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


class VTableTraceAttack : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

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
	int Execute(void *pthis, void *pevattacker, float flDamage, float *direction, void *tr, int bitsDamageType);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,void *pevattacker,float flDamage,float *direction,void *tr,int bits);
};
// int TakeDamage(entvars_t *inflictor, entvars_t *attacker, float damage, int type);
class VTableTakeDamage : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

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
	int Execute(void *pthis, void *inflictor, void *attacker, float damage, int type);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis,void *inflictor,void *attacker,float damage,int type);
};
// int BloodColor(void) TODO
class VTableBloodColor : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};



// void Killed(entvars_t *attacker, int gib)
class VTableKilled : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param attacker			The attacker who caused the inflictor to damage the victim.
	 * @param gib				Whether to gib or not.
	 * @noreturn
	 */
	void Execute(void *pthis, void *attacker, int gib);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,void *attacker, int gib);
};
// void Restart(void) CS ONLY
class VTableRestart : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis);
};
// int GetToggleState(void) TODO
class VTableGetToggleState : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};



// void AddPoints(int points, int allownegative)
class VTableAddPoints : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis, int points, int allowneg);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,int points,int allownegative);
};
// void AddPointsToTeam(int points, int allownegative)
class VTableAddPointsToTeam : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis, int points, int allowneg);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,int points,int allownegative);
};

// int AddPlayerItem(CBasePlayerItem *item);
class VTableAddPlayerItem : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param item				The private data of the item being added.
	 * @return					Unsure.  Does not appear to be used.
	 */
	int Execute(void *pthis, void *item);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 * @param item				The private data of the item being added.
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis,void *item);
};
// int RemovePlayerItem(CBasePlayerItem *item);
class VTableRemovePlayerItem : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param item				The private data of the item being added.
	 * @return					Unsure.  Does not appear to be used.
	 */
	int Execute(void *pthis, void *item);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 * @param item				The private data of the item being added.
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis,void *item);
};










// int Is(void) TODO
class VTableIsMoving : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};



// int Is(void) TODO
class VTableDamageDecal : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};




// int Is(void) TODO
class VTableIsSneaking : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int Is(void) TODO
class VTableIsAlive : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int Is(void) TODO
class VTableIsBSPModel : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int IsInWorld(void) TODO
class VTableIsInWorld : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int IsPlayer(void) TODO
class VTableIsPlayer : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// int IsNetClient(void) TODO
class VTableIsNetClient : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 */
	int Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static int HAM_CDECL EntryPoint(int id,void *pthis);
};


// const char *TeamID(void) TODO
class VTableTeamID : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	const char *Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static const char* HAM_CDECL EntryPoint(int id,void *pthis);
};

// void Think(void) TODO
class VTableThink : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis);
};
// void Touch(void *pOther) TODO
class VTableTouch : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis, void *other);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,void *other);
};
// void Use(CBaseEntity *activator, CBaseEntity *caller, USE_TYPE type, float value)
class VTableUse : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

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
	void Execute(void *pthis, void *activator, void *caller, int type, float value);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis, void *activator, void *caller, int type, float value);

};
// void Blocked(CBaseEntity *pOther)
class VTableBlocked : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis, void *other);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis,void *other);
};

// CBaseEntity *Respawn(void)
class VTableRespawn : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void *Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void* HAM_CDECL EntryPoint(int id,void *pthis);
};

// void UpdateOwner(void) TODO
class VTableUpdateOwner : public VTableEntryBase
{
public:
	static unsigned int				*pevoffset;	/**< Offset of pev value (globally stored) */
	static unsigned int				*pevset;		/**< Whether or not pev entry has been set */
	static unsigned int				 *baseoffset;	/**< Offset of the base class (only needed for GCC 2.95). */
	static unsigned int				 *baseset;		/**< Whether or base offset value has been set. */
	static unsigned int				  index;		/**< This entry's virtual table index. */
	static unsigned int				  indexset;		/**< Whether or not this entry's virtual table index has been set. */

	/**
	 * Initialize this table hook. This also registers our required keyvalue suffixes to the file parser.
	 *
	 * @param poffset			Pointer to an integer that stores the pev offset for this mod.
	 * @param pset				Pointer to an integer that tells whether pev offset was set or not.
	 * @param baseoffs			Pointer to an integer that stores the class base offset for this mod. (GCC 2.95 only required)
	 * @param baseset			Pointer to an integer that tells whether class base offset has been set.
	 * @noreturn
	 */
	static void Initialize(unsigned int *poffset, unsigned int *pset, unsigned int *baseoffs, unsigned int *baseset);

	/**
	 * Called when one of this table entry's keyvalues is caught in a config file.
	 *
	 * @param key				The keyvalue suffix ("<mod>_<os>_" is removed)
	 * @param data				The data this keyvalue is set to.
	 * @noreturn
	 */
	static void KeyValue(const char *key, const char *data);

	/**
	 * Called immediately after the config file is done being parsed.  Register our natives here.
	 *
	 * @noreturn
	 */
	static void ConfigDone(void);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterNative(AMX *amx, cell *params);

	/**
	 * A plugin is registering this entry's virtual hook.  This is a normal native callback.
	 * 
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell RegisterIDNative(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell NativeCall(AMX *amx, cell *params);

	/**
	 * A plugin is requesting a direct call of this entry's virtual function, and will be exposed to all hooks.  This is a normal native callback.
	 *
	 * @param amx				The AMX structure for the plugin.
	 * @param params			The parameters passed from the plugin.
	 * @return					1 on success, 0 on failure.  It only fails if the callback function is not found.
	 */
	static cell ENativeCall(AMX *amx, cell *params);

	/**
	 * Hook this entry's function!  This creates our trampoline and modifies the virtual table.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param outtrampoline		The trampoline that was created.
	 * @param origfunc			The original function that was hooked.
	 * @noreturn
	 */
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);

	/**
	 * Checks if the virtual function is already being hooked or not.  If it's not, it begins hooking it.  Either way it registers a forward and adds it to our vector.
	 *
	 * @param manager			The VTableManager this is a child of.
	 * @param vtable			The virtual table we're molesting.
	 * @param plugin			The plugin that's requesting this.
	 * @param funcid			The function id of the callback.
	 * @noreturn
	 */
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid, int post);	

	/**
	 * Execute the command.  This is called directly from our global hook function.
	 *
	 * @param pthis				The "this" pointer, cast to a void.  The victim.
	 * @param other				Entity that's blocking.
	 * @noreturn
	 */
	void Execute(void *pthis);

	/**
	 * The hook that is directly called by the trampoline.
	 *
	 * @param id				The index of the hook to call.
	 * @param pthis				The "this" pointer. 
	 */
	static void HAM_CDECL EntryPoint(int id,void *pthis);
};



//TraceAttack( entvars_t *pevAttacker, float flDamage, Vector vecDir, TraceResult *ptr, int bitsDamageType);
/*class VTableTraceAttack : public VTableEntryBase
{
public:
	static void CreateHook(VTableManager *manager, void **vtable, int id, void **outtrampoline, void **origfunc);
	static void Hook(VTableManager *manager, void **vtable, AMX *plugin, int funcid);	
	void Execute(void *pthis, entvars_t *attacker, float damage, float *direction, TraceResult *tr, int damagetype);
};
*/
#endif // VTABLEENTRIES_H
