#if defined _hamsandwich_included
  #endinput
#endif
#define _hamsandwich_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib hamsandwich
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib hamsandwich
 #endif
#else
 #pragma library hamsandwich
#endif

/**
 * Ham Sandwich general usage
 * -
 * Mods supported:
 *   cs:
 *      * 1.6 - legimate, most up to date version
 *         * linux 32bit and windows
 *      * czero - legimate, most up to date version
 *         * linux 32bit and windows
 *         * czero untested, theoretically it should work
 *   dod:
 *      * whatever the most up to date legitimate version is (1.3?)
 *         * linux and windows
 *         * windows untested, should work
 *   tfc:
 *      * whatever most up to date legitimate version is
 *         * linux and windows
 *         * windows untested, should work
 *   ns:
 *      * 3.2 beta 2
 *         * linux and windows
 *         * windows untested, should work
 *   ts:
 *      * 2.1
 *         * linux and windows
 *         * windows untested, should work
 *   sven:
 *     * 3.0
 *         * windows
 *         * windows untested, should work
 * -
 * The module will automatically use the relevant function depending
 * on what game mod is loaded.  You only have to tell it to do the damage.
 * -
 * Be VERY VERY careful.  I check _NO_ bounds in any of the natives
 * Also, these call the game's private function, and I do not have
 * the time nor desire to figure out the quirks of each mod's function
 * -
 * The USE natives haven't been tested yet, but should work!
 * -
 * The only mod I tested the windows version of takedamage
 * for was cs 1.6.  It's much easier for me to test linux.
 */

/** 
 * Calls the mod's private damage routine.
 * -
 * parameters:
 *        id: index of the entity that is to be damaged
 *
 * inflictor: the entity that is "doing the damage" (eg a weapon)
 *
 *  attacker: the entity who controls the inflictor (eg weapon holder)
 *
 *    damage: how much damage to do to the victim
 *
 *      type: bitmask of damage flags.  Most mods do not
 *            follow the standard HLSDK damage flags, so
 *            you may have to do some testing!
 *            The only flags I figured out were CS related:
 *               * (1<<6)  = no slowdown on damage
 *               * (1<<13) = always gib
 *               * (1<<12) = never gib
 *
 * -
 * Note: Setting damage to a ridiculously high value can mess up
 *       some mods!
 */
native hs_takedamage(id,inflictor,attacker,Float:damage,type);
native hs_etakedamage(id,inflictor,attacker,Float:damage,type);


/** 
 * Calls the mod's private use routine.
 * -
 * parameters:
 *        id: index of the entity that is to be used
 *
 * activator: the entity that is "doing the use" (eg a trigger_multiple)
 *
 *    caller: the entity who controls the activator (eg the player who trigger the trigger_multiple)
 *
 *  use_type: how to use the entity
 *            TYPICAL settings in HLSDK are:
 *            0 = USE_OFF    - turns the object "off"
 *            1 = USE_ON     - turns the object "on"
 *            2 = USE_SET    - sets the object use state (uses the value flag)
 *            3 = USE_TOGGLE - toggles the state (from on->off and off->on)
 *
 * use_value: set when USE_SET is invoked
 *            TYPICALLY USE_SET is only used for players using an object
 *            when he begins using it, the use_value is set to 1.0
 *            when he's done using it, the use_value is set to 0.0
 *
 * -
 * NOTE: This native was not tested at all beyond theory tests!
 */
native hs_use(id,activator,caller,use_type,Float:use_value);
native hs_euse(id,activator,caller,use_type,Float:use_value);



enum 
{
	HAM_UNSET = 0,
	HAM_IGNORED,
	HAM_HANDLED,
	HAM_OVERRIDE,
	HAM_SUPERCEDE
};

/**
 * Forwards all takedamage routines that would occur to the given
 * classname to the public provided.
 *
 * Parameters are:
 * function(IDVictim, IDInflictor, IDAttacker, Float:Damage, DamageType)
 *
 * Note: For now, this will also intercept calls to hs_takedamage
 *       that is very likely to change shortly in the future.
 */
native register_takedamage(const classname[], const function[]);

/**
 * Forwards all use routines that would occur to the given
 * classname to the public provided.
 *
 * Parameters are:
 * function(IDUsed, IDActivator, IDCaller, UseType, Float:UseValue)
 *
 * Note: For now, this will also intercept calls to hs_use
 *       that is very likely to change shortly in the future.
 */
native register_use(const classname[], const function[]);