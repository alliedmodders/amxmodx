/*
*	TSExtended Function Natives
*	(c) 2017, SNMetamorph
*	This file provided as is (without any warranties).
*/

// Require module loading
#if AMXX_VERSION_NUM >= 175
 #pragma reqlib tse
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib tse
 #endif
#else
 #pragma library tse
#endif

// Items
enum {
	TSE_ITEM_NONE = 0,
	TSE_ITEM_KUNGFU,
	TSE_ITEM_SUPERJMP
};

// Attachments
enum {
	TSE_ATM_NONE = 0,
	TSE_ATM_SILENCER,
	TSE_ATM_LASERSIGHT,
	TSE_ATM_FLASHLIGHT = 4,
	TSE_ATM_SCOPE = 8
};

// Stunts
enum {
	TSE_STN_NONE = 0,
	TSE_STN_DUCK,
	TSE_STN_ROLL,
	TSE_STN_DIVE,
	TSE_STN_GETUP,
	TSE_STN_FLIP
};

// Fire modes
enum {
	TSE_FM_FULLAUTO = 0,
	TSE_FM_SEMIAUTO,
	TSE_FM_BURST,
	TSE_FM_PUMP,
	TSE_FM_FREESEMI,
	TSE_FM_FREEFULL
};

// Powerups
enum {
	TSE_PWUP_NONE = 0,
	TSE_PWUP_RANDOM = 0,
	TSE_PWUP_SLOWMO = 1,
	TSE_PWUP_INFAMMO = 2,
	TSE_PWUP_SLOWPAUSE = 8,
	TSE_PWUP_DFIRERATE = 16,
	TSE_PWUP_GRENADE = 32,
	TSE_PWUP_HEALTH = 64,
	TSE_PWUP_ARMOR = 128
};

// Statuses
enum {
	TSE_STAT_NONE = 0,
	TSE_STAT_DEAD,
	TSE_STAT_3KILLS,
	TSE_STAT_6KILLS,
	TSE_STAT_SPECIALIST,
	TSE_STAT_UNSTOPABLE,
	TSE_STAT_THEONE = 10,
	TSE_STAT_WAITING
};

// Weapons
enum {
	TSE_WPN_GLOCK18 = 1,
	TSE_WPN_92F,
	TSE_WPN_UZI,
	TSE_WPN_M3,
	TSE_WPN_M4A1,
	TSE_WPN_MP5SD,
	TSE_WPN_MP5K,
	TSE_WPN_ABERETTAS,
	TSE_WPN_MK23,
	TSE_WPN_AMK23,
	TSE_WPN_USAS,
	TSE_WPN_DEAGLE,
	TSE_WPN_AK47,
	TSE_WPN_57,
	TSE_WPN_AUG,
	TSE_WPN_AUZI,
	TSE_WPN_SKORP,
	TSE_WPN_M82A1,
	TSE_WPN_MP7,
	TSE_WPN_SPAS,
	TSE_WPN_GCOLTS,
	TSE_WPN_GLOCK20,
	TSE_WPN_UMP,
	TSE_WPN_M61GRENADE,
	TSE_WPN_CKNIFE,
	TSE_WPN_MOSSBERG,
	TSE_WPN_M16A4,
	TSE_WPN_MK1,
	TSE_WPN_C4,
	TSE_WPN_A57,
	TSE_WPN_RBULL,
	TSE_WPN_M60E3,
	TSE_WPN_SAWED_OFF,
	TSE_WPN_KATANA,
	TSE_WPN_SKNIFE,
	TSE_WPN_CONTENDER,
	TSE_WPN_ASKORP
};

// Forwards

/* Called when a player was stunted */
forward client_onstunt(idx, stunttype);

/* Called when a player makes melee hit */
forward client_onmeleehit(idx, Float:damage, Float:duration);

/* Called when a player picks up powerup */
forward client_onpickuppwup(idx, type, duration);

// Returns value of free player slots (weight), on error returns -1
native tse_getuserslots(idx);

// Sets value of free player slots (weight), on error returns 0
native tse_setuserslots(idx, val);

// Returns value of player cash, on error returns -1
native tse_getusercash(idx);

// Sets value of player cash, on error returns 0
native tse_setusercash(idx, val);

// Gets if player has a kevlar (returns 1), or not (returns 0), on error returns -1
native tse_getuserkevlar(idx);

// Returns user stamina count, on error returns -1
native tse_getuserstamina(idx);

// Sets user stamina count, on error returns 0
native tse_setuserstamina(idx, Float:val);

// Returns float value of effects physics speed (speed of sparks, bullets, surrounding players sounds, broken glass pieces and other particles), on error returns -1
native tse_geteffectsphysspd(idx);

// Sets float value of effects physics speed, on error returns 0
native tse_seteffectsphysspd(idx, Float:val);

// Returns float value of player physics speed (speed of animation, movement, surrounding particles sounds, stunting, throwed knives), on error returns -1
native tse_getuserphysspd(idx);

// Sets float value of player physics speed, on error returns 0
native tse_setuserphysspd(idx, Float:val);

/* 
*  Sets weapon shooting params for specified weapon of specified player, on error returns 0 
*  weapid - weapon ID (TSE_WPN_*)
*  Float:acc - float value of accuracy
*  Float:vkick - float value of vertical kick
*  Float:acckick - float value of accuracy kick
*  Float:dmg - float value of damage
*  cost - cost of weapon in shop
*/
native tse_sendweapparams(idx, weapid, Float:acc, Float:vkick, Float:acckick, Float:dmg, cost);

// Returns player current items (TSE_ITEM_*), on error returns -1
// WARNING: If you want check presence of who items, use them sum
native tse_getusercurritems(idx);

// Sets player current items (TSE_ITEM_*), on error returns 0
native tse_setusercurritems(idx, val);

// Returns player current weapon ID (TSE_WPN_*), on error returns -1
native tse_getusercurrweap(idx);

// Returns player weapon entity (weapon_tsgun), if it not found returns 0
native tse_getuserweapent(idx);

// Sets player's weapon fire mode (TSE_FM_*), on error returns 0
native tse_setweapfiremode(idx, weap, mode);

// Gets player's weapon fire mode (TSE_FM_*), on error returns -1
native tse_getweapfiremode(idx, weap);

/* 
* Sets player weapon attachments (TSE_ATM_*), on error returns 0
* To sets multiple attacments, use their sum
* For example: tse_setweapatcments(pl, TSE_WPN_M4A1, TSE_ATM_SILENCER + TSE_ATM_LASERSIGHT + TSE_ATM_SCOPE)
*/
native tse_setweapatcments(idx, weap, attachments, isactive=1);

// Sets player weapon count of bullets in clip (for identify weapon use TSE_WPN_*), on error returns 0
native tse_setweapclip(idx, weap, clip);

// Sets player weapon count of bullets totally (for identify weapon use TSE_WPN_*), on error returns 0
native tse_setweapammo(idx, weap, ammo);

// Sets player weapon readiness status (works with pump-action weapons), on error returns 0
native tse_setweapready(idx, weap, isready);

// Checks if player has the weapon with the given ID, if it returns 1, otherwise 0
native tse_isuserhasweap(idx, weap);

// Creates powerup in specified position, with specified type and time of life (the time through which created powerup will disappear completely)
native tse_createpwup(type, Float:coord[3], ttl=120);

// Gives powerup to user with specified type and duration (in seconds), on error returns 0
native tse_giveuserpwup(idx, type, duration);

// Creates weapon in specified position, and with specified attachments (TSE_ATM_*), time of life, count of bullets in clip, on error returns 0
native tse_createweap(weap, Float:coord[3], clip, atcments, ttl=120);

// Gives weapon to user with specified count of bullets in clip and attachments (TSE_ATM_*), on error returns 0
native tse_giveuserweap(idx, weap, clip, atcments=0);

// Returns user current powerup (TSE_PWUP_*), on error returns 0
native tse_getuserpwup(idx);

// Returns user current state (TSE_STN_*), on error returns 0
native tse_getuserstate(idx);

// Returns user current status (text near nick in scoreboard) (TSE_STAT_*), on error returns -1
native tse_getuserstatus(idx);

// Sets user current status (TSE_STAT_*), on error returns 0
native tse_setuserstatus(idx, status);

// Returns user current active powerup (TSE_PWUP_*), on error returns -1
native tse_getuseractivepwup(idx);

// Sets user powerup duration (in seconds), on error returns 0
native tse_setuserpwupduration(idx, duration);

// Sets user fake powerup effect (as though user had a speficied powerup and user activated it), on error returns 0
native tse_setfakepwup(idx, pwupid, Float:duration);

/* Configures fake powerup options (player and effects physical speed, aura distance)
*  WARNING: THIS FUNCTION WORKS ONLY WITH FUNCTION tse_setfakepwup AND POWERUP TYPE TSE_PWUP_SLOWMO OR TS_PWUP_SLOWPAUSE ONLY!
*  Using example:   
* 	   tse_setfakepwup(pl, TSE_PWUP_SLOWMO, 10.0);
*      tse_configslowing(pl, 0.25, 1.50);
*/
native tse_configslowing(idx, Float:speed, Float:auradist);

/* Configures player melee hit damage and duration
*  WARNING: WORKS ONLY WITH FORWARD client_onmeleehit
*  Using example: 
*		public client_onmeleehit(pl, Float:dmg, Float:time)
*		{
*			tse_configmeleeatk(pl, 1000.0, time);
*		}
*/
native tse_configmeleeatk(idx, Float:dmg, Float:time);
