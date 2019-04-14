// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Half-Life SDK Constants
//

#if defined _hlsdk_const_included
    #endinput
#endif
#define _hlsdk_const_included

/**
 * pev(entity, pev_button) or pev(entity, pev_oldbuttons) values
 */
#define IN_ATTACK                       (1<<0)
#define IN_JUMP                         (1<<1)
#define IN_DUCK                         (1<<2)
#define IN_FORWARD                      (1<<3)
#define IN_BACK                         (1<<4)
#define IN_USE                          (1<<5)
#define IN_CANCEL                       (1<<6)
#define IN_LEFT                         (1<<7)
#define IN_RIGHT                        (1<<8)
#define IN_MOVELEFT                     (1<<9)
#define IN_MOVERIGHT                    (1<<10)
#define IN_ATTACK2                      (1<<11)
#define IN_RUN                          (1<<12)
#define IN_RELOAD                       (1<<13)
#define IN_ALT1                         (1<<14)
#define IN_SCORE                        (1<<15)     // Used by client.dll for when scoreboard is held down

/**
 * pev(entity, pev_flags) values
 */
#define FL_FLY                          (1<<0)      // Changes the SV_Movestep() behavior to not need to be on ground
#define FL_SWIM                         (1<<1)      // Changes the SV_Movestep() behavior to not need to be on ground (but stay in water)
#define FL_CONVEYOR                     (1<<2)
#define FL_CLIENT                       (1<<3)
#define FL_INWATER                      (1<<4)
#define FL_MONSTER                      (1<<5)
#define FL_GODMODE                      (1<<6)
#define FL_NOTARGET                     (1<<7)
#define FL_SKIPLOCALHOST                (1<<8)      // Don't send entity to local host, it's predicting this entity itself
#define FL_ONGROUND                     (1<<9)      // At rest / on the ground
#define FL_PARTIALGROUND                (1<<10)     // Not all corners are valid
#define FL_WATERJUMP                    (1<<11)     // Player jumping out of water
#define FL_FROZEN                       (1<<12)     // Player is frozen for 3rd person camera
#define FL_FAKECLIENT                   (1<<13)     // JAC: fake client, simulated server side; don't send network messages to them
#define FL_DUCKING                      (1<<14)     // Player flag -- Player is fully crouched
#define FL_FLOAT                        (1<<15)     // Apply floating force to this entity when in water
#define FL_GRAPHED                      (1<<16)     // Worldgraph has this ent listed as something that blocks a connection
#define FL_IMMUNE_WATER                 (1<<17)
#define FL_IMMUNE_SLIME                 (1<<18)
#define FL_IMMUNE_LAVA                  (1<<19)
#define FL_PROXY                        (1<<20)     // This is a spectator proxy
#define FL_ALWAYSTHINK                  (1<<21)     // Brush model flag -- call think every frame regardless of nextthink - ltime (for constantly changing velocity/path)
#define FL_BASEVELOCITY                 (1<<22)     // Base velocity has been applied this frame (used to convert base velocity into momentum)
#define FL_MONSTERCLIP                  (1<<23)     // Only collide in with monsters who have FL_MONSTERCLIP set
#define FL_ONTRAIN                      (1<<24)     // Player is _controlling_ a train, so movement commands should be ignored on client during prediction.
#define FL_WORLDBRUSH                   (1<<25)     // Not moveable/removeable brush entity (really part of the world, but represented as an entity for transparency or something)
#define FL_SPECTATOR                    (1<<26)     // This client is a spectator, don't run touch functions, etc.
#define FL_CUSTOMENTITY                 (1<<29)     // This is a custom entity
#define FL_KILLME                       (1<<30)     // This entity is marked for death -- This allows the engine to kill ents at the appropriate time
#define FL_DORMANT                      (1<<31)     // Entity is dormant, no updates to client

/**
 * engfunc(EngFunc_WalkMove, entity, Float:yaw, Float:dist, iMode) iMode values
 */
#define WALKMOVE_NORMAL                 0           // Normal walkmove
#define WALKMOVE_WORLDONLY              1           // Doesn't hit ANY entities, no matter what the solid type
#define WALKMOVE_CHECKONLY              2           // Move, but don't touch triggers

/**
 * engfunc(EngFunc_MoveToOrigin, entity, Float:goal[3], Float:distance, moveType) moveType values
 */
#define MOVE_NORMAL                     0           // normal move in the direction monster is facing
#define MOVE_STRAFE                     1           // moves in direction specified, no matter which way monster is facing

/**
 * pev(entity, pev_movetype) values
 */
#define MOVETYPE_NONE                   0           // Never moves
#define MOVETYPE_WALK                   3           // Player only - moving on the ground
#define MOVETYPE_STEP                   4           // Gravity, special edge handling -- monsters use this
#define MOVETYPE_FLY                    5           // No gravity, but still collides with stuff
#define MOVETYPE_TOSS                   6           // Gravity/Collisions
#define MOVETYPE_PUSH                   7           // No clip to world, push and crush
#define MOVETYPE_NOCLIP                 8           // No gravity, no collisions, still do velocity/avelocity
#define MOVETYPE_FLYMISSILE             9           // Extra size to monsters
#define MOVETYPE_BOUNCE                 10          // Just like Toss, but reflect velocity when contacting surfaces
#define MOVETYPE_BOUNCEMISSILE          11          // Bounce w/o gravity
#define MOVETYPE_FOLLOW                 12          // Track movement of aiment
#define MOVETYPE_PUSHSTEP               13          // BSP model that needs physics/world collisions (uses nearest hull for world collision)

/**
 * pev(entity, pev_solid) values
 *
 * @note Some movetypes will cause collisions independent of SOLID_NOT and
 *       SOLID_TRIGGER when the entity moves. SOLID only effects OTHER entities
 *       colliding with this one when they move - UGH!
 */
#define SOLID_NOT                       0           // No interaction with other objects
#define SOLID_TRIGGER                   1           // Touch on edge, but not blocking
#define SOLID_BBOX                      2           // Touch on edge, block
#define SOLID_SLIDEBOX                  3           // Touch on edge, but not an onground
#define SOLID_BSP                       4           // BSP clip, touch on edge, block

/**
 * pev(entity, pev_deadflag) values
 */
#define DEAD_NO                         0           // Alive
#define DEAD_DYING                      1           // Playing death animation or still falling off of a ledge waiting to hit ground
#define DEAD_DEAD                       2           // Dead, lying still
#define DEAD_RESPAWNABLE                3
#define DEAD_DISCARDBODY                4

/**
 * new Float:takedamage, pev(entity, pev_takedamage, takedamage) values
 */
#define DAMAGE_NO                       0.0
#define DAMAGE_YES                      1.0
#define DAMAGE_AIM                      2.0

/**
 * pev(entity, pev_effects) values
 */
#define EF_BRIGHTFIELD                  1           // Swirling cloud of particles
#define EF_MUZZLEFLASH                  2           // Single frame ELIGHT on entity attachment 0
#define EF_BRIGHTLIGHT                  4           // DLIGHT centered at entity origin
#define EF_DIMLIGHT                     8           // Player flashlight
#define EF_INVLIGHT                     16          // Get lighting from ceiling
#define EF_NOINTERP                     32          // Don't interpolate the next frame
#define EF_LIGHT                        64          // Rocket flare glow sprite
#define EF_NODRAW                       128         // Don't draw entity

/**
 * Spectating camera mode constants
 *
 * @note These constants are linked to different camera modes available when you
 *       are spectating (either dead or when in spectator team). Usually this is
 *       stored in the pev_iuser1 field in Counter-Strike and Half-Life games.
 */
#define OBS_NONE                        0
#define OBS_CHASE_LOCKED                1           // Locked Chase Cam
#define OBS_CHASE_FREE                  2           // Free Chase Cam
#define OBS_ROAMING                     3           // Free Look
#define OBS_IN_EYE                      4           // First Person
#define OBS_MAP_FREE                    5           // Free Overview
#define OBS_MAP_CHASE                   6           // Chase Overview

/**
 * engfunc(EngFunc_PointContents, Float:origin) return values
 */
#define CONTENTS_EMPTY                  -1
#define CONTENTS_SOLID                  -2
#define CONTENTS_WATER                  -3
#define CONTENTS_SLIME                  -4
#define CONTENTS_LAVA                   -5
#define CONTENTS_SKY                    -6
#define CONTENTS_ORIGIN                 -7          // Removed at csg time
#define CONTENTS_CLIP                   -8          // Changed to contents_solid
#define CONTENTS_CURRENT_0              -9
#define CONTENTS_CURRENT_90             -10
#define CONTENTS_CURRENT_180            -11
#define CONTENTS_CURRENT_270            -12
#define CONTENTS_CURRENT_UP             -13
#define CONTENTS_CURRENT_DOWN           -14
#define CONTENTS_TRANSLUCENT            -15
#define CONTENTS_LADDER                 -16
#define CONTENT_FLYFIELD                -17
#define CONTENT_GRAVITY_FLYFIELD        -18
#define CONTENT_FOG                     -19

/**
 * Instant damage values for use with the 3rd parameter of the "Damage" client
 * message.
 */
#define DMG_GENERIC                     0           // Generic damage was done
#define DMG_CRUSH                       (1<<0)      // Crushed by falling or moving object
#define DMG_BULLET                      (1<<1)      // Shot
#define DMG_SLASH                       (1<<2)      // Cut, clawed, stabbed
#define DMG_BURN                        (1<<3)      // Heat burned
#define DMG_FREEZE                      (1<<4)      // Frozen
#define DMG_FALL                        (1<<5)      // Fell too far
#define DMG_BLAST                       (1<<6)      // Explosive blast damage
#define DMG_CLUB                        (1<<7)      // Crowbar, punch, headbutt
#define DMG_SHOCK                       (1<<8)      // Electric shock
#define DMG_SONIC                       (1<<9)      // Sound pulse shockwave
#define DMG_ENERGYBEAM                  (1<<10)     // Laser or other high energy beam
#define DMG_NEVERGIB                    (1<<12)     // With this bit OR'd in, no damage type will be able to gib victims upon death
#define DMG_ALWAYSGIB                   (1<<13)     // With this bit OR'd in, any damage type can be made to gib victims upon death.
#define DMG_DROWN                       (1<<14)     // Drowning
#define DMG_PARALYZE                    (1<<15)     // Slows affected creature down
#define DMG_NERVEGAS                    (1<<16)     // Nerve toxins, very bad
#define DMG_POISON                      (1<<17)     // Blood poisioning
#define DMG_RADIATION                   (1<<18)     // Radiation exposure
#define DMG_DROWNRECOVER                (1<<19)     // Drowning recovery
#define DMG_ACID                        (1<<20)     // Toxic chemicals or acid burns
#define DMG_SLOWBURN                    (1<<21)     // In an oven
#define DMG_SLOWFREEZE                  (1<<22)     // In a subzero freezer
#define DMG_MORTAR                      (1<<23)     // Hit by air raid (done to distinguish grenade from mortar)
#define DMG_GRENADE                     (1<<24)     // Counter-Strike only - Hit by HE grenade
#define DMG_TIMEBASED                   (~(0x3fff)) // Mask for time-based damage

/**
* Gib values used on client kill based on instant damage values
*/
#define GIB_NORMAL                      0 // Gib if entity was overkilled
#define GIB_NEVER                       1 // Never gib, no matter how much death damage is done ( freezing, etc )
#define GIB_ALWAYS                      2 // Always gib ( Houndeye Shock, Barnacle Bite )
#define GIB_TRY_HEALTH                	-9000 // Gib players if their health is under this value. (GIB_NEVER overrides this value)

/**
 * Valid constants for fNoMonsters parameter of EngFunc_TraceLine,
 * EngFunc_TraceMonsterHull, EngFunc_TraceHull and EngFunc_TraceSphere.
 */
#define DONT_IGNORE_MONSTERS            0
#define IGNORE_MONSTERS                 1
#define IGNORE_MISSILE                  2
#define IGNORE_GLASS                    0x100

/**
 * The hullnumber paramater of EngFunc_TraceHull, EngFunc_TraceModel and
 * DLLFunc_GetHullBounds
 */
#define HULL_POINT                      0
#define HULL_HUMAN                      1
#define HULL_LARGE                      2
#define HULL_HEAD                       3

/**
 * global_get(glb_trace_flags)
 */
#define FTRACE_SIMPLEBOX                (1<<0)      // Traceline with a simple box

/**
 * Used with get/set_es(es_handle, ES_eFlags, ...) (entity_state data structure)
 */
#define EFLAG_SLERP                     1           // Do studio interpolation of this entity

/**
 * @section pev(entity, pev_spawnflags) values
 */

/**
 * func_train
 */
#define SF_TRAIN_WAIT_RETRIGGER         1
#define SF_TRAIN_START_ON               4           // Train is initially moving
#define SF_TRAIN_PASSABLE               8           // Train is not solid -- used to make water trains

/**
 * func_wall_toggle
 */
#define SF_WALL_START_OFF               0x0001
#define SF_WALL_NOTSOLID                0x0008

/**
 * func_converyor
 */
#define SF_CONVEYOR_VISUAL              0x0001
#define SF_CONVEYOR_NOTSOLID            0x0002

/**
 * func_button
 */
#define SF_BUTTON_DONTMOVE              1
#define SF_BUTTON_TOGGLE                32          // Button stays pushed until reactivated
#define SF_BUTTON_SPARK_IF_OFF          64          // Button sparks in OFF state
#define SF_BUTTON_TOUCH_ONLY            256         // Button only fires as a result of USE key.

/**
 * func_rot_button
 */
#define SF_ROTBUTTON_NOTSOLID           1
#define SF_ROTBUTTON_BACKWARDS          2

/**
 * env_global
 */
#define SF_GLOBAL_SET                   1           // Set global state to initial state on spawn

/**
 * multisource
 */
#define SF_MULTI_INIT                   1

/**
 * momentary_rot_button
 */
#define SF_MOMENTARY_DOOR               0x0001

/**
 * button_target
 */
#define SF_BTARGET_USE                  0x0001
#define SF_BTARGET_ON                   0x0002

/**
 * func_door, func_water, func_door_rotating, momementary_door
 */
#define SF_DOOR_ROTATE_Y                0
#define SF_DOOR_START_OPEN              1
#define SF_DOOR_ROTATE_BACKWARDS        2
#define SF_DOOR_PASSABLE                8
#define SF_DOOR_ONEWAY                  16
#define SF_DOOR_NO_AUTO_RETURN          32
#define SF_DOOR_ROTATE_Z                64
#define SF_DOOR_ROTATE_X                128
#define SF_DOOR_USE_ONLY                256         // Door must be opened by player's use button
#define SF_DOOR_NOMONSTERS              512         // Monster can't open
#define SF_DOOR_TOUCH_ONLY_CLIENTS      1024       	// Only clients can touch
#define SF_DOOR_SILENT                  0x80000000  // This bit marks that func_door are actually func_water

/**
 * gibshooter
 */
#define SF_GIBSHOOTER_REPEATABLE        1           // Allows a gibshooter to be refired

/**
 * env_funnel
 */
#define SF_FUNNEL_REVERSE               1           // Funnel effect repels particles instead of attracting them

/**
 * env_bubbles
 */
#define SF_BUBBLES_STARTOFF             0x0001

/**
 * env_blood
 */
#define SF_BLOOD_RANDOM                 0x0001
#define SF_BLOOD_STREAM                 0x0002
#define SF_BLOOD_PLAYER                 0x0004
#define SF_BLOOD_DECAL                  0x0008

/**
 * env_shake
 */
#define SF_SHAKE_EVERYONE               0x0001      // Don't check radius
#define SF_SHAKE_DISRUPT                0x0002      // Disrupt controls
#define SF_SHAKE_INAIR                  0x0004      // Shake players in air

/**
 * env_fade
 */
#define SF_FADE_IN                      0x0001      // Fade in, not out
#define SF_FADE_MODULATE                0x0002      // Modulate, don't blend
#define SF_FADE_ONLYONE                 0x0004

/**
 * env_beam, env_lightning
 */
#define SF_BEAM_STARTON                 0x0001
#define SF_BEAM_TOGGLE                  0x0002
#define SF_BEAM_RANDOM                  0x0004
#define SF_BEAM_RING                    0x0008
#define SF_BEAM_SPARKSTART              0x0010
#define SF_BEAM_SPARKEND                0x0020
#define SF_BEAM_DECALS                  0x0040
#define SF_BEAM_SHADEIN                 0x0080
#define SF_BEAM_SHADEOUT                0x0100
#define SF_BEAM_TEMPORARY               0x8000

/**
 * env_sprite
 */
#define SF_SPRITE_STARTON               0x0001
#define SF_SPRITE_ONCE                  0x0002
#define SF_SPRITE_TEMPORARY             0x8000

/**
 * env_message
 */
#define SF_MESSAGE_ONCE                 0x0001      // Fade in, not out
#define SF_MESSAGE_ALL                  0x0002      // Send to all clients

/**
 * env_explosion
 */
#define SF_ENVEXPLOSION_NODAMAGE        (1<<0)      // When set, ENV_EXPLOSION will not actually inflict damage
#define SF_ENVEXPLOSION_REPEATABLE      (1<<1)      // Can this entity be refired?
#define SF_ENVEXPLOSION_NOFIREBALL      (1<<2)      // Don't draw the fireball
#define SF_ENVEXPLOSION_NOSMOKE         (1<<3)      // Don't draw the smoke
#define SF_ENVEXPLOSION_NODECAL         (1<<4)      // Don't make a scorch mark
#define SF_ENVEXPLOSION_NOSPARKS        (1<<5)      // Don't make a scorch mark

/**
 * func_tank
 */
#define SF_TANK_ACTIVE                  0x0001
#define SF_TANK_PLAYER                  0x0002
#define SF_TANK_HUMANS                  0x0004
#define SF_TANK_ALIENS                  0x0008
#define SF_TANK_LINEOFSIGHT             0x0010
#define SF_TANK_CANCONTROL              0x0020
#define SF_TANK_SOUNDON                 0x8000

/**
 * grenade
 */
#define SF_DETONATE                     0x0001

/**
 * item_suit
 */
#define SF_SUIT_SHORTLOGON              0x0001

/**
 * game_score
 */
#define SF_SCORE_NEGATIVE               0x0001
#define SF_SCORE_TEAM                   0x0002

/**
 * game_text
 */
#define SF_ENVTEXT_ALLPLAYERS           0x0001

/**
 * game_team_master
 */
#define SF_TEAMMASTER_FIREONCE          0x0001
#define SF_TEAMMASTER_ANYTEAM           0x0002

/**
 * game_team_set
 */
#define SF_TEAMSET_FIREONCE             0x0001
#define SF_TEAMSET_CLEARTEAM            0x0002

/**
 * game_player_hurt
 */
#define SF_PKILL_FIREONCE               0x0001

/**
 * game_counter
 */
#define SF_GAMECOUNT_FIREONCE           0x0001
#define SF_GAMECOUNT_RESET              0x0002

/**
 * game_player_equip
 */
#define SF_PLAYEREQUIP_USEONLY          0x0001

/**
 * game_player_team
 */
#define SF_PTEAM_FIREONCE               0x0001
#define SF_PTEAM_KILL                   0x0002
#define SF_PTEAM_GIB                    0x0004

/**
 * func_trackchange
 */
#define SF_PLAT_TOGGLE                  0x0001
#define SF_TRACK_ACTIVATETRAIN          0x00000001
#define SF_TRACK_RELINK                 0x00000002
#define SF_TRACK_ROTMOVE                0x00000004
#define SF_TRACK_STARTBOTTOM            0x00000008
#define SF_TRACK_DONT_MOVE              0x00000010

/**
 * func_tracktrain
 */
#define SF_TRACKTRAIN_NOPITCH           0x0001
#define SF_TRACKTRAIN_NOCONTROL         0x0002
#define SF_TRACKTRAIN_FORWARDONLY       0x0004
#define SF_TRACKTRAIN_PASSABLE          0x0008
#define SF_PATH_DISABLED                0x00000001
#define SF_PATH_FIREONCE                0x00000002
#define SF_PATH_ALTREVERSE              0x00000004
#define SF_PATH_DISABLE_TRAIN           0x00000008
#define SF_PATH_ALTERNATE               0x00008000
#define SF_CORNER_WAITFORTRIG           0x001
#define SF_CORNER_TELEPORT              0x002
#define SF_CORNER_FIREONCE              0x004

/**
* func_plat
*/
#define SF_PLAT_TOGGLE                  0x0001

/**
* path_track
*/
#define SF_PATH_DISABLED                0x00000001
#define SF_PATH_FIREONCE                0x00000002
#define SF_PATH_ALTREVERSE              0x00000004
#define SF_PATH_DISABLE_TRAIN           0x00000008
#define SF_PATH_ALTERNATE               0x00008000

/**
* path_corner
*/
#define SF_CORNER_WAITFORTRIG           0x001
#define SF_CORNER_TELEPORT              0x002
#define SF_CORNER_FIREONCE              0x004

/**
 * trigger_push
 */
#define SF_TRIGGER_PUSH_START_OFF       2           // Spawnflag that makes trigger_push spawn turned OFF

/**
 * trigger_hurt
 */
#define SF_TRIGGER_HURT_TARGETONCE      1           // Only fire hurt target once
#define SF_TRIGGER_HURT_START_OFF       2           // Spawnflag that makes trigger_push spawn turned OFF
#define SF_TRIGGER_HURT_NO_CLIENTS      8           // Spawnflag that makes trigger_push spawn turned OFF
#define SF_TRIGGER_HURT_CLIENTONLYFIRE  16          // Trigger hurt will only fire its target if it is hurting a client
#define SF_TRIGGER_HURT_CLIENTONLYTOUCH 32          // Only clients may touch this trigger

/**
 * trigger_auto
 */
#define SF_AUTO_FIREONCE                0x0001
#define SF_AUTO_NORESET                 0x0002

/**
 * trigger_relay
 */
#define SF_RELAY_FIREONCE               0x0001

/**
 * multi_manager
 */
#define SF_MULTIMAN_CLONE               0x80000000
#define SF_MULTIMAN_THREAD              0x00000001

/**
 * env_render
 * @note These are flags to indicate masking off various render parameters that
 *       are usually copied to the targets
 */
#define SF_RENDER_MASKFX                (1<<0)
#define SF_RENDER_MASKAMT               (1<<1)
#define SF_RENDER_MASKMODE              (1<<2)
#define SF_RENDER_MASKCOLOR             (1<<3)

/**
 * trigger_changelevel
 */
#define SF_CHANGELEVEL_USEONLY          0x0002

/**
 * trigger_endsection
 */
#define SF_ENDSECTION_USEONLY           0x0001

/**
 * trigger_camera
 */
#define SF_CAMERA_PLAYER_POSITION       1
#define SF_CAMERA_PLAYER_TARGET         2
#define SF_CAMERA_PLAYER_TAKECONTROL    4

/**
 * func_rotating
 */
#define SF_BRUSH_ROTATE_Y_AXIS          0
#define SF_BRUSH_ROTATE_INSTANT         1
#define SF_BRUSH_ROTATE_BACKWARDS       2
#define SF_BRUSH_ROTATE_Z_AXIS          4
#define SF_BRUSH_ROTATE_X_AXIS          8
#define SF_PENDULUM_AUTO_RETURN         16
#define SF_PENDULUM_PASSABLE            32
#define SF_BRUSH_ROTATE_SMALLRADIUS     128
#define SF_BRUSH_ROTATE_MEDIUMRADIUS    256
#define SF_BRUSH_ROTATE_LARGERADIUS     512

/**
 * triggers
 */
#define SF_TRIGGER_ALLOWMONSTERS        1           // Monsters allowed to fire this trigger
#define SF_TRIGGER_NOCLIENTS            2           // Players not allowed to fire this trigger
#define SF_TRIGGER_PUSHABLES            4           // Only pushables can fire this trigger

#define SF_TRIG_PUSH_ONCE               1

/**
* trigger_multiple
*/
#define SF_TRIGGER_MULTIPLE_NOTOUCH     0x0001

/**
* trigger_counter
*/
#define SF_TRIGGER_COUNTER_NOMESSAGE    0x0001

/**
 * func_breakable
 */
#define SF_BREAK_TRIGGER_ONLY           1           // May only be broken by trigger
#define SF_BREAK_TOUCH                  2           // Can be 'crashed through' by running player (plate glass)
#define SF_BREAK_PRESSURE               4           // Can be broken by a player standing on it
#define SF_BREAK_CROWBAR                256         // Instant break if hit with crowbar

/**
 * func_pushable (also func_breakable, so don't collide with those flags)
 */
#define SF_PUSH_BREAKABLE               128

/**
 * light_spawn
 */
#define SF_LIGHT_START_OFF              1
#define SPAWNFLAG_NOMESSAGE             1
#define SPAWNFLAG_NOTOUCH               1
#define SPAWNFLAG_DROIDONLY             4
#define SPAWNFLAG_USEONLY               1           // Can't be touched, must be used (buttons)

/**
 * Monster Spawnflags
 */
#define SF_MONSTER_WAIT_TILL_SEEN       1           // Spawnflag that makes monsters wait until player can see them before attacking
#define SF_MONSTER_GAG                  2           // No idle noises from this monster
#define SF_MONSTER_HITMONSTERCLIP       4
#define SF_MONSTER_PRISONER             16          // Monster won't attack anyone, no one will attacke him
#define SF_MONSTER_WAIT_FOR_SCRIPT      128         // Spawnflag that makes monsters wait to check for attacking until the script is done or they've been attacked
#define SF_MONSTER_PREDISASTER          256         // This is a predisaster scientist or barney; influences how they speak
#define SF_MONSTER_FADECORPSE           512         // Fade out corpse after death
#define SF_MONSTER_FALL_TO_GROUND       0x80000000
#define SF_MONSTER_TURRET_AUTOACTIVATE  32
#define SF_MONSTER_TURRET_STARTINACTIVE 64
#define SF_MONSTER_WAIT_UNTIL_PROVOKED  64          // Don't attack the player unless provoked

/**
 * info_decal
 */
#define SF_DECAL_NOTINDEATHMATCH        2048

/**
 * worldspawn
 */
#define SF_WORLD_DARK                   0x0001      // Fade from black at startup
#define SF_WORLD_TITLE                  0x0002      // Display game title at startup
#define SF_WORLD_FORCETEAM              0x0004      // Force teams

/**
 * Set this bit on guns and stuff that should never respawn
 */
#define SF_NORESPAWN                    (1<<30)

/**
 * @endsection
 */

/**
 * Train status values
 */
#define TRAIN_ACTIVE  0x80
#define TRAIN_NEW     0xc0

#define TRAIN_OFF     0x00
#define TRAIN_NEUTRAL 0x01
#define TRAIN_SLOW    0x02
#define TRAIN_MEDIUM  0x03
#define TRAIN_FAST    0x04
#define TRAIN_BACK    0x05

/**
 * Valve Mod Weapon Constants
 */
#define HLI_HEALTHKIT       1
#define HLI_ANTIDOTE        2
#define HLI_SECURITY        3
#define HLI_BATTERY         4

#define HLW_NONE            0
#define HLW_CROWBAR         1
#define HLW_GLOCK           2
#define HLW_PYTHON          3
#define HLW_MP5             4
#define HLW_CHAINGUN        5
#define HLW_CROSSBOW        6
#define HLW_SHOTGUN         7
#define HLW_RPG             8
#define HLW_GAUSS           9
#define HLW_EGON            10
#define HLW_HORNETGUN       11
#define HLW_HANDGRENADE     12
#define HLW_TRIPMINE        13
#define HLW_SATCHEL         14
#define HLW_SNARK           15
#define HLW_SUIT            31
#define HLW_ALLWEAPONS      (~(1<<HLW_SUIT))

/**
 * Item's flags
 */
#define ITEM_FLAG_SELECTONEMPTY     (1<<0)
#define ITEM_FLAG_NOAUTORELOAD      (1<<1)
#define ITEM_FLAG_NOAUTOSWITCHEMPTY (1<<2)
#define ITEM_FLAG_LIMITINWORLD      (1<<3)
#define ITEM_FLAG_EXHAUSTIBLE       (1<<4)

/**
* PlaybackEvent flags
*/
#define FEV_NOTHOST     (1<<0)  // Skip local host for event send.
#define FEV_RELIABLE    (1<<1)  // Send the event reliably.  You must specify the origin and angles
                                // for this to work correctly on the server for anything
                                // that depends on the event origin/angles.  I.e., the origin/angles are not
                                // taken from the invoking edict for reliable events.
#define FEV_GLOBAL      (1<<2)  // Don't restrict to PAS/PVS, send this event to _everybody_ on the server ( useful for stopping CHAN_STATIC
                                //  sounds started by client event when client is not in PVS anymore ( hwguy in TFC e.g. ).
#define FEV_UPDATE      (1<<3)  // If this client already has one of these events in its queue, just update the event instead of sending it as a duplicate
#define FEV_HOSTONLY    (1<<4)  // Only send to entity specified as the invoker
#define FEV_SERVER      (1<<5)  // Only send if the event was created on the server.
#define FEV_CLIENT      (1<<6)  // Only issue event client side ( from shared code )

/**
 * Cap bits to indicate what an object's capabilities are, currently used for
 * save/restore and level transitions.
 */
#define FCAP_CUSTOMSAVE                 0x00000001
#define FCAP_ACROSS_TRANSITION          0x00000002      // should transfer between transitions
#define FCAP_MUST_SPAWN                 0x00000004      // Spawn after restore
#define FCAP_DONT_SAVE                  0x80000000      // Don't save this
#define FCAP_IMPULSE_USE                0x00000008      // can be used by the player
#define FCAP_CONTINUOUS_USE             0x00000010      // can be used by the player
#define FCAP_ONOFF_USE                  0x00000020      // can be used by the player
#define FCAP_DIRECTIONAL_USE            0x00000040      // Player sends +/- 1 when using (currently only tracktrains)
#define FCAP_MASTER                     0x00000080      // Can be used to "master" other entities (like multisource)
#define FCAP_MUST_RESET                 0x00000100 // Should reset on the new round
#define FCAP_MUST_RELEASE               0x00000200 // Should release on the new round
#define FCAP_DONT_SAVE                  0x80000000 // Don't save this

// UNDONE: This will ignore transition volumes (trigger_transition), but not the PVS!!!
#define FCAP_FORCE_TRANSITION           0x00000080 // ALWAYS goes across transitions

/**
* All monsters need this data
*/
#define DONT_BLEED                      -1
#define BLOOD_COLOR_RED                 247
#define BLOOD_COLOR_YELLOW              195
#define BLOOD_COLOR_GREEN               BLOOD_COLOR_YELLOW

/**
 * Player physics flags
 */
#define PFLAG_ONLADDER                  (1<<0)
#define PFLAG_ONSWING                   (1<<0)
#define PFLAG_ONTRAIN                   (1<<1)
#define PFLAG_ONBARNACLE                (1<<2)
#define PFLAG_DUCKING                   (1<<3) // In the process of ducking, but totally squatted yet
#define PFLAG_USING                     (1<<4) // Using a continuous entity
#define PFLAG_OBSERVER                  (1<<5) // Player is locked in stationary cam mode. Spectators can move, observers can't.

/**
* Player hide HUD values
*/
#define HIDEHUD_WEAPONS                 (1<<0)
#define HIDEHUD_FLASHLIGHT              (1<<1)
#define HIDEHUD_ALL                     (1<<2)
#define HIDEHUD_HEALTH                  (1<<3)
#define HIDEHUD_TIMER                   (1<<4)
#define HIDEHUD_MONEY                   (1<<5)
#define HIDEHUD_CROSSHAIR               (1<<6)
#define HIDEHUD_OBSERVER_CROSSHAIR      (1<<7)

/**
* Entity classification
*/
#define CLASS_NONE              0
#define CLASS_MACHINE           1
#define CLASS_PLAYER            2
#define CLASS_HUMAN_PASSIVE     3
#define CLASS_HUMAN_MILITARY    4
#define CLASS_ALIEN_MILITARY    5
#define CLASS_ALIEN_PASSIVE     6
#define CLASS_ALIEN_MONSTER     7
#define CLASS_ALIEN_PREY        8
#define CLASS_ALIEN_PREDATOR    9
#define CLASS_INSECT            10
#define CLASS_PLAYER_ALLY       11
#define CLASS_PLAYER_BIOWEAPON  12 // hornets and snarks.launched by players
#define CLASS_ALIEN_BIOWEAPON   13 // hornets and snarks.launched by the alien menace
#define CLASS_VEHICLE           14
#define CLASS_BARNACLE          99 // special because no one pays attention to it, and it eats a wide cross-section of creatures.

/**
* Entities that toggle (buttons/triggers/doors) need this
*/
#define TS_AT_TOP      0
#define TS_AT_BOTTOM   1
#define TS_GOING_UP    2
#define TS_GOING_DOWN  3

/**
* Entity use states (buttons/triggers/platforms)
*/
#define USE_OFF     0
#define USE_ON      1
#define USE_SET     2
#define USE_TOGGLE  3
