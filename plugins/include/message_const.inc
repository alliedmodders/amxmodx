// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Message Constants
//

#if defined _message_const_included
	#endinput
#endif
#define _message_const_included

/**
 * Destination types for message_begin()
 */
#define	MSG_BROADCAST               0        // Unreliable to all
#define	MSG_ONE                     1        // Reliable to one (msg_entity)
#define	MSG_ALL                     2        // Reliable to all
#define	MSG_INIT                    3        // Write to the init string
#define MSG_PVS                     4        // Ents in PVS of org
#define MSG_PAS                     5        // Ents in PAS of org
#define MSG_PVS_R                   6        // Reliable to PVS
#define MSG_PAS_R                   7        // Reliable to PAS
#define MSG_ONE_UNRELIABLE          8        // Send to one client, but don't put in reliable stream, put in unreliable datagram (could be dropped)
#define	MSG_SPEC                    9        // Sends to all spectator proxies

/**
 * Hardcoded message types for message_begin()
 *
 * @note Look at the actual HLSDK for details
 */
#define SVC_BAD 			0
#define SVC_NOP				1
#define SVC_DISCONNECT			2
#define SVC_EVENT			3
#define SVC_VERSION			4
#define SVC_SETVIEW			5
#define SVC_SOUND			6
#define SVC_TIME			7
#define SVC_PRINT			8
#define SVC_STUFFTEXT			9
#define SVC_SETANGLE			10
#define SVC_SERVERINFO			11
#define SVC_LIGHTSTYLE			12
#define SVC_UPDATEUSERINFO		13
#define SVC_DELTADESCRIPTION		14
#define SVC_CLIENTDATA			15
#define SVC_STOPSOUND			16
#define SVC_PINGS			17
#define SVC_PARTICLE			18
#define SVC_DAMAGE			19
#define SVC_SPAWNSTATIC			20
#define SVC_EVENT_RELIABLE		21
#define SVC_SPAWNBASELINE		22
#define SVC_TEMPENTITY			23
#define SVC_SETPAUSE			24
#define SVC_SIGNONNUM			25
#define SVC_CENTERPRINT			26
#define SVC_KILLEDMONSTER		27
#define SVC_FOUNDSECRET			28
#define SVC_SPAWNSTATICSOUND		29
#define SVC_INTERMISSION		30
#define SVC_FINALE			31
#define SVC_CDTRACK			32
#define SVC_RESTORE			33
#define SVC_CUTSCENE			34
#define SVC_WEAPONANIM			35
#define SVC_DECALNAME			36
#define SVC_ROOMTYPE			37
#define SVC_ADDANGLE			38
#define SVC_NEWUSERMSG			39
#define SVC_PACKETENTITIES		40
#define SVC_DELTAPACKETENTITIES		41
#define SVC_CHOKE			42
#define SVC_RESOURCELIST		43
#define SVC_NEWMOVEVARS			44
#define SVC_RESOURCEREQUEST		45
#define SVC_CUSTOMIZATION		46
#define SVC_CROSSHAIRANGLE		47
#define SVC_SOUNDFADE			48
#define SVC_FILETXFERFAILED		49
#define SVC_HLTV			50
#define SVC_DIRECTOR			51
#define SVC_VOICEINIT			52
#define SVC_VOICEDATA			53
#define SVC_SENDEXTRAINFO		54
#define SVC_TIMESCALE			55
#define SVC_RESOURCELOCATION 		56
#define SVC_SENDCVARVALUE 		57
#define SVC_SENDCVARVALUE2 		58

/**
 * Flags for set_msg_block()
 */
#define BLOCK_NOT                   0
#define BLOCK_ONCE                  1
#define BLOCK_SET                   2

/**
 * Message argument types used with get_msg_argtype() and set_msg_arg_*
 */
enum
{
	ARG_BYTE = 1,       /* int */
	ARG_CHAR,           /* int */
	ARG_SHORT,          /* int */
	ARG_LONG,           /* int */
	ARG_ANGLE,          /* float */
	ARG_COORD,          /* float */
	ARG_STRING,         /* string */
	ARG_ENTITY,         /* int */
};

/**
 * @section TempEntity messages for message_begin()
 */

/**
 * Beam effect between two points
 *
 * @note
 * write_byte(TE_BEAMPOINTS)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define	TE_BEAMPOINTS               0

/**
 * Beam effect between a point and an entity
 *
 * @note
 * write_byte(TE_BEAMENTPOINT)
 * write_short(start entity)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
 #define TE_BEAMENTPOINT            1

/**
 * Particle effect plus ricochet sound
 *
 * @note
 * write_byte(TE_GUNSHOT)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 */
#define	TE_GUNSHOT                  2

/**
 * Additive sprite, 2 dynamic lights, flickering particles, explosion sound,
 * move vertically 8 pps
 *
 * @note
 * write_byte(TE_EXPLOSION)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(sprite index)
 * write_byte(scale in 0.1's)
 * write_byte(framerate)
 * write_byte(flags)
 */
#define	TE_EXPLOSION                3

/**
 * Flags for the TE_EXPLOSION effect, controlling its performance and aesthetic
 * features
 */
#define TE_EXPLFLAG_NONE            0        // All flags clear makes default Half-Life explosion
#define TE_EXPLFLAG_NOADDITIVE      1        // Sprite will be drawn opaque (ensure that the sprite you send is a non-additive sprite)
#define TE_EXPLFLAG_NODLIGHTS       2        // Do not render dynamic lights
#define TE_EXPLFLAG_NOSOUND         4        // Do not play client explosion sound
#define TE_EXPLFLAG_NOPARTICLES     8        // Do not draw particles

/**
 * Quake1 "tarbaby" explosion with sound
 *
 * @note
 * write_byte(TE_TAREXPLOSION)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 */
#define	TE_TAREXPLOSION             4

/**
 * Alphablend sprite, move vertically 30pps
 *
 * @note
 * write_byte(TE_SMOKE)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(sprite index)
 * write_byte(scale in 0.1's)
 * write_byte(framerate)
 */
#define	TE_SMOKE                    5

/**
 * Tracer effect from point to point
 *
 * @note
 * write_byte(TE_TRACER)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 */
#define	TE_TRACER                   6

/**
 * TE_BEAMPOINTS with simplified parameters
 *
 * @note
 * write_byte(TE_LIGHTNING)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_byte(life in 0.1's)
 * write_byte(width in 0.1's)
 * write_byte(amplitude in 0.01's)
 * write_short(sprite model index)
 */
#define	TE_LIGHTNING                7

/**
 * TE_BEAMENTS
 *
 * @note
 * write_byte(TE_BEAMENTS)
 * write_short(start entity)
 * write_short(end entity)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define	TE_BEAMENTS                 8

/**
 * 8 random tracers with gravity, ricochet sprite
 *
 * @note
 * write_byte(TE_SPARKS)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 */
#define	TE_SPARKS                   9

/**
 * Quake1 lava splash
 *
 * @note
 * write_byte(TE_LAVASPLASH)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 */
#define	TE_LAVASPLASH               10

/**
 * Quake1 teleport splash
 *
 * @note
 * write_byte(TE_TELEPORT)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 */
#define	TE_TELEPORT                 11

/**
 * Quake1 colormaped (base palette) particle explosion with sound
 *
 * @note
 * write_byte(TE_EXPLOSION2)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(starting color)
 * write_byte(num colors)
 */
#define TE_EXPLOSION2               12

/**
 * Decal from the .BSP file
 *
 * @note
 * write_byte(TE_BSPDECAL)
 * write_coord(position.x) decal position (center of texture in world)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(texture index of precached decal texture name)
 * write_short(entity index)
 * [optional - write_short(index of model of above entity) only included if previous short is non-zero (not the world)]
 */
#define TE_BSPDECAL                 13

/**
 * Tracers moving toward a point
 *
 * @note
 * write_byte(TE_IMPLOSION)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(radius)
 * write_byte(count)
 * write_byte(life in 0.1's)
 */
#define TE_IMPLOSION                14

/**
 * Line of moving glow sprites with gravity, fadeout, and collisions
 *
 * @note
 * write_byte(TE_SPRITETRAIL)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_short(sprite index)
 * write_byte(count)
 * write_byte(life in 0.1's)
 * write_byte(scale in 0.1's)
 * write_byte(velocity along vector in 10's)
 * write_byte(randomness of velocity in 10's)
 */
#define TE_SPRITETRAIL              15

/**
 * Additive sprite, plays 1 cycle
 *
 * @note
 * write_byte(TE_SPRITE)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(sprite index)
 * write_byte(scale in 0.1's)
 * write_byte(brightness)
 */
#define TE_SPRITE                   17

/**
 * A beam with a sprite at the end
 *
 * @note
 * write_byte(TE_BEAMSPRITE)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_short(beam sprite index)
 * write_short(end sprite index)
 */
#define TE_BEAMSPRITE               18

/**
 * Screen aligned beam ring, expands to max radius over lifetime
 *
 * @note
 * write_byte(TE_BEAMTORUS)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(axis.x)
 * write_coord(axis.y)
 * write_coord(axis.z)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define TE_BEAMTORUS                19

/**
 * Disk that expands to max radius over lifetime
 *
 * @note
 * write_byte(TE_BEAMDISK)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(axis.x)
 * write_coord(axis.y)
 * write_coord(axis.z)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define TE_BEAMDISK                 20

/**
 * Cylinder that expands to max radius over lifetime
 *
 * @note
 * write_byte(TE_BEAMCYLINDER)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(axis.x)
 * write_coord(axis.y)
 * write_coord(axis.z)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define TE_BEAMCYLINDER             21

/**
 * Create a line of decaying beam segments until entity stops moving
 *
 * @note
 * write_byte(TE_BEAMFOLLOW)
 * write_short(entity:attachment to follow)
 * write_short(sprite index)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 */
#define TE_BEAMFOLLOW               22

/**
 * TE_GLOWSPRITE
 *
 * @note
 * write_byte(TE_GLOWSPRITE)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(model index)
 * write_byte(scale / 10)
 * write_byte(size)
 * write_byte(brightness)
 */
#define TE_GLOWSPRITE               23

/**
 * Connect a beam ring to two entities
 *
 * @note
 * write_byte(TE_BEAMRING)
 * write_short(start entity)
 * write_short(end entity)
 * write_short(sprite index)
 * write_byte(starting frame)
 * write_byte(frame rate in 0.1's)
 * write_byte(life in 0.1's)
 * write_byte(line width in 0.1's)
 * write_byte(noise amplitude in 0.01's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(brightness)
 * write_byte(scroll speed in 0.1's)
 */
#define TE_BEAMRING                 24

/**
 * Oriented shower of tracers
 *
 * @note
 * write_byte(TE_STREAK_SPLASH)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(vector.x)
 * write_coord(vector.y)
 * write_coord(vector.z)
 * write_byte(color)
 * write_short(count)
 * write_short(base speed)
 * write_short(random velocity)
 */
#define TE_STREAK_SPLASH            25

/**
 * Dynamic light, effect world, minor entity effect
 *
 * @note
 * write_byte(TE_DLIGHT)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(radius in 10's)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(life in 10's)
 * write_byte(decay rate in 10's)
 */
#define TE_DLIGHT                   27

/**
 * Point entity light, no world effect
 *
 * @note
 * write_byte(TE_ELIGHT)
 * write_short(entity:attachment to follow)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(radius)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 * write_byte(life in 0.1's)
 * write_coord(decay rate)
 */
#define TE_ELIGHT                   28

/**
 * TE_TEXTMESSAGE
 *
 * @note
 * write_byte(TE_TEXTMESSAGE)
 * write_byte(channel)
 * write_short(x) -1 = center)
 * write_short(y) -1 = center)
 * write_byte(effect) 0 = fade in/fade out, 1 is flickery credits, 2 is write out (training room)
 * write_byte(red) - text color
 * write_byte(green)
 * write_byte(blue)
 * write_byte(alpha)
 * write_byte(red) - effect color
 * write_byte(green)
 * write_byte(blue)
 * write_byte(alpha)
 * write_short(fadein time)
 * write_short(fadeout time)
 * write_short(hold time)
 * [optional] write_short(fxtime) time the highlight lags behing the leading text in effect 2
 * write_string(text message) 512 chars max string size
 */
#define TE_TEXTMESSAGE              29

/**
 * TE_LINE
 *
 * @note
 * write_byte(TE_LINE)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 * write_short(life in 0.1 s)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 */
#define TE_LINE                     30

/**
 * TE_BOX
 *
 * @note
 * write_byte(TE_BOX)
 * write_coord(boxmins.x)
 * write_coord(boxmins.y)
 * write_coord(boxmins.z)
 * write_coord(boxmaxs.x)
 * write_coord(boxmaxs.y)
 * write_coord(boxmaxs.z)
 * write_short(life in 0.1 s)
 * write_byte(red)
 * write_byte(green)
 * write_byte(blue)
 */
#define TE_BOX                      31

/**
 * Kill all beams attached to entity
 *
 * @note
 * write_byte(TE_KILLBEAM)
 * write_short(entity)
 */
#define TE_KILLBEAM                 99

/**
 * TE_LARGEFUNNEL
 *
 * @note
 * write_byte(TE_LARGEFUNNEL)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(sprite index)
 * write_short(flags)
 */
#define TE_LARGEFUNNEL              100

/**
 * Particle spray
 *
 * @note
 * write_byte(TE_BLOODSTREAM)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(vector.x)
 * write_coord(vector.y)
 * write_coord(vector.z)
 * write_byte(color)
 * write_byte(speed)
 */
#define	TE_BLOODSTREAM              101

/**
 * Line of particles every 5 units, dies in 30 seconds
 *
 * @note
 * write_byte(TE_SHOWLINE)
 * write_coord(startposition.x)
 * write_coord(startposition.y)
 * write_coord(startposition.z)
 * write_coord(endposition.x)
 * write_coord(endposition.y)
 * write_coord(endposition.z)
 */
#define	TE_SHOWLINE                 102

/**
 * Particle spray
 *
 * @note
 * write_byte(TE_BLOOD)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(vector.x)
 * write_coord(vector.y)
 * write_coord(vector.z)
 * write_byte(color)
 * write_byte(speed)
 */
#define TE_BLOOD                    103

/**
 * Decal applied to a brush entity (not the world)
 *
 * @note
 * write_byte(TE_DECAL)
 * write_coord(position.x) decal position (center of texture in world)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(texture index of precached decal texture name)
 * write_short(entity index)
 */
#define TE_DECAL                    104

/**
 * Create alpha sprites inside of entity, float upwards
 *
 * @note
 * write_byte(TE_FIZZ)
 * write_short(entity)
 * write_short(sprite index)
 * write_byte density)
 */
#define TE_FIZZ                     105

/**
 * Create a moving model that bounces and makes a sound when it hits
 *
 * @note
 * write_byte(TE_MODEL)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_angle(initial yaw)
 * write_short(model index)
 * write_byte(bounce sound type)
 * write_byte(life in 0.1's)
 */
#define TE_MODEL                    106

/**
 * Spherical shower of models, picks from set
 *
 * @note
 * write_byte(TE_EXPLODEMODEL)
 * write_coord(origin.x)
 * write_coord(origin.y)
 * write_coord(origin.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_short(model index)
 * write_short(count)
 * write_byte(life in 0.1's)
 */
#define TE_EXPLODEMODEL             107

/**
 * Box of models or sprites
 *
 * @note
 * write_byte(TE_BREAKMODEL)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(size.x)
 * write_coord(size.y)
 * write_coord(size.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_byte(random velocity in 10's)
 * write_short(sprite or model index)
 * write_byte(count)
 * write_byte(life in 0.1 secs)
 * write_byte(flags)
 */
#define TE_BREAKMODEL               108

/**
 * Decal and ricochet sound
 *
 * @note
 * write_byte(TE_GUNSHOTDECAL)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(entity index???)
 * write_byte(decal???)
 */
#define TE_GUNSHOTDECAL             109

/**
 * Spray of alpha sprites
 *
 * @note
 * write_byte(TE_SPRITE_SPRAY)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_short(sprite index)
 * write_byte(count)
 * write_byte(speed)
 * write_byte(noise)
 */
#define TE_SPRITE_SPRAY             110

/**
 * Quick spark sprite, client ricochet sound.
 *
 * @note
 * write_byte(TE_ARMOR_RICOCHET)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(scale in 0.1's)
 */
#define TE_ARMOR_RICOCHET           111

/**
 * TE_PLAYERDECAL
 *
 * @note
 * write_byte(TE_PLAYERDECAL)
 * write_byte(playerindex)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(entity???)
 * write_byte(decal number)
 * [optional] write_short(model index)
 */
#define TE_PLAYERDECAL              112

/**
 * Create alpha sprites inside of box, float upwards
 *
 * @note
 * write_byte(TE_BUBBLES)
 * write_coord(position.x) (min start position)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(position.x) (max start position)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(float height)
 * write_short(model index)
 * write_byte(count)
 * write_coord(speed)
 */
#define TE_BUBBLES                  113

/**
 * Create alpha sprites along a line, float upwards
 *
 * @note
 * write_byte(TE_BUBBLETRAIL)
 * write_coord(position.x) (min start position)
 * write_coord(position.y) (min start position)
 * write_coord(position.z) (min start position)
 * write_coord(position.x) (max start position)
 * write_coord(position.y) (max start position)
 * write_coord(position.z) (max start position)
 * write_coord(float height)
 * write_short(model index)
 * write_byte(count)
 * write_coord(speed)
 */
#define TE_BUBBLETRAIL              114

/**
 * Spray of opaque sprite1's that fall, single sprite2 for 1..2 secs (this is a high-priority tent)
 *
 * @note
 * write_byte(TE_BLOODSPRITE)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_short(sprite1 index)
 * write_short(sprite2 index)
 * write_byte(color)
 * write_byte(scale)
 */
#define TE_BLOODSPRITE              115

/**
 * Decal applied to the world brush
 *
 * @note
 * write_byte(TE_WORLDDECAL)
 * write_coord(position.x) decal position (center of texture in world)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(texture index of precached decal texture name)
 */
#define TE_WORLDDECAL               116

/**
 * Decal (with texture index > 256) applied to world brush
 *
 * @note
 * write_byte(TE_WORLDDECALHIGH)
 * write_coord(position.x) decal position (center of texture in world)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(texture index of precached decal texture name - 256)
 */
#define TE_WORLDDECALHIGH           117

/**
 * Same as TE_DECAL, but the texture index was greater than 256
 *
 * @note
 * write_byte(TE_DECALHIGH)
 * write_coord(position.x) decal position (center of texture in world)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_byte(texture index of precached decal texture name - 256)
 * write_short(entity index)
 */
#define TE_DECALHIGH                118

/**
 * Makes a projectile (like a nail) (this is a high-priority tent)
 *
 * @note
 * write_byte(TE_PROJECTILE)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_short(modelindex)
 * write_byte(life)
 * write_byte(owner)  projectile won't collide with owner (if owner == 0, projectile will hit any client).
 */
#define TE_PROJECTILE               119

/**
 * Throws a shower of sprites or models
 *
 * @note
 * write_byte(TE_SPRAY)
 * write_coord(position.x)
 * write_coord(position.y)
 * write_coord(position.z)
 * write_coord(direction.x)
 * write_coord(direction.y)
 * write_coord(direction.z)
 * write_short(modelindex)
 * write_byte(count)
 * write_byte(speed)
 * write_byte(noise)
 * write_byte(rendermode)
 */
#define TE_SPRAY                    120

/**
 * Sprites emit from a player's bounding box (ONLY use for players!)
 *
 * @note
 * write_byte(TE_PLAYERSPRITES)
 * write_short(playernum)
 * write_short(sprite modelindex)
 * write_byte(count)
 * write_byte(variance) (0 = no variance in size) (10 = 10% variance in size)
 */
#define TE_PLAYERSPRITES            121

/**
 * Very similar to lavasplash
 *
 * @note
 * write_byte(TE_PARTICLEBURST)
 * write_coord(origin)
 * write_short(radius)
 * write_byte(particle color)
 * write_byte(duration * 10) (will be randomized a bit)
 */
#define TE_PARTICLEBURST            122

/**
 * Makes a field of fire
 *
 * @note
 * write_byte(TE_FIREFIELD)
 * write_coord(origin)
 * write_short(radius) (fire is made in a square around origin. -radius, -radius to radius, radius)
 * write_short(modelindex)
 * write_byte(count)
 * write_byte(flags)
 * write_byte(duration (in seconds) * 10) (will be randomized a bit)
 */
#define TE_FIREFIELD                123

/**
 * Flags for the TE_FIREFIELD effect, controlling its performance and aesthetic
 * features
 */
#define TEFIRE_FLAG_ALLFLOAT        1        // All sprites will drift upwards as they animate
#define TEFIRE_FLAG_SOMEFLOAT       2        // Some of the sprites will drift upwards. (50% chance)
#define TEFIRE_FLAG_LOOP            4        // If set, sprite plays at 15 fps, otherwise plays at whatever rate stretches the animation over the sprite's duration.
#define TEFIRE_FLAG_ALPHA           8        // If set, sprite is rendered alpha blended at 50% else, opaque
#define TEFIRE_FLAG_PLANAR          16       // If set, all fire sprites have same initial Z instead of randomly filling a cube.

/**
 * Attaches a TENT to a player (this is a high-priority tent)
 *
 * @note
 * write_byte(TE_PLAYERATTACHMENT)
 * write_byte(entity index of player)
 * write_coord(vertical offset) (attachment origin.z = player origin.z + vertical offset)
 * write_short(model index)
 * write_short(life * 10 )
 */
#define TE_PLAYERATTACHMENT         124

/**
 * Will expire all TENTS attached to a player.
 *
 * @note
 * write_byte(TE_KILLPLAYERATTACHMENTS)
 * write_byte(entity index of player)
 */
#define TE_KILLPLAYERATTACHMENTS    125

/**
 * Much more compact shotgun message
 *
 * @note This message is used to make a client approximate a 'spray' of gunfire.
 *       Any weapon that fires more than one bullet per frame and fires in a bit
 *       of a spread is a good candidate for MULTIGUNSHOT use. (shotguns)
 * @note This effect makes the client do traces for each bullet, these client
 *       traces ignore entities that have studio models.Traces are 4096 long.
 * @note
 * write_byte(TE_MULTIGUNSHOT)
 * write_coord(origin.x)
 * write_coord(origin.y)
 * write_coord(origin.z)
 * write_coord(direction.x)
 * write_coord(direction.y)
 * write_coord(direction.z)
 * write_coord(x noise * 100)
 * write_coord(y noise * 100)
 * write_byte(count)
 * write_byte(bullethole decal texture index)
 */
#define TE_MULTIGUNSHOT             126


/**
 * Larger message than the standard tracer, but allows some customization.
 *
 * @note
 * write_byte(TE_USERTRACER)
 * write_coord(origin.x)
 * write_coord(origin.y)
 * write_coord(origin.z)
 * write_coord(velocity.x)
 * write_coord(velocity.y)
 * write_coord(velocity.z)
 * write_byte(life * 10)
 * write_byte(color) this is an index into an array of color vectors in the engine. (0 - )
 * write_byte(length * 10)
 */
#define TE_USERTRACER               127

/**
 * @endsection
 */

/**
 * From hltv.h from the HLSDK, these are used in conjunction with SVC_DIRECTOR
 * sub commands of svc_director
 */
#define DRC_CMD_NONE				0	// NULL director command
#define DRC_CMD_START				1	// start director mode
#define DRC_CMD_EVENT				2	// informs about director command
#define DRC_CMD_MODE				3	// switches camera modes
#define DRC_CMD_CAMERA				4	// sets camera registers
#define DRC_CMD_TIMESCALE			5	// sets time scale
#define DRC_CMD_MESSAGE				6	// send HUD centerprint
#define DRC_CMD_SOUND				7	// plays a particular sound
#define DRC_CMD_STATUS				8	// status info about broadcast
#define DRC_CMD_BANNER				9	// banner file name for HLTV gui
#define	DRC_CMD_FADE				10	// send screen fade command
#define DRC_CMD_SHAKE				11	// send screen shake command
#define DRC_CMD_STUFFTEXT			12	// like the normal svc_stufftext but as director command

#define DRC_CMD_LAST				12

/**
 * HLTV_EVENT event flags
 */
#define DRC_FLAG_PRIO_MASK		0x0F	// priorities between 0 and 15 (15 most important)
#define DRC_FLAG_SIDE			(1<<4)	//
#define DRC_FLAG_DRAMATIC		(1<<5)	// is a dramatic scene
#define DRC_FLAG_SLOWMOTION		(1<<6)  // would look good in SloMo
#define DRC_FLAG_FACEPLAYER		(1<<7)  // player is doning something (reload/defuse bomb etc)
#define DRC_FLAG_INTRO			(1<<8)	// is a introduction scene
#define DRC_FLAG_FINAL			(1<<9)	// is a final scene
#define DRC_FLAG_NO_RANDOM		(1<<10)	// don't randomize event data

#define MAX_DIRECTOR_CMD_PARAMETERS		4
#define MAX_DIRECTOR_CMD_STRING			128
