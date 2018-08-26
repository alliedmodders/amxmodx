// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Temporary Entity Functions
//

#if defined _tempent_included
	#endinput
#endif

#define _tempent_included

/**
 * Creates a beam between two points.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=3s
 *
 * @param startpos      Starting coordinates of the beam
 * @param endpos        Ending coordinates of the beam
 * @param sprite        The sprite index to use in the beam
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_between_points(startpos[3], endpos[3], sprite, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a beam between an entity and a point.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=20s
 *
 * @param startent      Primary entity id
 * @param endpos        Ending coordinates of the beam
 * @param sprite        The sprite index to use in the beam
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_from_entity(startent, endpos[3], sprite, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a gunshot that consists of a particle effect and a ricochet sound.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=36s
 *
 * @param position      Gunshot coordinates
 * @param receiver      Client index that will be able to see the gunshot
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_gunshot(position[3], receiver = 0, bool:reliable = true)

/**
 * Creates an explosion.
 *
 * @note A common sprite to use is "sprites/zerogxplode.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=43s
 *
 * @param position      Position of the explosion
 * @param sprite        The additive sprite index to use in the explosion
 * @param scale         The scale of the sprite in the explosion (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param flags         Explosion flags:
 *                        TE_EXPLFLAG_NONE               - default Half-Life explosion
 *                        TE_EXPLFLAG_NOADDITIVE         - sprite will be drawn opaque
 *                        TE_EXPLFLAG_NODLIGHTS          - don't render the dynamic lights
 *                        TE_EXPLFLAG_NOSOUND            - don't play the explosion sound
 *                        TE_EXPLFLAG_NOPARTICLES        - don't draw the particles
 * @param receiver      Client index that will be able to see the explosion
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_explosion(position[3], sprite, scale = 10, framerate = 30, flags = TE_EXPLFLAG_NONE, receiver = 0, bool:reliable = true)

/**
 * Creates the Quake "tarbaby" explosion with sound.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=55s
 *
 * @param position      Position of the explosion
 * @param receiver      Client index that will be able to see the explosion
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_tar_explosion(position[3], receiver = 0, bool:reliable = true)

/**
 * Creates smoke (a rising alphablend sprite at 30 pps).
 *
 * @note A common sprite to use is "sprites/steam1.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=1m2s
 *
 * @param position      Position of the smoke effect
 * @param sprite        The alphablend sprite index to use for the smoke
 * @param scale         The scale of the smoke (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param receiver      Client index that will be able to see the smoke
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_smoke(position[3], sprite, scale = 10, framerate = 30, receiver = 0, bool:reliable = true)

/**
 * Creates a tracer effect from one point to another.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=1m12s
 *
 * @param startpos      Starting position of the tracer
 * @param endpos        Ending position of the tracer
 * @param receiver      Client index that will be able to see the tracer
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_tracer(startpos[3], endpos[3], receiver = 0, bool:reliable = true)

/**
 * Creates a beam between two entities.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=1m26s
 *
 * @param startent      Primary entity id
 * @param endent        Secondary entity id
 * @param sprite        The sprite index to use in the beam
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_between_entities(startent, endent, sprite, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates 8 random tracers with gravity and a ricochet sprite.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=1m41s
 *
 * @param position      Position of the effect
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_sparks(position[3], receiver = 0, bool:reliable = true)

/**
 * Creates a Quake-style lava splash.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=1m49s
 *
 * @param position      Position of the effect
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_lava_splash(position[3], receiver = 0, bool:reliable = true)

/**
 * Creates a Quake-style teleport splash.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=2m6s
 *
 * @param position      Position of the effect
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_teleport_splash(position[3], receiver = 0, bool:reliable = true)

/**
 * Creates a Quake colormapped (base palette) particle explosion with sound.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=2m19s
 *
 * @param position      Position of the explosion
 * @param startcolor    Starting color (0 - 255)
 * @param numcolors     Number of colors (1 - 255)
 * @param receiver      Client index that will be able to see the explosion
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_colored_explosion(position[3], startcolor = 0, numcolors = 255, receiver = 0, bool:reliable = true)

/**
 * Places a decal from the .BSP file.
 *
 * @note Using a decal index that doesn't exist will crash the client.
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=2m30s
 *
 * @param position      Position of the decal (center of texture in world)
 * @param texture       Texture index of precached decal texture name
 * @param entity        Entity index or 0 for world
 * @param entabove      Model index of the entity above (only available if
 *                      the "entity" argument is non-zero)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_place_decal_from_bsp_file(position[3], texture, entity = 0, entabove = 0, receiver = 0, bool:reliable = true)

/**
 * Creates tracers moving towards a point.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=2m44s
 *
 * @param position      Position of the implosion effect
 * @param radius        Implosion radius
 * @param count         Number of tracers to generate
 * @param life          The length of time the effect shall remain
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_implosion(position[3], radius = 64, count = 10, life = 3, receiver = 0, bool:reliable = true)

/**
 * Creates a line of moving glow sprites or models with gravity, fadeout,
 * and collisions.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=2m58s
 *
 * @param startpos      Starting position of the effect
 * @param endpos        Ending position of the effect
 * @param sprite        Sprite index
 * @param count         Number of models/sprites to generate
 * @param life          The length of time the effect shall remain
 * @param scale         Scale of the effect
 * @param velocity      Velocity along vector
 * @param randomness    Randomness of the velocity
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_model_trail(startpos[3], endpos[3], sprite, count = 1, life = 10, scale = 10, velocity = 10, randomness = 10, receiver = 0, bool:reliable = true)

/**
 * Displays an additive sprite that plays one cycle.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=3m7s
 *
 * @param position      Sprite position
 * @param sprite        Sprite index
 * @param scale         Scale of the sprite
 * @param brightness    Brightness of the sprite
 * @param receiver      Client index that will be able to see the sprite
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_display_additive_sprite(position[3], sprite, scale = 5, brightness = 255, receiver = 0, bool:reliable = true)

/**
 * Creates a beam with a sprite at the end of the beam.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=3m29s
 *
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param startpos      Starting position of the beam
 * @param endpos        Ending position of the beam
 * @param beamid        Sprite index for the beam body
 * @param endid         Sprite index for the end of the beam
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_sprite(startpos[3], endpos[3], beamid, endid, receiver = 0, bool:reliable = true)

/**
 * Creates a screen aligned beam ring that expands to the maximum radius
 * over lifetime.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=3m40s
 *
 * @param position      Starting coordinates of the beam
 * @param sprite        The sprite index to use in the beam
 * @param axis          Beam axis
 * @param startframe    The frame to start with in the sprite
 * @param framerate     The frame rate to show the sprite at
 * @param life          The length of time the beam shall remain
 * @param width         The width of the beam
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_ring(position[3], sprite, axis[3] = {0, 0, 0}, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a beam disk that expands to the maximum radius over lifetime.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=3m58s
 *
 * @param position      Starting coordinates of the beam
 * @param sprite        The sprite index to use in the beam
 * @param axis          Beam axis
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_disk(position[3], sprite, axis[3] = {0, 0, 0}, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a beam cylinder that expands to the maximum radius over lifetime.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=4m17s
 *
 * @param position      Starting coordinates of the beam
 * @param sprite        The sprite index to use in the beam
 * @param axis          Beam axis
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_cylinder(position[3], sprite, axis[3] = {0, 0, 0}, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a decaying beam that follows the entity until it stops moving. 
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note When the entity stops moving, the beam will become visible again
 *       once the entity starts moving.
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=4m31s
 *
 * @param entity        Entity that the beam will follow
 * @param sprite        The sprite index to use in the beam
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_following_beam(entity, sprite, life = 10, width = 10, r = 0, g = 0, b = 255, a = 75, receiver = 0, bool:reliable = true)

/**
 * Displays a glowing sprite.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=4m43s
 *
 * @param position      Sprite position
 * @param sprite        Sprite index
 * @param scale         Sprite scale (0 - 255)
 * @param size          Sprite size (0 - 255)
 * @param brightness    Sprite brightness (0 - 255)
 * @param receiver      Client index that will be able to see the sprite
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_display_glow_sprite(position[3], sprite, scale = 10, size = 10, brightness = 150, receiver = 0, bool:reliable = true)

/**
 * Creates a beam ring between two entities.
 *
 * @note A common sprite to use is "sprites/laserbeam.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=5m10s
 *
 * @param startent      Primary entity id
 * @param endent        Secondary entity id
 * @param sprite        The sprite index to use in the beam
 * @param startframe    The frame to start with in the sprite (0 - 255)
 * @param framerate     The frame rate to show the sprite at (0 - 255)
 * @param life          The length of time the beam shall remain (0 - 255)
 * @param width         The width of the beam (0 - 255)
 * @param noise         The noise amplitude of the beam, this controls
 *                      the distorsion of the beam (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Beam brightness (alpha) (0 - 255)
 * @param speed         The scroll speed of the beam (0 - 255)
 * @param receiver      Client index that will be able to see the beam
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_beam_ring_between_entities(startent, endent, sprite, startframe = 0, framerate = 30, life = 10, width = 10, noise = 0, r = 0, g = 0, b = 255, a = 75, speed = 0, receiver = 0, bool:reliable = true)

/**
 * Creates an oriented shower of tracers.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=5m34s
 *
 * @param position      Position of the effect
 * @param direction     Effect direction
 * @param color         Effect color (https://wiki.alliedmods.net/images/Palette.png)
 * @param count         Number of tracers to create
 * @param speed         The scroll speed of the effect
 * @param velocity      Random velocity
 * @param receiver      Client index that will be able to see the tracers
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_tracer_shower(position[3], direction[3] = {0, 0, 0}, color = 12, count = 1, speed = 0, velocity = 10, receiver = 0, bool:reliable = true)

/**
 * Creates a dynamic light with a world effect.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=5m47s
 *
 * @param position      Position of the light
 * @param radius        Light radius (0 - 255)
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param life          The length of time the light shall remain (0 - 255)
 * @param decay         Light decay rate (0 - 255)
 * @param receiver      Client index that will be able to see the light
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_dynamic_light(position[3], radius = 10, r = 255, g = 255, b = 255, life = 10, decay = 10, receiver = 0, bool:reliable = true)

/**
 * Creates a point entity light with no world effect.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=6m7s
 *
 * @param entity        Entity or client to apply the light on
 * @param position      Position of the light
 * @param radius        Light radius
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param life          The length of time the light shall remain (0 - 255)
 * @param decay         Light decay rate
 * @param receiver      Client index that will be able to see the light
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_entity_light(entity, position[3] = {0, 0, 0}, radius = 50, r = 255, g = 255, b = 255, life = 10, decay = 10, receiver = 0, bool:reliable = true)

/**
 * Draws a simple line.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=6m32s
 *
 * @param startpos      Starting position of the line
 * @param endpos        Ending position of the line
 * @param life          Line life
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param receiver      Client index that will be able to see the line
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_draw_line(startpos[3], endpos[3], life = 10, r = 0, g = 0, b = 255, receiver = 0, bool:reliable = true)

/**
 * Creates a simple box.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=6m45s
 *
 * @param startpos      Starting position of the box
 * @param endpos        Ending position of the box
 * @param life          Box life
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param receiver      Client index that will be able to see the box
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_box(startpos[3], endpos[3], life = 10, r = 0, g = 0, b = 255, receiver = 0, bool:reliable = true)

/**
 * Removes all beams attached to an entity.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=7m7s
 *
 * @param entity        Entity id to remove attached beams from
 * @param receiver      Client index that will be able to see the changes
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_remove_all_beams_from_entity(entity, receiver = 0, bool:reliable = true)

/**
 * Creates a large group of sprites or models accompanied by green dots
 * that float up or down until they reach the point set in the "position" argument.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=7m10s
 *
 * @param position      Effect position
 * @param sprite        Sprite index to use
 * @param flag          List of valid flags:
 *                        LF_FLOAT_DOWN           - float downwards and end in the point set in the "position" argument
 *                        LF_FLOAT_UP             - start from the point set in the "position" argument and float upwards
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_large_funnel(position[3], sprite, flag = LF_FLOAT_DOWN, receiver = 0, bool:reliable = true)

/**
 * Creates dripping blood particles.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=7m35s
 *
 * @param position      Starting position of the blood
 * @param direction     Blood direction
 * @param color         Blood color (https://wiki.alliedmods.net/images/Palette.png)
 * @param count         Number of blood particles to generate (0 - 255)
 * @param receiver      Client index that will be able to see the blood
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_bloodstream(position[3], direction[3] = {0, 0, 0}, color = 78, count = 1, receiver = 0, bool:reliable = true)

/**
 * Draws a line of blood particles spread 5 units from each other that
 * disappears after 30 seconds.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=7m53s
 *
 * @param startpos      Starting position of the line
 * @param endpos        Ending position of the line
 * @param receiver      Client index that will be able to see the line
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_draw_blood_line(startpos[3], endpos[3], receiver = 0, bool:reliable = true)

/**
 * Sprays blood particles from a given point.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=7m59s
 *
 * @param position      Position from where the blood will be sprayed
 * @param direction     Blood spraying direction
 * @param color         Blood color (https://wiki.alliedmods.net/images/Palette.png)
 * @param speed         Speed at which the blood particles will be sprayed (0 - 255)
 * @param receiver      Client index that will be able to see the particles
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_spray_blood(position[3], direction[3] = {0, 0, 0}, color = 78, speed = 30, receiver = 0, bool:reliable = true)

/**
 * Applies a decal to a brush entity (not the world).
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=8m12s
 *
 * @param position      Position of the decal (center of texture in world)
 * @param texture       Texture index of precached decal texture name (0 - 511)
 * @param entity        Entity index to apply the decal to
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_place_brush_decal(position[3], texture, entity = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a moving model or sprite that bounces and makes a sound when it hits.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=8m16s
 *
 * @param position      Position of the model
 * @param modelid       Model index
 * @param velocity      Model velocity
 * @param yaw           Initial yaw
 * @param bouncesound   Bounce sound type:
 *                        BounceSound_Null
 *                        BounceSound_Shell
 *                        BounceSound_ShotShell
 * @param life          The length of time the model shall remain (0 - 255)
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_bouncing_model(position[3], modelid, velocity[3] = {0, 0, 0}, yaw = 0, BounceSounds:bouncesound = BounceSound_Null, life = 10, receiver = 0, bool:reliable = true)

/**
 * Creates model or sprite with a blinking orange aura effect.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=8m28s
 *
 * @param position      Position of the model
 * @param modelid       Model index
 * @param speed         Model speed
 * @param count         Number of models to generate
 * @param life          The length of time the model shall remain (0 - 255)
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_explode_model(position[3], modelid, speed = 0, count = 1, life = 10, receiver = 0, bool:reliable = true)

/**
 * Creates a model or sprite entity that slowly disappears until it's gone.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ
 *
 * @param position      Position of the model
 * @param size          Size of the model
 * @param velocity      Model velocity
 * @param modelid       Model index
 * @param random        Random velocity (0 - 255)
 * @param count         Number of model pieces to generate (0 - 255)
 * @param life          The length of time the model shall remain (0 - 255)
 * @param flags         Break model flags:
 *                        BreakModel_TypeMask
 *                        BreakModel_Glass
 *                        BreakModel_Metal
 *                        BreakModel_Flesh
 *                        BreakModel_Wood
 *                        BreakModel_Smoke
 *                        BreakModel_Trans
 *                        BreakModel_Concrete
 *                        BreakModel_2
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_break_model(position[3], modelid, size[3] = {50, 50, 50}, velocity[3] = {0, 0, 0}, random = 0, count = 1, life = 10, flags = 0, receiver = 0, bool:reliable = true)

/**
 * Places a gunshot decal on an entity or the world and plays a ricochet sound.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=8m41s
 *
 * @param position      Position of the decal
 * @param decal         Decal index (0 - 255)
 * @param entity        Entity to apply the decal to or 0 for world
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_place_gunshot_decal(position[3], decal = 41, entity = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a spray of alpha sprites or models.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=8m52s
 *
 * @param position      Position of the effect
 * @param sprite        Sprite index
 * @param velocity      Spray velocity
 * @param count         Number of sprays to generate (0 - 255)
 * @param speed         The scroll speed of the effect (0 - 255)
 * @param noise         The noise amplitude of the effect - this
 *                      controls the distorsion of the effect (0 - 255)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_sprite_spray(position[3], sprite, velocity[3] = {0, 0, 0}, count = 1, speed = 0, noise = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a quick spray sprite with a ricochet sound.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m3s
 *
 * @param position      Position of the effect
 * @param scale         Scale of the effect (0 - 255)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_armor_ricochet(position[3], scale = 10, receiver = 0, bool:reliable = true)

/**
 * Places a player spray on an entity or the world.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m16s
 *
 * @param position      Position of the decal
 * @param decal         Decal index (0 - 255)
 * @param entity        Entity to apply the decal to or 0 for world
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_place_player_spray(position[3], player, spray = 0, entity = 0, receiver = 0, bool:reliable = true)

/**
 * Creates alpha sprites or models inside of a box that float upwards.
 *
 * @note A common sprite to use is "sprites/bubble.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m22s
 *
 * @param startpos      Start position
 * @param endpos        End position
 * @param sprite        Sprite index
 * @param count         Number of sprites to generate (0 - 255)
 * @param randomness    Randoness of the floating direction
 * @param height        Float height
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_bubble_box(startpos[3], endpos[3], sprite, count = 3, randomness = 0, height = 50, receiver = 0, bool:reliable = true)

/**
 * Creates alpha sprites or models along a line that float upwards.
 *
 * @note A common sprite to use is "sprites/bubble.spr"
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m37s
 *
 * @param startpos      Start position
 * @param endpos        End position
 * @param sprite        Sprite index
 * @param count         Number of sprites to generate (0 - 255)
 * @param randomness    Randoness of the floating direction
 * @param height        Float height
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_bubble_line(startpos[3], endpos[3], sprite, count = 3, randomness = 0, height = 50, receiver = 0, bool:reliable = true)

/**
 * Creates an spray of opaque sprites or models that fall to another sprite or model.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m44s
 *
 * @param position      Effect position
 * @param sprite1id     Primary sprite index
 * @param sprite2id     Secondary sprite index
 * @param color         Sprite color (https://wiki.alliedmods.net/images/Palette.png)
 * @param scale         Sprite scale (0 - 255)
 * @param receiver      Client index that will be able to see the sprite
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_display_falling_sprite(position[3], sprite1id, sprite2id, color = 78, scale = 10, receiver = 0, bool:reliable = true)

/**
 * Applies a decal to the world brush.
 *
 * @note Using a decal index that doesn't exist will crash the client.
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=9m56s
 *
 * @param position      Decal position (center of texture in world)
 * @param texture       Texture index of precached decal texture name (0 - 511)
 * @param receiver      Client index that will be able to see the decal
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_place_world_decal(position[3], texture, receiver = 0, bool:reliable = true)

/**
 * Creates a nail-like projectile.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=10m12s
 *
 * @param position      Projectile position
 * @param velocity      Projectile velocity
 * @param model         Model index that will be used for the projectile
 * @param life          The length of time the projectile shall remain (0 - 255)
 * @param owner         The projectile won't collide with the owner, if set to 0,
 *                      the projectile will hit any client
 * @param receiver      Client index that will be able to see the projectile
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_projectile(position[3], velocity[3], model, life = 30, owner = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a shower of sprites or models.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=10m27s
 *
 * @param position      Effect position
 * @param model         Model index that will be used for the effect
 * @param direction     Effect direction
 * @param count         Number of sprites/models to generate (0 - 255)
 * @param speed         The scroll speed of the effect (0 - 255)
 * @param noise         The noise amplitude of the effect - this
 *                      controls the distorsion of the effect (0 - 255)
 * @param rendermode    Render mode - one of kRender* constants
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_sprite_shower(position[3], model, direction[3] = {0, 0, 0}, count = 1, speed = 0, noise = 0, rendermode = kRenderNormal, receiver = 0, bool:reliable = true)

/**
 * Emits sprites or models from a player's bounding box.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=10m39s
 *
 * @param player        Client index to emit the sprites from (can't be 0)
 * @param sprite        Sprite index
 * @param count         Number of sprites to generate (0 - 255)
 * @param variance      Variance in size (0 = no variance; 10 = 10% variance) (0 - 255)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_emit_sprite_from_player(player, sprite, count = 1, variance = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a particle burst similar to te_create_lava_splash.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=10m55s
 *
 * @param position      Effect position
 * @param radius        Effect radius
 * @param color         Particle color (https://wiki.alliedmods.net/images/Palette.png)
 * @param duration      Duration of the effect (will be randomized a bit) (0 - 255)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_particle_burst(position[3], radius = 30, color = 106, duration = 5, receiver = 0, bool:reliable = true)

/**
 * Creates a field of fire.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=11m7s
 *
 * @param position      Effect position
 * @param sprite        Sprite or model index to use for the fire
 * @param radius        Effect radius
 * @param count         Number of fields to generate (0 - 255)
 * @param duration      Duration of the effect (will be randomized a bit) (0 - 255)
 * @param flags         Available flags:
 *                        TEFIRE_FLAG_ALLFLOAT    - all sprites will drift upwards as they animate
 *                        TEFIRE_FLAG_SOMEFLOAT   - some of the sprites will drift upwards (50% chance)
 *                        TEFIRE_FLAG_LOOP        - if set, sprite plays at 15 fps, otherwise plays at whatever rate stretches the animation over the sprite's duration
 *                        TEFIRE_FLAG_ALPHA       - if set, sprite is rendered alpha blended at 50% else, opaque
 *                        TEFIRE_FLAG_PLANAR      - if set, all fire sprites have same initial Z instead of randomly filling a cube
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_fire_field(position[3], sprite, radius = 5, count = 1, duration = 10, flags = TEFIRE_FLAG_ALLFLOAT, receiver = 0, bool:reliable = true)

/**
 * Attaches a temporary entity model to a client.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=11m16s
 *
 * @param player        Client index to attach the model to
 * @param model         Model index
 * @param offset        Vertical offset (attachment origin.z = player origin.z + vertical offset)
 * @param life          The length of time the model shall remain
 * @param receiver      Client index that will be able to see the model
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_attach_model_to_player(player, model, offset = 0, life = 5, receiver = 0, bool:reliable = true)

/**
 * Kills all temporary entity models attached to a client.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=11m24s
 *
 * @param player        Client index to remove the attachments from
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_remove_all_player_attachments(player, receiver = 0, bool:reliable = true)

/**
 * Much more compact shotgun shot native.
 *
 * @note This native is used to make a client approximate a 'spray' of gunfire.
 *       Any weapon that fires more than one bullet per frame and fires in a bit
 *       of a spread is a good candidate for MULTIGUNSHOT use. (shotguns)
 * @note This effect makes the client do traces for each bullet, these client
 *       traces ignore entities that have studio models. Traces are 4096 long.
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=11m27s
 *
 * @param position      Gunshot position
 * @param direction     Gunshot direction
 * @param decal         Bullethole decal texture index (0 - 255)
 * @param count         Number of bulletholes to generate (0 - 255)
 * @param noise_x       X noise multiplied by 100
 * @param noise_y       Y noise multiplied by 100
 * @param receiver      Client index that will be able to see the Gunshot
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_multi_gunshot(position[3], direction[3], decal = 105, count = 1, noise_x = 0, noise_y = 0, receiver = 0, bool:reliable = true)

/**
 * Creates a tracer effect and allows more customization than te_create_tracer.
 *
 * @note Video preview of this and all other te_ natives can be found here:
 *       https://youtu.be/szW-bSMPuyQ?t=11m36s
 *
 * @param position      Effect position
 * @param velocity      Effect velocity
 * @param life          The length of time the effect shall remain, multiplied by 10 (0 - 255)
 * @param color         Tracer color (https://wiki.alliedmods.net/images/Palette.png)
 * @param length        Length of the tracer (0 - 255)
 * @param receiver      Client index that will be able to see the effect
 *                      or 0 for all clients
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "receiver" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native te_create_user_tracer(position[3], velocity[3], life = 1, color = 106, length = 1, receiver = 0, bool:reliable = true)
