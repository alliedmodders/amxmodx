/* NS2AMX.INC
 * Declares all natives/forwards for the ns2amxx module.
 * version 0.2
 */
#if defined __ns2amxx__
 	#endinput
#endif
#define __ns2amxx__ 02
#include <ns2amxxdefines>

/* Returns the number of specified entities in map if value is 0
   otherwise it returns the index of the #th entity of that class. */
native ns_get_ent(classname[],Value);

/* Same as get_ent, but only returns fully built structures (if Value > 0) */
native ns_get_build(classname[],Value,Number=0,NSVersion=NS_VERSION);

/* Get's spawn point for specified team (type). */
native ns_get_spawn(type,number=0,Float:ret[3]);

/* Returns 1 if the map is combat, 0 otherwise. */
native ns_is_combat()

/* Used to change the speed of players.  Since this can vary, a static speed isn't set.
   Rather, the module will add the speedchange to the player's maxspeed every frame.
   Set speedchange to a negative value to slow the player down. */
native ns_get_speedchange(id)
native ns_set_speedchange(id,speed)
native ns_get_maxspeed(id)

/* Forces a player to spawn.  I would not use this while the player's deadflags = 1 or 0.  
   Only use when the player is dead with deadflags of 2 or higher. 
   For class, use the fields specified in the NS Mini SDK (1=marine,3=skulk,4=gorge,5=lerk,6=fade,7=onos) */
native ns_spawn_player(id,class=1,health=100,armor=25)

/* Configures display of the custom menus.  This display is _very_ similar to set_hudmessage() */
native ns_set_menu(id,r=NSMENU_R,g=NSMENU_G,b=NSMENU_B,x=NSMENU_X,y=NSMENU_Y, effects=0, Float:fadeintime=0.0, Float:fadeouttime=0.0,channel1=4,channel2=3)

/* Displays a custom menu.  It will execute command(id,keys) when the menu is pressed. */
native ns_show_menu(id,szCommand[],szText[],keys,time)

/* Forces ns2amx to set the player's model/skin/body to the specified each frame.
   Note that the model will not go back to standard when the player dies/changes classes.
   Your plugin will have to tell the module to stop using the custom model.  
   Leave the second parameter out of any of these commands to force the module to go back to standard. */
native ns_set_player_model(id,szModel[]="")
native ns_set_player_skin(id,skin=-1)
native ns_set_player_body(id,body=-1)

#include <ns2amxxhelpers>
