/* BrainBread Fun Module
 *
 * (c) 2005, XxAvalanchexX (converted to module by Rukia)
 *
 * This file is provided as is (no warranties).
 */

#if defined _brainbread_included
  #endinput
#endif
#define _brainbread_included

#pragma library BBFUN

#include <bb_const>
#include <bb_stocks>

native bb_is_user_zombie(id)
native bb_reset_user_hud(id)

native bb_show_message(id,Float:time = -1,message[],message2[] = "")
native bb_show_objective(id,message[] = "")
native bb_show_progress_bar(id,message[],time = 10)

native bb_get_user_skill(id)
native bb_set_user_skill(id,skill)
stock bb_add_user_skill(id,skill) bb_set_user_skill(id,bb_get_user_skill(id) + skill)

native Float:bb_get_user_exp(id)
native bb_set_user_exp(id,Float:exp)
stock bb_add_user_exp(id,Float:exp) bb_set_user_exp(id,bb_get_user_exp(id) + exp)

native Float:bb_get_user_points(id)
native bb_set_user_points(id,Float:points)
stock bb_add_user_points(id,Float:points) bb_set_user_ponts(id,bb_get_user_points(id) + points)

native bb_get_user_level(id)
native bb_set_user_level(id,level)
stock bb_add_user_level(id,level) bb_set_user_level(id,bb_get_user_level(id) + level)

native bb_get_user_speed(id)
native bb_set_user_speed(id,speed)
stock bb_add_user_speed(id,speed) bb_set_user_speed(id,bb_get_user_speed(id) + speed)

native bb_get_user_hitpoints(id)
native bb_set_user_hitpoints(id,hitpoints)
stock bb_add_user_hitpoints(id,hitpoints) bb_set_user_hitpoints(id,bb_get_user_hitpoints(id) + hitpoints)