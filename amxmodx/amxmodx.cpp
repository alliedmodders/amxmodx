/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#include <string>
#include <time.h>
#include "amxmodx.h"

static cell AMX_NATIVE_CALL get_xvar_id(AMX *amx, cell *params)
{
  int len;
  char* sName = get_amxstring(amx,params[1],0,len);
  cell ptr;

  for ( CPluginMngr::iterator a = g_plugins.begin(); a ; ++a )
  {
  if ( (*a).isValid() && amx_FindPubVar( (*a).getAMX() , sName , &ptr ) == AMX_ERR_NONE )
    return g_xvars.put( (*a).getAMX() , get_amxaddr(  (*a).getAMX() , ptr )  );
  }
  return -1;
}

static cell AMX_NATIVE_CALL get_xvar_num(AMX *amx, cell *params)
{
  return  g_xvars.getValue(params[1]);
}

static cell AMX_NATIVE_CALL set_xvar_num(AMX *amx, cell *params)
{
  if ( g_xvars.setValue( params[1] , params[2] ) ){
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
  return 1;
}

static cell AMX_NATIVE_CALL xvar_exists(AMX *amx, cell *params)
{
   return (get_xvar_id( amx , params ) != -1) ? 1 : 0;
}

static cell AMX_NATIVE_CALL emit_sound(AMX *amx, cell *params) /* 7 param */
{

  int len;
  char* szSample = get_amxstring(amx,params[3],0,len);
  float vol = *(REAL *)((void *)&params[4]);
  float att = *(REAL *)((void *)&params[5]);
  int channel = params[2];
  int pitch = params[7];
  int flags = params[6];

  if (params[1] == 0) {
    for(int i = 1; i <= gpGlobals->maxClients ; ++i){
      CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
      if (pPlayer->ingame)
        EMIT_SOUND_DYN2(pPlayer->pEdict,channel, szSample, vol, att,flags , pitch);
    }
  }
  else
  {
  edict_t* pEdict = INDEXENT(params[1]);
  if (!FNullEnt(pEdict))
    EMIT_SOUND_DYN2(pEdict,channel , szSample, vol, att, flags, pitch);
  }

  return 1;
}

static cell AMX_NATIVE_CALL server_print(AMX *amx, cell *params) /* 1 param */
{
  int len;
  g_langMngr.SetDefLang(LANG_SERVER);			// Default language = server
  char* message = format_amxstring(amx,params,1,len);
  if ( len > 254 ) len = 254;
  message[len++]='\n';
  message[len]=0;
  SERVER_PRINT( message );
  return len;
}

static cell AMX_NATIVE_CALL engclient_print(AMX *amx, cell *params)  /* 3 param */
{
	int len;
	char *msg;
	if (params[1] == 0)
	{
		for(int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
			if (pPlayer->ingame)
			{
				g_langMngr.SetDefLang(i);
				msg = format_amxstring(amx, params, 3, len);
				msg[len++] = '\n';
				msg[len] = 0;
				CLIENT_PRINT(pPlayer->pEdict, (PRINT_TYPE)(int)params[2], msg);
			}
		}
	}
	else
	{
		int index = params[1];
		if (index < 1 || index > gpGlobals->maxClients){
			amx_RaiseError(amx,AMX_ERR_NATIVE);
			return 0;
		}
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
		if (pPlayer->ingame)
		{
			g_langMngr.SetDefLang(index);
			msg = format_amxstring(amx, params, 3, len);
			msg[len++] = '\n';
			msg[len] = 0;
			CLIENT_PRINT(pPlayer->pEdict, (PRINT_TYPE)(int)params[2], msg);
		}
	}
	return len;
}

static cell AMX_NATIVE_CALL console_cmd(AMX *amx, cell *params) /* 2 param */
{
  int index = params[1];
  g_langMngr.SetDefLang(index);
  int len;
  char* cmd = format_amxstring(amx,params,2,len);
  cmd[len++]='\n';
  cmd[len]=0;

  if (index < 1 || index > gpGlobals->maxClients){
    SERVER_COMMAND( cmd );
  }
  else{
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
    if ( !pPlayer->bot && pPlayer->initialized )  CLIENT_COMMAND(pPlayer->pEdict, cmd );
  }

  return len;
}

static cell AMX_NATIVE_CALL console_print(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];
	if (index < 1 || index > gpGlobals->maxClients)
		g_langMngr.SetDefLang(LANG_SERVER);
	else
		g_langMngr.SetDefLang(index);

	int len;
	char* message = format_amxstring(amx,params,2,len);
	if (len > 254 )
		len = 254;
	message[len++] = '\n';
	message[len] = 0;
	if (index < 1 || index > gpGlobals->maxClients)
		SERVER_PRINT( message );
	else
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
		if (pPlayer->ingame)
			UTIL_ClientPrint(pPlayer->pEdict, 2 , message );
	}
	return len;
}


static cell AMX_NATIVE_CALL client_print(AMX *amx, cell *params) /* 3 param */
{
	int len;
	char *msg;
	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
			if (pPlayer->ingame)
			{
				g_langMngr.SetDefLang(i);
				msg = format_amxstring(amx, params, 3, len);
				msg[len++] = '\n';
				msg[len] = 0;
				UTIL_ClientPrint(NULL, params[2], msg);
			}
		}
	}
	else
	{
		int index = params[1];
		if (index < 1 || index > gpGlobals->maxClients)
		{
			amx_RaiseError(amx,AMX_ERR_NATIVE);
			return 0;
		}
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
		g_langMngr.SetDefLang(index);
		msg = format_amxstring(amx, params, 3, len);
		msg[len++] = '\n';
		msg[len] = 0;
		if (pPlayer->ingame)
			UTIL_ClientPrint(pPlayer->pEdict, params[2], format_amxstring(amx, params, 3, len));
	}
	return len;
}

static cell AMX_NATIVE_CALL show_motd(AMX *amx, cell *params)  /* 2 param */
{
  int ilen;
  const char* szHead = get_amxstring(amx,params[3],0,ilen);
  if ( !ilen ) szHead = hostname->string;
  char* szBody = get_amxstring(amx,params[2],1,ilen);
  int iFile = 0;
  char* sToShow = NULL;// = szBody;
  if (ilen<128) sToShow = (char*)LOAD_FILE_FOR_ME( szBody , &iFile );
  if (!iFile)
    sToShow = szBody;
  else
    ilen = iFile;
  if (params[1] == 0) {
    for(int i = 1; i <= gpGlobals->maxClients; ++i){
      CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
      if (pPlayer->ingame)
        UTIL_ShowMOTD(pPlayer->pEdict, sToShow , ilen, szHead);
    }
  }
  else {
    int index = params[1];
    if (index < 1 || index > gpGlobals->maxClients){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      if (iFile) FREE_FILE(sToShow);
      return 0;
    }
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
    if (pPlayer->ingame)
      UTIL_ShowMOTD(pPlayer->pEdict, sToShow,ilen, szHead );
  }
  if (iFile) FREE_FILE(sToShow);
  return 1;
}

static cell AMX_NATIVE_CALL set_hudmessage(AMX *amx, cell *params)  /* 11 param */
{
  g_hudset.a1 = 0;
  g_hudset.a2 = 0;
  g_hudset.r2 = 255;
  g_hudset.g2 = 255;
  g_hudset.b2 = 250;
  g_hudset.r1 = params[1];
  g_hudset.g1 = params[2];
  g_hudset.b1 = params[3];
  g_hudset.x = *(REAL *)((void *)&params[4]);
  g_hudset.y = *(REAL *)((void *)&params[5]);
  g_hudset.effect = params[6];
  g_hudset.fxTime = *(REAL *)((void *)&params[7]);
  g_hudset.holdTime = *(REAL *)((void *)&params[8]);
  g_hudset.fadeinTime = *(REAL *)((void *)&params[9]);
  g_hudset.fadeoutTime = *(REAL *)((void *)&params[10]);
  g_hudset.channel = params[11];
  return 1;
}

static cell AMX_NATIVE_CALL show_hudmessage(AMX *amx, cell *params)  /* 2 param */
{
  int len;
  g_langMngr.SetDefLang(params[1]);
  char* message = UTIL_SplitHudMessage(  format_amxstring(amx,params,2,len) );
  if (params[1] == 0) {
    UTIL_HudMessage(NULL, g_hudset, message );
  }
  else {
    int index = params[1];
    if (index < 1 || index > gpGlobals->maxClients){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      return 0;
    }
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
    if (pPlayer->ingame)
      UTIL_HudMessage(pPlayer->pEdict, g_hudset, message );
  }
  return len;
}

static cell AMX_NATIVE_CALL get_user_name(AMX *amx, cell *params)  /* 3 param */
{
  int index = params[1];
  return set_amxstring(amx,params[2],(index<1||index>gpGlobals->maxClients) ?
    hostname->string : g_players[index].name.str() , params[3]);
}

static cell AMX_NATIVE_CALL get_user_index(AMX *amx, cell *params)  /* 1 param */
{
  int i;
  char* sptemp = get_amxstring(amx,params[1],0,i);
  for(i = 1; i <= gpGlobals->maxClients; ++i) {
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
    if ( strcmp(pPlayer->name.str(), sptemp) == 0 )
      return i;
  }

  return 0;
}

static cell AMX_NATIVE_CALL is_dedicated_server(AMX *amx, cell *params)
{
  return (IS_DEDICATED_SERVER() ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_linux_server(AMX *amx, cell *params)
{
#ifdef __linux__
  return 1;
#else
  return 0;
#endif
}

static cell AMX_NATIVE_CALL is_jit_enabled(AMX *amx, cell *params)		// PM: Useless ;P
{
#ifdef JIT
	return 1;
#else
	return 0;
#endif
}

static cell AMX_NATIVE_CALL is_map_valid(AMX *amx, cell *params)  /* 1 param */
{
  int ilen;
  return (IS_MAP_VALID(get_amxstring(amx,params[1],0,ilen)) ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_connected(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return (pPlayer->ingame ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_connecting(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return ( !pPlayer->ingame && pPlayer->initialized
    && (GETPLAYERUSERID(pPlayer->pEdict)>0) ) ? 1 : 0;
}

static cell AMX_NATIVE_CALL is_user_bot(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  return (GET_PLAYER_POINTER_I(index)->bot ? 1 : 0);
}


static cell AMX_NATIVE_CALL is_user_hltv(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  return ((GET_PLAYER_POINTER_I(index)->pEdict->v.flags & FL_PROXY) ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_alive(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return ((pPlayer->ingame && pPlayer->IsAlive()) ? 1 : 0);
}

static cell AMX_NATIVE_CALL get_user_frags(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.frags : 0);
}

static cell AMX_NATIVE_CALL get_user_deaths(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return (cell)(pPlayer->ingame ? pPlayer->deaths : 0);
}


static cell AMX_NATIVE_CALL get_user_armor(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.armorvalue : 0);
}

static cell AMX_NATIVE_CALL get_user_health(AMX *amx, cell *params) /*  param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.health : 0);
}

static cell AMX_NATIVE_CALL get_user_userid(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  return pPlayer->initialized ? GETPLAYERUSERID(pPlayer->pEdict) : -1;
}


static cell AMX_NATIVE_CALL get_user_authid(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  const char* authid = 0;
  if (index>0&&index<=gpGlobals->maxClients)
  authid = GETPLAYERAUTHID(g_players[index].pEdict);
  return set_amxstring(amx,params[2], authid ? authid : "" ,params[3]);
}

static cell AMX_NATIVE_CALL is_user_authorized(AMX *amx, cell *params)
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients)
    return 0;
  return GET_PLAYER_POINTER_I(index)->authorized;
}

static cell AMX_NATIVE_CALL get_weaponname(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 1 || index >= MAX_WEAPONS ){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  return set_amxstring(amx,params[2],g_weaponsData[index].fullName.str(),params[3]);
}

static cell AMX_NATIVE_CALL get_user_weapons(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    cell *cpNum = get_amxaddr(amx,params[3]);
    cell *cpIds = get_amxaddr(amx,params[2]);
	*cpIds = 0;
    int weapons = pPlayer->pEdict->v.weapons & ~(1<<31); // don't count last element
    for(int i = 1; i < MAX_WEAPONS; ++i){
      if (weapons & (1<<i)) {
        *(cpIds+(*cpNum)) = i;
        (*cpNum)++;
      }
    }
    return weapons;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_origin(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    int mode = params[3];
    cell *cpOrigin = get_amxaddr(amx,params[2]);
    if (mode == 4) {
      cpOrigin[0] = (long int)pPlayer->lastHit.x;
      cpOrigin[1] = (long int)pPlayer->lastHit.y;
      cpOrigin[2] = (long int)pPlayer->lastHit.z;
      return 1;
    }
    edict_t* edict = pPlayer->pEdict;
    Vector pos = edict->v.origin;
    if (mode && mode!=2)
      pos = pos + edict->v.view_ofs;
    if (mode > 1) {
      Vector vec;
      Vector v_angle = edict->v.v_angle;
      float v_vec[3];
      v_vec[0] = v_angle.x;
      v_vec[1] = v_angle.y;
      v_vec[2] = v_angle.z;
      ANGLEVECTORS( v_vec, vec, NULL, NULL);
      TraceResult trEnd;
      Vector v_dest = pos+vec  * 9999;
      float f_pos[3];
      f_pos[0] = pos.x;
      f_pos[1] = pos.y;
      f_pos[2] = pos.z;
      float f_dest[3];
      f_dest[0] = v_dest.x;
      f_dest[1] = v_dest.y;
      f_dest[2] = v_dest.z;
      TRACE_LINE( f_pos , f_dest,  0 , edict, &trEnd );
      pos = (trEnd.flFraction < 1.0) ? trEnd.vecEndPos : Vector(0,0,0);
    }
    cpOrigin[0] = (long int)pos.x;
    cpOrigin[1] = (long int)pos.y;
    cpOrigin[2] = (long int)pos.z;
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_ip(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  char *ptr;
  char szIp[32];
  strcpy(szIp,(index<1||index>gpGlobals->maxClients)?
    CVAR_GET_STRING("net_address"):g_players[index].ip.str());
  if (params[4] && (ptr = strstr(szIp,":"))!=0)
    *ptr = '\0';
  return set_amxstring(amx,params[2],szIp,params[3]);
}

static cell AMX_NATIVE_CALL get_user_attacker(AMX *amx, cell *params) /* 2 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  edict_t *enemy = NULL;
  if (pPlayer->ingame){
    enemy = pPlayer->pEdict->v.dmg_inflictor;
    if (!FNullEnt(enemy)) {
      int weapon = 0;
      if (enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT) ){
        pPlayer = GET_PLAYER_POINTER(enemy);
        weapon = pPlayer->current;
      }
      else if( g_grenades.find( enemy, &pPlayer, weapon  ) )
        enemy = pPlayer->pEdict;
      else
        enemy = NULL;

      if (enemy){
        switch(*params/sizeof(cell)){
        case 3: *get_amxaddr(amx,params[3]) = pPlayer->aiming;
        case 2: *get_amxaddr(amx,params[2]) = weapon;
        }
      }
    }
  }
  return (enemy ? pPlayer->index : 0);
}

static cell AMX_NATIVE_CALL user_has_weapon(AMX *amx,cell *params)
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  edict_t *pEntity = pPlayer->pEdict;
  if (params[3] == -1)
  {
    if ((pEntity->v.weapons & (1<<params[2])) > 0)
    {
      return 1;
    }
  }
  else
  {
    if ((pEntity->v.weapons & (1<<params[2])) > 0)
    {
      if (params[3] == 0)
      {
        pEntity->v.weapons &= ~(1<<params[2]);
        return 1;
      }
      return 0;
    }
    else
    {
      if (params[3] == 1)
      {
        pEntity->v.weapons |= (1<<params[2]);
        return 1;
      }
    }
    return 0;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
  int wpn = pPlayer->current;
    cell *cpTemp = get_amxaddr(amx,params[2]);
    *cpTemp = pPlayer->weapons[wpn].clip;
    cpTemp = get_amxaddr(amx,params[3]);
    *cpTemp = pPlayer->weapons[wpn].ammo;
    return wpn;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_ammo(AMX *amx, cell *params) /* 4 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    int wpn = params[2];
    if (wpn < 1 || wpn >= MAX_WEAPONS ){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      return 0;
    }
    cell *cpTemp = get_amxaddr(amx,params[3]);
    *cpTemp = pPlayer->weapons[wpn].clip;
    cpTemp = get_amxaddr(amx,params[4]);
    *cpTemp = pPlayer->weapons[wpn].ammo;
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_team(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return -1;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    if ( params[3] )
    set_amxstring(amx,params[2],pPlayer->team.str(),params[3]);
    return pPlayer->teamId;
  }
  return -1;
}

static cell AMX_NATIVE_CALL show_menu(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sMenu = get_amxstring(amx,params[3],0,ilen);
  int numparam = *params/sizeof(cell);
  int menuid = 0;
  if (numparam == 4)
    menuid = g_menucmds.findMenuId(get_amxstring(amx, params[4], 1, ilen), amx);
  else
    menuid = g_menucmds.findMenuId(sMenu, amx);
  int keys = params[2];
  int time = params[4];
  if (params[1] == 0) {
    for(int i = 1; i <= gpGlobals->maxClients; ++i){
      CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
      if (pPlayer->ingame){
        pPlayer->keys = keys;
        pPlayer->menu = menuid;
        UTIL_ShowMenu(pPlayer->pEdict, keys, time, sMenu, ilen );
      }
    }
  }
  else {
    int index = params[1];
    if (index < 1 || index > gpGlobals->maxClients){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      return 0;
    }
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
    if (pPlayer->ingame){
      pPlayer->keys = keys;
      pPlayer->menu = menuid;
      UTIL_ShowMenu(pPlayer->pEdict, keys, time, sMenu, ilen );
    }
  }
  return 1;
}



static cell AMX_NATIVE_CALL register_plugin(AMX *amx, cell *params) /* 3 param */
{
  CPluginMngr::CPlugin* a = g_plugins.findPluginFast( amx );
  int i;
  a->setTitle(  get_amxstring(amx,params[1],0,i) );
  a->setVersion( get_amxstring(amx,params[2],0,i) );
  a->setAuthor(  get_amxstring(amx,params[3],0,i)  );
  return 1;
}

static cell AMX_NATIVE_CALL register_menucmd(AMX *amx, cell *params) /* 3 param */
{
  CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast( amx );
  int ilen, idx;
  char* sptemp = get_amxstring(amx,params[3],0,ilen);

  if(amx_FindPublic(amx, sptemp ,&idx)!=AMX_ERR_NONE) {
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",sptemp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }

  g_menucmds.registerMenuCmd( plugin , params[1] ,  params[2] , idx );

  return 1;
}

static cell AMX_NATIVE_CALL get_plugin(AMX *amx, cell *params) /* 11 param */
{
  CPluginMngr::CPlugin* a;

  if ( params[1]<0 )
  a = g_plugins.findPluginFast( amx );
  else
  a = g_plugins.findPlugin( (int)params[1] );

  if (a){
    set_amxstring(amx,params[2],a->getName(),params[3]);
    set_amxstring(amx,params[4],a->getTitle(),params[5]);
    set_amxstring(amx,params[6],a->getVersion(),params[7]);
    set_amxstring(amx,params[8],a->getAuthor(),params[9]);
    set_amxstring(amx,params[10],a->getStatus(),params[11]);
    return a->getId();
  }
  return -1;
}

static cell AMX_NATIVE_CALL get_pluginsnum(AMX *amx, cell *params)
{
  return g_plugins.getPluginsNum();
}



static cell AMX_NATIVE_CALL register_concmd(AMX *amx, cell *params) /* 4 param */
{
  CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast( amx );
  int i, idx = 0;
  char* temp = get_amxstring(amx,params[2],0, i );
  if(amx_FindPublic(amx, temp ,&idx)!=AMX_ERR_NONE) {
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",temp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  temp = get_amxstring(amx,params[1],0, i );
  char* info = get_amxstring(amx,params[4],1, i );
  CmdMngr::Command* cmd;
  int access = params[3];
  bool listable = true;
  if ( access < 0 ) { // is access is -1 then hide from listing
  access = 0;
  listable = false;
  }
  if ( (cmd = g_commands.registerCommand( plugin , idx ,  temp ,  info , access ,listable )) == NULL)
    return 0;
  cmd->setCmdType(  CMD_ConsoleCommand );

  REG_SVR_COMMAND( (char*)cmd->getCommand() , plugin_srvcmd );
  return 1;
}

static cell AMX_NATIVE_CALL register_clcmd(AMX *amx, cell *params) /* 4 param */
{
  CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast( amx );
  int i, idx = 0;
  char* temp = get_amxstring(amx,params[2],0, i );
  if(amx_FindPublic(amx, temp ,&idx)!=AMX_ERR_NONE) {
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",temp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  temp = get_amxstring(amx,params[1],0, i );
  char* info = get_amxstring(amx,params[4],1, i );
  CmdMngr::Command* cmd;
  int access = params[3];
  bool listable = true;
  if ( access < 0 ) { // is access is -1 then hide from listing
  access = 0;
  listable = false;
  }
  if ( (cmd = g_commands.registerCommand(plugin , idx , temp ,  info , access , listable )) == NULL)
    return 0;
  cmd->setCmdType( CMD_ClientCommand );
  return 1;
}

static cell AMX_NATIVE_CALL register_srvcmd(AMX *amx, cell *params) /* 2 param */
{
  CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast( amx );
  int i, idx = 0;
  char* temp = get_amxstring(amx,params[2],0, i );
  if(amx_FindPublic(amx, temp ,&idx)!=AMX_ERR_NONE) {
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",temp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  temp = get_amxstring(amx,params[1],0, i );
  char* info = get_amxstring(amx,params[4],1, i );
  CmdMngr::Command* cmd;
  int access = params[3];
  bool listable = true;
  if ( access < 0 ) { // is access is -1 then hide from listing
  access = 0;
  listable = false;
  }
  if ( (cmd = g_commands.registerCommand(plugin ,idx , temp , info ,access,listable )) == NULL)
    return 0;
  cmd->setCmdType( CMD_ServerCommand );
  REG_SVR_COMMAND( (char*)cmd->getCommand()  , plugin_srvcmd );
  return 0;
}

static cell AMX_NATIVE_CALL get_concmd(AMX *amx, cell *params) /* 7 param */
{
  int who = params[8];

  if ( who > 0 ) // id of player - client command
    who = CMD_ClientCommand;
  else if ( who == 0 ) // server
    who = CMD_ServerCommand;
  else // -1 parameter - all commands
    who = CMD_ConsoleCommand;

  CmdMngr::Command* cmd = g_commands.getCmd(params[1] ,who  , params[7] );

  if ( cmd == 0 ) return 0;
    set_amxstring(amx,params[2], cmd->getCmdLine() ,params[3]);
    set_amxstring(amx,params[5], cmd->getCmdInfo() ,params[6]);
    cell *cpFlags = get_amxaddr(amx,params[4]);
    *cpFlags = cmd->getFlags();
    return 1;
}

static cell AMX_NATIVE_CALL get_clcmd(AMX *amx, cell *params) /* 7 param */
{
  CmdMngr::Command* cmd = g_commands.getCmd(params[1] ,CMD_ClientCommand ,  params[7]);
  if ( cmd == 0 ) return 0;
    set_amxstring(amx,params[2], cmd->getCmdLine() ,params[3]);
    set_amxstring(amx,params[5], cmd->getCmdInfo() ,params[6]);
    cell *cpFlags = get_amxaddr(amx,params[4]);
    *cpFlags = cmd->getFlags();

    return 1;
}

static cell AMX_NATIVE_CALL get_srvcmd(AMX *amx, cell *params)
{
  CmdMngr::Command* cmd = g_commands.getCmd(params[1] ,CMD_ServerCommand  , params[7] );
  if ( cmd == 0 ) return 0;
    set_amxstring(amx,params[2], cmd->getCmdLine() ,params[3]);
    set_amxstring(amx,params[5], cmd->getCmdInfo() ,params[6]);
    cell *cpFlags = get_amxaddr(amx,params[4]);
    *cpFlags = cmd->getFlags();
    return 1;
}

static cell AMX_NATIVE_CALL get_srvcmdsnum(AMX *amx, cell *params)
{
  return g_commands.getCmdNum(  CMD_ServerCommand  ,  params[1] );
}

static cell AMX_NATIVE_CALL get_clcmdsnum(AMX *amx, cell *params) /* 1 param */
{
  return g_commands.getCmdNum(  CMD_ClientCommand  ,  params[1] );
}

static cell AMX_NATIVE_CALL get_concmdsnum(AMX *amx, cell *params) /* 1 param */
{
  int who = params[2];
  if ( who > 0 )
  return g_commands.getCmdNum(  CMD_ClientCommand  ,  params[1] );
  if ( who == 0 )
  return g_commands.getCmdNum(  CMD_ServerCommand  ,  params[1] );
  return g_commands.getCmdNum(  CMD_ConsoleCommand  ,  params[1] );
}


static cell AMX_NATIVE_CALL register_event(AMX *amx, cell *params) /* 2 param */
{
  CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast( amx );

  int len, pos, iFunction;

  char* sTemp = get_amxstring(amx,params[1],0,len);

  if ( (pos = g_events.getEventId(  sTemp  )) == 0 ) {
    AMXXLOG_Log("[AMXX] Invalid event (name \"%s\") (plugin \"%s\")", sTemp , plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }

  sTemp = get_amxstring(amx,params[2],0,len);

  if ( amx_FindPublic(amx, sTemp , &iFunction) != AMX_ERR_NONE){
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",sTemp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }

  int numparam = *params/sizeof(cell);

  int flags = 0;

  if ( numparam > 2)
    flags = UTIL_ReadFlags( get_amxstring(amx,params[3],0,len) );

  EventsMngr::ClEvent* a =
    g_events.registerEvent( plugin , iFunction , flags , pos   );

  if ( a == 0 ) return 0;

  for(int i = 4; i <= numparam; ++i)
  a->registerFilter( get_amxstring(amx,params[i],0,len) );

  return 1;
}

static cell AMX_NATIVE_CALL user_kill(AMX *amx, cell *params) /* 2 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame && pPlayer->IsAlive()){
  float bef = pPlayer->pEdict->v.frags;
    MDLL_ClientKill(pPlayer->pEdict);
    if (params[2]) pPlayer->pEdict->v.frags = bef;
    return 1;
  }

  return 0;
}

static cell AMX_NATIVE_CALL user_slap(AMX *amx, cell *params) /* 2 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  int power = abs((int)params[2]);
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame && pPlayer->IsAlive()){
    if (pPlayer->pEdict->v.health <= power) {
      float bef = pPlayer->pEdict->v.frags;
      MDLL_ClientKill(pPlayer->pEdict);
      pPlayer->pEdict->v.frags = bef;
    }
    else {
      edict_t *pEdict = pPlayer->pEdict;
      int numparam = *params/sizeof(cell);
      if (numparam<3 || params[3]) {
        pEdict->v.velocity.x += RANDOM_LONG(-600,600);
        pEdict->v.velocity.y += RANDOM_LONG(-180,180);
        pEdict->v.velocity.z += RANDOM_LONG(100,200);
      }
      else {
        Vector v_forward, v_right;
	Vector vang = pEdict->v.angles;
	float fang[3];
	fang[0] = vang.x;
	fang[1] = vang.y;
	fang[2] = vang.z;
        ANGLEVECTORS( fang, v_forward, v_right, NULL );
        pEdict->v.velocity = pEdict->v.velocity + v_forward * 220 + Vector(0,0,200);
      }
      pEdict->v.punchangle.x = RANDOM_LONG(-10,10);
      pEdict->v.punchangle.y = RANDOM_LONG(-10,10);
      pEdict->v.health -= power;
      int armor = (int)pEdict->v.armorvalue;
      armor -= power;
      if (armor < 0) armor = 0;
      pEdict->v.armorvalue = armor;
      pEdict->v.dmg_inflictor = pEdict;
      if (g_bmod_cstrike){
        static const char *cs_sound[4] = {
          "player/bhit_flesh-3.wav",
          "player/bhit_flesh-2.wav",
          "player/pl_die1.wav",
          "player/pl_pain6.wav"  };
        EMIT_SOUND_DYN2(pEdict, CHAN_VOICE, cs_sound[RANDOM_LONG(0,3)], 1.0, ATTN_NORM, 0, PITCH_NORM);
      }
      else{
        static const char *bit_sound[3] = {
          "weapons/cbar_hitbod1.wav",
          "weapons/cbar_hitbod2.wav",
          "weapons/cbar_hitbod3.wav" };
        EMIT_SOUND_DYN2(pEdict, CHAN_VOICE, bit_sound[RANDOM_LONG(0,2)], 1.0, ATTN_NORM, 0, PITCH_NORM);
      }
    }
    return 1;
  }

  return 0;
}

static cell AMX_NATIVE_CALL server_cmd(AMX *amx, cell *params) /* 1 param */
{
  int len;
  g_langMngr.SetDefLang(LANG_SERVER);
  char* cmd = format_amxstring(amx,params,1,len);
  cmd[len++]='\n';
  cmd[len]=0;
  SERVER_COMMAND( cmd );
  return len;
}

static cell AMX_NATIVE_CALL client_cmd(AMX *amx, cell *params) /* 2 param */
{
  int len;
  char* cmd = format_amxstring(amx,params,2,len);
  cmd[len++]='\n';
  cmd[len]=0;

  if (params[1] == 0) {
    for(int i = 1; i <= gpGlobals->maxClients; ++i){
      CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
      if (!pPlayer->bot &&  pPlayer->initialized /*&& pPlayer->ingame*/ )
        CLIENT_COMMAND(pPlayer->pEdict, cmd );
    }
  }
  else {
    int index = params[1];
    if (index < 1 || index > gpGlobals->maxClients){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      return 0;
    }
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
    if ( !pPlayer->bot && pPlayer->initialized /*&& pPlayer->ingame*/ )
    CLIENT_COMMAND(pPlayer->pEdict, cmd );
  }
  return len;
}

static cell AMX_NATIVE_CALL get_cvar_string(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  return set_amxstring(amx,params[2],CVAR_GET_STRING(sptemp),params[3]);
}

static cell AMX_NATIVE_CALL get_cvar_float(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  REAL pFloat = CVAR_GET_FLOAT(get_amxstring(amx,params[1],0,ilen));
  return *(cell*)((void *)&pFloat);
}

static cell AMX_NATIVE_CALL set_cvar_float(AMX *amx, cell *params) /* 2 param */
{
  int ilen;
  CVAR_SET_FLOAT(get_amxstring(amx,params[1],0,ilen),*(REAL *)((void *)&params[2]));
  return 1;
}

static cell AMX_NATIVE_CALL get_cvar_num(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  return (int)CVAR_GET_FLOAT(get_amxstring(amx,params[1],0,ilen));
}

static cell AMX_NATIVE_CALL set_cvar_num(AMX *amx, cell *params) /* 2 param */
{
  int ilen;
  CVAR_SET_FLOAT(get_amxstring(amx,params[1],0,ilen),(float)params[2]);
  return 1;
}

static cell AMX_NATIVE_CALL set_cvar_string(AMX *amx, cell *params) /* 2 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  char* szValue = get_amxstring(amx,params[2],1,ilen);
  CVAR_SET_STRING(sptemp,szValue);
  return 1;
}

static cell AMX_NATIVE_CALL message_begin(AMX *amx, cell *params) /* 4 param */
{
	int numparam = *params/sizeof(cell);
	float vecOrigin[3];
	cell *cpOrigin;
	switch (params[1]){
  case MSG_BROADCAST:
  case MSG_ALL:
  case MSG_SPEC:
	  MESSAGE_BEGIN( params[1], params[2],NULL );
	  break;
  case MSG_PVS: case MSG_PAS:
	  if (numparam < 3) {
		  amx_RaiseError(amx,AMX_ERR_NATIVE);
		  return 0;
	  }
	  cpOrigin = get_amxaddr(amx,params[3]);
	  vecOrigin[0] = *cpOrigin;
	  vecOrigin[1] = *(cpOrigin+1);
	  vecOrigin[2] = *(cpOrigin+2);
	  MESSAGE_BEGIN( params[1], params[2] , vecOrigin );
	  break;
  case MSG_ONE:
	  if (numparam < 4) {
		  amx_RaiseError(amx,AMX_ERR_NATIVE);
		  return 0;
	  }
	  MESSAGE_BEGIN( MSG_ONE, params[2], NULL, INDEXENT(params[4]) );
	  break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL message_end(AMX *amx, cell *params)
{
	MESSAGE_END();
	return 1;
}

static cell AMX_NATIVE_CALL write_byte(AMX *amx, cell *params) /* 1 param */
{
	WRITE_BYTE( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_char(AMX *amx, cell *params) /* 1 param */
{
	WRITE_CHAR( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_short(AMX *amx, cell *params)  /* 1 param */
{
	WRITE_SHORT( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_long(AMX *amx, cell *params)  /* 1 param */
{
	WRITE_LONG( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_entity(AMX *amx, cell *params) /* 1 param */
{
	WRITE_ENTITY( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_angle(AMX *amx, cell *params) /* 1 param */
{
	WRITE_ANGLE( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_coord(AMX *amx, cell *params) /* 1 param */
{
	WRITE_COORD( params[1] );
	return 1;
}

static cell AMX_NATIVE_CALL write_string(AMX *amx, cell *params) /* 1 param */
{
	int a;
	WRITE_STRING( get_amxstring(amx,params[1],3,a) );
	return 1;
}

static cell AMX_NATIVE_CALL log_message(AMX *amx, cell *params) /* 1 param */
{
  int len;
  g_langMngr.SetDefLang(LANG_SERVER);
  char* message =  format_amxstring(amx,params,1,len);
  message[len++]='\n';
  message[len]=0;
  ALERT( at_logged, "%s", message );
  return len;
}

static cell AMX_NATIVE_CALL log_to_file(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  char* szFile = get_amxstring(amx,params[1],0,ilen);
  FILE*fp;
  const char* filename = build_pathname("%s/%s",g_log_dir.str(),szFile);
  bool first_time = true;
  if ((fp=fopen(filename,"r"))!=NULL){
    first_time = false;
    fclose(fp);
  }
  if ((fp=fopen(filename,"a")) == NULL){
    //amx_RaiseError(amx,AMX_ERR_NATIVE);
  //would cause too much troubles in old plugins
    return 0;
  }
  char date[32];
  time_t td; time(&td);
  strftime(date,31,"%m/%d/%Y - %H:%M:%S",localtime(&td));
  int len;
  g_langMngr.SetDefLang(LANG_SERVER);
  char* message =  format_amxstring(amx,params,2,len);
  message[len++]='\n';
  message[len]=0;
  if ( first_time ){
    char game_dir[512];
    GET_GAME_DIR(game_dir);
    filename = build_pathname("%s/%s",g_log_dir.str(),szFile);
    fprintf(fp,"L %s: Log file started (file \"%s\") (game \"%s\") (amx \"%s\")\n",
      date,filename,g_mod_name.str(),Plugin_info.version);
    print_srvconsole("L %s: Log file started (file \"%s\") (game \"%s\") (amx \"%s\")\n",
      date,filename,g_mod_name.str(),Plugin_info.version);
  }
  fprintf(fp,"L %s: %s",date,message);
  print_srvconsole("L %s: %s",date,message);
  fclose(fp);
  return 1;
}

static cell AMX_NATIVE_CALL num_to_word(AMX *amx, cell *params) /* 3 param */
{
  char sptemp[512];
  UTIL_IntToString(params[1], sptemp);
  return set_amxstring(amx,params[2],sptemp,params[3]);
}


static cell AMX_NATIVE_CALL get_timeleft(AMX *amx, cell *params)
{
  float flCvarTimeLimit = mp_timelimit->value;

  if (flCvarTimeLimit) {
    int iReturn = (int)((g_game_timeleft + flCvarTimeLimit * 60.0) - gpGlobals->time);
    return (iReturn < 0) ? 0 : iReturn;
  }

  return 0;
}

static cell AMX_NATIVE_CALL get_time(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  time_t td = time(NULL);
  tm* lt = localtime(&td);
  if ( lt == 0 ) {
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
  char szDate[512];
  strftime(szDate,511,sptemp, lt );
  return set_amxstring(amx,params[2],szDate,params[3]);
}

static cell AMX_NATIVE_CALL format_time(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[3],0,ilen);
  time_t tim = params[4];
  time_t td = ( tim  != -1 ) ? tim : time(NULL);
  tm* lt = localtime(&td);
  if ( lt == 0 )  {
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
  char szDate[512];
  strftime(szDate,511,sptemp, lt );
  return set_amxstring(amx,params[1],szDate,params[2]);

}

static cell AMX_NATIVE_CALL parse_time(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sTime = get_amxstring(amx,params[1],1,ilen);
  char* sFormat = get_amxstring(amx,params[2],0,ilen);
  tm* mytime;
  time_t td;
  if ( params[3] == -1 )  {
  td = time(NULL);
  mytime = localtime(&td);
  if ( mytime == 0 ){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  strptime (sTime,sFormat,mytime , 0 );
  }
  else  {
  td = params[3];
  mytime = localtime(&td);
  if ( mytime == 0 ){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  strptime (sTime,sFormat,mytime , 1 );
  }
  return mktime(mytime);
}

static cell AMX_NATIVE_CALL get_systime(AMX *amx, cell *params) /* 3 param */
{
  time_t td = time(NULL);
  td  += params[1];
  return td;
}

static cell AMX_NATIVE_CALL read_datanum(AMX *amx, cell *params) /* 0 param */
{
  return g_events.getArgNum();
}

static cell AMX_NATIVE_CALL read_data(AMX *amx, cell *params) /* 3 param */
{
	if (params[0] == 0)
	{
		return g_events.getCurrentMsgType();
	}
  switch( *params/sizeof(cell) ) {
  case 1:
  return g_events.getArgInteger( params[1] );
  case 3:
  return set_amxstring(amx,params[2], g_events.getArgString( params[1] ),*get_amxaddr(amx,params[3]));
  default:
  cell *fCell = get_amxaddr(amx,params[2]);
  REAL pFloat = amx_ctof(fCell);
  pFloat = g_events.getArgFloat( params[1] );
  return (int)(pFloat);
  }
}

static cell AMX_NATIVE_CALL get_playersnum(AMX *amx, cell *params)
{
  if (!params[1])
    return g_players_num;

  int a = 0;
  for(int i = 1; i <= gpGlobals->maxClients; ++i){
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
    if ( pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0) )
      ++a;
  }

  return a;
}

static cell AMX_NATIVE_CALL get_players(AMX *amx, cell *params) /* 4 param */
{
  int iNum = 0;
  int ilen;
  char* sptemp = get_amxstring(amx,params[3],0,ilen);
  int flags = UTIL_ReadFlags(sptemp);

  cell *aPlayers = get_amxaddr(amx,params[1]);
  cell *iMax = get_amxaddr(amx,params[2]);

  int team = 0;

  if (flags & 48) {
    sptemp = get_amxstring(amx,params[4],0,ilen);

    if ( flags & 16 ) {
      if (flags & 64)
        team = g_teamsIds.findTeamId( sptemp );
      else
        team = g_teamsIds.findTeamIdCase( sptemp );
    }
  }

  for(int i = 1; i <= gpGlobals->maxClients; ++i){
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
    if (pPlayer->ingame){
      if (pPlayer->IsAlive() ? (flags & 2) : (flags & 1))
        continue;
      if (pPlayer->bot ? (flags & 4) : (flags & 8))
        continue;
      if ((flags & 16) && (pPlayer->teamId != team) )
    continue;
    /*if ( flags & 16  ) {
          if (flags & 64){
            if (strcmpi(pPlayer->team.str(),sptemp))
              continue;
          }
          else if (strcmp(pPlayer->team.str(),sptemp))
              continue;
    }*/
      if (flags & 32){
          if (flags & 64){
            if (stristr(pPlayer->name.str(),sptemp)==NULL)
              continue;
          }
          else if (strstr(pPlayer->name.str(),sptemp)==NULL)
              continue;
      }
      aPlayers[iNum++] = i;
    }
  }

  *iMax = iNum;
  return 1;
}

static cell AMX_NATIVE_CALL find_player(AMX *amx, cell *params) /* 1 param */
{
  int ilen, userid = 0;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  int flags = UTIL_ReadFlags(sptemp);
  if (flags&31)
    sptemp = get_amxstring(amx,params[2],0,ilen);
  else if (flags&1024)
    userid = *get_amxaddr(amx,params[2]);
  // a b c d e f g h i j k l
  int result = 0;
  for(int i = 1; i <= gpGlobals->maxClients; ++i){
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
    if (pPlayer->ingame){
      if (pPlayer->IsAlive()?(flags&64):(flags&32))
        continue;
      if (pPlayer->bot?(flags&128):(flags&256))
        continue;
      if (flags&1){
        if (flags&2048) {
          if (strcmpi(pPlayer->name.str(),sptemp))
            continue;
        }
        else if (strcmp(pPlayer->name.str(),sptemp))
          continue;
      }
      if (flags&2){
        if (flags&2048) {
          if (stristr(pPlayer->name.str(),sptemp)==NULL)
            continue;
        }
        else if (strstr(pPlayer->name.str(),sptemp)==NULL)
          continue;
      }
      if (flags&4){
        const char* authid = GETPLAYERAUTHID(pPlayer->pEdict);
        if (!authid || strcmp(authid,sptemp))
          continue;
      }
      if (flags&1024){
        if (userid != GETPLAYERUSERID(pPlayer->pEdict))
          continue;
      }
      if (flags&8){
        if (strncmp(pPlayer->ip.str(),sptemp,ilen))
          continue;
      }
      if (flags&16){
        if (flags&2048) {
          if (strcmpi(pPlayer->team.str(),sptemp))
            continue;
        }
        else if (strcmp(pPlayer->team.str(),sptemp))
          continue;
      }
      result = i;
      if ((flags&512)==0)
        break;
    }
  }

  return result;
}

static cell AMX_NATIVE_CALL get_maxplayers(AMX *amx, cell *params)
{
  return gpGlobals->maxClients;
}

static cell AMX_NATIVE_CALL get_gametime(AMX *amx, cell *params)
{
  REAL pFloat = gpGlobals->time;
  return *(cell*)((void *)&pFloat);
}

static cell AMX_NATIVE_CALL get_mapname(AMX *amx, cell *params) /* 2 param */
{
  return set_amxstring(amx,params[1],STRING(gpGlobals->mapname),params[2]);
}

static cell AMX_NATIVE_CALL get_modname(AMX *amx, cell *params) /* 2 param */
{
  return set_amxstring(amx,params[1],g_mod_name.str(),params[2]);
}


static cell AMX_NATIVE_CALL get_localinfo(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  return set_amxstring(amx,params[2],LOCALINFO(sptemp),params[3]);
}

static cell AMX_NATIVE_CALL set_localinfo(AMX *amx, cell *params) /* 2 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  char* szValue = get_amxstring(amx,params[2],1,ilen);
  SET_LOCALINFO(sptemp,szValue);
  return 1;
}

static cell AMX_NATIVE_CALL get_user_info(AMX *amx, cell *params) /* 4 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  int ilen;
  char* sptemp = get_amxstring(amx,params[2],0,ilen);
  return set_amxstring(amx,params[3],ENTITY_KEYVALUE(pPlayer->pEdict,sptemp ),params[4]);
  return 1;
}

static cell AMX_NATIVE_CALL set_user_info(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  int ilen;
  char* sptemp = get_amxstring(amx,params[2],0,ilen);
  char* szValue = get_amxstring(amx,params[3],1,ilen);
  ENTITY_SET_KEYVALUE(pPlayer->pEdict,sptemp,szValue);
  return 1;
}

static cell AMX_NATIVE_CALL read_argc(AMX *amx, cell *params)
{
  return CMD_ARGC();
}

static cell AMX_NATIVE_CALL read_argv(AMX *amx, cell *params) /* 3 param */
{
  return set_amxstring(amx,params[2],  /*( params[1] < 0 ||
    params[1] >= CMD_ARGC() ) ? "" : */CMD_ARGV(params[1])   , params[3]);
}

static cell AMX_NATIVE_CALL read_args(AMX *amx, cell *params) /* 2 param */
{
  const char* sValue = CMD_ARGS();
  return set_amxstring(amx,params[1],sValue ? sValue : "",params[2]);
}

static cell AMX_NATIVE_CALL get_user_msgid(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  return GET_USER_MSG_ID(PLID, sptemp , NULL );
}

static cell AMX_NATIVE_CALL set_task(AMX *amx, cell *params) /* 2 param */
{

  CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);

  int a, iFunc;

  char* stemp = get_amxstring(amx,params[2],1, a );

  if (amx_FindPublic(amx, stemp , &iFunc) != AMX_ERR_NONE){
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",stemp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }

  float base = *(REAL *)((void *)&params[1]);

  if ( base < 0.1 )
    base = 0.1;

  char* temp = get_amxstring(amx,params[6],0,a);

  g_tasksMngr.registerTask( plugin ,
    iFunc , UTIL_ReadFlags(temp), params[3], base ,
    params[5] ,
    get_amxaddr(amx,params[4]) , params[7] );

  return 1;
}

static cell AMX_NATIVE_CALL remove_task(AMX *amx, cell *params) /* 1 param */
{
  return g_tasksMngr.removeTasks( params[1] ,  params[2] ? 0 : amx   );
}

static cell AMX_NATIVE_CALL change_task(AMX *amx, cell *params)
{
  REAL flNewTime = amx_ctof(params[2]);
  return g_tasksMngr.changeTasks(params[1], params[3] ? 0 : amx, flNewTime);
}

static cell AMX_NATIVE_CALL task_exists(AMX *amx, cell *params) /* 1 param */
{
  return g_tasksMngr.taskExists(  params[1] ,  params[2] ? 0 : amx  );
}

static cell AMX_NATIVE_CALL cvar_exists(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  return (CVAR_GET_POINTER(get_amxstring(amx,params[1],0,ilen))?1:0);
}

static cell AMX_NATIVE_CALL register_cvar(AMX *amx, cell *params) /* 3 param */
{
  int i;
  char* temp = get_amxstring(amx,params[1],0,i);

  if (  !g_cvars.find(  temp ) )
  {
  CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);
  CCVar* cvar = new CCVar( temp , plugin->getName() , params[3] ,
    *(REAL *)((void *)&params[4])  );

  if ( cvar == 0 )
    return 0;

  g_cvars.put( cvar );

  if ( CVAR_GET_POINTER(temp) == 0 )
    CVAR_REGISTER( cvar->getCvar() );

  CVAR_SET_STRING( temp ,get_amxstring(amx,params[2],1,i));
  return 1;
  }

  return 0;
}

static cell AMX_NATIVE_CALL get_user_ping(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    cell *cpPing = get_amxaddr(amx,params[2]);
    cell *cpLoss = get_amxaddr(amx,params[3]);
    int ping, loss;
    PLAYER_CNX_STATS(pPlayer->pEdict , &ping, &loss);
    *cpPing = ping;
    *cpLoss = loss;
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_time(AMX *amx, cell *params) /* 1 param */
{
  int index = params[1];
  if (index<1||index>gpGlobals->maxClients)
    return 0;
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    int time = (int)(gpGlobals->time - (params[2] ? pPlayer->playtime : pPlayer->time));
    return time;
  }
  return 0;
}

static cell AMX_NATIVE_CALL server_exec(AMX *amx, cell *params)
{
  SERVER_EXECUTE( );
  return 1;
}

static cell AMX_NATIVE_CALL engclient_cmd(AMX *amx, cell *params) /* 4 param */
{
  int ilen;
  const char* szCmd = get_amxstring(amx,params[2],0,ilen);
  const char* sArg1 = get_amxstring(amx,params[3],1,ilen);
  if ( ilen == 0 ) sArg1 = 0;
  const char* sArg2 = get_amxstring(amx,params[4],2,ilen);
  if ( ilen == 0 ) sArg2 = 0;
  if (params[1] == 0) {
    for(int i = 1; i <= gpGlobals->maxClients; ++i){
      CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
      if (pPlayer->ingame /*&&  pPlayer->initialized */)
        UTIL_FakeClientCommand(pPlayer->pEdict, szCmd,sArg1,sArg2);
    }
  }
  else {
    int index = params[1];
    if (index < 1 || index > gpGlobals->maxClients){
      amx_RaiseError(amx,AMX_ERR_NATIVE);
      return 0;
    }
    CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if ( /*pPlayer->initialized && */pPlayer->ingame )
    UTIL_FakeClientCommand(pPlayer->pEdict, szCmd,sArg1,sArg2);
  }
  return 1;
}

static cell AMX_NATIVE_CALL pause(AMX *amx, cell *params) /* 3 param */
{
  int ilen;
  char* temp = get_amxstring(amx,params[1],0,ilen);
  int flags = UTIL_ReadFlags(temp);

  CPluginMngr::CPlugin *plugin = 0;

  if ( flags & 2  ) { // pause function
  if (flags&4){ //look out side the plugin
      temp = get_amxstring(amx,params[3],0,ilen);
      plugin = g_plugins.findPlugin(temp);
    }
    else  plugin = g_plugins.findPluginFast(amx);
  if ( !plugin ) return 0; // plugin not found
  temp = get_amxstring(amx,params[2],0,ilen);
    int err, index;
    if ((err = amx_FindPublic( plugin->getAMX(), temp , &index) )!= AMX_ERR_NONE){
      AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")", temp,plugin->getName() );
      return 0;
    }
  plugin->pauseFunction( index );
    return 1;
  }
  else  if (flags&4){
    temp = get_amxstring(amx,params[2],0,ilen);
    plugin = g_plugins.findPlugin(temp);
  }
  else
    plugin = g_plugins.findPluginFast(amx);
  if ( plugin && plugin->isValid() ){
    if ( flags & 8 )
    plugin->setStatus( ps_stopped );
    else if ( flags & 16 )
    plugin->setStatus( ps_locked );
    else
    plugin->pausePlugin();
  return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL unpause(AMX *amx, cell *params) /* 3 param */
{

  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  int flags = UTIL_ReadFlags(sptemp);
  CPluginMngr::CPlugin *plugin = 0;
  if (flags&2) {
    if (flags&4){
      sptemp = get_amxstring(amx,params[3],0,ilen);
      plugin = g_plugins.findPlugin(sptemp);
    }
    else
      plugin = g_plugins.findPluginFast(amx);
    if ( !plugin ) return 0;
    sptemp = get_amxstring(amx,params[2],0,ilen);
    int err, index;
    if ((err = amx_FindPublic(plugin->getAMX(), sptemp , &index) )!= AMX_ERR_NONE){
      AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")", sptemp,plugin->getName() );
      return 0;
    }
  plugin->unpauseFunction( index );
    return 1;
  }
  else  if (flags&4){
    sptemp = get_amxstring(amx,params[2],0,ilen);
    plugin = g_plugins.findPlugin(sptemp);
  }
  else
    plugin = g_plugins.findPluginFast( amx);
  if (plugin && plugin->isValid()) {
    plugin->unpausePlugin();
    return 1;
  }

  return 0;

}

static cell AMX_NATIVE_CALL read_flags(AMX *amx, cell *params) /* 1 param */
{
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  return UTIL_ReadFlags(sptemp);
}

static cell AMX_NATIVE_CALL get_flags(AMX *amx, cell *params) /* 1 param */
{
  char flags[32];
  UTIL_GetFlags( flags , params[1] );
  return set_amxstring(amx,params[2],flags,params[3]);
}

static cell AMX_NATIVE_CALL get_user_flags(AMX *amx, cell *params) /* 2 param */
{
  int index = params[1];
  if (index<0||index>gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  int id = params[2];
  if (id < 0) id = 0;
  if (id > 31) id = 31;
  return GET_PLAYER_POINTER_I(index)->flags[id];
}

static cell AMX_NATIVE_CALL set_user_flags(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 0 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  int flag = params[2];
  int id = params[3];
  if (id < 0) id = 0;
  if (id > 31) id = 31;
  pPlayer->flags[id] |= flag;
  return 1;
}

static cell AMX_NATIVE_CALL remove_user_flags(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 0 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  int flag = params[2];
  int id = params[3];
  if (id < 0) id = 0;
  if (id > 31) id = 31;
  pPlayer->flags[id] &= ~flag;
  return 1;
}

static cell AMX_NATIVE_CALL register_menuid(AMX *amx, cell *params) /* 1 param */
{
  int i;
  char* temp = get_amxstring(amx,params[1],0,i);
  AMX* a = (*params/sizeof(cell) < 2 || params[2] ) ? 0 : amx;
  return g_menucmds.registerMenuId( temp , a );
}

static cell AMX_NATIVE_CALL get_user_menu(AMX *amx, cell *params) /* 3 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  cell *cpMenu = get_amxaddr(amx,params[2]);
  cell *cpKeys = get_amxaddr(amx,params[3]);
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  if (pPlayer->ingame){
    *cpMenu = pPlayer->menu;
    *cpKeys = pPlayer->keys;
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL precache_sound(AMX *amx, cell *params) /* 1 param */
{
  if ( g_dontprecache ) {
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  PRECACHE_SOUND((char*)STRING(ALLOC_STRING(sptemp)));
  return 1;
}

static cell AMX_NATIVE_CALL precache_model(AMX *amx, cell *params) /* 1 param */
{
  if ( g_dontprecache ) {
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  int ilen;
  char* sptemp = get_amxstring(amx,params[1],0,ilen);
  return PRECACHE_MODEL((char*)STRING(ALLOC_STRING(sptemp)));
}

static cell AMX_NATIVE_CALL get_distance(AMX *amx, cell *params) /* 2 param */
{
  cell *cpVec1 = get_amxaddr(amx,params[1]);
  cell *cpVec2 = get_amxaddr(amx,params[2]);
  Vector vec1 = Vector(cpVec1[0],cpVec1[1],cpVec1[2]);
  Vector vec2 = Vector(cpVec2[0],cpVec2[1],cpVec2[2]);
  int iDist = (int)((vec1 - vec2).Length());
  return iDist;
}

static cell AMX_NATIVE_CALL random_float(AMX *amx, cell *params) /* 2 param */
{
  float one = *(REAL *)((void *)&params[1]);
  float two = *(REAL *)((void *)&params[2]);
  REAL fRnd = RANDOM_FLOAT(one,two);
  return *(cell*)((void *)&fRnd);
}

static cell AMX_NATIVE_CALL random_num(AMX *amx, cell *params) /* 2 param */
{
  return RANDOM_LONG(params[1],params[2]);
}

static cell AMX_NATIVE_CALL remove_quotes(AMX *amx, cell *params) /* 1 param */
{
  cell *text = get_amxaddr(amx,params[1]);
  if (*text=='\"') {
    register cell *temp = text;
    int len = 0;
    while(*temp++) ++len; // get length
    cell *src = text;
  if ( src[len-1]=='\r' )
    src[--len] = 0;
    if (src[--len]=='\"'){
      src[len] = 0;
      temp = src+1;
      while((*src++ = *temp++))
        ;
      return 1;
    }
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_user_aiming(AMX *amx, cell *params) /* 4 param */
{
  int index = params[1];
  if (index < 1 || index > gpGlobals->maxClients){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
  cell *cpId = get_amxaddr(amx,params[2]);
  cell *cpBody = get_amxaddr(amx,params[3]);
  cell fCell;
  REAL *pFloat = (REAL *)((void *)&fCell);
  *pFloat = 0.0;
  if (pPlayer->ingame) {
    edict_t* edict = pPlayer->pEdict;
    Vector v_forward;
    Vector v_src = edict->v.origin + edict->v.view_ofs;
    ANGLEVECTORS( edict->v.v_angle , v_forward, NULL, NULL );
    TraceResult trEnd;
    Vector v_dest = v_src + v_forward * params[4];
    TRACE_LINE( v_src , v_dest,  0 , edict, &trEnd );
    *cpId = FNullEnt(trEnd.pHit) ? 0 : ENTINDEX(trEnd.pHit);
    *cpBody = trEnd.iHitgroup;
    if (trEnd.flFraction < 1.0) {
      *pFloat = (trEnd.vecEndPos - v_src).Length();
      return fCell;
    }
    else {
      return fCell;
    }
  }
  *cpId = 0;
  *cpBody = 0;
  return fCell;

}

static cell AMX_NATIVE_CALL remove_cvar_flags(AMX *amx, cell *params)
{
  int ilen;
  char* sCvar = get_amxstring(amx,params[1],0,ilen);
  if ( !strcmp(sCvar,"amx_version") || !strcmp(sCvar,"amxmodx_version") ||
    !strcmp(sCvar,"fun_version") ||  !strcmp(sCvar,"sv_cheats")  )
    return 0;
  cvar_t* pCvar = CVAR_GET_POINTER(sCvar);
  if (pCvar)  {
    pCvar->flags &= ~((int)(params[2]));
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL get_cvar_flags(AMX *amx, cell *params)
{
  int ilen;
  char* sCvar = get_amxstring(amx,params[1],0,ilen);
  cvar_t* pCvar = CVAR_GET_POINTER(sCvar);
  return pCvar ? pCvar->flags : 0;
}

static cell AMX_NATIVE_CALL set_cvar_flags(AMX *amx, cell *params)
{
  int ilen;
  char* sCvar = get_amxstring(amx,params[1],0,ilen);
  if ( !strcmp(sCvar,"amx_version") || !strcmp(sCvar,"amxmodx_version") ||
    !strcmp(sCvar,"fun_version")  ||  !strcmp(sCvar,"sv_cheats") )
    return 0;
  cvar_t* pCvar = CVAR_GET_POINTER(sCvar);
  if (pCvar)  {
    pCvar->flags |= (int)(params[2]);
    return 1;
  }
  return 0;
}

static cell AMX_NATIVE_CALL force_unmodified(AMX *amx, cell *params)
{
  int a;
  cell *cpVec1 = get_amxaddr(amx,params[2]);
  cell *cpVec2 = get_amxaddr(amx,params[3]);
  Vector vec1 = Vector(cpVec1[0],cpVec1[1],cpVec1[2]);
  Vector vec2 = Vector(cpVec2[0],cpVec2[1],cpVec2[2]);
  char* filename = get_amxstring(amx,params[4],0,a);

  ForceObject* aaa = new ForceObject(filename ,  (FORCE_TYPE)((int)(params[1])) , vec1 ,  vec2 , amx);

  if ( aaa  )
  {
    if ( stristr(filename,".wav") )
      g_forcesounds.put( aaa );
    else if ( stristr(filename,".mdl") )
      g_forcemodels.put( aaa );
    else
      g_forcegeneric.put( aaa );

    return 1;
  }
  return 0;
}


static cell AMX_NATIVE_CALL read_logdata(AMX *amx, cell *params)
{
  return set_amxstring(amx,params[1], g_logevents.getLogString() ,params[2]);
}

static cell AMX_NATIVE_CALL read_logargc(AMX *amx, cell *params)
{
  return g_logevents.getLogArgNum();
}

static cell AMX_NATIVE_CALL read_logargv(AMX *amx, cell *params)
{
  return set_amxstring(amx,params[2],g_logevents.getLogArg(params[1]),params[3]);
}

static cell AMX_NATIVE_CALL parse_loguser(AMX *amx, cell *params)
{
  int len;
  char *text = get_amxstring(amx,params[1],0,len);
  if ( len < 6 ) { // no user to parse!?
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
/******** GET TEAM **********/
  char* end = text + --len;
  *end = 0;
  while ( *end!='<' && len-- )
  --end;
  ++end;
  cell *cPtr = get_amxaddr(amx,params[7]);
  int max = params[8]; // get TEAM
//  print_srvconsole("Got team: %s (Len %d)\n",end,len);
  while ( max-- && *end )
  *cPtr++ = *end++;
  *cPtr = 0;
/******** GET AUTHID **********/
  if ( len <= 0 ) {
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
  end = text + --len;
  *end = 0;
  while (  *end!='<' && len-- )
  --end;
  ++end;
  cPtr = get_amxaddr(amx,params[5]);
  max = params[6]; // get AUTHID
 // print_srvconsole("Got auth: %s (Len %d)\n",end,len);
  while ( max-- && *end )
  *cPtr++ = *end++;
  *cPtr = 0;
/******** GET USERID **********/
  if ( len <= 0 ) {
  amx_RaiseError(amx,AMX_ERR_NATIVE);
  return 0;
  }
  end = text + --len;
  *end = 0;
  while ( *end!='<' && len-- )
  --end;
//  print_srvconsole("Got userid: %s (Len %d)\n",end + 1,len);
  if ( *(cPtr = get_amxaddr(amx,params[4])) != -2 )
  *cPtr = atoi( end + 1 );
/******** GET NAME **********/
  *end = 0;
  cPtr = get_amxaddr(amx,params[2]);
  max = params[3]; // get NAME
//  print_srvconsole("Got name: %s (Len %d)\n",text,len);
  while ( max-- && *text )
  *cPtr++ = *text++;
  *cPtr = 0;
  return 1;
}

static cell AMX_NATIVE_CALL register_logevent(AMX *amx, cell *params)
{
  CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);

  int a, iFunc;

  char* temp = get_amxstring(amx,params[1],0, a );

  if (amx_FindPublic(amx, temp , &iFunc) != AMX_ERR_NONE){
    AMXXLOG_Log("[AMXX] Function is not present (function \"%s\") (plugin \"%s\")",
    temp,plugin->getName() );
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }

  LogEventsMngr::CLogEvent* r =
    g_logevents.registerLogEvent( plugin ,  iFunc, params[2]  );

  if ( r == 0 ) return 0;

  int numparam = *params/sizeof(cell);

  for(int i = 3; i <= numparam; ++i)
  r->registerFilter( get_amxstring(amx,params[i],0, a ) );

  return 1;
}

// native is_module_loaded(const name[]);
static cell AMX_NATIVE_CALL is_module_loaded(AMX *amx, cell *params)
{
	// param1: name
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);
	int id = 0;
	for (CList<CModule>::iterator iter = g_modules.begin(); iter; ++iter)
	{
		if (stricmp((*iter).getName(), name) == 0)
			return id;
		++id;
	}
	return -1;
}

// native is_plugin_loaded(const name[]);
static cell AMX_NATIVE_CALL is_plugin_loaded(AMX *amx, cell *params)
{
	// param1: name
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);
	int id = 0;
	for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
	{
		if (stricmp((*iter).getName(), name) == 0)
			return id;
		++id;
	}
	return -1;
}
// native get_modulesnum();
static cell AMX_NATIVE_CALL get_modulesnum(AMX *amx, cell *params)
{
	return (cell)countModules(CountModules_All);
}

// native get_module(id, name[], nameLen, author[], authorLen, version[], versionLen, &status);
static cell AMX_NATIVE_CALL get_module(AMX *amx, cell *params)
{
	CList<CModule>::iterator moduleIter;

	// find the module
	int i = params[1];
	for (moduleIter = g_modules.begin(); moduleIter && i; ++moduleIter)
		--i;

	if (i != 0  ||  !moduleIter)
		return -1;						// not found

	// set name, author, version
	if ((*moduleIter).isAmxx()) 	 
	{ 	 
		const amxx_module_info_s *info = (*moduleIter).getInfoNew(); 	 
		set_amxstring(amx, params[2], info->name, params[3]); 	 
		set_amxstring(amx, params[4], info->author, params[5]); 	 
		set_amxstring(amx, params[6], info->version, params[7]); 	 
	} 	 
	else 	 
	{
		module_info_s *info = (*moduleIter).getInfo();
		set_amxstring(amx, params[2], info->name, params[3]);
		set_amxstring(amx, params[4], info->author, params[5]);
		set_amxstring(amx, params[6], info->version, params[7]);
	}

	// compatibility problem possible
	int numParams = params[0] / sizeof(cell);
	if (numParams < 8)
	{
		CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
		AMXXLOG_Log("[AMXX] get_module: call to a previous version (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
	}

	// set status
	cell *addr;
	if (amx_GetAddr(amx, params[8], &addr) != AMX_ERR_NONE)
	{
		CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
		AMXXLOG_Log("[AMXX] get_module: invalid reference (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
	}

	*addr = (cell)(*moduleIter).getStatusValue();

	return params[1];
}

// native log_amx(const msg[], ...);
static cell AMX_NATIVE_CALL log_amx(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);
	int len;

	g_langMngr.SetDefLang(LANG_SERVER);
	AMXXLOG_Log("[%s] %s", plugin->getName(), format_amxstring(amx, params, 1, len));
	return 0;
}

/*********************************************************************/
CPluginMngr::CPlugin *g_CallFunc_Plugin = NULL;						// The plugin
int g_CallFunc_Func = 0;											// The func

struct CallFunc_ParamInfo
{
	unsigned char flags;											// flags
	cell byrefAddr;													// byref address in caller plugin
	cell size;														// byref size
};

#define CALLFUNC_MAXPARAMS 64										/* Maximal params number */
cell g_CallFunc_Params[CALLFUNC_MAXPARAMS] = {0};					// Params
CallFunc_ParamInfo g_CallFunc_ParamInfo[CALLFUNC_MAXPARAMS] = {{0}};	// Flags
int g_CallFunc_CurParam = 0;										// Current param id

#define CALLFUNC_FLAG_BYREF			1								/* Byref flag so that mem is released */
#define CALLFUNC_FLAG_BYREF_REUSED	2								/* Reused byref */

// native callfunc_begin(const func[], const plugin[]="");
static cell AMX_NATIVE_CALL callfunc_begin(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
	if (g_CallFunc_Plugin)
	{
		// scripter's fault
		AMXXLOG_Log("[AMXX] callfunc_begin called without callfunc_end (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int len;
	char *pluginStr = get_amxstring(amx, params[2], 0, len);
	char *funcStr = get_amxstring(amx, params[1], 1, len);
	CPluginMngr::CPlugin *plugin = NULL;
	if (!pluginStr || !*pluginStr)
		plugin = curPlugin;
	else
		plugin = g_plugins.findPlugin(pluginStr);

	if (!plugin)
	{
		return -1;		// plugin not found: -1
	}

	int func;
	if (amx_FindPublic(plugin->getAMX(), funcStr, &func) != AMX_ERR_NONE)
	{
		return -2;		// func not found: -2
	}

	// set globals
	g_CallFunc_Plugin = plugin;
	g_CallFunc_Func = func;

	return 1;		// success: 1
}

// native callfunc_end();
static cell AMX_NATIVE_CALL callfunc_end(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		AMXXLOG_Log("[AMXX] callfunc_end called without callfunc_begin (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// call the func
	cell retVal;
	int err;

	// copy the globs so the called func can also use callfunc
	cell gparams[CALLFUNC_MAXPARAMS];
	CallFunc_ParamInfo gparamInfo[CALLFUNC_MAXPARAMS];

	CPluginMngr::CPlugin *plugin = g_CallFunc_Plugin;
	int func = g_CallFunc_Func;
	int curParam = g_CallFunc_CurParam;

	memcpy(gparams, g_CallFunc_Params, sizeof(cell) * curParam);
	memcpy(gparamInfo, g_CallFunc_ParamInfo, sizeof(CallFunc_ParamInfo) * curParam);

	// cleanup
	g_CallFunc_Plugin = NULL;
	g_CallFunc_CurParam = 0;

	// actual call
	if ((err = amx_Execv(plugin->getAMX(), &retVal, func, curParam, gparams)) != AMX_ERR_NONE)
	{
		AMXXLOG_Log("[AMXX] Run time error %d on line %ld (plugin \"%s\")", err, curPlugin->getAMX()->curline, curPlugin->getName());
		return 0;
	}

	// process byref params (not byref_reused)
	for (int i = 0; i < curParam; ++i)
	{
		if (gparamInfo[i].flags & CALLFUNC_FLAG_BYREF)
		{
			// copy back so that references work
			AMX *amxCaller = curPlugin->getAMX();
			AMX *amxCalled = plugin->getAMX();
			AMX_HEADER *hdrCaller = (AMX_HEADER *)amxCaller->base;
			AMX_HEADER *hdrCalled = (AMX_HEADER *)amxCalled->base;
			memcpy(	/** DEST ADDR **/
				(amxCaller->data ? amxCaller->data : (amxCaller->base + hdrCaller->dat)) + gparamInfo[i].byrefAddr,
					/** SOURCE ADDR **/
				(amxCalled->data ? amxCalled->data : (amxCalled->base + hdrCalled->dat)) + gparams[i],
					/** SIZE **/
				gparamInfo[i].size * sizeof(cell));

			// free memory used for params passed by reference
			amx_Release(amxCalled, gparams[i]);
		}
	}

	return retVal;
}

// native callfunc_push_int(value);
// native callfunc_push_float(Float: value);
static cell callfunc_push_byval(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		AMXXLOG_Log("[AMXX] callfunc_push_xxx called without callfunc_begin (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		AMXXLOG_Log("[AMXX] callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = 0;
	g_CallFunc_Params[g_CallFunc_CurParam++] = params[1];

	return 0;
}

// native callfunc_push_intref(&value);
// native callfunc_push_floatref(Float: &value);
static cell callfunc_push_byref(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		AMXXLOG_Log("[AMXX] callfunc_push_xxx called without callfunc_begin (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		AMXXLOG_Log("[AMXX] callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// search for the address; if it is found, dont create a new copy
	for (int i = 0; i < g_CallFunc_CurParam; ++i)
	{
		if ((g_CallFunc_ParamInfo[i].flags & CALLFUNC_FLAG_BYREF) &&
			(g_CallFunc_ParamInfo[i].byrefAddr == params[1]))
		{
			// the byrefAddr and size params should not be used; set them anyways...
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF_REUSED;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
			g_CallFunc_Params[g_CallFunc_CurParam++] = g_CallFunc_Params[i];
			// we are done
			return 0;
		}
	}

	// not found; create an own copy
	// allocate memory
	cell *phys_addr;
	cell amx_addr;
	amx_Allot(g_CallFunc_Plugin->getAMX(),
		1,				// 1 cell
		&amx_addr,
		&phys_addr);

	// copy the value to the allocated memory
	cell *phys_addr2;
	amx_GetAddr(curPlugin->getAMX(), params[1], &phys_addr2);
	*phys_addr = *phys_addr2;

	// push the address and set the reference flag so that memory is released after function call.
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
	g_CallFunc_Params[g_CallFunc_CurParam++] = amx_addr;

	return 0;
}

// native callfunc_push_str(value[]);
static cell callfunc_push_str(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		AMXXLOG_Log("[AMXX] callfunc_push_xxx called without callfunc_begin (plugin \"%s\", line %d)", curPlugin->getName(), amx->curline);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		AMXXLOG_Log("[AMXX] callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// search for the address; if it is found, dont create a new copy
	for (int i = 0; i < g_CallFunc_CurParam; ++i)
	{
		if ((g_CallFunc_ParamInfo[i].flags & CALLFUNC_FLAG_BYREF) &&
			(g_CallFunc_ParamInfo[i].byrefAddr == params[1]))
		{
			// the byrefAddr and size params should not be used; set them anyways...
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF_REUSED;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
			g_CallFunc_Params[g_CallFunc_CurParam++] = g_CallFunc_Params[i];
			// we are done
			return 0;
		}
	}

	// not found; create an own copy
	// get the string and its length
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	// allocate enough memory for the string
	cell *phys_addr;
	cell amx_addr;
	amx_Allot(g_CallFunc_Plugin->getAMX(),
		len + 1,			// length + terminator
		&amx_addr,
		&phys_addr);

	// copy it to the allocated memory
	// we assume it's unpacked
	// :NOTE: 4th parameter use_wchar since Small Abstract Machine 2.5.0
	amx_SetString(phys_addr, str, 0, 0);

	// push the address and set the reference flag so that memory is released after function call.
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = len + 1;
	g_CallFunc_Params[g_CallFunc_CurParam++] = amx_addr;

	return 0;
}

// get_langsnum();
static cell get_langsnum(AMX *amx, cell *params)
{
	return g_langMngr.GetLangsNum();
}

// get_lang(id, name[(at least 3)]);
static cell get_lang(AMX *amx, cell *params)
{
	set_amxstring(amx, params[2], g_langMngr.GetLangName(params[1]), 2);
	return 0;
}

// register_dictionary(const filename[]);
static cell register_dictionary(AMX *amx, cell *params)
{
	int len;
	g_langMngr.MergeDefinitionFile(build_pathname("%s/lang/%s",
		get_localinfo("amxx_datadir", "addons/amxx/data"), get_amxstring(amx, params[1], 1, len)));
	return 0;
}

// lang_exists(const name[]);
static cell lang_exists(AMX *amx, cell *params)
{
	int len;
	return g_langMngr.LangExists(get_amxstring(amx, params[1], 1, len)) ? 1 : 0;
}

AMX_NATIVE_INFO amxmod_Natives[] = {
  { "client_cmd",       client_cmd },
  { "client_print",     client_print },
  { "console_cmd",      console_cmd },
  { "console_print",    console_print },
  { "cvar_exists",      cvar_exists },
  { "emit_sound",       emit_sound },
  { "engclient_cmd",    engclient_cmd },
  { "engclient_print",  engclient_print },
  { "find_player",      find_player },
  { "force_unmodified", force_unmodified },
  { "format_time",      format_time},
  { "get_clcmd",        get_clcmd},
  { "get_clcmdsnum",    get_clcmdsnum},
  { "get_concmd",       get_concmd},
  { "get_concmdsnum",   get_concmdsnum},
  { "get_cvar_flags",   get_cvar_flags },
  { "get_cvar_float",   get_cvar_float },
  { "get_cvar_num",     get_cvar_num },
  { "get_cvar_string",  get_cvar_string },
  { "get_distance",     get_distance },
  { "get_flags",        get_flags },
  { "get_gametime",     get_gametime},
  { "get_localinfo",    get_localinfo},
  { "get_mapname",      get_mapname},
  { "get_maxplayers",   get_maxplayers },
  { "get_modname",      get_modname},
  { "get_players",      get_players },
  { "get_playersnum",   get_playersnum },
  { "get_plugin",       get_plugin },
  { "get_pluginsnum",   get_pluginsnum },
  { "get_srvcmd",       get_srvcmd },
  { "get_srvcmdsnum",   get_srvcmdsnum },
  { "get_systime",      get_systime},
  { "get_time",         get_time},
  { "get_timeleft",     get_timeleft},
  { "get_user_aiming",  get_user_aiming },
  { "get_user_ammo",    get_user_ammo},
  { "get_user_armor",   get_user_armor },
  { "get_user_attacker",get_user_attacker },
  { "get_user_authid",  get_user_authid },
  { "get_user_flags",   get_user_flags },
  { "get_user_frags",   get_user_frags },
  { "get_user_deaths",  get_user_deaths },
  { "get_user_health",  get_user_health },
  { "get_user_index",   get_user_index },
  { "get_user_info",    get_user_info },
  { "get_user_ip",      get_user_ip },
  { "get_user_menu",    get_user_menu},
  { "get_user_msgid",   get_user_msgid},
  { "get_user_name",    get_user_name },
  { "get_user_origin",  get_user_origin},
  { "get_user_ping",    get_user_ping },
  { "get_user_team",    get_user_team },
  { "get_user_time",    get_user_time },
  { "get_user_userid",  get_user_userid },
  { "user_has_weapon",	user_has_weapon },
  { "get_user_weapon",  get_user_weapon},
  { "get_user_weapons", get_user_weapons},
  { "get_weaponname",   get_weaponname},
  { "get_xvar_float",   get_xvar_num },
  { "get_xvar_id",      get_xvar_id },
  { "get_xvar_num",     get_xvar_num },
  { "is_dedicated_server",is_dedicated_server },
  { "is_linux_server",    is_linux_server },
  { "is_jit_enabled",		is_jit_enabled },
  { "is_user_authorized", is_user_authorized },
  { "is_map_valid",     is_map_valid },
  { "is_user_alive",    is_user_alive },
  { "is_user_bot",      is_user_bot },
  { "is_user_connected",  is_user_connected },
  { "is_user_connecting", is_user_connecting },
  { "is_user_hltv",     is_user_hltv },
  { "log_message",      log_message },
  { "log_to_file",      log_to_file },
  { "num_to_word",      num_to_word },
  { "parse_loguser",    parse_loguser },
  { "parse_time",        parse_time },
  { "pause",            pause },
  { "precache_model",   precache_model },
  { "precache_sound",   precache_sound },
  { "random_float",     random_float },
  { "random_num",       random_num },
  { "read_argc",        read_argc },
  { "read_args",        read_args },
  { "read_argv",        read_argv },
  { "read_data",        read_data },
  { "read_datanum",      read_datanum },
  { "read_flags",       read_flags },
  { "read_logargc",     read_logargc },
  { "read_logargv",     read_logargv },
  { "read_logdata",     read_logdata },
  { "register_clcmd",   register_clcmd },
  { "register_concmd",   register_concmd },
  { "register_cvar",    register_cvar },
  { "register_event",   register_event },
  { "register_logevent",register_logevent},
  { "register_menucmd", register_menucmd },
  { "register_menuid",  register_menuid },
  { "register_plugin",  register_plugin },
  { "register_srvcmd",  register_srvcmd },
  { "remove_cvar_flags",    remove_cvar_flags },
  { "remove_quotes",    remove_quotes },
  { "remove_task",      remove_task },
  { "change_task",		change_task },
  { "remove_user_flags",  remove_user_flags },
  { "server_cmd",       server_cmd },
  { "server_exec",      server_exec },
  { "server_print",     server_print },
  { "set_cvar_flags",   set_cvar_flags },
  { "set_cvar_float",   set_cvar_float },
  { "set_cvar_num",     set_cvar_num },
  { "set_cvar_string",  set_cvar_string },
  { "set_hudmessage",   set_hudmessage },
  { "set_localinfo",    set_localinfo},
  { "set_task",         set_task },
  { "set_user_flags",   set_user_flags},
  { "set_user_info",    set_user_info },
  { "set_xvar_float",   set_xvar_num },
  { "set_xvar_num",     set_xvar_num },
  { "show_hudmessage",  show_hudmessage },
  { "show_menu",      show_menu },
  { "show_motd",      show_motd },
  { "task_exists",    task_exists },
  { "unpause",        unpause },
  { "user_kill",      user_kill },
  { "user_slap",      user_slap },
  { "xvar_exists",    xvar_exists },
  { "is_module_loaded", is_module_loaded },
  { "is_plugin_loaded", is_plugin_loaded },
  { "get_modulesnum", get_modulesnum },
  { "get_module", get_module },
  { "log_amx", log_amx },
  { "callfunc_begin", callfunc_begin },
  { "callfunc_end", callfunc_end },
  { "callfunc_push_int", callfunc_push_byval },
  { "callfunc_push_str", callfunc_push_str },
  { "callfunc_push_float", callfunc_push_byval },
  { "callfunc_push_intrf", callfunc_push_byref },
  { "callfunc_push_floatrf", callfunc_push_byref },
  { "message_begin",    message_begin },
  { "message_end",      message_end },
  { "write_angle",    write_angle },
  { "write_byte",     write_byte },
  { "write_char",     write_char },
  { "write_coord",    write_coord },
  { "write_entity",   write_entity },
  { "write_long",     write_long },
  { "write_short",    write_short },
  { "write_string",   write_string },
  { "get_langsnum",			get_langsnum },
  { "get_lang",				get_lang },
  { "register_dictionary",	register_dictionary },
  { "lang_exists",			lang_exists },
  { NULL, NULL }
};
