/* AMX Mod X script.
*   Multilingual System Plugin
*
* by the AMX Mod X Development Team
*
* This file is part of AMX Mod X.
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

#include <amxmodx>
#include <amxmisc>

new g_userLang[33][3]
new g_menuLang[33][2]
new g_serverLang
new g_langNum
new g_coloredMenus

public plugin_init() {
  register_plugin("Multi-Lingual System",AMXX_VERSION_STR,"AMXX Dev Team")
  register_dictionary("multilingual.txt")
  register_dictionary("common.txt")
  register_cvar("amxx_language","en",FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_SPONLY)
  register_concmd("amxx_setlang","cmdLang",ADMIN_CFG,"<language>")
  register_clcmd("amxx_setlangmenu","cmdLangMenu",ADMIN_ALL)
  register_menu("Language Menu",1023,"actionMenu")

  new lang[3]
  if ( vaultdata_exists("server_language") ) {
    get_vaultdata("server_language",lang,2)
  }
  else {
    copy(lang,2,"en")
    set_vaultdata("server_language",lang)
  }
  set_cvar_string("amxx_language",lang)
  g_serverLang = get_lang_id(lang)

  g_langNum = get_langsnum()
  g_coloredMenus = colored_menus()
}

public client_infochanged(id) {
  new lang[3]
  get_user_info(id,"lang",lang,2)

  if ( lang_exists(lang) )
    copy(g_userLang[id],2,lang)
  else if ( g_userLang[id][0] )
    set_user_info(id,"lang",g_userLang[id])
  else
    set_user_info(id,"lang","en")
}

public cmdLang(id,level,cid) {
  if (!cmd_access(id,level,cid,2))
    return PLUGIN_HANDLED

  new arg[3]
  read_argv(1,arg,2)

  if ( !lang_exists(arg) ) {
    console_print(id,"[AMXX] %L",id,"LANG_NOT_EXISTS")
    return PLUGIN_HANDLED
  }

  set_vaultdata("server_language",arg)
  set_cvar_string("amxx_language",arg)
  g_serverLang = get_lang_id(arg)

  return PLUGIN_HANDLED
}

public cmdLangMenu(id,level,cid) {
  g_menuLang[id][0] = get_lang_id(g_userLang[id])
  g_menuLang[id][1] = g_serverLang

  showMenu(id)

  return PLUGIN_HANDLED
}

showMenu(id) {
  new menuBody[512],perso_lang[64],pLang[3]

  format(perso_lang,63,"%L",id,"PERSO_LANG")
  get_lang(g_menuLang[id][0],pLang)

  new len = format( menuBody,511,(g_coloredMenus ? "\y%L\w^n^n" : "%L^n^n"),id,"LANG_MENU" )
  len += format( menuBody[len],511-len,(g_coloredMenus ? "1. %s\R\r%s\w^n" : "1. %s %s^n"),perso_lang,pLang )

  if ( access(id,ADMIN_CFG) ) {
    new server_lang[64],sLang[3]
    format(perso_lang,63,"%L",id,"SERVER_LANG")
    get_lang(g_menuLang[id][1],sLang)
    len += format( menuBody[len],511-len,(g_coloredMenus ? "2. %s\R\r%s\w^n^n" : "2. %s %s^n^n"),server_lang,sLang )
    len += format( menuBody[len],511-len,"3. %L",id,"SAVE_LANG" )
  }
  else {
    len += format( menuBody[len],511-len,"^n2. %L",id,"SAVE_LANG" )
  }
  format( menuBody[len],511-len,"^n^n0. %L",id,"EXIT" )

  show_menu(id,MENU_KEY_0|MENU_KEY_1|MENU_KEY_2|MENU_KEY_3,menuBody,-1,"Language Menu")
}

public actionMenu(id,key) {
  new isAdmin = access(id,ADMIN_CFG)

  if ( key==0 ) {
    if ( g_menuLang[id][0]<(g_langNum-1) )
      g_menuLang[id][0]++
    else
      g_menuLang[id][0] = 0
    showMenu(id)
  }

  if ( isAdmin && (key==1) ) {
    if ( g_menuLang[id][1]<(g_langNum-1) )
      g_menuLang[id][1]++
    else
      g_menuLang[id][1] = 0
    showMenu(id)
  }

  if ( isAdmin && (key==2) ) {
    new sLang[3]
    get_lang(g_menuLang[id][1],sLang)
    set_vaultdata("server_language",sLang)
    set_cvar_string("amxx_language",sLang)
    g_serverLang = g_menuLang[id][1]
    client_print(id,print_chat,"%L",id,"SET_LANG_SERVER",sLang)
  }

  if ( ( isAdmin && (key==2) ) || ( !isAdmin && (key==1) ) ) {
    new pLang[3]
    get_lang(g_menuLang[id][0],pLang)
    copy(g_userLang[id],2,pLang)
    set_user_info(id,"lang",pLang)
    client_print(id,print_chat,"%L",id,"SET_LANG_USER",pLang)
    showMenu(id)
  }
}

get_lang_id(lang[]) {
  new tLang[3]
  for (new i=0;i<g_langNum;i++) {
    get_lang(i,tLang)
    if ( equali(tLang,lang) )
      return i
  }

  return 0
}