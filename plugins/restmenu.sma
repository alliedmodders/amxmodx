/* AMX Mod X
*   Restrict Weapons Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

// Uncomment if you want to have seperate settings for each map
//#define MAPSETTINGS

#include <amxmodx>
#include <amxmisc>

#if !defined NO_STEAM
#define MAXMENUPOS 34
#else
#define MAXMENUPOS 31
#endif

new g_Position[33]
new g_Modified
new g_blockPos[112]
new g_saveFile[64]
new g_Restricted[] = "* This item is restricted *"
new g_menusNames[7][] = {
"pistol",
"shotgun",
"sub",
"rifle",
"machine",
"equip",
"ammo"
}
new g_MenuTitle[7][] = {
"Handguns",
"Shotguns",
"Sub-Machine Guns",
"Assault & Sniper Rifles",
"Machine Guns",
"Equipment",
"Ammunition"
}
new g_menusSets[7][2] = {
#if !defined NO_STEAM
{0,6},{6,8},{8,13},{13,23},{23,24},{24,32},{32,34}
#else
{0,6},{6,8},{8,13},{13,21},{21,22},{22,29},{29,31}
#endif
}
#if !defined NO_STEAM
new g_AliasBlockNum
new g_AliasBlock[MAXMENUPOS]
#endif

// First position is a position of menu (0 for ammo, 1 for pistols, 6 for equipment etc.)
// Second is a key for TERRORIST (all is key are minus one, 1 is 0, 2 is 1 etc.)
// Third is a key for CT
// Position with -1 doesn't exist
new g_Keys[MAXMENUPOS][3] = { 
#if !defined NO_STEAM
  {1,1,1}, // H&K USP .45 Tactical
  {1,0,0}, // Glock18 Select Fire
  {1,3,3}, // Desert Eagle .50AE
  {1,2,2}, // SIG P228
  {1,4,-1}, // Dual Beretta 96G Elite
  {1,-1,4}, // FN Five-Seven
  {2,0,0}, // Benelli M3 Super90
  {2,1,1}, // Benelli XM1014
  {3,1,1}, // H&K MP5-Navy
  {3,-1,0}, // Steyr Tactical Machine Pistol
  {3,3,3}, // FN P90
  {3,0,-1}, // Ingram MAC-10
  {3,2,2}, // H&K UMP45
  {4,1,-1}, // AK-47
  {4,0,-1}, // Gali
  {4,-1,0}, // Famas
  {4,3,-1}, // Sig SG-552 Commando
  {4,-1,2}, // Colt M4A1 Carbine
  {4,-1,3}, // Steyr Aug
  {4,2,1}, // Steyr Scout
  {4,4,5}, // AI Arctic Warfare/Magnum
  {4,5,-1}, // H&K G3/SG-1 Sniper Rifle
  {4,-1,4}, // Sig SG-550 Sniper
  {5,0,0},  // FN M249 Para
  {6,0,0}, // Kevlar Vest
  {6,1,1}, // Kevlar Vest & Helmet
  {6,2,2}, // Flashbang
  {6,3,3}, // HE Grenade
  {6,4,4}, // Smoke Grenade
  {6,-1,6}, // Defuse Kit
  {6,5,5}, // NightVision Goggles
  {6,-1,7}, // Tactical Shield
  {0,5,5}, // Primary weapon ammo
  {0,6,6} // Secondary weapon ammo
#else
  {1,0,0}, // H&K USP .45 Tactical
  {1,1,1}, // Glock18 Select Fire
  {1,2,2}, // Desert Eagle .50AE
  {1,3,3}, // SIG P228
  {1,4,-1}, // Dual Beretta 96G Elite
  {1,-1,5}, // FN Five-Seven
  {2,0,0}, // Benelli M3 Super90
  {2,1,1}, // Benelli XM1014
  {3,0,0}, // H&K MP5-Navy
  {3,-1,1}, // Steyr Tactical Machine Pistol
  {3,2,2}, // FN P90
  {3,3,-1}, // Ingram MAC-10
  {3,4,4}, // H&K UMP45
  {4,0,-1}, // AK-47
  {4,1,-1}, // Sig SG-552 Commando
  {4,-1,2}, // Colt M4A1 Carbine
  {4,-1,3}, // Steyr Aug
  {4,4,4}, // Steyr Scout
  {4,5,5}, // AI Arctic Warfare/Magnum
  {4,6,-1}, // H&K G3/SG-1 Sniper Rifle
  {4,-1,7}, // Sig SG-550 Sniper
  {5,0,0},  // FN M249 Para
  {6,0,0}, // Kevlar Vest
  {6,1,1}, // Kevlar Vest & Helmet
  {6,2,2}, // Flashbang
  {6,3,3}, // HE Grenade
  {6,4,4}, // Smoke Grenade
  {6,-1,5}, // Defuse Kit
  {6,6,6}, // NightVision Goggles
  {0,5,5}, // Primary weapon ammo
  {0,6,6} // Secondary weapon ammo  
#endif
}

new g_WeaponNames[MAXMENUPOS][] = {
  "H&K USP .45 Tactical",
  "Glock18 Select Fire",
  "Desert Eagle .50AE",
  "SIG P228",
  "Dual Beretta 96G Elite",
  "FN Five-Seven",
  "Benelli M3 Super90",
  "Benelli XM1014",
  "H&K MP5-Navy",
  "Steyr Tactical Machine Pistol",
  "FN P90",
  "Ingram MAC-10",
  "H&K UMP45",
  "AK-47",
#if !defined NO_STEAM
  "Gali",
  "Famas",
#endif
  "Sig SG-552 Commando",
  "Colt M4A1 Carbine",
  "Steyr Aug",
  "Steyr Scout",
  "AI Arctic Warfare/Magnum",
  "H&K G3/SG-1 Sniper Rifle",
  "Sig SG-550 Sniper",
  "FN M249 Para",
  "Kevlar Vest",
  "Kevlar Vest & Helmet",
  "Flashbang",
  "HE Grenade",
  "Smoke Grenade",
  "Defuse Kit",
  "NightVision Goggles",
#if !defined NO_STEAM 
  "Tactical Shield",
#endif
  "Primary weapon ammo",
  "Secondary weapon ammo"  
}

new g_MenuItem[MAXMENUPOS][] = { 
  "\yHandguns^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w^n", 

  "\yShotguns^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w^n", 

  "\ySub-Machine Guns^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w^n", 

  "\yAssault Rifles^n\w^n%d. %s\y\R%s^n\w", 
#if !defined NO_STEAM
  "%d. %s\y\R%s^n\w",
  "%d. %s\y\R%s^n\w",
#endif  
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w^n", 

  "\ySniper Rifles^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w^n", 

  "\yMachine Guns^n\w^n%d. %s\y\R%s^n\w^n", 
  
  "\yEquipment^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w", 
#if !defined NO_STEAM
  "%d. %s\y\R%s^n\w",
  "%d. %s\y\R%s^n\w^n",
#else
  "%d. %s\y\R%s^n\w^n",
#endif

  "\yAmmunition^n\w^n%d. %s\y\R%s^n\w", 
  "%d. %s\y\R%s^n\w"
} 

new g_Aliases[MAXMENUPOS][] = {
"usp",//Pistols
"glock",
"deagle",
"p228",
"elites",
"fn57",

"m3",//Shotguns
"xm1014",

"mp5",//SMG
"tmp",
"p90",
"mac10",
"ump45",

"ak47",//Rifles
#if !defined NO_STEAM
"galil",
"famas",
#endif
"sg552",
"m4a1",
"aug",
"scout",
"awp",
"g3sg1",
"sg550",

"m249", //Machine Gun

"vest",//Equipment
"vesthelm",
"flash",
"hegren",
"sgren",
"defuser",
"nvgs",
#if !defined NO_STEAM
"shield",
#endif
"primammo",//Ammo
"secammo"
}

#if !defined NO_STEAM
new g_Aliases2[MAXMENUPOS][] = {
"km45",//Pistols
"9x19mm",
"nighthawk",
"228compact",
"elites",
"fiveseven",

"12gauge",//Shotguns
"autoshotgun",

"smg",//SMG
"mp",
"c90",
"mac10",
"ump45",

"cv47",//Rifles
"defender",
"clarion",
"krieg552",
"m4a1",
"bullpup",
"scout",
"magnum",
"d3au1",
"krieg550",

"m249", //Machine Gun

"vest",//Equipment
"vesthelm",
"flash",
"hegren",
"sgren",
"defuser",
"nvgs",
"shield",
"primammo",//Ammo
"secammo"
}
#endif

public plugin_init(){ 
  register_plugin("Restrict Weapons","0.15","AMXX Dev Team")
  register_clcmd("buyammo1","ammoRest1")
  register_clcmd("buyammo2","ammoRest2")  
#if !defined NO_STEAM
  register_clcmd("cl_setautobuy","blockcommand")
  register_clcmd("cl_autobuy","blockcommand")
  register_clcmd("cl_setrebuy","blockcommand")
  register_clcmd("cl_rebuy","blockcommand")
#endif
  register_clcmd("amx_restmenu","cmdMenu",ADMIN_CFG,"- displays weapons restriction menu")
  register_menucmd(register_menuid("#Buy", 1 ),511,"menuBuy")
  register_menucmd(register_menuid("\yRestrict Weapons"),1023,"actionMenu")
  register_menucmd(register_menuid("BuyPistol", 1 ),511,"menuPistol")
  register_menucmd(register_menuid("BuyShotgun", 1 ),511,"menuShotgun")
  register_menucmd(register_menuid("BuySub", 1 ),511,"menuSub")
  register_menucmd(register_menuid("BuyRifle", 1 ),511,"menuRifle")
  register_menucmd(register_menuid("BuyMachine", 1 ),511,"menuMachine")
  register_menucmd(register_menuid("BuyItem", 1 ),511,"menuItem")
  register_menucmd(-28,511,"menuBuy" )
  register_menucmd(-29,511,"menuPistol" )
  register_menucmd(-30,511,"menuShotgun")
  register_menucmd(-32,511,"menuSub")
  register_menucmd(-31,511,"menuRifle")
  register_menucmd(-33,511,"menuMachine")
  register_menucmd(-34,511,"menuItem")
  register_concmd("amx_restrict","cmdRest",ADMIN_CFG,"- displays help for weapons restriction")

#if defined MAPSETTINGS
  new mapname[32]
  get_mapname(mapname,31)
  build_path(g_saveFile,63,"addons/amxx/configs/weaprest_%s.ini",mapname)
#else
  build_path(g_saveFile,63,"addons/amxx/configs/weaprest.ini")
#endif  
  loadSettings(g_saveFile)
}

setWeapon( a , action  ){
  new b, m = g_Keys[a][0] * 8
  if (g_Keys[a][1] != -1) {
    b = m + g_Keys[a][1]
    if ( action == 2 )
      g_blockPos[ b ] = 1 - g_blockPos[ b ]
    else
      g_blockPos[ b ] = action
  }
  if (g_Keys[a][2] != -1) {
    b = m + g_Keys[a][2] + 56
    if ( action == 2 )
      g_blockPos[ b ] = 1 - g_blockPos[ b ]
    else
      g_blockPos[ b ] = action
  }
#if !defined NO_STEAM
  for(new i = 0; i < g_AliasBlockNum; ++i)
    if ( g_AliasBlock[ i ] == a ){
      if ( !action || action == 2 ) {
        --g_AliasBlockNum
        for(new j = i; j < g_AliasBlockNum; ++j )
          g_AliasBlock[ j ] = g_AliasBlock[ j + 1 ]
      }
      return
    }
  if ( action && g_AliasBlockNum < MAXMENUPOS )
    g_AliasBlock[ g_AliasBlockNum++ ] = a
#endif    
}

findMenuId( name[] ){
  for(new i = 0; i < 7 ; ++i)
    if( equal( name , g_menusNames[i] ) )
      return i
  return -1
}

findAliasId( name[] ){
  for(new i = 0; i < MAXMENUPOS ; ++i)
    if( equal( name , g_Aliases[i] ) )
      return i
  return -1
}

switchCommand( id, action ){
  new c = read_argc()
  if ( c < 3 ){
    setc( g_blockPos, 112, action )
    console_print( id , "Equipment and weapons have been %srestricted" , action ? "" : "un" )
    g_Modified = true
  }
  else {
    new arg[32], a
    new bool:found = false
    for(new b = 2; b < c; ++b){
      read_argv(b,arg,31)
      if ( (a = findMenuId( arg )) != -1 ){  
        c = g_menusSets[a][1]
        for(new i = g_menusSets[a][0]; i < c; ++i)
          setWeapon( i , action )
        console_print( id , "%s %s been %srestricted" , g_MenuTitle[a], (a<5) ? "have" : "has" , action ? "" : "un" )
        g_Modified = found = true
      }
      else if ( (a = findAliasId( arg )) != -1  ){
        g_Modified = found = true
        setWeapon( a , action )
        console_print( id , "%s has been %srestricted" , g_WeaponNames[a], action ? "" : "un" )
      }
    }
    if ( !found )
      console_print( id , "Couldn't find such equipment or weapon" )
  }
}

positionBlocked( a ) {
  new m = g_Keys[a][0] * 8
  new d = ( g_Keys[a][1]==-1) ? 0 : g_blockPos[ m + g_Keys[a][1] ]
  d += ( g_Keys[a][2]==-1) ? 0 : g_blockPos[ m + g_Keys[a][2] + 56  ]
  return d
}

public cmdRest(id,level,cid){ 
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  new cmd[8]
  read_argv(1,cmd,7)
  if ( equali( "on" , cmd ) )
    switchCommand( id, 1 )
  else if ( equali( "off" , cmd ) )
    switchCommand( id, 0 )
  else if ( equali( "list" , cmd ) ) {
    new arg1[8]
    new start = read_argv(2,arg1,7) ? str_to_num(arg1) : 1
    if (--start < 0) start = 0
    if (start >= MAXMENUPOS) start = MAXMENUPOS - 1
    new end = start + 10
    if (end > MAXMENUPOS) end = MAXMENUPOS  
    console_print(id, "^n----- Weapons Restriction: -----")
    console_print(id, "     %-32.31s   %-10.9s   %-9.8s","name","value","status")
    if ( start != -1 ) { 
      for(new a = start; a < end; ++a){
        console_print(id, "%3d: %-32.31s   %-10.9s   %-9.8s",a + 1,
        g_WeaponNames[a], g_Aliases[a], positionBlocked(a) ? "ON" : "OFF")
      }
    }
    console_print(id,"----- Entries %i - %i of %i -----",start+1,end,MAXMENUPOS)    
    if (end < MAXMENUPOS)
      console_print(id,"----- Use 'amx_restrict list %i' for more -----",end+1)
    else
      console_print(id,"----- Use 'amx_restrict list 1' for begin -----")
  }  
  else if ( equali( "save" , cmd ) ) {
    if ( saveSettings( g_saveFile ) ){
      console_print( id , "Configuration has been saved (file ^"%s^")" , g_saveFile )
      g_Modified = false
    }
    else console_print( id , "Couldn't save configuration (file ^"%s^")" , g_saveFile )
  }
  else if ( equali( "load" , cmd ) ) {
    setc( g_blockPos, 112, 0 ) // Clear current settings
    new arg1[64]
    if ( read_argv(2, arg1 , 63 ) ) build_path( arg1 , 63, "$basedir/%s", arg1 )
    else copy( arg1, 63,  g_saveFile )
    if ( loadSettings( arg1 ) ){
      console_print( id , "Configuration has been loaded (file ^"%s^")" , arg1 )
      g_Modified = true
    }
    else console_print( id , "Couldn't load configuration (file ^"%s^")" , arg1 )
  }
  else {
    console_print(id,"Usage:  amx_restrict <command> [value]")
    console_print(id,"Commands:")
    console_print(id,"^ton - set restriction on whole equipment")    
    console_print(id,"^toff - remove restriction from whole equipment")   
    console_print(id,"^ton <value> [...] - set specified restriction")
    console_print(id,"^toff <value> [...] - remove specified restriction")
    console_print(id,"^tlist - display list of available equipment and weapons")
    console_print(id,"^tsave - save restriction")
    console_print(id,"^tload [file] - load restriction [from a file]")
    console_print(id,"Available values to restrict are:^nammo, equip, pistol, shotgun, sub, rifle, machine")
    console_print(id,"Type 'amx_restrict list' for more specified values")    
  }
  return PLUGIN_HANDLED
} 

displayMenu(id,pos){ 
   if (pos < 0) return
   new menubody[512], start = pos * 7 
   if (start >= MAXMENUPOS) start = pos = g_Position[id] = 0 
   new len = format(menubody,511,"\yRestrict Weapons\R%d/5^n\w^n",pos+1) 
   new end = start + 7, keys = (1<<9)|(1<<7), k = 0
   if (end > MAXMENUPOS) end = MAXMENUPOS 
   for(new a = start; a < end; ++a){ 
      keys |= (1<<k)
      len += format(menubody[len],511-len,g_MenuItem[a],++k,g_WeaponNames[a], 
        positionBlocked(a) ? "ON" : "OFF" )
   } 
   len += format(menubody[len],511-len,"^n8. Save settings \y\R%s^n\w",g_Modified?"*":"") 
   if (end != MAXMENUPOS){ 
      format(menubody[len],511-len,"^n9. More...^n0. %s", pos ? "Back" : "Exit") 
      keys |= (1<<8) 
   }
   else format(menubody[len],511-len,"^n0. %s", pos ? "Back" : "Exit") 
   show_menu(id,keys,menubody) 
} 

public actionMenu(id,key){
   switch(key){
   case 7: {
      if (saveSettings(g_saveFile)){
        g_Modified = false
        client_print(id,print_chat,"* Configuration saved successfully")
      }
      else client_print(id,print_chat,"* Configuration saving failed!!!")
      displayMenu(id,g_Position[id])
    }
   case 8: displayMenu(id,++g_Position[id])
   case 9: displayMenu(id,--g_Position[id])
   default: {
        setWeapon( g_Position[id] * 7 + key , 2 )
        g_Modified = true
        displayMenu(id,g_Position[id])
      }
   }
   return PLUGIN_HANDLED
}

#if !defined NO_STEAM
public client_command( id ){
  if ( g_AliasBlockNum  ) {
    new arg[13]
    if ( read_argv( 0, arg , 12 ) > 11 ) /* Longest buy command has 11 chars so if command is longer then don't care */
      return PLUGIN_CONTINUE
    new a = 0
    do {
      if ( equal( g_Aliases[g_AliasBlock[ a ]] , arg  ) ||
           equal( g_Aliases2[g_AliasBlock[ a ]] , arg  ) ) {
        client_print(id,print_center,g_Restricted )
        return PLUGIN_HANDLED
      }
    } while( ++a < g_AliasBlockNum )
  }
  return PLUGIN_CONTINUE
}
#endif

#if !defined NO_STEAM
public blockcommand(id) {
    client_print(id,print_center, g_Restricted )
    return PLUGIN_HANDLED 
}
#endif

public cmdMenu(id,level,cid){ 
  if (cmd_access(id,level,cid,1)) 
    displayMenu(id, g_Position[id] = 0 ) 
  return PLUGIN_HANDLED 
} 

checkRest(id,menu,key){
  if ( g_blockPos[ (menu * 8 + key) + (get_user_team(id) - 1) * 56 ] ){
    engclient_cmd(id,"menuselect","10")
    //client_cmd(id,"slot10")
    client_print(id,print_center, g_Restricted )
    return PLUGIN_HANDLED 
  } 
  return PLUGIN_CONTINUE
} 

public ammoRest1(id)        return checkRest(id,0,5)
public ammoRest2(id)        return checkRest(id,0,6)
public menuBuy(id,key)      return checkRest(id,0,key)
public menuPistol(id,key)   return checkRest(id,1,key)
public menuShotgun(id,key)  return checkRest(id,2,key)
public menuSub(id,key)      return checkRest(id,3,key)
public menuRifle(id,key)    return checkRest(id,4,key)
public menuMachine(id,key)  return checkRest(id,5,key)
public menuItem(id,key)     return checkRest(id,6,key)

saveSettings(filename[]){ 
  if (file_exists(filename)) 
    delete_file(filename) 
  if (!write_file(filename,"; Generated by Restrict Weapons Plugin. Do not modify!^n; value name")) 
    return 0
  new text[64]    
  for(new a = 0; a < MAXMENUPOS; ++a){ 
    if ( positionBlocked( a ) ) {
      format(text,63,"%-16.15s ; %s", g_Aliases[a] , g_WeaponNames[a])
      write_file(filename,text)
    } 
  }
  return 1
} 

loadSettings(filename[]){
  if (!file_exists(filename)) return 0
  new text[16]
  new a, pos = 0
  while (read_file(filename,pos++,text,15, a )){
    if ( text[0] == ';' || !a ) continue // line is a comment
    parse( text, text , 15 )
    if ( (a = findAliasId( text )) != -1 )
      setWeapon( a , 1 )
  }
  return 1
}