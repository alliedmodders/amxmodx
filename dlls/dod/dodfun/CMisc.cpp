/*
 * dodfun 
 * Copyright (c) 2004 Lukasz Wlasinski
 *
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#include "amxxmodule.h"
#include "CMisc.h"
#include "dodfun.h"

// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Disconnect(){
	ingame = staminaSet = fuseSet = bot = false;
}

void CPlayer::PutInServer(){
	ingame = true;
}

void CPlayer::Connect(){
	bot = IsBot();
}


void CPlayer::Init( int pi, edict_t* pe )
{
    pEdict = pe;
    index = pi;
	current = 0;
	ingame =  staminaSet = fuseSet = bot = false;

}

void CPlayer::killPlayer(){
	pEdict->v.dmg_inflictor = NULL;
	pEdict->v.health = 0;
	pEdict->v.deadflag = DEAD_RESPAWNABLE;
	pEdict->v.weaponmodel = 0;
	pEdict->v.weapons = 0;
}

void CPlayer::setTeamName( const char *szName ){
	
	for (int i=0;i<16;i++){
		*( (char*)pEdict->pvPrivateData + STEAM_PDOFFSET_TEAMNAME + i ) = szName[i];
	}
}

void CPlayer::getTeamName(char * szName ){
	for (int i=0;i<16;i++){
		szName[i] = *( (char*)pEdict->pvPrivateData + STEAM_PDOFFSET_TEAMNAME + i );
	}
}

void CObjective::SetKeyValue( int index, char *keyname, char *value ){

	KeyValueData pkvd;

	pkvd.szClassName = (char *)STRING(obj[index].pEdict->v.classname);
	pkvd.szKeyName = keyname; // type
	pkvd.szValue = value;
	pkvd.fHandled = false;

	MDLL_KeyValue(obj[index].pEdict, &pkvd);

}

void CObjective::InitObj(int dest , edict_t* ed ){
	MESSAGE_BEGIN( dest, gmsgInitObj,0,ed );
	WRITE_BYTE( count );
	for ( int i=0; i<count; i++ ){
		WRITE_SHORT(ENTINDEX(obj[i].pEdict));
		WRITE_BYTE( obj[i].index );
		WRITE_BYTE( obj[i].owner );
		WRITE_BYTE( obj[i].visible );
		WRITE_BYTE( obj[i].icon_neutral );
		WRITE_BYTE( obj[i].icon_allies );
		WRITE_BYTE( obj[i].icon_axis );
		WRITE_COORD( obj[i].origin_x );
		WRITE_COORD( obj[i].origin_y );
	}
	MESSAGE_END();
}

void CObjective::SetObj(int index){
	MESSAGE_BEGIN(MSG_ALL,gmsgSetObj);
	WRITE_BYTE(obj[index].index);
	WRITE_BYTE(obj[index].owner);
	WRITE_BYTE(0);
	MESSAGE_END();
}

void CObjective::UpdateOwner(int index, int team){
	obj[index].owner = team;
	GET_CP_PD(obj[index].pEdict).owner = team;

	switch ( team ){
		case 0:
			obj[index].pEdict->v.model = MAKE_STRING( GET_CP_PD(obj[index].pEdict).model_neutral );
			obj[index].pEdict->v.body =  GET_CP_PD(obj[index].pEdict).model_body_neutral;
		break;
		case 1:
			obj[index].pEdict->v.model = MAKE_STRING( GET_CP_PD(obj[index].pEdict).model_allies );
			obj[index].pEdict->v.body = GET_CP_PD(obj[index].pEdict).model_body_allies;
		break;
		case 2:
			obj[index].pEdict->v.model = MAKE_STRING( GET_CP_PD(obj[index].pEdict).model_axis );
			obj[index].pEdict->v.body = GET_CP_PD(obj[index].pEdict).model_body_axis;
		break;
	}
	mObjects.SetObj(index);
}

void CObjective::Sort(){
	objinfo_t temp;
	for	(int j=0;j<count-1;j++ ){
		for	(int i=0;i<count-1;i++ ){
			if ( obj[i].index > obj[i+1].index ){
				temp = obj[i+1];
				obj[i+1] = obj[i];
				obj[i] = temp;
			}
		}
	}
}
