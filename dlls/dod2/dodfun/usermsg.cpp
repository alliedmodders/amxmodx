/*
 * DoDFun 
 * Copyright (c) 2004 £ukasz W³asiñski
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
#include "dodfun.h"

void Client_CurWeapon(void* mValue){
  static int iState;
  static int iId;
  switch (mState++){
  case 0: 
    iState = *(int*)mValue;
    break;
  case 1:
    if (!iState) break; 
    iId = *(int*)mValue;
	mPlayer->current = iId;
	break;
  }
}

void Client_InitObj(void* mValue){
  static int num;

  if ( mDest != MSG_ALL )
	  return;

  switch (mState++){
  case 0:
	  num = 0;
	  mObjects.count = *(int*)mValue;
	  if ( mObjects.count == 0 )
		  mObjects.Clear();
	  break;
  case 1:
	  mObjects.obj[num].pEdict = INDEXENT(*(int*)mValue);
	  break;
  case 2:
	  mObjects.obj[num].index = *(int*)mValue;
	  break;
  case 3:
	  mObjects.obj[num].default_owner = *(int*)mValue;
	  mObjects.obj[num].owner = mObjects.obj[num].default_owner;
	  break;
  case 4:
	  mObjects.obj[num].visible = *(int*)mValue; 
	  break;
  case 5:
	  mObjects.obj[num].icon_neutral = *(int*)mValue;
	  break;
  case 6:
	  mObjects.obj[num].icon_allies = *(int*)mValue;
	  break;
  case 7:
	  mObjects.obj[num].icon_axis = *(int*)mValue;
	  break;
  case 8:
	  mObjects.obj[num].origin_x = *(float*)mValue;
	  break;
  case 9: // 8,9 coord
	  mObjects.obj[num].origin_y = *(float*)mValue;
	  mState = 1;
	  num++;
	  if ( num == mObjects.count ){
		  mObjects.Sort();
		  MF_ExecuteForward( iFInitCP );
	  }

	  break;
  }
}

void Client_SetObj(void* mValue){
  static int id;
  switch (mState++){
  case 0:
	  id = *(int*)mValue; 
     break;
  case 1:
	  mObjects.obj[id].owner = *(int*)mValue;
	break;
  }
}
