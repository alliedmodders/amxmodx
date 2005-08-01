/*
 * Copyright (c) 2003-2004 Lukasz Wlasinski
 *
 *    This file is part of TS XMod.
 *
 *    TS XMod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    TS XMod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with TS XMod; if not, write to the Free Software Foundation,
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

#include "CPlayer.h"
#include "tsfun.h"

// *****************************************************
// class CPlayer
// *****************************************************

void CPlayer::Disconnect()
{
	ingame = false;
}
void CPlayer::PutInServer()
{
	ingame = true;

	killingSpree = 0;
	items = 0;
    lastFrag = 0;
    lastKill = 0.0;
	is_specialist = 0;
	set_454 = -1.0;
	set_455 = -1.0;
	set_423 = -1;

	//debug
	frags = 0;

}
void CPlayer::Connect( const char* ippp )
{
	strcpy(ip,ippp);
}

void CPlayer::Init( int pi, edict_t* pe )
{
    pEdict = pe;
    index = pi;
	current = 0;
	ingame = true;
}

long CPlayer::GetOffsetInt(long off)
{
#ifdef __linux__
	off += 5;
#endif

	return *((int *)pEdict->pvPrivateData + off);
}

float CPlayer::GetOffsetFloat(long off)
{
#ifdef __linux__
	off += 5;
#endif

	return *((float *)pEdict->pvPrivateData + off);
}


void CPlayer::SetOffsetInt(long off, long set)
{
#ifdef __linux__
	off += 5;
#endif

	*((int *)pEdict->pvPrivateData + off) = set;
}

void CPlayer::SetOffsetFloat(long off, float set)
{
#ifdef __linux__
	off += 5;
#endif

	*((float *)pEdict->pvPrivateData + off) = set;
}