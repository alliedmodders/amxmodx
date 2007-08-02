/* AMX Mod X 
 *   Natural Selection Module 
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

/* This file contains the initialization routine and message hooks
 * for the message handler.  Note that all of the message hooks
 * except for MessageBegin_Post are NOT hooked unless the gameDLL
 * is sending a message we care about.
 */

#include "sdk/amxxmodule.h"

#include "ns.h"
#include "utilfunctions.h"

#include "GameManager.h"
#include "MessageHandler.h"

MessageHandler *MessageLists[256];

MessageHandler *HookedMessage;

bool MessageHandler_Initialized=false;

// This is called through ServerActivate_Post
void Initialize_MessageHandler(void)
{
	if (MessageHandler_Initialized)
	{
		return;
	}

	MessageHandler_Initialized=true;

	int i=0;

	while (i<255)
	{
		MessageLists[i++]=NULL;
	};

	// Hook our messages
	int index;

	index=GET_USER_MSG_ID(&Plugin_info,"Countdown",NULL);

	if (index)
	{
		MessageLists[index]=new MessageCountDown;
	}

	index=GET_USER_MSG_ID(&Plugin_info,"GameStatus",NULL);

	if (index)
	{
		MessageLists[index]=new MessageGameStatus;
	}

#if 0
	index=GET_USER_MSG_ID(&Plugin_info,"Particles",NULL);
	
	if (index)
	{
		MessageLists[index]=new MessageDebug;
	}
#endif
	// Start hooking messagebegin_post
	g_pengfuncsTable_Post->pfnMessageBegin=MessageBegin_Post;

};

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	// Sanity check, should never matter
	if (msg_type < 0 || msg_type > 255)
	{
		RETURN_META(MRES_IGNORED);
	}

	// Has a hooked message?
	if (MessageLists[msg_type]!=NULL) 
	{
		// Should this message be hooked?
		if (MessageLists[msg_type]->Begin(msg_dest,msg_type,pOrigin,ed)==1)
		{
			// This message is going to all be hooked

			// save pointer to our class
			HookedMessage=MessageLists[msg_type];

			// Tell metamod to forward
			g_pengfuncsTable_Post->pfnWriteByte=WriteByte_Post;
			g_pengfuncsTable_Post->pfnWriteChar=WriteChar_Post;
			g_pengfuncsTable_Post->pfnWriteShort=WriteShort_Post;
			g_pengfuncsTable_Post->pfnWriteLong=WriteLong_Post;
			g_pengfuncsTable_Post->pfnWriteAngle=WriteAngle_Post;
			g_pengfuncsTable_Post->pfnWriteCoord=WriteCoord_Post;
			g_pengfuncsTable_Post->pfnWriteString=WriteString_Post;
			g_pengfuncsTable_Post->pfnWriteEntity=WriteEntity_Post;
			g_pengfuncsTable_Post->pfnMessageEnd=MessageEnd_Post;


		}
	}
	RETURN_META(MRES_IGNORED);
}

void MessageEnd_Post(void)
{
	HookedMessage->End();

	HookedMessage=NULL;

	// Stop metamod forwarding
	g_pengfuncsTable_Post->pfnWriteByte=NULL;
	g_pengfuncsTable_Post->pfnWriteChar=NULL;
	g_pengfuncsTable_Post->pfnWriteShort=NULL;
	g_pengfuncsTable_Post->pfnWriteLong=NULL;
	g_pengfuncsTable_Post->pfnWriteAngle=NULL;
	g_pengfuncsTable_Post->pfnWriteCoord=NULL;
	g_pengfuncsTable_Post->pfnWriteString=NULL;
	g_pengfuncsTable_Post->pfnWriteEntity=NULL;
	g_pengfuncsTable_Post->pfnMessageEnd=NULL;

	RETURN_META(MRES_IGNORED);
};

void WriteByte_Post(int iValue)
{
	HookedMessage->WriteByte(iValue);
	RETURN_META(MRES_IGNORED);
};

void WriteChar_Post(int iValue)
{
	HookedMessage->WriteChar(iValue);
	RETURN_META(MRES_IGNORED);
};

void WriteShort_Post(int iValue)
{
	HookedMessage->WriteShort(iValue);
	RETURN_META(MRES_IGNORED);
};

void WriteLong_Post(int iValue)
{
	HookedMessage->WriteLong(iValue);
	RETURN_META(MRES_IGNORED);
};

void WriteAngle_Post(float flValue)
{
	HookedMessage->WriteAngle(flValue);
	RETURN_META(MRES_IGNORED);
};

void WriteCoord_Post(float flValue)
{
	HookedMessage->WriteCoord(flValue);
	RETURN_META(MRES_IGNORED);
};

void WriteString_Post(const char *sz)
{
	HookedMessage->WriteString(sz);
	RETURN_META(MRES_IGNORED);
};

void WriteEntity_Post(int iValue)
{
	HookedMessage->WriteEntity(iValue);
	RETURN_META(MRES_IGNORED);
};

