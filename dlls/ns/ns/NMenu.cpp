#include "ns.h"

static cell AMX_NATIVE_CALL ns_set_menu(AMX *amx,cell *params)
{
	if (params[1] == 0) // Setting this for all
	{
		int i;
		for (i=0;i<=32;i++)
		{
			CPlayer *p = GET_PLAYER_I(i);
			
			p->menuhudtext.r1=params[2];
			p->menuhudtext.g1=params[3];
			p->menuhudtext.b1=params[4];
			p->menuhudtext.x=CELL_TO_FLOAT(params[5]);
			p->menuhudtext.y=CELL_TO_FLOAT(params[6]);
			p->menuhudtext.effect=params[7];
			p->menuhudtext.fxTime=CELL_TO_FLOAT(params[8]);
			p->menuhudtext.fadeinTime=CELL_TO_FLOAT(params[9]);
			p->menuhudtext.channel=params[10];
			p->menucmd.channel1=params[10];
			p->menucmd.channel2=params[11];

		}
	}
	else // Setting this specific
	{
		CPlayer *p = GET_PLAYER_I(params[1]);
		p->menuhudtext.r1=params[2];
		p->menuhudtext.g1=params[3];
		p->menuhudtext.b1=params[4];
		p->menuhudtext.x=CELL_TO_FLOAT(params[5]);
		p->menuhudtext.y=CELL_TO_FLOAT(params[6]);
		p->menuhudtext.effect=params[7];
		p->menuhudtext.fxTime=CELL_TO_FLOAT(params[8]);
		p->menuhudtext.fadeinTime=CELL_TO_FLOAT(params[9]);
		p->menuhudtext.channel=params[10];
		p->menucmd.channel1=params[10];
		p->menucmd.channel2=params[11];
	}
	return 1;
}

static cell AMX_NATIVE_CALL ns_show_menu(AMX *amx,cell *params)
{
	// Format: show_ns_menu(id=0,szcommand,sztext,keys,time)
	edict_t *pEntity=NULL;
	CPlayer *p = GET_PLAYER_I(params[1]);
	int iFunctionIndex;
	if (params[1]!=0 && params[1]<=gpGlobals->maxClients)
	{
		pEntity=INDEXENT2(params[1]);
		if (FNullEnt(INDEXENT2(params[1])))
			return 0;
	}
	int len;
	int ifx;
	char sTemp[20];
	char sText[512];
	strncpy(sTemp,MF_GetAmxString(amx,params[2],0,&len),19);
	strncpy(sText,MF_GetAmxString(amx,params[3],1,&len),511);
	sTemp[19]='\0';
	sText[511]='\0';
	if (MF_AmxFindPublic(amx,sTemp,&ifx) == AMX_ERR_NONE)
		iFunctionIndex = MF_RegisterSPForward(amx,ifx,FP_CELL, FP_CELL,FP_DONE);
	else
	{
		MF_Log("(ns_show_menu): Function not found: %s",sTemp);
		return 0;
	}
	if (pEntity==NULL)
	{
		int i;
		for (i=1;i<=gpGlobals->maxClients;i++)
		{
			CPlayer *p = GET_PLAYER_I(i);
			if (p->connected) /* Grrrrrrrrr */
			{
				p->menucmd.iFunctionIndex=iFunctionIndex;
				p->menucmd.inmenu=true;
				p->menucmd.time=gpGlobals->time+(float)params[5];
				sprintf(p->menucmd.text,"%s",sText);
				p->menucmd.keys=params[4];
				p->menuhudtext.channel = p->menucmd.chan ? p->menucmd.channel1 : p->menucmd.channel2;
				p->menuhudtext.holdTime=(float)params[5];
				p->menucmd.chan=p->menuhudtext.channel;
				p->menucmd.chan = p->menucmd.chan ? 0 : 1;
//				UTIL_EmptyMenu(INDEXENT2(i),params[4],params[5]);
//				HudMessage(i,p->menuhudtext,sText);
			}
		}
		UTIL_EmptyMenu(0,params[4],params[5]);
		HudMessage(0,p->menuhudtext,sText);
	}
	else
	{
		CPlayer *p = GET_PLAYER_I(params[1]);
		if (p->connected)
		{
			p->menucmd.iFunctionIndex=iFunctionIndex;
			p->menucmd.inmenu=true;
			p->menucmd.time=gpGlobals->time+(float)params[5];
			sprintf(p->menucmd.text,"%s",sText);
			p->menucmd.keys=params[4];
			p->menuhudtext.channel = p->menucmd.chan ? p->menucmd.channel1 : p->menucmd.channel2;
			UTIL_EmptyMenu(INDEXENT2(params[1]),params[4],params[5]);
			p->menuhudtext.holdTime=(float)params[5];
			HudMessage(params[1],p->menuhudtext,sText);
			p->menucmd.chan=p->menuhudtext.channel;
			p->menucmd.chan = p->menucmd.chan ? 0 : 1;
		}
	}
	return 1;
}
AMX_NATIVE_INFO ns_menu_natives[] = {
	{ "ns_set_menu",	ns_set_menu },
	{ "ns_show_menu",	ns_show_menu },
	{ NULL, NULL }
};

