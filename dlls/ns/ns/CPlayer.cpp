#include "ns.h"

void CPlayer::PreThink()
{
	if (!connected)
	{
		// This usually means it's a bot...
		// So just emulate connections
		Connect();
		bot=true;
	}
	int tClass = GetClass();
	if (tClass != iclass)
		ChangeClass(tClass);
	oldimpulse=pev->impulse;
}
void CPlayer::PreThink_Post()
{
	// Trying to incorperate this into PostThink_Post led to really *weird* results (i don't think it was propagated to the client properly).
	// Change the users speed here
	maxspeed=(int)pev->maxspeed;
	pev->maxspeed+=speedchange;
}

void CPlayer::PostThink_Post()
{
	// Change the player's models...
	if (custommodel)
		SET_MODEL(edict,model);
	if (customskin)
		pev->skin=skin;
	if (custombody)
		pev->body=body;
}
void CPlayer::ChangeClass(int newclass)
{
	MF_ExecuteForward(ChangeclassForward, index, newclass, iclass, oldimpulse);
	iclass=newclass;
}

int CPlayer::GetClass()
{
  if (pev->deadflag)
    return CLASS_DEAD;
  if (!pev->team)
    return CLASS_NOTEAM;
  switch (pev->iuser3)
  {
  case 1:
    // Light armor marine..
    if (pev->iuser4 & MASK_HEAVYARMOR)
      return CLASS_HEAVY;
    if (pev->iuser4 & MASK_JETPACK)
      return CLASS_JETPACK;
    return CLASS_MARINE;
  case 2:
    return CLASS_COMMANDER;
  case 3:
    return CLASS_SKULK;
  case 4:
    return CLASS_GORGE;
  case 5:
    return CLASS_LERK;
  case 6:
    return CLASS_FADE;
  case 7:
    return CLASS_ONOS;
  case 8:
    return CLASS_GESTATE;
  }
  return CLASS_UNKNOWN;

}
void CPlayer::Connect()
{
	connected=true;
	bot=false;
	Reset();
}
void CPlayer::Disconnect()
{
	connected=false;
	bot=false;
	Reset();
}
void CPlayer::Reset()
{
	this->custombody=false;
	this->custommodel=false;
	this->customskin=false;
	this->maxspeed=0;
	this->speedchange=0;
	this->menucmd.chan=0;
	this->menucmd.channel1=0;
	this->menucmd.channel2=0;
	this->menucmd.iFunctionIndex=0;
	this->menucmd.inmenu=false;
	this->menucmd.keys=0;
	this->menucmd.text[0]='\0';
	this->menucmd.time=0.0;
	this->model[0]='\0';
	this->skin=0;
	this->body=0;
	this->fov=0.0;
	this->foved=false;
}
BOOL CPlayer::ClientCommand()
{
	if (menucmd.inmenu)
	{
		if (FStrEq(CMD_ARGV(0),"menuselect"))
		{
			int key=0;
			int key_mask=0;
			menucmd.inmenu=false;
			if (gpGlobals->time > menucmd.time)
			{
				menucmd.inmenu=0;
				return false;
			}
			key=atoi(CMD_ARGV(1)) - 1;
			key_mask=(1<<key);
			if (key_mask & menucmd.keys)
			{
				ClearHudMessage(edict,menuhudtext," ");
				int temp = this->menucmd.iFunctionIndex;
				MF_ExecuteForward(this->menucmd.iFunctionIndex,this->index,key);
				MF_UnregisterSPForward(temp);
			}
			return true;
		}
	}
	return false;
}