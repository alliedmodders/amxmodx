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
	this->model[0]='\0';
	this->skin=0;
	this->body=0;
	this->fov=0.0;
	this->foved=false;
}