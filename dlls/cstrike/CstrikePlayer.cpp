// CstrikePlayer.cpp: implementation of the CCstrikePlayer class.
//
//////////////////////////////////////////////////////////////////////

#include "CstrikePlayer.h"
#include <string.h> // strcpy()

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CCstrikePlayer::CCstrikePlayer()
{
	//SetOnline(false);
	SetModelled(false);
	//SetTime(0.0);
	SetInspectModel(false);
}

/*bool CCstrikePlayer::GetOnline()
{
	return online;
}

bool CCstrikePlayer::SetOnline(bool onlineIn)
{
	return online = onlineIn;
}*/

bool CCstrikePlayer::GetModelled()
{
	return modelled;
}

bool CCstrikePlayer::SetModelled(bool modelledIn)
{
	if (!modelledIn)
		SetInspectModel(false);

	return modelled = modelledIn;
}

const char* CCstrikePlayer::GetModel()
{
	return model;
}

const char* CCstrikePlayer::SetModel(const char* modelIn)
{
	//SetTime(0.0);
	return strcpy(model, modelIn);
}

/*float CCstrikePlayer::GetTime()
{
	return time;
}

void CCstrikePlayer::SetTime(float timeIn)
{
	time = timeIn;
}
*/
bool CCstrikePlayer::GetInspectModel()
{
	return inspectModel;
}

void CCstrikePlayer::SetInspectModel(bool inspectModelIn)
{
	inspectModel = inspectModelIn;
}