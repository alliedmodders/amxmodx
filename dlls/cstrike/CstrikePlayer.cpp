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
	modelled = false;
	inspectModel = false;
	online = false;
}

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

void CCstrikePlayer::SetModel(const char* modelIn)
{
	strcpy(model, modelIn);
}

bool CCstrikePlayer::GetInspectModel()
{
	return inspectModel;
}

void CCstrikePlayer::SetInspectModel(bool inspectModelIn)
{
	inspectModel = inspectModelIn;
}

bool CCstrikePlayer::GetOnline()
{
	return online;
}

void CCstrikePlayer::SetOnline(bool onlineIn)
{
	online = onlineIn;
}

void CCstrikePlayer::Initialize()
{
	SetModelled(false);
	SetInspectModel(false);
	SetOnline(false);
}