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
	SetModelled(false);
	SetInspectModel(false);
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