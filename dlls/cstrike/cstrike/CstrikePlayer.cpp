// CstrikePlayer.cpp: implementation of the CCstrikePlayer class.
//
//////////////////////////////////////////////////////////////////////

#include "CstrikePlayer.h"

#if defined _MSC_VER
	#if _MSC_VER >= 1400
		// MSVC8 - disable deprecation warnings for "unsafe" CRT functions
		#define _CRT_SECURE_NO_DEPRECATE
	#endif
#endif

#include <string.h> // strcpy()

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CCstrikePlayer::CCstrikePlayer()
{
	modelled = false;
	inspectModel = false;
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
