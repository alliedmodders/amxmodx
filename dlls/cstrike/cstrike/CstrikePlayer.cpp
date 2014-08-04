// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#include "CstrikePlayer.h"
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
