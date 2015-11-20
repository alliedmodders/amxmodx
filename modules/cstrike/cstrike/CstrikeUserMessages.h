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

#ifndef CSTRIKE_USER_MESSAGES_H
#define CSTRIKE_USER_MESSAGES_H

extern int MessageIdArmorType;
extern int MessageIdHLTV;
extern int MessageIdMoney;
extern int MessageIdResetHUD;
extern int MessageIdScoreAttrib;
extern int MessageIdScoreInfo;
extern int MessageIdSetFOV;
extern int MessageIdStatusIcon;
extern int MessageIdTeamInfo;
extern int MessageIdTextMsg;

void EnableMessageHooks();
void DisableMessageHooks(bool force = false);

#endif // CSTRIKE_USER_MESSAGES_H
