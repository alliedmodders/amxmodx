// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#ifndef HOOK_SPECIALBOT_H
#define HOOK_SPECIALBOT_H

#include "ham_utils.h"
#include <amtl/am-vector.h>
#include "forward.h"

class CRegisterHamParams
{
public:
	AMX *amx;
	int func;
	char *function;
	int post;
	Forward *pfwd;
	
	CRegisterHamParams(AMX *arg_amx, int &arg_func, const char *arg_function, int &arg_post, Forward *arg_pfwd);
	~CRegisterHamParams();
private:
	CRegisterHamParams(){}
};

class CHamSpecialBotHandler
{
public:
	CHamSpecialBotHandler();
	void CheckClientKeyValue(int &clientIndex, char *infobuffer, const char *key, const char *value);
	void RegisterHamSpecialBot(AMX *amx, int &func, const char *function, int &post, Forward *pfwd);

private:
	void RegisterChecked(AMX *amx, int &func, const char *function, int &post, Forward *pfwd);

	ke::Vector<CRegisterHamParams*> m_RHP_list;
	void **m_specialbot_vtable;
};

#endif // HOOK_SPECIALBOT_H
