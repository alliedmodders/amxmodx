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

#include "hook_specialbot.h"
#include "hooklist.h"
#include "hook.h"

extern ke::Vector<Hook*> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];
extern hook_t hooklist[];


CRegisterHamParams::CRegisterHamParams(AMX *arg_amx, int &arg_func, const char *arg_function, int &arg_post, Forward *arg_pfwd)
{
	amx = arg_amx;
	func = arg_func;
	function = new char[strlen(arg_function)+1];
	strcpy(function, arg_function);
	post = arg_post;
	pfwd = arg_pfwd;
}

CRegisterHamParams::~CRegisterHamParams()
{
	delete[] function;
}


CHamSpecialBotHandler::CHamSpecialBotHandler()
{
	m_specialbot_vtable = NULL;
}

void CHamSpecialBotHandler::CheckClientKeyValue(int &clientIndex, char *infobuffer, const char *key, const char *value)
{
	if(m_specialbot_vtable != NULL)
		return;

	edict_t *pEdict = MF_GetPlayerEdict(clientIndex);
	if((pEdict->v.flags & FL_FAKECLIENT) != FL_FAKECLIENT)
	{
		const char *auth = GETPLAYERAUTHID(pEdict); 	 
		if (auth && (strcmp(auth, "BOT") != 0)) 	
			return;
	}

	if(strcmp(key, "*bot") != 0 || strcmp(value, "1") != 0)
		return;

	m_specialbot_vtable = GetVTable(pEdict->pvPrivateData, Offsets.GetBase());

	if(m_RHP_list.empty())
		return;

	for (size_t i = 0; i < m_RHP_list.length(); ++i)
	{
		CRegisterHamParams *item = m_RHP_list.at(i);
		RegisterChecked(item->amx, item->func, item->function, item->post, item->pfwd);
		delete item;
	}

	m_RHP_list.clear();
}

void CHamSpecialBotHandler::RegisterHamSpecialBot(AMX *amx, int &func, const char *function, int &post, Forward *pfwd)
{	
	if(m_specialbot_vtable == NULL)
	{
		m_RHP_list.append(new CRegisterHamParams(amx, func, function, post, pfwd));
		return;
	}

	RegisterChecked(amx, func, function, post, pfwd);
}

void CHamSpecialBotHandler::RegisterChecked(AMX *amx, int &func, const char *function, int &post, Forward *pfwd)
{
	pfwd->AddRef();

	void **vtable = m_specialbot_vtable;
	int **ivtable=(int **)vtable;

	void *vfunction=(void *)ivtable[hooklist[func].vtid];

	for (size_t i = 0; i < hooks[func].length(); ++i)
	{
		if (hooks[func].at(i)->tramp == vfunction)
		{
			// Yes, this function is hooked
			if (post)
			{
				hooks[func].at(i)->post.append(pfwd);
			}
			else
			{
				hooks[func].at(i)->pre.append(pfwd);
			}
			return;
		}
	}

	char classname[] = "player";

	// If we got here, the function is not hooked
	Hook *hook = new Hook(vtable, hooklist[func].vtid, hooklist[func].targetfunc, hooklist[func].isvoid, hooklist[func].needsretbuf, hooklist[func].paramcount, classname);
	hooks[func].append(hook);

	if (post)
	{
		hook->post.append(pfwd);
	}
	else
	{
		hook->pre.append(pfwd);
	}
}
