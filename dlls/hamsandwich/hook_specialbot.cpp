/* Ham Sandwich
 *   Copyright 2007-2014
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#include "hook_specialbot.h"
#include "hooklist.h"
#include "hook.h"

extern CVector<Hook*> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];
extern hook_t hooklist[];


CRegisterHamParams::CRegisterHamParams(AMX *arg_amx, int &arg_func, const char *arg_function, int &arg_post, int &arg_fwd)
{
	amx = arg_amx;
	func = arg_func;
	function = new char[strlen(arg_function)+1];
	strcpy(function, arg_function);
	post = arg_post;
	fwd = arg_fwd;
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
	
	CVector<CRegisterHamParams*>::iterator i = m_RHP_list.begin();
	CVector<CRegisterHamParams*>::iterator end = m_RHP_list.end();
	for(; i!=end; i++)
	{
		RegisterChecked((*i)->amx, (*i)->func, (*i)->function, (*i)->post, (*i)->fwd);
		delete *i;
	}

	m_RHP_list.clear();
}

void CHamSpecialBotHandler::RegisterHamSpecialBot(AMX *amx, int &func, const char *function, int &post, int &fwd)
{	
	if(m_specialbot_vtable == NULL)
	{
		m_RHP_list.push_back( new CRegisterHamParams(amx, func, function, post, fwd) );
		return;
	}

	RegisterChecked(amx, func, function, post, fwd);
}

void CHamSpecialBotHandler::RegisterChecked(AMX *amx, int &func, const char *function, int &post, int &fwd)
{
	void **vtable = m_specialbot_vtable;
	int **ivtable=(int **)vtable;

	void *vfunction=(void *)ivtable[hooklist[func].vtid];

	CVector<Hook *>::iterator end=hooks[func].end();
	for (CVector<Hook *>::iterator i=hooks[func].begin();
		 i!=end;
		 ++i)
	{
		if ((*i)->tramp == vfunction)
		{
			// Yes, this function is hooked
			Forward *pfwd=new Forward(fwd);
			if (post)
			{
				(*i)->post.push_back(pfwd);
			}
			else
			{
				(*i)->pre.push_back(pfwd);
			}
			return;
		}
	}

	char classname[] = "player";

	// If we got here, the function is not hooked
	Hook *hook = new Hook(vtable, hooklist[func].vtid, hooklist[func].targetfunc, hooklist[func].isvoid, hooklist[func].needsretbuf, hooklist[func].paramcount, classname);
	hooks[func].push_back(hook);

	Forward *pfwd=new Forward(fwd);
	if (post)
	{
		hook->post.push_back(pfwd);
	}
	else
	{
		hook->pre.push_back(pfwd);
	}
}
