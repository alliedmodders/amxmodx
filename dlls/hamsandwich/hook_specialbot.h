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

#ifndef HOOK_SPECIALBOT_H
#define HOOK_SPECIALBOT_H

#include "ham_utils.h"
#include "CVector.h"

class CRegisterHamParams
{
public:
	AMX *amx;
	int func;
	char *function;
	int post;
	int fwd;
	
	CRegisterHamParams(AMX *arg_amx, int &arg_func, const char *arg_function, int &arg_post, int &arg_fwd);
	~CRegisterHamParams();
private:
	CRegisterHamParams(){}
};

class CHamSpecialBotHandler
{
public:
	CHamSpecialBotHandler();
	void CheckClientKeyValue(int &clientIndex, char *infobuffer, const char *key, const char *value);
	void RegisterHamSpecialBot(AMX *amx, int &func, const char *function, int &post, int &fwd);

private:
	void RegisterChecked(AMX *amx, int &func, const char *function, int &post, int &fwd);

	CVector<CRegisterHamParams*> m_RHP_list;
	void **m_specialbot_vtable;
};

#endif // HOOK_SPECIALBOT_H
