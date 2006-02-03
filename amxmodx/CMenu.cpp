/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"
#include "CMenu.h"

// *****************************************************
// class MenuMngr
// *****************************************************
MenuMngr::MenuCommand::MenuCommand(CPluginMngr::CPlugin *a, int mi, int k, int f)
{
	plugin = a;
	keys = k;
	menuid = mi;
	function = f;
	next = 0;
}

MenuMngr::~MenuMngr() 
{
	clear();
}

int MenuMngr::findMenuId(const char* name, AMX* amx)
{
	for (MenuIdEle* b = headid; b; b = b->next)
	{
		if ((!amx || !b->amx || amx == b->amx) && strstr(name,b->name.c_str()))
			return b->id;
	}
	
	return 0;
}

void MenuMngr::removeMenuId(int id)
{
	MenuIdEle *n = headid;
	MenuIdEle *l = NULL;
	while (n)
	{
		if (n->id == id)
		{
			if (l)
				l->next = n->next;
			else
				headid = n->next;
			delete n;
			break;
		}
		l = n;
		n = n->next;
	}

	MenuCommand *c = headcmd;
	MenuCommand *lc = NULL;
	while (c)
	{
		if (c->menuid == id)
		{
			if (lc)
				lc->next = c->next;
			else
				headcmd = c->next;
			delete c;
			break;
		}
		lc = c;
		c = c->next;
	}
}

int MenuMngr::registerMenuId(const char* n, AMX* a)
{
	int id = findMenuId(n, a);
	
	if (id)
		return id;
	
	headid = new MenuIdEle(n, a, headid);
	
	if (!headid)
		return 0;			// :TODO: Better error report
	
	return headid->id;
}

void MenuMngr::registerMenuCmd(CPluginMngr::CPlugin *a, int mi, int k, int f) 
{
	MenuCommand** temp = &headcmd;
	while (*temp) temp = &(*temp)->next;
	*temp = new MenuCommand(a, mi, k, f);
}

void MenuMngr::clear()
{
	while (headid)
	{
		MenuIdEle* a = headid->next;
		delete headid;
		headid = a;
	}

	while (headcmd)
	{
		MenuCommand* a = headcmd->next;
		delete headcmd;
		headcmd = a;
	}
}

int MenuMngr::MenuIdEle::uniqueid = 0;
