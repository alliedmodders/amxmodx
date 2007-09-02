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
MenuMngr::MenuCommand::MenuCommand(CPluginMngr::CPlugin *a, int mi, int k, int f, bool new_menu)
{
	plugin = a;
	keys = k;
	menuid = mi;
	next = 0;
	is_new_menu = new_menu;

	function = f;
}

MenuMngr::~MenuMngr() 
{
	clear();
	MenuMngr::MenuIdEle::uniqueid = 0;
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

int MenuMngr::registerMenuId(const char* n, AMX* a)
{
	int id = findMenuId(n, a);
	
	if (id)
	{
		return id;
	}
	
	headid = new MenuIdEle(n, a, headid);
	
	return headid->id;
}

void MenuMngr::registerMenuCmd(CPluginMngr::CPlugin *a, int mi, int k, int f, bool from_new_menu) 
{
	MenuCommand **temp = &headcmd;
	if (from_new_menu)
	{
		MenuCommand *ptr;
		while (*temp)
		{
			ptr = *temp;
			if (ptr->is_new_menu
				&& ptr->plugin == a
				&& ptr->menuid == mi)
			{
				if (g_forwards.isSameSPForward(ptr->function, f))
				{
					return;
				}
			}
			temp = &(*temp)->next;
		}
	} else {
		while (*temp)
		{
			temp = &(*temp)->next;
		}
	}
	*temp = new MenuCommand(a, mi, k, f, from_new_menu);
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

MenuMngr::iterator MenuMngr::SetWatchIter(MenuMngr::iterator iter)
{
	MenuMngr::iterator old = m_watch_iter;

	m_watch_iter = iter;

	return old;
}

int MenuMngr::MenuIdEle::uniqueid = 0;
