/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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
#include "newmenus.h"

CVector<Menu *> g_NewMenus;
CStack<int> g_MenuFreeStack;

void ClearMenus()
{
	for (size_t i = 0; i < g_NewMenus.size(); i++)
	{
		delete g_NewMenus[i];
	}
	
	g_NewMenus.clear();
	while (!g_MenuFreeStack.empty())
	{
		g_MenuFreeStack.pop();
	}
}

void validate_menu_text(char *str)
{
	if (!g_coloredmenus)
	{
		size_t offs = 0;
		while (*str)
		{
			if (*str == '\\')
			{
				str++;
				char c = tolower(*str);
				if (c == 'r' || c == 'w'
					|| c== 'y' || c == 'd')
				{
					str++;
					offs += 2;
					continue;
				}
			}
			if (offs)
			{
				*(str-offs) = *str;
			}
			str++;
		}
		if (offs)
		{
			*(str-offs) = '\0';
		}
	}
}

Menu::Menu(const char *title, AMX *amx, int fid) : m_Title(title), m_ItemColor("\\r"), 
m_NeverExit(false), m_AutoColors(g_coloredmenus), thisId(0), func(fid), 
isDestroying(false), items_per_page(7)
{
	CPluginMngr::CPlugin *pPlugin = g_plugins.findPluginFast(amx);
	menuId = g_menucmds.registerMenuId(title, amx);

	if (strcmp(pPlugin->getName(), "war3ft.amxx") == 0)
	{
		const char *version = pPlugin->getVersion();
		if (strncmp(pPlugin->getVersion(), "3.0 RC", 6) == 0
			&& atoi(&version[6]) <= 8)
		{
			g_menucmds.registerMenuCmd(
				g_plugins.findPluginFast(amx), 
				menuId, 
				-1, 
				g_forwards.duplicateSPForward(fid), 
				true);
		}
	}

	m_OptNames[abs(MENU_BACK)].assign("Back");
	m_OptNames[abs(MENU_MORE)].assign("More");
	m_OptNames[abs(MENU_EXIT)].assign("Exit");
}

Menu::~Menu()
{
	for (size_t i = 0; i < m_Items.size(); i++)
	{
		delete m_Items[i];
	}

	unregisterSPForward(this->func);
	
	m_Items.clear();
}

menuitem *Menu::AddItem(const char *name, const char *cmd, int access)
{
	menuitem *pItem = new menuitem;

	pItem->name.assign(name);
	pItem->cmd.assign(cmd);
	pItem->access = access;
	pItem->id = m_Items.size();
	pItem->handler = -1;
	pItem->isBlank = false;
	pItem->pfn = NULL;

	m_Items.push_back(pItem);

	return pItem;
}

menuitem *Menu::GetMenuItem(item_t item)
{
	if (item >= m_Items.size())
		return NULL;

	return m_Items[item];
}

size_t Menu::GetItemCount()
{
	return m_Items.size();
}

size_t Menu::GetPageCount()
{
	size_t items = GetItemCount();
	if (items_per_page == 0)
	{
		return 1;
	}

	return ((items/items_per_page) + ((items % items_per_page) ? 1 : 0));
}

int Menu::PagekeyToItem(page_t page, item_t key)
{
	size_t start = page * items_per_page;
	size_t num_pages = GetPageCount();

	if (num_pages == 1 || !items_per_page)
	{
		if (key > m_Items.size())
		{
			return MENU_EXIT;
		} else {
			return key-1;
		}
	} else {
		//first page
		if (page == 0)
		{
			/* The algorithm for spaces here is same as a middle page. */
			item_t new_key = key;
			for (size_t i=start; i<(start+key-1) && i<m_Items.size(); i++)
			{
				for (size_t j=0; j<m_Items[i]->blanks.size(); j++)
				{
					if (m_Items[i]->blanks[j].EatNumber())
					{
						if (!new_key)
						{
							break;
						}
						new_key--;
					}
					if (!new_key)
					{
						break;
					}
				}
			}
			key = new_key;
			if (key == items_per_page + 2)
			{
				return MENU_MORE;
			} else if (key == items_per_page + 3) {
				return MENU_EXIT;
			} else {
				return (start + key - 1);
			}
		} else if (page == num_pages - 1) {
			//last page
			item_t item_tracker = 0; //  tracks how many valid items we have passed so far.
			size_t remaining = m_Items.size() - start;
			item_t new_key = key;
			
			// For every item that takes up a slot (item or padded blank)
			// we subtract one from new key.
			// For every item (not blanks), we increase item_tracker.
			// When new_key equals 0, item_tracker will then be set to
			// whatever valid item was selected.
			for (size_t i=m_Items.size() - remaining; i<m_Items.size(); i++)
			{
				item_tracker++;
				
				if (new_key<=1) // If new_key is 0, or will be 0 after the next decrease
				{
					new_key=0;
					break;
				}
				
				new_key--;
				
				for (size_t j=0; j<m_Items[i]->blanks.size(); j++)
				{
					if (m_Items[i]->blanks[j].EatNumber())
					{
						new_key--;
					}
					if (!new_key)
					{
						break;
					}
				}
			}
			// If new_key doesn't equal zero, then a back/exit button was pressed.
			if (new_key!=0)
			{
				if (key == items_per_page + 1)
				{
					return MENU_BACK;
				}
				else if (key == items_per_page + 3)
				{
					return MENU_EXIT;
				}
				// MENU_MORE should never happen here.
			}
			// otherwise our item is now start + item_tracker - 1
			return (start + item_tracker - 1);
		} else {
			/* The algorithm for spaces here is a bit harder.  We have to subtract 
			 * one from the key for each space we find along the way.
			 */
			item_t new_key = key;
			for (size_t i=start; i<(start+items_per_page-1) && i<m_Items.size(); i++)
			{
				for (size_t j=0; j<m_Items[i]->blanks.size(); j++)
				{
					if (m_Items[i]->blanks[j].EatNumber())
					{
						if (!new_key)
						{
							break;
						}
						new_key--;
					}
					if (!new_key)
					{
						break;
					}
				}
			}
			key = new_key;
			if (key > items_per_page && (key-items_per_page<=3))
			{
				unsigned int num = key - items_per_page - 1;
				static int map[] = {MENU_BACK, MENU_MORE, MENU_EXIT};
				return map[num];
			} else {
				return (start + key - 1);
			}
		}
	}
}

bool Menu::Display(int player, page_t page)
{
	int keys = 0;
	const char *str = GetTextString(player, page, keys);

	if (!str)
		return false;

	static char buffer[2048];
	int len = _snprintf(buffer, sizeof(buffer)-1, "%s", str);

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(player);

	pPlayer->keys = 0;
	pPlayer->menu = 0;

	UTIL_FakeClientCommand(pPlayer->pEdict, "menuselect", "10", 0);

	pPlayer->keys = keys;
	pPlayer->menu = menuId;
	pPlayer->newmenu = thisId;
	pPlayer->page = (int)page;

	UTIL_ShowMenu(pPlayer->pEdict, keys, -1, buffer, len);

	return true;
}

const char *Menu::GetTextString(int player, page_t page, int &keys)
{
	page_t pages = GetPageCount();
	item_t numItems = GetItemCount();

	if (page >= pages)
		return NULL;

	m_Text.clear();

	char buffer[255];
	if (items_per_page && (pages != 1))
	{
		if (m_AutoColors)
			_snprintf(buffer, sizeof(buffer)-1, "\\y%s %d/%d\n\\w\n", m_Title.c_str(), page + 1, pages);
		else
			_snprintf(buffer, sizeof(buffer)-1, "%s %d/%d\n\n", m_Title.c_str(), page + 1, pages);
	} else {
		if (m_AutoColors)
			_snprintf(buffer, sizeof(buffer)-1, "\\y%s\n\\w\n", m_Title.c_str());
		else
			_snprintf(buffer, sizeof(buffer)-1, "%s\n\n", m_Title.c_str());
	}
	
	m_Text.append(buffer);

	enum
	{
		Display_Back = (1<<0),
		Display_Next = (1<<1),
	};

	int flags = Display_Back|Display_Next;

	item_t start = page * items_per_page;
	item_t end = 0;
	if (items_per_page)
	{
		if (start + items_per_page >= numItems)
		{
			end = numItems;
			flags &= ~Display_Next;
		} else {
			end = start + items_per_page;
		}
	} else {
		end = numItems;
		if (end > 10)
		{
			end = 10;
		}
	}

	if (page == 0)
	{
		flags &= ~Display_Back;
	}
	
	menuitem *pItem = NULL;
	
	int option = 0;
	keys = 0;
	bool enabled = true;
	int ret = 0;
	int slots = 0;
	int option_display = 0;
	
	for (item_t i = start; i < end; i++)
	{
		// reset enabled
		enabled = true;
		pItem = m_Items[i];
		
		if (pItem->access && !(pItem->access & g_players[player].flags[0]))
		{
			enabled = false;
		}
		
		if (pItem->handler != -1)
		{
			ret = executeForwards(pItem->handler, static_cast<cell>(player), static_cast<cell>(thisId), static_cast<cell>(i));
			if (ret == ITEM_ENABLED)
			{
				enabled = true;
			} else if (ret == ITEM_DISABLED) {
				enabled = false;
			}
		}
		
		if (pItem->pfn)
		{
			ret = (pItem->pfn)(player, thisId, i);
			if (ret == ITEM_ENABLED)
			{
				enabled = true;
			} else if (ret == ITEM_DISABLED) {
				enabled = false;
			}
		}

		if (pItem->isBlank)
		{
			enabled = false;
		}

		if (enabled)
		{
			keys |= (1<<option);
		}
		
		option_display = ++option;
		if (option_display == 10)
		{
			option_display = 0;
		}

		if (pItem->isBlank)
		{
			_snprintf(buffer, sizeof(buffer)-1, "%s\n", pItem->name.c_str());
		}
		else if (enabled)
		{
			if (m_AutoColors) 
			{
				_snprintf(buffer, sizeof(buffer)-1, "%s%d.\\w %s\n", m_ItemColor.c_str(),option_display, pItem->name.c_str());
			} else {
				_snprintf(buffer, sizeof(buffer)-1, "%d. %s\n", option_display, pItem->name.c_str());
			}
		} else {
			if (m_AutoColors)
			{
				_snprintf(buffer, sizeof(buffer)-1, "\\d%d. %s\n\\w", option_display, pItem->name.c_str());
			} else {
				_snprintf(buffer, sizeof(buffer)-1, "#. %s\n", pItem->name.c_str());
			}
		}
		slots++;

		m_Text.append(buffer);

		//attach blanks
		if (pItem->blanks.size())
		{
			for (size_t j=0; j<pItem->blanks.size(); j++)
			{
				if (pItem->blanks[j].EatNumber())
				{
					option++;
				}
				m_Text.append(pItem->blanks[j].GetDisplay());
				m_Text.append("\n");
				slots++;
			}
		}
	}

	if (items_per_page)
	{
		/* Pad spaces until we reach the end of the max possible items */
		for (unsigned int i=(unsigned)slots; i<items_per_page; i++)
		{
			m_Text.append("\n");
			option++;
		}
		/* Make sure there is at least one visual pad */
		m_Text.append("\n");

		/* Don't bother if there is only one page */
		if (pages > 1)
		{
			if (flags & Display_Back)
			{
				keys |= (1<<option++);
				if (m_AutoColors)
				{
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						"%s%d. \\w%s\n", 
						m_ItemColor.c_str(), 
						option == 10 ? 0 : option, 
						m_OptNames[abs(MENU_BACK)].c_str());
				} else {
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						"%d. %s\n", 
						option == 10 ? 0 : option, 
						m_OptNames[abs(MENU_BACK)].c_str());
				}
			} else {
				option++;
				if (m_AutoColors)
				{
					_snprintf(buffer,
						sizeof(buffer)-1,
						"\\d%d. %s\n\\w",
						option == 10 ? 0 : option,
						m_OptNames[abs(MENU_BACK)].c_str());
				} else {
					_snprintf(buffer, sizeof(buffer)-1, "#. %s\n", m_OptNames[abs(MENU_BACK)].c_str());
				}
			}
			m_Text.append(buffer);
	
			if (flags & Display_Next)
			{
				keys |= (1<<option++);
				if (m_AutoColors)
				{
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						"%s%d. \\w%s\n", 
						m_ItemColor.c_str(), 
						option == 10 ? 0 : option, 
						m_OptNames[abs(MENU_MORE)].c_str());
				} else {
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						"%d. %s\n", 
						option == 10 ? 0 : option, 
						m_OptNames[abs(MENU_MORE)].c_str());
				}
			} else {
				option++;
				if (m_AutoColors)
				{
					_snprintf(buffer,
						sizeof(buffer)-1,
						"\\d%d. %s\n\\w",
						option == 10 ? 0 : option,
						m_OptNames[abs(MENU_MORE)].c_str());
				} else {
					_snprintf(buffer, sizeof(buffer)-1, "#. %s\n", m_OptNames[abs(MENU_MORE)].c_str());
				}
			}
			m_Text.append(buffer);
		} else {
			/* Keep padding */
			option += 2;
		}
	}
	
	if ((items_per_page && !m_NeverExit) || (m_ForceExit && numItems < 10))
	{
		/* Visual pad has not been added yet */
		if (!items_per_page)
			m_Text.append("\n");
		
		keys |= (1<<option++);
		if (m_AutoColors)
		{
			_snprintf(buffer, 
				sizeof(buffer)-1, 
				"%s%d. \\w%s\n", 
				m_ItemColor.c_str(), 
				option == 10 ? 0 : option, 
				m_OptNames[abs(MENU_EXIT)].c_str());
		} else {
			_snprintf(buffer, 
				sizeof(buffer)-1, 
				"%d. %s\n", 
				option == 10 ? 0 : option, 
				m_OptNames[abs(MENU_EXIT)].c_str());
		}
		m_Text.append(buffer);
	}
	
	return m_Text.c_str();
}

#define GETMENU(p) if (p >= (int)g_NewMenus.size() || p < 0 || !g_NewMenus[p] || g_NewMenus[p]->isDestroying) { \
	LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d(%d)", p, g_NewMenus.size()); \
	return 0; } \
	Menu *pMenu = g_NewMenus[p];

//Makes a new menu handle (-1 for failure)
//native csdm_makemenu(title[]);
static cell AMX_NATIVE_CALL menu_create(AMX *amx, cell *params)
{
	int len;
	char *title = get_amxstring(amx, params[1], 0, len);
	validate_menu_text(title);
	char *handler = get_amxstring(amx, params[2], 1, len);

	int func = registerSPForwardByName(amx, handler, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	
	if (func == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Invalid function \"%s\"", handler);
		return 0;
	}

	Menu *pMenu = new Menu(title, amx, func);

	if (g_MenuFreeStack.empty())
	{
		g_NewMenus.push_back(pMenu);
		pMenu->thisId = (int)g_NewMenus.size() - 1;
	} else {
		int pos = g_MenuFreeStack.front();
		g_MenuFreeStack.pop();
		g_NewMenus[pos] = pMenu;
		pMenu->thisId = pos;
	}

	return pMenu->thisId;
}
static cell AMX_NATIVE_CALL menu_addblank(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	if (params[2] && (!pMenu->items_per_page && pMenu->GetItemCount() >= 10))
	{
		LogError(amx, AMX_ERR_NATIVE, "Non-paginated menus are limited to 10 items.");
		return 0;
	}

	if (!pMenu->m_Items.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Blanks can only be added after items.");
		return 0;
	}

	menuitem *item = pMenu->m_Items[pMenu->m_Items.size() - 1];

	BlankItem a;

	a.SetBlank();

	if (params[2] == 1)
		a.SetEatNumber(true);

	else
		a.SetEatNumber(false);

	item->blanks.push_back(a);

	return 1;
}
static cell AMX_NATIVE_CALL menu_addtext(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	if (params[2] && (!pMenu->items_per_page && pMenu->GetItemCount() >= 10))
	{
		LogError(amx, AMX_ERR_NATIVE, "Non-paginated menus are limited to 10 items.");
		return 0;
	}

	if (!pMenu->m_Items.size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Blanks can only be added after items.");
		return 0;
	}

	menuitem *item = pMenu->m_Items[pMenu->m_Items.size() - 1];

	BlankItem a;

	int len;
	a.SetText(get_amxstring(amx, params[2], 0, len));

	if (params[3] == 1)
		a.SetEatNumber(true);

	else
		a.SetEatNumber(false);

	item->blanks.push_back(a);

	return 1;
}

static cell AMX_NATIVE_CALL menu_addblank2(AMX *amx, cell *params)
{
	GETMENU(params[1]);
	
	if (!pMenu->items_per_page && pMenu->GetItemCount() >= 10)
	{
		LogError(amx, AMX_ERR_NATIVE, "Non-paginated menus are limited to 10 items.");
		return 0;
	}
	
	menuitem *pItem = pMenu->AddItem("", "", 0);
	pItem->isBlank = true;
	
	return 1;
}
static cell AMX_NATIVE_CALL menu_addtext2(AMX *amx, cell *params)
{
	int len;
	char *name;
	
	GETMENU(params[1]);
	
	if (!pMenu->items_per_page && pMenu->GetItemCount() >= 10)
	{
		LogError(amx, AMX_ERR_NATIVE, "Non-paginated menus are limited to 10 items.");
		return 0;
	}
	
	name = get_amxstring(amx, params[2], 0, len);
	validate_menu_text(name);
	
	menuitem *pItem = pMenu->AddItem(name, "", 0);
	pItem->isBlank = true;
	
	return 1;
}

//Adds an item to the menu (returns current item count - 1)
//native menu_additem(menu, const name[], const command[]="", access=0);
static cell AMX_NATIVE_CALL menu_additem(AMX *amx, cell *params)
{
	int len;
	char *name, *cmd;
	int access;

	GETMENU(params[1]);

	if (!pMenu->items_per_page && pMenu->GetItemCount() >= 10)
	{
		LogError(amx, AMX_ERR_NATIVE, "Non-paginated menus are limited to 10 items.");
		return 0;
	}

	name = get_amxstring(amx, params[2], 0, len);
	validate_menu_text(name);
	cmd = get_amxstring(amx, params[3], 1, len);
	access = params[4];

	menuitem *pItem = pMenu->AddItem(name, cmd, access);

	pItem->handler = params[5];

	return 1;
}

//Returns the number of pages in a menu
//native csdm_menu_pages(menu);
static cell AMX_NATIVE_CALL menu_pages(AMX *amx, cell *params)
{
	GETMENU(params[1]);
	return pMenu->GetPageCount();
}

//Returns the number of items in a menu
//native csdm_menu_items(menu);
static cell AMX_NATIVE_CALL menu_items(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	return pMenu->GetItemCount();
}

//Builds the menu string for a specific page (set title to 0 to not include title)
//page indices start at 0!
static cell AMX_NATIVE_CALL menu_display(AMX *amx, cell *params)
{
	GETMENU(params[2]);
	
	int player = params[1];
	int page = params[3];
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(player);
	
	/* If the stupid handler keeps drawing menus, 
	 * We need to keep cancelling them.  But we put in a quick infinite loop
	 * counter to prevent this from going nuts.
	 */
	int menu;
	int loops = 0;
	while ((menu = pPlayer->newmenu) >= 0)
	{
		if ((size_t)menu >= g_NewMenus.size() || !g_NewMenus[menu])
		{
			break;
		}

		Menu *pOther = g_NewMenus[menu];

		pPlayer->newmenu = -1;
		pPlayer->menu = 0;
		executeForwards(pOther->func, 
			static_cast<cell>(player),
			static_cast<cell>(pOther->thisId),
			static_cast<cell>(MENU_EXIT));

		/* Infinite loop counter */
		if (++loops >= 10)
		{
			LogError(amx, AMX_ERR_NATIVE, "Plugin called menu_display when item=MENU_EXIT");
			return 0;
		}
	}

	// This will set the expire time of the menu to infinite
	pPlayer->menuexpire = INFINITE;

	return pMenu->Display(player, page);
}

//Finds the id of a menu item for a specific page and key value.
//Note that key should be from 0-6, as it only displays 7 per page.
//page indices start at 0
//native menu_keyid(menu, page, key);
static cell AMX_NATIVE_CALL menu_find_id(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	page_t page = static_cast<page_t>(params[2]);
	item_t key = static_cast<item_t>(params[3]);

	return pMenu->PagekeyToItem(page, key);
}

//Gets info about a menu option
//native menu_item_getinfo(menu, item, &access, command[], cmdlen, name[]="", namelen=0, &callback);
static cell AMX_NATIVE_CALL menu_item_getinfo(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	menuitem *pItem = pMenu->GetMenuItem(static_cast<item_t>(params[2]));

	if (!pItem)
		return 0;

	cell *addr = get_amxaddr(amx, params[3]);
	addr[0] = pItem->access;

	set_amxstring(amx, params[4], pItem->cmd.c_str(), params[5]);
	set_amxstring(amx, params[6], pItem->name.c_str(), params[7]);

	if (params[8])
	{
		addr = get_amxaddr(amx, params[8]);
		if (addr)
			addr[0] = pItem->handler;
	}

	return 1;
}

static cell AMX_NATIVE_CALL menu_makecallback(AMX *amx, cell *params)
{
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);

	int id = registerSPForwardByName(amx, name, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (id == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Invalid function %s", name);
		return -1;
	}

	return id;
}

static cell AMX_NATIVE_CALL menu_item_setname(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	menuitem *pItem = pMenu->GetMenuItem(static_cast<item_t>(params[2]));

	if (!pItem)
		return 0;

	int len;
	char *name;

	name = get_amxstring(amx, params[3], 0, len);

	pItem->name.assign(name);

	return 1;
}

static cell AMX_NATIVE_CALL menu_item_setcmd(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	menuitem *pItem = pMenu->GetMenuItem(static_cast<item_t>(params[2]));

	if (!pItem)
		return 0;

	int len;
	char *cmd;

	cmd = get_amxstring(amx, params[3], 0, len);

	pItem->cmd.assign(cmd);

	return 1;
}

static cell AMX_NATIVE_CALL menu_item_setcall(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	menuitem *pItem = pMenu->GetMenuItem(static_cast<item_t>(params[2]));

	if (!pItem)
		return 0;

	pItem->handler = params[3];

	return 1;
}

static cell AMX_NATIVE_CALL menu_setprop(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	int len = params[0] / sizeof(cell);
	if (len < 3)
	{
		LogError(amx, AMX_ERR_NATIVE, "Expected 3 parameters");
		return 0;
	}

	switch (params[2])
	{
	case MPROP_SET_NUMBER_COLOR:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_ItemColor.assign(str);
			break;
		}
	case MPROP_PERPAGE:
		{
			cell count = *get_amxaddr(amx, params[3]);
			if (count < 0 || count > 7)
			{
				LogError(amx, AMX_ERR_NATIVE, "Cannot set %d items per page", count);
				return 0;
			}
			pMenu->items_per_page = count;
			break;
		}
	case MPROP_BACKNAME:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_OptNames[abs(MENU_BACK)].assign(str);
			break;
		}
	case MPROP_NEXTNAME:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_OptNames[abs(MENU_MORE)].assign(str);
			break;
		}
	case MPROP_EXITNAME:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_OptNames[abs(MENU_EXIT)].assign(str);
			break;
		}
	case MPROP_TITLE:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			pMenu->m_Title.assign(str);
			break;
		}
	case MPROP_EXITALL:
		{
			cell ans = *get_amxaddr(amx, params[3]);
			if (ans == 1 || ans == 0)
			{
				pMenu->m_NeverExit = false;
				pMenu->m_ForceExit = false;
			} else if (ans == 2) {
				pMenu->m_NeverExit = false;
				pMenu->m_ForceExit = true;
			} else if (ans == -1) {
				pMenu->m_NeverExit = true;
				pMenu->m_ForceExit = false;
			}
			break;
		}
	case MPROP_ORDER:
		{
			/* Ignored as of 1.8.0 */
			break;
		}
	case MPROP_NOCOLORS:
		{
			pMenu->m_AutoColors = *get_amxaddr(amx, params[3]) ? true : false;
			break;
		}
	case MPROP_PADMENU:
		{
			/* Ignored as of 1.8.0 */
			break;
		}
	default:
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid menu setting: %d", params[1]);
			return 0;
		}
	}

	return 1;
}

#define GETMENU_R(p) if (p >= (int)g_NewMenus.size() || p < 0 || !g_NewMenus[p]) { \
	LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d(%d)", p, g_NewMenus.size()); \
	return 0; } \
	Menu *pMenu = g_NewMenus[p];

static cell AMX_NATIVE_CALL menu_cancel(AMX *amx, cell *params)
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", index);
		return 0;
	}

	CPlayer *player = GET_PLAYER_POINTER_I(index);
	if (!player->ingame)
	{
		LogError(amx, AMX_ERR_NATIVE, "Played %d is not in game", index);
		return 0;
	}

	int menu = player->newmenu;
	if (menu < 0 || menu >= (int)g_NewMenus.size() || !g_NewMenus[menu])
		return 0;

	Menu *pMenu = g_NewMenus[menu];

	player->newmenu = -1;
	player->menu = 0;
	executeForwards(pMenu->func, 
		static_cast<cell>(index),
		static_cast<cell>(pMenu->thisId),
		static_cast<cell>(MENU_EXIT));

	return 1;
}

static cell AMX_NATIVE_CALL menu_destroy(AMX *amx, cell *params)
{
	GETMENU_R(params[1]);

	if (pMenu->isDestroying)
	{
		return 0;	//prevent infinite recursion
	}

	pMenu->isDestroying = true;

	CPlayer *player;
	for (int i=1; i<=gpGlobals->maxClients; i++)
	{
		player = GET_PLAYER_POINTER_I(i);
		if (player->newmenu == pMenu->thisId)
		{
			player->newmenu = -1;
			player->menu = 0;
			executeForwards(pMenu->func, 
				static_cast<cell>(i), 
				static_cast<cell>(pMenu->thisId),
				static_cast<cell>(MENU_EXIT));
		}
	}
	g_NewMenus[params[1]] = NULL;
	delete pMenu;
	g_MenuFreeStack.push(params[1]);

	return 1;
}

static cell AMX_NATIVE_CALL player_menu_info(AMX *amx, cell *params)
{
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", params[1]);
		return 0;
	}

	CPlayer *player = GET_PLAYER_POINTER_I(params[1]);
	if (!player->ingame)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not ingame", params[1]);
		return 0;
	}

	cell *m = get_amxaddr(amx, params[2]);
	cell *n = get_amxaddr(amx, params[3]);

	*m = player->menu;
	*n = player->newmenu;

	if (params[0] / sizeof(cell) == 4)
	{
		cell *addr = get_amxaddr(amx, params[4]);
		*addr = player->page;
	}

	if ( (*m != 0 && *m != -1) || (*n != -1))
	{
		return 1;
	}

	return 0;
}

AMX_NATIVE_INFO g_NewMenuNatives[] = 
{
	{"menu_create",				menu_create},
	{"menu_additem",			menu_additem},
	{"menu_addblank",			menu_addblank},
	{"menu_addtext",			menu_addtext},
	{"menu_pages",				menu_pages},
	{"menu_items",				menu_items},
	{"menu_display",			menu_display},
	{"menu_find_id",			menu_find_id},
	{"menu_item_getinfo",		menu_item_getinfo},
	{"menu_makecallback",		menu_makecallback},
	{"menu_item_setcall",		menu_item_setcall},
	{"menu_item_setcmd",		menu_item_setcmd},
	{"menu_item_setname",		menu_item_setname},
	{"menu_destroy",			menu_destroy},
	{"menu_setprop",			menu_setprop},
	{"menu_cancel",				menu_cancel},
	{"player_menu_info",		player_menu_info},
	{"menu_addblank2",			menu_addblank2},
	{"menu_addtext2",			menu_addtext2},
	{NULL,						NULL},
};
