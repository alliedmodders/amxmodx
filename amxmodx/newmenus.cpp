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
#include "newmenus.h"

CVector<Menu *> g_NewMenus;

void ClearMenus()
{
	for (size_t i = 0; i < g_NewMenus.size(); i++)
		delete g_NewMenus[i];
	
	g_NewMenus.clear();
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
					|| c== 'w' || c == 'd')
				{
					str++;
					offs += 2;
					continue;
				}
			}
			if (offs)
				*(str-offs) = *str;
			str++;
		}
		if (offs)
			*(str-offs) = '\0';
	}
}

Menu::Menu(const char *title, int mid, int tid)
{
	m_Title.assign(title);
	menuId = mid;
	thisId = tid;

	m_OptNames[abs(MENU_BACK)].assign("Back");
	m_OptNames[abs(MENU_MORE)].assign("More");
	m_OptNames[abs(MENU_EXIT)].assign("Exit");

	m_OptOrders[0] = MENU_BACK;
	m_OptOrders[1] = MENU_MORE;
	m_OptOrders[2] = MENU_EXIT;

	m_AlwaysExit = false;
	m_NeverExit = false;
	m_AutoColors = g_coloredmenus;

	items_per_page = 7;
	func = 0;
	padding = 0;
}

Menu::~Menu()
{
	for (size_t i = 0; i < m_Items.size(); i++)
		delete m_Items[i];
	
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
		return 1;

	return ((items/items_per_page) + ((items % items_per_page) ? 1 : 0));
}

int Menu::PagekeyToItem(page_t page, item_t key)
{
	size_t start = page * items_per_page;
	size_t num_pages = GetPageCount();

	if (num_pages == 1 || !items_per_page)
	{
		if (m_AlwaysExit && key > m_Items.size())
			return MENU_EXIT;
		else
			return key-1;
	} else {
		//first page
		if (page == 0)
		{
			if (key == items_per_page + 1)
				return MENU_MORE;
			else if (key == items_per_page + 2)
				return MENU_EXIT;
			else
				return (start + key - 1);
		} else if (page == num_pages - 1) {
			//last page
			size_t remaining = m_Items.size() - start;
			if (key == remaining + 1)
			{
				return MENU_BACK;
			} else if (key == remaining + 2) {
				return MENU_EXIT;
			} else {
				return (start + key - 1);
			}
		} else {
			if (key > items_per_page && (key-items_per_page<=3))
			{
				return m_OptOrders[key-items_per_page-1];
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
	if (m_AutoColors)
		_snprintf(buffer, sizeof(buffer)-1, "\\y%s %d/%d\n\\w\n", m_Title.c_str(), page + 1, pages);
	else
		_snprintf(buffer, sizeof(buffer)-1, "%s %d/%d\n\n", m_Title.c_str(), page + 1, pages);
	
	m_Text.append(buffer);

	enum
	{
		Display_Back = (1<<0),
		Display_Next = (1<<1),
		Display_Exit = (1<<2),
	};

	int flags = Display_Back|Display_Next;
	item_t start = page * items_per_page;
	item_t end = 0;
	if (items_per_page)
	{
		if (start + items_per_page >= numItems)
		{
			end = numItems - 1;
			flags &= ~Display_Next;
		} else {
			end = start + items_per_page - 1;
		}
		if (!m_NeverExit && (m_AlwaysExit || (page == 0 || page == pages-1)))
			flags |= Display_Exit;
	} else {
		end = numItems - 1;
		if (end > 10)
			end = 10;
		flags = 0;
	}

	if (page == 0)
		flags &= ~Display_Back;
	
	menuitem *pItem = NULL;
	
	int option = 0;
	keys = 0;
	bool enabled = true;
	int ret = 0;
	int slots = 0;
	
	for (item_t i = start; i <= end; i++)
	{
		pItem = m_Items[i];
		
		if (pItem->access && !(pItem->access & g_players[player].flags[0]))
			enabled = false;
		
		if (pItem->handler != -1)
		{
			ret = executeForwards(pItem->handler, static_cast<cell>(player), static_cast<cell>(thisId), static_cast<cell>(i));
			if (ret == ITEM_ENABLED)
				enabled = true;
			else if (ret == ITEM_DISABLED)
				enabled = false;
		}
		
		if (pItem->pfn)
		{
			ret = (pItem->pfn)(player, thisId, i);
			if (ret == ITEM_ENABLED)
				enabled = true;
			else if (ret == ITEM_DISABLED)
				enabled = false;
		}
		
		if (enabled)
		{
			keys |= (1<<option);
			if (m_AutoColors) 
				_snprintf(buffer, sizeof(buffer)-1, "\\r%d.\\w %s\n", ++option, pItem->name.c_str());
			else
				_snprintf(buffer, sizeof(buffer)-1, "%d. %s\n", ++option, pItem->name.c_str());
		} else {
			if (m_AutoColors)
			{
				_snprintf(buffer, sizeof(buffer)-1, "\\d%d. %s\n\\w", ++option, pItem->name.c_str());
			} else {
				_snprintf(buffer, sizeof(buffer)-1, "#. %s\n", pItem->name.c_str());
				option++;
			}
		}
		slots++;

		m_Text.append(buffer);

		//attach blanks
		if (pItem->blanks.size())
		{
			for (size_t j=0; j<pItem->blanks.size(); j++)
			{
				if (pItem->blanks[j] == 1)
					option++;
				m_Text.append("\n");
				slots++;
			}
		}
	}

	if (padding == 1 && items_per_page)
	{
		int pad = items_per_page;
		if (flags & Display_Back)
			pad--;
		if (flags & Display_Next)
			pad--;
		if (flags & Display_Exit)
			pad--;
		for (int i=slots+1; i<=pad; i++)
		{
			m_Text.append("\n");
			option++;
		}
	}

	for (int i=0; i<3; i++)
	{
		switch (m_OptOrders[i])
		{
		case MENU_BACK:
			{
				if (flags & Display_Back)
				{
					keys |= (1<<option++);
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						m_AutoColors ? "\\r%d. \\w%s\n" : "%d. %s\n", 
						option, 
						m_OptNames[abs(MENU_BACK)].c_str()
						);
					m_Text.append(buffer);
				}
				break;
			}
		case MENU_MORE:
			{
				if (flags & Display_Next)
				{
					keys |= (1<<option++);
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						m_AutoColors ? "\\r%d. \\w%s\n" : "%d. %s\n", 
						option, 
						m_OptNames[abs(MENU_MORE)].c_str()
						);
					m_Text.append(buffer);
				}
				break;
			}
		case MENU_EXIT:
			{
				if (flags & Display_Exit)
				{
					keys |= (1<<option++);
					_snprintf(buffer, 
						sizeof(buffer)-1, 
						m_AutoColors ? "\\r%d. \\w%s\n" : "%d. %s\n", 
						option, 
						m_OptNames[abs(MENU_EXIT)].c_str()
						);
					m_Text.append(buffer);
				}
				break;
			}
		}
	}
	
	return m_Text.c_str();
}

#define GETMENU(p) if (p >= (int)g_NewMenus.size() || p < 0 || !g_NewMenus[p]) { \
	LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d", p); \
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

	int id = g_menucmds.registerMenuId(title, amx);
	g_menucmds.registerMenuCmd(g_plugins.findPluginFast(amx), id, 1023, func);

	Menu *pMenu = new Menu(title, id, (int)g_NewMenus.size());

	pMenu->func = func;

	g_NewMenus.push_back(pMenu);

	return (int)g_NewMenus.size() - 1;
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
	item->blanks.push_back(params[2]);

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
			g_menucmds.removeMenuId(pMenu->menuId);
			pMenu->m_Title.assign(str);
			pMenu->menuId = g_menucmds.registerMenuId(str, amx);
			g_menucmds.registerMenuCmd(
				g_plugins.findPluginFast(amx),
				pMenu->menuId,
				1023,
				pMenu->func);
			break;
		}
	case MPROP_EXITALL:
		{
			cell ans = *get_amxaddr(amx, params[3]);
			if (ans == 1)
			{
				pMenu->m_AlwaysExit = true;
				pMenu->m_NeverExit = false;
			} else if (ans == 0) {
				pMenu->m_AlwaysExit = false;
				pMenu->m_NeverExit = false;
			} else if (ans == -1) {
				pMenu->m_NeverExit = true;
				pMenu->m_AlwaysExit = false;
			}
			break;
		}
	case MPROP_ORDER:
		{
			cell *addr = get_amxaddr(amx, params[3]);
			pMenu->m_OptOrders[0] = addr[0];
			pMenu->m_OptOrders[1] = addr[1];
			pMenu->m_OptOrders[2] = addr[2];
			break;
		}
	case MPROP_NOCOLORS:
		{
			pMenu->m_AutoColors = *get_amxaddr(amx, params[3]) ? true : false;
			break;
		}
	case MPROP_PADMENU:
		{
			pMenu->padding = *get_amxaddr(amx, params[3]);
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

static cell AMX_NATIVE_CALL menu_destroy(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	g_NewMenus[params[1]] = NULL;
	g_menucmds.removeMenuId(pMenu->menuId);
	CPlayer *player;
	for (int i=1; i<=gpGlobals->maxClients; i++)
	{
		player = GET_PLAYER_POINTER_I(i);
		if (player->newmenu == pMenu->menuId)
			player->newmenu = -1;
	}
	delete pMenu;

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

	if ( (*m != 0 && *m != -1) || (*n != -1))
		return 1;

	return 0;
}

AMX_NATIVE_INFO g_NewMenuNatives[] = 
{
	{"menu_create",				menu_create},
	{"menu_additem",			menu_additem},
	{"menu_addblank",			menu_addblank},
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
	{"player_menu_info",		player_menu_info},
	{NULL,						NULL},
};
