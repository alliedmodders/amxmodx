// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "CMenu.h"
#include "newmenus.h"
#include "format.h"

ke::Vector<Menu *> g_NewMenus;
CStack<int> g_MenuFreeStack;

void ClearMenus()
{
	for (size_t i = 0; i < g_NewMenus.length(); i++)
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

Menu *get_menu_by_id(int id)
{
	if (id < 0 || size_t(id) >= g_NewMenus.length() || !g_NewMenus[id])
		return NULL;

	return g_NewMenus[id];
}

bool CloseNewMenus(CPlayer *pPlayer)
{
	// If the stupid handler keeps drawing menus,
	// We need to keep cancelling them.  But we put in a quick infinite loop
	// counter to prevent this from going nuts. 

	int loops = 0;
	Menu *pMenu;

	while ((pMenu = get_menu_by_id(pPlayer->newmenu)))
	{
		pMenu->Close(pPlayer->index);

		if (++loops >= 10)
		{
			return false;
		}
	}

	return true;
}

Menu::Menu(const char *title, AMX *amx, int fid, bool use_ml) : m_Title(title), m_ItemColor("\\r"), 
m_NeverExit(false), m_ForceExit(false), m_AutoColors(g_coloredmenus), thisId(0), func(fid), 
isDestroying(false), pageCallback(-1), showPageNumber(true), useMultilingual(use_ml), amx(amx), items_per_page(7)
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

	m_OptNames[abs(MENU_BACK)] = "Back";
	m_OptNames[abs(MENU_MORE)] = "More";
	m_OptNames[abs(MENU_EXIT)] = "Exit";
}

Menu::~Menu()
{
	for (size_t i = 0; i < m_Items.length(); i++)
	{
		delete m_Items[i];
	}

	unregisterSPForward(this->func);
	unregisterSPForward(this->pageCallback);
	
	m_Items.clear();
}

menuitem *Menu::AddItem(const char *name, const char *cmd, int access)
{
	menuitem *pItem = new menuitem;

	pItem->name = name;
	pItem->cmd = cmd;
	pItem->access = access;
	pItem->id = m_Items.length();
	pItem->handler = -1;
	pItem->isBlank = false;
	pItem->pfn = NULL;

	m_Items.append(pItem);

	return pItem;
}

menuitem *Menu::GetMenuItem(item_t item)
{
	if (item >= m_Items.length())
		return NULL;

	return m_Items[item];
}

size_t Menu::GetItemCount()
{
	return m_Items.length();
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
		if (key > m_Items.length())
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
			for (size_t i=start; i<(start+key-1) && i<m_Items.length(); i++)
			{
				for (size_t j=0; j<m_Items[i]->blanks.length(); j++)
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
			size_t remaining = m_Items.length() - start;
			item_t new_key = key;
			
			// For every item that takes up a slot (item or padded blank)
			// we subtract one from new key.
			// For every item (not blanks), we increase item_tracker.
			// When new_key equals 0, item_tracker will then be set to
			// whatever valid item was selected.
			for (size_t i=m_Items.length() - remaining; i<m_Items.length(); i++)
			{
				item_tracker++;
				
				if (new_key<=1) // If new_key is 0, or will be 0 after the next decrease
				{
					new_key=0;
					break;
				}
				
				new_key--;
				
				for (size_t j=0; j<m_Items[i]->blanks.length(); j++)
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
			for (size_t i=start; i<(start+items_per_page-1) && i<m_Items.length(); i++)
			{
				for (size_t j=0; j<m_Items[i]->blanks.length(); j++)
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
	int len = ke::SafeSprintf(buffer, sizeof(buffer), "%s", str);

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(player);

	pPlayer->keys = keys;
	pPlayer->menu = menuId;
	pPlayer->newmenu = thisId;
	pPlayer->page = (int)page;

	UTIL_ShowMenu(pPlayer->pEdict, keys, -1, buffer, len);

	return true;
}

void Menu::Close(int player) 
{
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(player);

	int status;
	if (gpGlobals->time > pPlayer->menuexpire)
		status = MENU_TIMEOUT;
	else
		status = MENU_EXIT;

	pPlayer->keys = 0;
	pPlayer->menu = 0;
	pPlayer->newmenu = -1;

	executeForwards(func,
		static_cast<cell>(player),
		static_cast<cell>(thisId),
		static_cast<cell>(status));
}

const char *Menu::GetTextString(int player, page_t page, int &keys)
{
	page_t pages = GetPageCount();
	item_t numItems = GetItemCount();

	if (page >= pages)
		return NULL;

	m_Text = nullptr;


	auto title = m_Title.chars();

	if (this->useMultilingual)
	{
		const auto language = playerlang(player);
		const auto definition = translate(this->amx, language, title);

		if (definition)
		{
			title = definition;
		}
	}

	char buffer[255];
	if (showPageNumber && items_per_page && (pages != 1))
	{
		if (m_AutoColors)
			ke::SafeSprintf(buffer, sizeof(buffer), "\\y%s %d/%d\n\\w\n", title, page + 1, pages);
		else
			ke::SafeSprintf(buffer, sizeof(buffer), "%s %d/%d\n\n", title, page + 1, pages);
	} else {
		if (m_AutoColors)
			ke::SafeSprintf(buffer, sizeof(buffer), "\\y%s\n\\w\n", title);
		else
			ke::SafeSprintf(buffer, sizeof(buffer), "%s\n\n", title);
	}
	
	m_Text = m_Text + buffer;

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

		auto itemName = pItem->name.chars();

		if (this->useMultilingual)
		{
			const auto language = playerlang(player);
			const auto definition = translate(this->amx, language, itemName);

			if (definition)
			{
				itemName = definition;
			}
		}

		if (pItem->isBlank)
		{
			ke::SafeSprintf(buffer, sizeof(buffer), "%s\n", itemName);
		}
		else if (enabled)
		{
			if (m_AutoColors)
			{
				ke::SafeSprintf(buffer, sizeof(buffer), "%s%d.\\w %s\n", m_ItemColor.chars(),option_display, itemName);
			} else {
				ke::SafeSprintf(buffer, sizeof(buffer), "%d. %s\n", option_display, itemName);
			}
		} else {
			if (m_AutoColors)
			{
				ke::SafeSprintf(buffer, sizeof(buffer), "\\d%d. %s\n\\w", option_display, itemName);
			} else {
				ke::SafeSprintf(buffer, sizeof(buffer), "#. %s\n", itemName);
			}
		}
		slots++;

		m_Text = m_Text + buffer;

		//attach blanks
		if (pItem->blanks.length())
		{
			for (size_t j=0; j<pItem->blanks.length(); j++)
			{
				if (pItem->blanks[j].EatNumber())
				{
					option++;
				}
				m_Text = m_Text + pItem->blanks[j].GetDisplay();
				m_Text = m_Text + "\n";
				slots++;
			}
		}
	}

	if (items_per_page)
	{
		/* Pad spaces until we reach the end of the max possible items */
		for (unsigned int i=(unsigned)slots; i<items_per_page; i++)
		{
			m_Text = m_Text + "\n";
			option++;
		}
		/* Make sure there is at least one visual pad */
		m_Text = m_Text + "\n";

		/* Don't bother if there is only one page */
		if (pages > 1)
		{

			auto tempItemName = m_OptNames[abs(MENU_BACK)].chars();

			if (this->useMultilingual)
			{
				const auto language = playerlang(player);
				const auto definition = translate(this->amx, language, tempItemName);

				if (definition)
				{
					tempItemName = definition;
				}
			}

			if (flags & Display_Back)
			{
				keys |= (1<<option++);
				if (m_AutoColors)
				{
					ke::SafeSprintf(buffer,
						sizeof(buffer), 
						"%s%d. \\w%s\n", 
						m_ItemColor.chars(), 
						option == 10 ? 0 : option, 
						tempItemName);
				} else {
					ke::SafeSprintf(buffer,
						sizeof(buffer), 
						"%d. %s\n", 
						option == 10 ? 0 : option, 
						tempItemName);
				}
			} else {
				option++;
				if (m_AutoColors)
				{
					ke::SafeSprintf(buffer,
						sizeof(buffer),
						"\\d%d. %s\n\\w",
						option == 10 ? 0 : option,
						tempItemName);
				} else {
					ke::SafeSprintf(buffer, sizeof(buffer), "#. %s\n", tempItemName);
				}
			}
			m_Text = m_Text + buffer;

			tempItemName = m_OptNames[abs(MENU_MORE)].chars();

			if (this->useMultilingual)
			{
				const auto language = playerlang(player);
				const auto definition = translate(this->amx, language, tempItemName);

				if (definition)
				{
					tempItemName = definition;
				}
			}

			if (flags & Display_Next)
			{
				keys |= (1<<option++);
				if (m_AutoColors)
				{
					ke::SafeSprintf(buffer,
						sizeof(buffer), 
						"%s%d. \\w%s\n", 
						m_ItemColor.chars(), 
						option == 10 ? 0 : option, 
						tempItemName);
				} else {
					ke::SafeSprintf(buffer,
						sizeof(buffer), 
						"%d. %s\n", 
						option == 10 ? 0 : option, 
						tempItemName);
				}
			} else {
				option++;
				if (m_AutoColors)
				{
					ke::SafeSprintf(buffer,
						sizeof(buffer),
						"\\d%d. %s\n\\w",
						option == 10 ? 0 : option,
						tempItemName);
				} else {
					ke::SafeSprintf(buffer, sizeof(buffer), "#. %s\n", tempItemName);
				}
			}
			m_Text = m_Text + buffer;
		} else {
			/* Keep padding */
			option += 2;
		}
	}

	if ((items_per_page && !m_NeverExit) || (m_ForceExit && numItems < 10))
	{
		auto exitName = m_OptNames[abs(MENU_EXIT)].chars();

		if (this->useMultilingual)
		{
			const auto language = playerlang(player);
			const auto definition = translate(this->amx, language, exitName);

			if (definition)
			{
				exitName = definition;
			}
		}

		/* Visual pad has not been added yet */
		if (!items_per_page)
			m_Text = m_Text + "\n";

		keys |= (1<<option++);
		if (m_AutoColors)
		{
			ke::SafeSprintf(buffer,
				sizeof(buffer), 
				"%s%d. \\w%s\n", 
				m_ItemColor.chars(), 
				option == 10 ? 0 : option, 
				exitName);
		} else {
			ke::SafeSprintf(buffer,
				sizeof(buffer), 
				"%d. %s\n", 
				option == 10 ? 0 : option, 
				exitName);
		}
		m_Text = m_Text + buffer;
	}

	return m_Text.ptr();
}

#define GETMENU(p) Menu *pMenu = get_menu_by_id(p); \
	if (pMenu == NULL || pMenu->isDestroying) { \
	LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d(%d)", p, g_NewMenus.length()); \
	return 0; }

// native menu_create(const title[], const handler[], bool:ml = false);
static cell AMX_NATIVE_CALL menu_create(AMX *amx, cell *params)
{
	enum args { arg_count, arg_title, arg_handler, arg_ml };

	int length;
	const auto title    = get_amxstring(amx, params[arg_title], 0, length);
	const auto handler  = get_amxstring(amx, params[arg_handler], 1, length);
	const auto callback = registerSPForwardByName(amx, handler, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (callback == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, R"(Invalid function "%s")", handler);
		return 0;
	}

	validate_menu_text(title);

	auto pMenu = new Menu(title, amx, callback, params[arg_ml] != 0);

	if (g_MenuFreeStack.empty())
	{
		g_NewMenus.append(pMenu);

		pMenu->thisId = static_cast<int>(g_NewMenus.length()) - 1;
	}
	else
	{
		const auto position = g_MenuFreeStack.front();

		g_MenuFreeStack.pop();
		g_NewMenus[position] = pMenu;

		pMenu->thisId = position;
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

	if (!pMenu->m_Items.length())
	{
		LogError(amx, AMX_ERR_NATIVE, "Blanks can only be added after items.");
		return 0;
	}

	menuitem *item = pMenu->m_Items[pMenu->m_Items.length() - 1];

	BlankItem a;

	a.SetBlank();

	if (params[2] == 1)
		a.SetEatNumber(true);

	else
		a.SetEatNumber(false);

	item->blanks.append(ke::Move(a));

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

	if (!pMenu->m_Items.length())
	{
		LogError(amx, AMX_ERR_NATIVE, "Blanks can only be added after items.");
		return 0;
	}

	menuitem *item = pMenu->m_Items[pMenu->m_Items.length() - 1];

	BlankItem a;

	int len;
	a.SetText(get_amxstring(amx, params[2], 0, len));

	if (params[3] == 1)
		a.SetEatNumber(true);

	else
		a.SetEatNumber(false);

	item->blanks.append(ke::Move(a));

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
	auto handle = params[2];
	GETMENU(handle);

	int player = params[1];
	int page = params[3];
	
	if (player < 1 || player > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d.", player);
		return 0;
	}
	
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(player);
	
	if (!pPlayer->ingame)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not in game.", player);
		return 0;
	}

	if (!CloseNewMenus(pPlayer))
	{
		LogError(amx, AMX_ERR_NATIVE, "Plugin called menu_display when item=MENU_EXIT");
		return 0;
	}

	if (!g_NewMenus[handle])
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d (was previously destroyed).", handle);
		return 0;
	}

	if (g_bmod_cstrike)
	{
		enum JoinState { Joined = 0 };
		enum MenuState { Menu_OFF = 0, Menu_ChooseTeam = 1, Menu_ChooseAppearance = 3 };

		GET_OFFSET("CBasePlayer", m_iJoiningState);
		GET_OFFSET("CBasePlayer", m_iMenu);

		if (get_pdata<int>(pPlayer->pEdict, m_iJoiningState) == Joined || (get_pdata<int>(pPlayer->pEdict, m_iMenu) != Menu_ChooseTeam && get_pdata<int>(pPlayer->pEdict, m_iMenu) != Menu_ChooseAppearance))
		{
			set_pdata<int>(pPlayer->pEdict, m_iMenu, Menu_OFF);
		}
	}

	int time = -1;
	if (params[0] / sizeof(cell) >= 4)
		time = params[4];

	if (time < 0)
		pPlayer->menuexpire = static_cast<float>(INFINITE);
	else
		pPlayer->menuexpire = gpGlobals->time + static_cast<float>(time);

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

	set_amxstring(amx, params[4], pItem->cmd.chars(), params[5]);
	set_amxstring(amx, params[6], pItem->name.chars(), params[7]);

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

	pItem->name = name;

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

	pItem->cmd = cmd;

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

static cell AMX_NATIVE_CALL menu_item_setaccess(AMX *amx, cell *params)
{
	GETMENU(params[1]);

	menuitem *pItem = pMenu->GetMenuItem(static_cast<item_t>(params[2]));

	if (!pItem)
		return 0;

	pItem->access = params[3];

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
	case MPROP_PAGE_CALLBACK:
		{
			const char *str = get_amxstring_null(amx, params[3], 0, len);
			if (str == nullptr)
			{
				unregisterSPForward(pMenu->pageCallback);
				pMenu->pageCallback = -1;
				break;
			}

			int callback = registerSPForwardByName(amx, str, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
			if (callback < 0)
			{
				LogError(amx, AMX_ERR_NATIVE, "Function %s not present", str);
				return 0;
			}

			unregisterSPForward(pMenu->pageCallback);
			pMenu->pageCallback = callback;

			break;
		}
	case MPROP_SHOWPAGE:
		{
			pMenu->showPageNumber = *get_amxaddr(amx, params[3]) != 0;
			break;
		}
	case MPROP_SET_NUMBER_COLOR:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_ItemColor = str;
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
			pMenu->m_OptNames[abs(MENU_BACK)] = str;
			break;
		}
	case MPROP_NEXTNAME:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_OptNames[abs(MENU_MORE)] = str;
			break;
		}
	case MPROP_EXITNAME:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			validate_menu_text(str);
			pMenu->m_OptNames[abs(MENU_EXIT)] = str;
			break;
		}
	case MPROP_TITLE:
		{
			char *str = get_amxstring(amx, params[3], 0, len);
			pMenu->m_Title = str;
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
			pMenu->m_AutoColors = *get_amxaddr(amx, params[3]) ? false : true;
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

#define GETMENU_R(p) Menu *pMenu = get_menu_by_id(p); \
	if (pMenu == NULL) { \
	LogError(amx, AMX_ERR_NATIVE, "Invalid menu id %d(%d)", p, g_NewMenus.length()); \
	return 0; }

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
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not in game", index);
		return 0;
	}

	if (Menu *pMenu = get_menu_by_id(player->newmenu)) {
		pMenu->Close(player->index);
		return 1;
	}

	return 0;
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
			pMenu->Close(player->index);
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
	{"menu_item_setaccess",		menu_item_setaccess},
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
