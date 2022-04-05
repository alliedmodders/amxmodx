// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _newmenus_included
  #endinput
#endif
#define _newmenus_included

/**
 * @section Menu properties for using in menu_setprop 
 */

/**
 * Menu will have an exit option (default)
 */
#define MEXIT_ALL		1

/**
 * Menu will have an exit option, even when pagination is disabled. 
 * There have to be less than 10 items in the menu or it won't appear. The exit 
 * option will be appended to the last item with no extra slot padding. If you 
 * want it in the 10th slot you have to pad it manually with menu_addblank2 
 */
#define MEXIT_FORCE		2

/**
 * Menu will not have an exit option
 */
#define MEXIT_NEVER		-1

/**
 * Number of items per page (param1 = number, 0=no paginating, 7=default)
 */
#define MPROP_PERPAGE	1

/**
 * Name of the back button (param1 = string)
 */
#define MPROP_BACKNAME	2		

/**
 * Name of the next button (param1 = string)
 */
#define MPROP_NEXTNAME	3

/**
 * Name of the exit button (param1 = string)
 */
#define MPROP_EXITNAME	4

/**
 * Menu title text (param1 = string)
 */
#define MPROP_TITLE		5

/**
 * Exit functionality (param1 = number, see MEXIT constants)
 */
#define MPROP_EXIT		6

/**
 * Sets whether colors are not auto (param1 = number, 0=default)
 */
#define MPROP_NOCOLORS	8

/**
 * Color indicator to use for numbers (param1 = string, "\r"=default)
 */
#define MPROP_NUMBER_COLOR	10

/**
 * Function to be called on Back and Next (param1 = string)
 * public function(id, status, menu); where status is either MENU_BACK or MENU_MORE
 * Pass NULL_STRING to disable the callback 
 */
#define MPROP_PAGE_CALLBACK	11	

/**
 * Whether to show the page number in menu title (param1 = bool, true = default)
 */
#define MPROP_SHOWPAGE 12

/**
 * @deprecated
 */
#define MEXIT_NORMAL	0		/* DEPRECATED, do not use (has no effect) */
#define MENUPAD_NONE	0		/* DEPRECATED, do not use (has no effect) */
#define MENUPAD_PAGE	1		/* DEPRECATED, do not use (has no effect) */
#define MPROP_ORDER		7		/* DEPRECATED, do not use (has no effect) */
#define MPROP_PADMENU	9		/* DEPRECATED, do not use (has no effect) */

/** @endsection */

/**
 * Creates a new menu object.
 *
 * @note The handler function should be prototyped as:
 *
 * public <function>(id, menu, item)
 *   id     - Client the menu is being acted upon.
 *   menu   - Menu resource identifier.
 *   item   - Item the client selected.  If less than 0, the menu was 
 *            cancelled and the item is a status code.  menu_display 
 *			  should never be called immediately if the item is a status 
 *			  code, for re-entrancy reasons.
 *
 * @note The handler function should always return PLUGIN_HANDLED to block 
 *       any old menu handlers from potentially feeding on the menu, unless 
 *       that is the desired functionality.
 *
 * @param title 		Title the menu should use.
 * @param handler		Name of the handler function.  The function will be invoked 
 *						once and only once to every menu_display() call.
 * @param ml			If true, the menu title and items will be looked up as multilingual keys
 *                      when the menu displays.
 * @return				Menu resource identifier which must be destroyed via 
 *						menu_destroy().  All menus are destroyed when the plugin 
 *						unloads.
 * @error				Function name not found.
 */
native menu_create(const title[], const handler[], bool:ml = false);

/**
 * Creates a menu item callback handler.
 *
 * @note The handler function should be prototyped as:
 *
 * public <function>(id, menu, item)
 *   id      - Client index being displayed to.
 *   menu    - Menu resource identifier.
 *   item    - Item being drawn.
 *   <return> - ITEM_IGNORE to use the default functionality.
 *              ITEM_ENABLED to explicitly enable or ITEM_DISABLED to explicitly disable.
 *
 * @param function		Function name.
 * @return				Menu callback ID.
 */
native menu_makecallback(const function[]);

/**
 * Adds an item to a menu.
 *
 * @param menu			Menu resource identifier.
 * @param name			Item text to display.
 * @param info			Item info string for internal information.
 * @param paccess		Access required by the player viewing the menu.
 * @param callback		If set to a valid ID from menu_makecallback(), the 
 *						callback will be invoked before drawing the item.
 * @noreturn
 * @error				Invalid menu resource.
 */
native menu_additem(menu, const name[], const info[]="", paccess=0, callback=-1);

/**
 * Returns the number of pages in a menu.
 *
 * @param menu			Menu resource identifier.
 * @return				Number of pages in the menu.
 * @error				Invalid menu resource.
 */
native menu_pages(menu);

/**
 * Returns the number of items in a menu.
 *
 * @param menu			Menu resource identifier.
 * @return				Number of items in the menu.
 * @error				Invalid menu resource.
 */
native menu_items(menu);

/**
 * Displays a menu to one client.  This should never be called from a handler 
 * when the item is less than 0 (i.e. calling this from a cancelled menu will 
 * result in an error).
 *
 * Starting with 1.8.3 this allows to specify a menu timeout similar to the 
 * show_menu native. If the menu exists on the client past the timeout *any* 
 * further action will send the MENU_TIMEOUT status code to the menu handler.
 * That includes actions which would otherwise send MENU_EXIT, such as the
 * client selecting an item or disconnecting and calling menu_cancel or 
 * menu_destroy on a live menu.
 *
 * @param id			Client index.
 * @param menu			Menu resource identifier.
 * @param page			Page to start from (starting from 0).
 * @param time			If >=0 menu will timeout after this many seconds
 * @noreturn
 * @error				Invalid menu resource or client index.
 */
native menu_display(id, menu, page=0, time=-1);

/**
 * Given a page on a menu and a keypress on that page, returns the item id selected.
 * If the item is less than 0, a special option was chosen (such as MENU_EXIT).
 *
 * @param menu			Menu resource identifier.
 * @param page			Page on the menu.
 * @param key			Key pressed (from 1 to 10).
 * @return				Item identifier, or <0 for a special selection code.
 * @error				Invalid menu resource.
 */
native menu_find_id(menu, page, key);

/**
 * Retrieves info about a menu item.
 *
 * @param menu			Menu resource identifier.
 * @param item			Item identifier.
 * @param access		Variable to store access value.
 * @param info			Buffer to store item info.
 * @param infolen		Item info buffer length.
 * @param name			Buffer to store item display text.
 * @param namelen		Item name buffer length.
 * @param callback		Callback ID.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource.
 */
native menu_item_getinfo(menu, item, &access = 0, info[] = "", infolen = 0, name[]="", namelen=0, &callback = 0);

/**
 * Sets an item's display text.
 *
 * @param menu			Menu resource identifier.
 * @param item			Item identifier.
 * @param name			New item display text.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource.
 */
native menu_item_setname(menu, item, const name[]);

/**
 * Sets an item's info string.
 *
 * @param menu			Menu resource identifier.
 * @param item			Item identifier.
 * @param info			New item info string.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource.
 */
native menu_item_setcmd(menu, item, const info[]);

/**
 * Sets an item's callback.
 *
 * @param menu			Menu resource identifier.
 * @param item			Item identifier.
 * @param callback		New callback from menu_makecallback(), or -1 to clear.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource.
 */
native menu_item_setcall(menu, item, callback=-1);

/**
 * Sets an item's access.
 *
 * @param menu			Menu resource identifier.
 * @param item			Item identifier.
 * @param access		New access required by the player for access to the item.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource.
 */
native menu_item_setaccess(menu, item, access=0);

/**
 * Destroys a menu.  Player menus will be cancelled (although may still linger 
 * on the HUD), and future attempts to access the menu resource will result in 
 * an error.
 *
 * This must be called if you create menus dynamically, otherwise you will 
 * leak memory.  For normal dynamic menus, you will destroy the menu in the 
 * handler function (remembering to handle the case of a menu being cancelled, 
 * it must still be destroyed).
 *
 * @param menu			Menu resource identifier.
 * @noreturn
 * @error				Invalid menu resource.
 */
native menu_destroy(menu);

/**
 * Returns information about a menu (if any) the client is currently viewing.
 *
 * @note If newmenu is valid, then the menu will refer to the menuid associated with 
 *       the title. If newmenu is not valid, and the menu is valid, then the player 
 *       is viewing a menu displayed with show_menu().
 *
 * @note Both may be invalid if the player is not viewing a menu.
 *
 * @param id			Client index.
 * @param menu			Variable to store old menu id.  If none, then <1 will be 
 *						stored.
 * @param newmenu		Variable to store new menu id.  If none, then -1 will be 
 *						stored.
 * @param menupage		Variable to store current page of the new menu, if any.
 * @return				1 if the player is viewing a menu, 0 otherwise.
 * @error				Invalid client.
 */
native player_menu_info(id, &menu, &newmenu, &menupage=0);

/**
 * Adds a blank line to a menu.
 * 
 * @note When using slot=1 this might break your menu. To achieve this functionality
 *       menu_addblank2 should be used.
 * 
 * @param menu			Menu resource identifier.
 * @param slot			1 (default) if the line should shift the numbering down.
 * 						0 if the line should be a visual shift only.
 * @noreturn
 * @error				Invalid menu resource.
 */
native menu_addblank(menu, slot=1);

/**
 * Adds a text line to a menu.
 *
 * @note When using slot=1 this might break your menu. To achieve this functionality
 *       menu_addtext2 should be used.
 * 
 * @param menu			Menu resource identifier.
 * @param text			Text to add.
 * @param slot			1 (default) if the line should shift the numbering down.
 * 						0 if the line should be a visual shift only.
 * @noreturn
 * @error				Invalid menu resource.
 */
native menu_addtext(menu, const text[], slot=1);

/**
 * Adds a blank line to a menu, always shifting the numbering down.
 * 
 * @note This will add a special item to create a blank line. It will affect the menu
 *       item count and pagination. These items can be modified later but will ignore 
 *       access and item callback results.
 * 
 * @param menu          Menu resource identifier.
 * 
 * @return              1 on success, 0 on failure.
 * @error               Invalid menu resource.
 *                      Too many items on non-paginated menu (max is 10)
 */
native menu_addblank2( menu );

/**
 * Adds a text line to a menu, always shifting the numbering down.
 * 
 * @note This will add a special item to create a blank line. It will affect the menu
 *       item count and pagination. These items can be modified later but will ignore 
 *       access and item callback results.
 *
 * @param menu          Menu resource identifier.
 * @param text          Text to add.
 * 
 * @return              1 on success, 0 on failure.
 * @error               Invalid menu resource.
 *                      Too many items on non-paginated menu (max is 10)
 */
native menu_addtext2( menu, const text[] );

/**
 * Sets a menu property.
 *
 * @param menu			Menu resource identifier.
 * @param prop			MPROP_ constant.
 * @param ...			Property parameters.
 * @return				1 on success, 0 on failure.
 * @error				Invalid menu resource or property.
 */
native menu_setprop(menu, prop, ...);

/**
 * Cancels a player's menu, effectively forcing the player to select MENU_EXIT.
 * The menu will still exist on their screen but any results are invalidated,
 * and the callback is invoked.
 *
 * @param player		Client index.
 * @noreturn
 * @error				Invalid client index.
 */
native menu_cancel(player);
