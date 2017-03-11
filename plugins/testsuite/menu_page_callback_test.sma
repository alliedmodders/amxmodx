#include <amxmodx>

new g_menuHandle;
new bool:g_isCallbackSet = false;

public plugin_init()
{
	register_plugin("Menu Pagination Callback Test", "1.0.0", "KliPPy");

	register_clcmd("say testmenu", "@Command_TestMenu");
	register_clcmd("say togglecallback", "@Command_ToggleCallback");

	g_menuHandle = menu_create("Test menu", "@MenuHandler_TestMenu");
	menu_additem(g_menuHandle, "Item 1");
	menu_additem(g_menuHandle, "Item 2");
	menu_additem(g_menuHandle, "Item 3");
	menu_additem(g_menuHandle, "Item 4");
	menu_additem(g_menuHandle, "Item 5");
	menu_additem(g_menuHandle, "Item 6");
	menu_additem(g_menuHandle, "Item 7");
	menu_additem(g_menuHandle, "item 8");

	menu_setprop(g_menuHandle, MPROP_PERPAGE, 2);
}

public plugin_end()
{
	menu_destroy(g_menuHandle);
}

@MenuHandler_TestMenu(id, menu, item)
{
	if(item == MENU_EXIT)
		return PLUGIN_HANDLED;

	new dump1, dump2[1], dump3;
	new itemName[32];
	menu_item_getinfo(menu, item, dump1, dump2, 0, itemName, charsmax(itemName), dump3);

	client_print(id, print_chat, "Selected: %s", itemName);

	return PLUGIN_HANDLED;
}

@PageCallback_TestMenu(id, status)
{
	if(status == MENU_BACK)
		client_print(id, print_chat, "Selected: MENU_BACK");
	else
		client_print(id, print_chat, "Selected: MENU_MORE");
}

@Command_TestMenu(id)
{
	menu_display(id, g_menuHandle);

	return PLUGIN_HANDLED;
}

@Command_ToggleCallback(id)
{
	if(g_isCallbackSet)
	{
		menu_setprop(g_menuHandle, MPROP_PAGE_CALLBACK, NULL_STRING);
		g_isCallbackSet = false;

		client_print(id, print_chat, "Callback set to OFF");
	}
	else
	{
		menu_setprop(g_menuHandle, MPROP_PAGE_CALLBACK, "@PageCallback_TestMenu");
		g_isCallbackSet = true;

		client_print(id, print_chat, "Callback set to ON");
	}
}