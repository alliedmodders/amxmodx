#include <amxmodx>

public plugin_init()
{
	register_plugin("Menu Tests", "1.0", "BAILOPAN")

	register_clcmd("menu_test1", "Test_Menu1")
}

public Test_Menu1(id, level, cid)
{
	new menu = menu_create("Character Upgrade:", "Test_Menu1_Handler")
	menu_additem(menu, "Gabezilla 1", "1", 0)
	menu_additem(menu, "Gabezilla 2", "2", 0)
	menu_additem(menu, "Gabezilla 3", "3", 0)
	menu_additem(menu, "Gabezilla 4", "4", 0)
	menu_additem(menu, "Gabezilla 5", "5", 0)
	menu_additem(menu, "Gabezilla 6", "6", 0)
	menu_addblank(menu, 7)
	menu_setprop(menu, MPROP_EXIT, MEXIT_ALL)
	menu_display(id, menu, 0)
}

public Test_Menu1_Handler(id, menu, item)
{
	client_print(id, print_chat, "Menu (%d->%d): Chose %d", menu, id, item)
	if (item == MENU_EXIT)
	{
		menu_destroy(menu)
		return PLUGIN_HANDLED
	}
	
	new cmd[32], name[32], access
	
	menu_item_getinfo(menu, item, access, cmd, 31, name, 31, access)
	
	client_print(id, print_chat, "Menu resolved to: %s (%s)", name, cmd)
	
	return PLUGIN_HANDLED
}

