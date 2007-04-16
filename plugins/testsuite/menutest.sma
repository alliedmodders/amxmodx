#include <amxmodx>

public plugin_init()
{
	register_plugin("Menu Tests", "1.0", "BAILOPAN")

	register_clcmd("menu_test1", "Test_Menu1")
	register_clcmd("menu_test2", "Test_Menu2")
	register_clcmd("menu_test3", "Test_Menu3")
	register_clcmd("menu_test4", "Test_Menu4")
	register_clcmd("menu_test5", "Test_Menu5")
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
	menu_additem(menu, "Gabezilla 7", "7", 0)
	menu_setprop(menu, MPROP_EXIT, MEXIT_NEVER)
	menu_display(id, menu, 0)
	return PLUGIN_HANDLED
}

public Test_Menu2(id, level, cid)
{
	new menu = menu_create("Character Upgrade:", "Test_Menu1_Handler")
	menu_additem(menu, "Gabezilla 1", "1", 0)
	menu_additem(menu, "Gabezilla 2", "2", 0)
	menu_additem(menu, "Gabezilla 3", "3", 0)
	menu_additem(menu, "Gabezilla 4", "4", 0)
	menu_additem(menu, "Gabezilla 5", "5", 0)
	menu_additem(menu, "Gabezilla 6", "6", 0)
	menu_display(id, menu, 0)
	return PLUGIN_HANDLED
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
	
	menu_destroy(menu)
	
	return PLUGIN_HANDLED
}

public Test_Menu3(id)
{
   new mHandleID = menu_create("Test Menu 3", "Test_Menu3_Handler")
   menu_additem(mHandleID, "test1", "1", 0)
   menu_additem(mHandleID, "test2", "2", 0)
   menu_additem(mHandleID, "test3", "3", 0)
   menu_additem(mHandleID, "test4", "4", 0)
   menu_additem(mHandleID, "test5", "5", 0)
   menu_additem(mHandleID, "test6", "6", 0)
   menu_additem(mHandleID, "test7", "7", 0)
   menu_additem(mHandleID, "test8", "8", 0)
   menu_additem(mHandleID, "test9", "9", 0)
   menu_additem(mHandleID, "test10", "10", 0)
   menu_additem(mHandleID, "test11", "11", 0)
   menu_addblank(mHandleID, 1)  // add blank got problem
   menu_setprop(mHandleID, MPROP_PERPAGE, 5)

   menu_display(id, mHandleID, 0)
   
   return PLUGIN_HANDLED
}

public Test_Menu3_Handler(id, menu, item)
{
   if (item == MENU_EXIT)
   {
	   menu_destroy(menu)
	   return PLUGIN_HANDLED
   }
   
   client_print(id, print_chat, "item = %d", item)
   
   menu_destroy(menu)
   
   return PLUGIN_HANDLED
}

public Test_Menu4(id)
{
   new mHandleID = menu_create("Test Menu 4", "Test_Menu4_Handler")
   menu_setprop(mHandleID, MPROP_PERPAGE, 0)
   menu_additem(mHandleID, "test1", "1", 0)
   menu_additem(mHandleID, "test2", "2", 0)
   menu_additem(mHandleID, "test3", "3", 0)
   menu_additem(mHandleID, "test4", "4", 0)
   menu_additem(mHandleID, "test5", "5", 0)
   menu_additem(mHandleID, "test6", "6", 0)
   menu_additem(mHandleID, "test7", "7", 0)
   menu_additem(mHandleID, "test8", "8", 0)
   menu_additem(mHandleID, "test9", "9", 0)

   menu_display(id, mHandleID, 0)
   
   return PLUGIN_HANDLED
}

public Test_Menu4_Handler(id, menu, item)
{
   if (item == MENU_EXIT)
   {
	   menu_destroy(menu)
	   return PLUGIN_HANDLED
   }
   
   client_print(id, print_chat, "item = %d", item)
   
   menu_destroy(menu)
   
   return PLUGIN_HANDLED
}

public Test_Menu5(id)
{
   new mHandleID = menu_create("Test Menu 5", "Test_Menu5_Handler")
   menu_additem(mHandleID, "test1", "1", 0)
   menu_additem(mHandleID, "test2", "2", 0)
   menu_additem(mHandleID, "test3", "3", 0)
   menu_additem(mHandleID, "test4", "4", 0)
   menu_additem(mHandleID, "test5", "5", 0)
   menu_additem(mHandleID, "test6", "6", 0)
   menu_additem(mHandleID, "test7", "7", 0)
   menu_additem(mHandleID, "test8", "8", 0)
   menu_additem(mHandleID, "test9", "9", 0)
   menu_additem(mHandleID, "test10", "10", 0)
   menu_additem(mHandleID, "test11", "11", 0)
   menu_addblank(mHandleID, 1)  // add blank got problem
   menu_setprop(mHandleID, MPROP_EXIT, MEXIT_NEVER)

   menu_display(id, mHandleID, 0)
   
   return PLUGIN_HANDLED
}

public Test_Menu5_Handler(id, menu, item)
{
   if (item == MENU_EXIT)
   {
	   menu_destroy(menu)
	   return PLUGIN_HANDLED
   }
   
   client_print(id, print_chat, "item = %d", item)
   
   menu_destroy(menu)
   
   return PLUGIN_HANDLED
}
