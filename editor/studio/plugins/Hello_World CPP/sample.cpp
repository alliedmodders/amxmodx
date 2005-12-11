#include "sample.h"
#include "studio_api.h"
#include <stdio.h>
#include <windows.h>
#include <string.h>


EXPORT void PluginLoad(load_info *LoadInfo) {
    LoadInfo->sPluginName = "C(++) Example"; // Set PlugIn-name
	LoadInfo->sPluginDescription = "Simple example for creating plugins with C(++)"; // Set description
	SendStudioMsg(SCM_MENU_ADDITEM, "Tools->Hello World! [C++]", -1); // Register menu items
	/* [...] */
}

EXPORT void PluginUnload() {
	SendStudioMsg(SCM_REMOVE_MENUITEM, "Hello World! [C++]", 0);
}

EXPORT int CustomItemClick(CHAR *Caption) {
	if (strcmp(Caption, "Hello World! [C++]") == 0) { // If caption is equal set text and return PLUGIN_HANDLED
		SendStudioMsg(SCM_EDITOR_SETTEXT, "Hello World!\n\nThis is an example for C(++) plugins for AMXX-Studio.", 0);
		return PLUGIN_HANDLED;
	}
	else
		return PLUGIN_CONTINUE;
}