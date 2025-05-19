#pragma once

// Flags to use for meta_cmd_doplug(), to operate on existing plugins; note
// "load" operates on a non-existing plugin thus isn't included here.
enum PLUG_CMD
{
	PC_NULL,
	PC_PAUSE,			// pause the plugin
	PC_UNPAUSE,			// unpause the plugin
	PC_UNLOAD,			// unload the plugin
	PC_RELOAD,			// unload the plugin and load it again
	PC_RETRY,			// retry a failed operation (usually load/attach)
	PC_INFO,			// show all info about the plugin
	PC_CLEAR,			// remove a failed plugin from the list
	PC_FORCE_UNLOAD,	// forcibly unload the plugin
	PC_REQUIRE,			// require that this plugin is loaded/running
};

void meta_register_cmdcvar();

void server_meta();

void cmd_meta_usage();
void cmd_meta_version();
void cmd_meta_gpl();

void cmd_meta_game();
void cmd_meta_refresh();
void cmd_meta_load();

void cmd_meta_pluginlist();
void cmd_meta_cmdlist();
void cmd_meta_cvarlist();
void cmd_meta_config();

void cmd_doplug(PLUG_CMD pcmd);

void client_meta(edict_t* pEntity);
void client_meta_usage(edict_t* pEntity);
void client_meta_version(edict_t* pEntity);
void client_meta_pluginlist(edict_t* pEntity);
