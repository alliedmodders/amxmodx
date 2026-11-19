#pragma once

// Flags for plugin to indicate when it can be be loaded/unloaded.
// NOTE: order is crucial, as greater/less comparisons are made.
enum PLUG_LOADTIME
{
	PT_NEVER,
	PT_STARTUP,             // should only be loaded/unloaded at initial hlds execution
	PT_CHANGELEVEL,         // can be loaded/unloaded between maps
	PT_ANYTIME,             // can be loaded/unloaded at any time
	PT_ANYPAUSE,            // can be loaded/unloaded at any time, and can be "paused" during a map
};

// Flags to indicate why the plugin is being unloaded.
enum PL_UNLOAD_REASON
{
	PNL_NULL,
	PNL_INI_DELETED,        // was deleted from plugins.ini
	PNL_FILE_NEWER,         // file on disk is newer than last load
	PNL_COMMAND,            // requested by server/console command
	PNL_CMD_FORCED,         // forced by server/console command
	PNL_DELAYED,            // delayed from previous request; can't tell origin

	                        // only used for 'real_reason' on MPlugin::unload()
	PNL_PLUGIN,             // requested by plugin function call
	PNL_PLG_FORCED,         // forced by plugin function call
	PNL_RELOAD,             // forced unload by reload()
};

// Information plugin provides about itself.
struct plugin_info_t
{
    const char*     ifvers;     // meta_interface version
    const char*     name;       // full name of plugin
    const char*     version;    // version
    const char*     date;       // date
    const char*     author;     // author name/email
    const char*     url;        // URL
    const char*     logtag;     // log message prefix (unused right now)
    PLUG_LOADTIME   loadable;   // when loadable
    PLUG_LOADTIME   unloadable; // when unloadable
};

extern plugin_info_t Plugin_info;

// Plugin identifier, passed to all Meta Utility Functions.
typedef plugin_info_t *plid_t;
#define PLID &Plugin_info
