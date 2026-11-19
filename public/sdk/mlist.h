#pragma once

#include "mplugin.h"	// class MPlugin
#include "plinfo.h"		// plid_t, etc

// Width required to printf above MAX, for show() functions.
#define WIDTH_MAX_PLUGINS	2

typedef std::list<MPlugin *> plugins_t;

// A list of plugins.
class MPluginList
{
public:
	MPluginList(const char* ifile);

	plugins_t* getPlugins();

	MPlugin* find(int pindex);								// find by index
	MPlugin* find(const char* findpath);					// find by pathname
	MPlugin* find(plid_t id);								// find by plid_t
	MPlugin* find_memloc(void* memptr);						// find by memory location
	MPlugin* find_match(const char* prefix, bool& unique);	// find by partial prefix match
	MPlugin* find_match(MPlugin* pmatch);					// find by platform_match()
	MPlugin* find(module_handle_t handle);					// find by handle
	MPlugin* add(MPlugin* padd);

	bool found_child_plugins(int source_index) const;

	bool ini_startup();										// read inifile at startup
	bool ini_refresh();										// re-read inifile
	bool cmd_addload(const char* args);						// load from console command
	MPlugin* plugin_addload(plid_t plid, const char* fname, PLUG_LOADTIME now); // load from plugin

	bool load();											// load the list, at startup
	bool refresh(PLUG_LOADTIME now);						// update from re-read inifile
	void unpause_all();										// unpause any paused plugins
	void unload_all();										// unload all plugins
	void retry_all(PLUG_LOADTIME now);						// retry any pending plugin actions
	void show(int source_index = 0);						// list plugins to console use dynamic alignment
	void show_static(int source_index = 0);					// list plugins to console use static alignment
	void show_client(edict_t* pEntity);						// list plugins to player client
	void clear_source_plugin_index(int source_index);
	void getWidthFields(int source_index, size_t &widthDescBest, size_t &widthFileBest, size_t &widthVersBest);

private:
	size_t m_last_index;
	plugins_t m_plugins;			// array of plugins
	char m_inifile[MAX_PATH];		// full pathname
};
