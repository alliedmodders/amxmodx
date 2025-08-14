#pragma once

// Our structure for storing engine references.
struct engine_t
{
	engine_t() : funcs(nullptr), globals(nullptr)
	{
		memset(&pl_funcs, 0, sizeof pl_funcs);
	}

	enginefuncs_t* funcs;       // engine funcs
	globalvars_t* globals;      // engine globals

	// Our modified version of the engine funcs, to give to plugins.
	enginefuncs_t pl_funcs;     // "modified" eng funcs we give to plugins
	CSysModule sys_module;
};

extern engine_t g_engine;
