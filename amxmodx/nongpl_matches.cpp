#include <string.h>
#include "nongpl_matches.h"

NONGPL_PLUGIN_T NONGPL_PLUGIN_LIST[] = 
{
	{"Live", "CZ Gun Game", "czgungame.amxx"},
	{NULL, NULL, NULL},
};

NONGPL_CVAR_T NONGPL_CVAR_LIST[] = 
{
	{"gg_mode", 0},
	{"gg_warmuptimer", 0},
	{"gg_ff", 0},
	{"gg_fflevel", 0},
	{"gg_stats", 0},
	{"gg_dm", 0},
	{"gg_turbo", 0},
	{"amx_ggreset", 1},
	{"amx_gg", 1},
	{NULL, 0},
};
