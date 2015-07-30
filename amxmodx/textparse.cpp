/** 
 * vim: set ts=4 sw=4 tw=99 noet:
 *
 * AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
 * Copyright (C) The AMX Mod X Development Team.
 *
 * This software is licensed under the GNU General Public License, version 3 or higher.
 * Additional exceptions apply. For full license details, see LICENSE.txt or visit:
 *     https://alliedmods.net/amxmodx-license
 */

#include "textparse.h"

NativeHandle<ParseInfo> TextParsersHandles;

cell createParser()
{
	return TextParsersHandles.create();
}

cell destroyParser(cell *handle)
{
	ParseInfo *p = TextParsersHandles.lookup(*handle);

	if (!p)
	{
		return 0;
	}

	if (TextParsersHandles.destroy(*handle))
	{
		*handle = 0;
		return 1;
	}

	return 0;
}


/**
 * SMC CONFIG.
 */

// native SMCParser:SMC_CreateParser();
static cell AMX_NATIVE_CALL SMC_CreateParser(AMX *amx, cell *params)
{
	return createParser();
}

// native SMC_SetParseStart(SMCParser:handle, const func[]);
static cell AMX_NATIVE_CALL SMC_SetParseStart(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid SMC parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)) && length)
	{
		p->parse_start = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->parse_start == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native SMC_SetParseEnd(SMCParser:handle, const func[]);
static cell AMX_NATIVE_CALL SMC_SetParseEnd(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid SMC parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)) && length)
	{
		p->parse_end = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->parse_end == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native SMC_SetReaders(SMCParser:smc, const kvFunc[], const nsFunc[] = "", const esFunc[] = "");
static cell AMX_NATIVE_CALL SMC_SetReaders(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid SMC parse handle (%d)", params[1]);
		return 0;
	}

	int kvLength = 0, nsLength = 0, esLength = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, kvLength)) && kvLength)
	{
		p->key_value = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_STRING, FP_CELL, FP_DONE);
	}

	if (kvLength && (funcName = get_amxstring(amx, params[3], 1, nsLength)) && nsLength)
	{
		p->new_section = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_CELL, FP_DONE);
	}

	if (kvLength && (funcName = get_amxstring(amx, params[4], 2, esLength)) && esLength)
	{
		p->end_section = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->key_value == -1 || (nsLength && p->new_section == -1) || (esLength && p->end_section == -1))
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native SMC_SetRawLine(SMCParser:handle, const func[]);
static cell AMX_NATIVE_CALL SMC_SetRawLine(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid SMC parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)) && length)
	{
		p->raw_line = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->raw_line == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native SMCError:SMC_ParseFile(SMCParser:handle, const file[], &line = 0, &col = 0, any:data = 0);
static cell AMX_NATIVE_CALL SMC_ParseFile(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid SMC parse handle (%d)", params[1]);
		return 0;
	}

	if (*params / sizeof(cell) >= 5)
	{
		p->data = params[5];
	}

	int length;
	const char *file = build_pathname("%s", get_amxstring(amx, params[2], 0, length));

	SMCStates states;
	SMCError p_err = textparsers->ParseFile_SMC(file, p, &states);

	*get_amxaddr(amx, params[3]) = states.line;
	*get_amxaddr(amx, params[4]) = states.col;

	return static_cast<cell>(p_err);
}

// native bool:SMC_GetErrorString(SMCError:error, buffer[], buf_max);
static cell AMX_NATIVE_CALL SMC_GetErrorString(AMX *amx, cell *params)
{
	const char *str = textparsers->GetSMCErrorString((SMCError)params[1]);

	if (!str)
	{
		return 0;
	}

	return set_amxstring(amx, params[2], str, params[3]);
}

// native SMC_DestroyParser(&SMCParser:handle);
static cell AMX_NATIVE_CALL SMC_DestroyParser(AMX *amx, cell *params)
{
	return destroyParser(get_amxaddr(amx, params[1]));
}


/**
 * INI CONFIG
 */

// native INIParser:INI_CreateParser();
static cell AMX_NATIVE_CALL INI_CreateParser(AMX *amx, cell *params)
{
	return createParser();
}

// native bool:INI_ParseFile(INIParser:handle, const file[], &line = 0, &col = 0, any:data = 0);
static cell AMX_NATIVE_CALL INI_ParseFile(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid INI parse handle (%d)", params[1]);
		return 0;
	}

	int length;
	const char *file = build_pathname("%s", get_amxstring(amx, params[2], 0, length));

	if (*params / sizeof(cell) >= 5)
	{
		p->data = params[5];
	}

	unsigned int line, col;
	bool result = textparsers->ParseFile_INI(file, p, &line, &col);

	*get_amxaddr(amx, params[3]) = line;
	*get_amxaddr(amx, params[4]) = col;

	return result;
}

// native INI_SetParseStart(INIParser:handle, const func[]);
static cell AMX_NATIVE_CALL INI_SetParseStart(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid INI parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)) && length)
	{
		p->parse_start = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->parse_start == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native INI_SetParseEnd(INIParser:handle, const func[]);
static cell AMX_NATIVE_CALL INI_SetParseEnd(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid INI parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)))
	{
		p->parse_end = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->parse_end == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native INI_SetReaders(INIParser:smc, const kvFunc[], const nsFunc[] = "" );
static cell AMX_NATIVE_CALL INI_SetReaders(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid INI parse handle (%d)", params[1]);
		return 0;
	}

	int kvLength = 0, nsLength = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, kvLength)) && kvLength)
	{
		p->key_value = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_STRING, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	}

	if (kvLength && (funcName = get_amxstring(amx, params[3], 1, nsLength)) && nsLength)
	{
		p->new_section = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->key_value == -1 || (nsLength && p->new_section == -1))
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native INI_SetRawLine(INIParser:handle, const func[]);
static cell AMX_NATIVE_CALL INI_SetRawLine(AMX *amx, cell *params)
{
	ParseInfo *p = TextParsersHandles.lookup(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid INI parse handle (%d)", params[1]);
		return 0;
	}

	int length = 0;
	const char *funcName = nullptr;

	if ((funcName = get_amxstring(amx, params[2], 0, length)) && length)
	{
		p->raw_line = registerSPForwardByName(amx, funcName, FP_CELL, FP_STRING, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	}

	if (p->raw_line == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", funcName, g_plugins.findPluginFast(amx)->getName());
		return 0;
	}

	return 1;
}

// native INI_DestroyParser(&INIParser:handle);
static cell AMX_NATIVE_CALL INI_DestroyParser(AMX *amx, cell *params)
{
	return destroyParser(get_amxaddr(amx, params[1]));
}


AMX_NATIVE_INFO g_TextParserNatives[] =
{
	{ "SMC_CreateParser"  , SMC_CreateParser   },
	{ "SMC_ParseFile"     , SMC_ParseFile      },
	{ "SMC_GetErrorString", SMC_GetErrorString },
	{ "SMC_SetParseStart" , SMC_SetParseStart  },
	{ "SMC_SetParseEnd"   , SMC_SetParseEnd    },
	{ "SMC_SetReaders"    , SMC_SetReaders     },
	{ "SMC_SetRawLine"    , SMC_SetRawLine     },
	{ "SMC_DestroyParser" , SMC_DestroyParser  },

	{ "INI_CreateParser"  , INI_CreateParser   },
	{ "INI_ParseFile"     , INI_ParseFile      },
	{ "INI_SetParseStart" , INI_SetParseStart  },
	{ "INI_SetParseEnd"   , INI_SetParseEnd    },
	{ "INI_SetReaders"    , INI_SetReaders     },
	{ "INI_SetRawLine"    , INI_SetRawLine     },
	{ "INI_DestroyParser" , INI_DestroyParser  },

	{ nullptr             , nullptr },
};
