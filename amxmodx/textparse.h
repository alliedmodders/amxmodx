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

#ifndef _INCLUDE_TEXTPARSE_H_
#define _INCLUDE_TEXTPARSE_H_

#include "amxmodx.h"
#include "CTextParsers.h"
#include "natives_handles.h"

class ParseInfo :
	public ITextListener_SMC,
	public ITextListener_INI
{
public:
	ParseInfo()
	{
		parse_start = -1;
		parse_end   = -1;
		new_section = -1;
		key_value   = -1;
		end_section = -1;
		raw_line    = -1;
		handle      = -1;
		data        = 0;
	}

public:

	/**
	 * SMC CONFIG.
	 */

	void ReadSMC_ParseStart()
	{
		if (parse_start != -1)	
			executeForwards(parse_start, handle, data);
	}

	void ReadSMC_ParseEnd(bool halted, bool failed)
	{
		if (parse_end != -1)	
			executeForwards(parse_end, handle, halted ? 1 : 0, failed ? 1 : 0, data);
	}

	SMCResult ReadSMC_NewSection(const SMCStates *states, const char *name)
	{
		if (new_section != -1)
			return (SMCResult)executeForwards(new_section, handle, name, data);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value)
	{
		if (key_value != -1)
			return (SMCResult)executeForwards(key_value, handle, key, value, data);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_LeavingSection(const SMCStates *states)
	{
		if (end_section != -1)
			return (SMCResult)executeForwards(end_section, handle, data);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_RawLine(const SMCStates *states, const char *line)
	{
		if (raw_line != -1)
			return (SMCResult)executeForwards(raw_line, handle, line, states->line, data);

		return SMCResult_Continue;
	}


	/**
	 * INI CONFIG.
	 */

	void ReadINI_ParseStart()
	{
		if (parse_start != -1)
			executeForwards(parse_start, handle, data);
	}

	void ReadINI_ParseEnd(bool halted)
	{
		if (parse_end != -1)
			executeForwards(parse_end, handle, halted ? 1 : 0, data);
	}

	bool ReadINI_NewSection(const char *section, bool invalid_tokens, bool close_bracket, bool extra_tokens, unsigned int *curtok)
	{
		if (new_section != -1)
			return executeForwards(new_section, handle, section, invalid_tokens, close_bracket, extra_tokens, *curtok, data) > 0 ? true : false;

		return true;
	}

	bool ReadINI_KeyValue(const char *key, const char *value, bool invalid_tokens, bool equal_token, bool quotes, unsigned int *curtok)
	{
		if (key_value != -1)
			return executeForwards(key_value, handle, key, value, invalid_tokens, equal_token, quotes, *curtok, data) > 0 ? true : false;

		return true;
	}

	bool ReadINI_RawLine(const char *line, unsigned int *curtok)
	{
		if (raw_line != -1)
			return executeForwards(raw_line, handle, line, *curtok, data) > 0 ? true : false;

		return true;
	}
public:
	int parse_start;
	int parse_end;
	int new_section;
	int key_value;
	int end_section;
	int raw_line;
	int handle;
	cell data;
};

extern NativeHandle<ParseInfo> TextParsersHandles;

#endif // _INCLUDE_TEXTPARSE_H_