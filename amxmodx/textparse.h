/**
* vim: set ts=4 :
* =============================================================================
* SourceMod
* Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
* =============================================================================
*
* This program is free software; you can redistribute it and/or modify it under
* the terms of the GNU General Public License, version 3.0, as published by the
* Free Software Foundation.
*
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
* details.
*
* You should have received a copy of the GNU General Public License along with
* this program.  If not, see <http://www.gnu.org/licenses/>.
*
* As a special exception, AlliedModders LLC gives you permission to link the
* code of this program (as well as its derivative works) to "Half-Life 2," the
* "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
* by the Valve Corporation.  You must obey the GNU General Public License in
* all respects for all other code used.  Additionally, AlliedModders LLC grants
* this exception to all derivative works.  AlliedModders LLC defines further
* exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
* or <http://www.sourcemod.net/license.php>.
*
* Version: $Id$
*/

#ifndef _INCLUDE_TEXTPARSE_H_
#define _INCLUDE_TEXTPARSE_H_

#include "amxmodx.h"
#include "CTextParsers.h"

class ParseInfo :
	public ITextListener_SMC,
	public ITextListener_INI
{
public:
	ParseInfo()
	{
		parse_start = -1;
		parse_end = -1;
		new_section = -1;
		key_value = -1;
		end_section = -1;
		raw_line = -1;
		handle = -1;
		ini_format = false;
	}

public:
	void ReadSMC_ParseStart()
	{
		if (parse_start != -1)	
			executeForwards(parse_start, handle);
	}
	void ReadINI_ParseStart()
	{
		if (parse_start != -1)	
			executeForwards(parse_start, handle);
	}

	void ReadSMC_ParseEnd(bool halted, bool failed)
	{
		if (parse_end != -1)	
			executeForwards(parse_end, handle, halted ? 1 : 0, failed ? 1 : 0);
	}
	void ReadINI_ParseEnd(bool halted, bool failed)
	{
		if (parse_end != -1)	
			executeForwards(parse_end, handle, halted ? 1 : 0, failed ? 1 : 0);
	}

	SMCResult ReadSMC_NewSection(const SMCStates *states, const char *name)
	{
		if (new_section != -1)
			return (SMCResult)executeForwards(new_section, handle, name);

		return SMCResult_Continue;
	}
	SMCResult ReadINI_NewSection(const char *section, bool invalid_tokens, bool close_bracket, bool extra_tokens, unsigned int *curtok)
	{
		if (new_section != -1) 
			return (SMCResult)executeForwards(new_section, handle, section, invalid_tokens, close_bracket, extra_tokens, *curtok);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value)
	{
		if (key_value != -1)
			return (SMCResult)executeForwards(key_value, handle, key, value);

		return SMCResult_Continue;
	}
	SMCResult ReadINI_KeyValue(const char *key, const char *value, bool invalid_tokens, bool equal_token, bool quotes, unsigned int *curtok)
	{
		if (key_value != -1)
			return (SMCResult)executeForwards(key_value, handle, key, value, invalid_tokens, equal_token, quotes, *curtok);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_LeavingSection(const SMCStates *states)
	{
		if (end_section != -1)
			return (SMCResult)executeForwards(end_section, handle);

		return SMCResult_Continue;
	}

	SMCResult ReadSMC_RawLine(const SMCStates *states, const char *line)
	{
		if (raw_line != -1)
			return (SMCResult)executeForwards(raw_line, handle, line, states->line);

		return SMCResult_Continue;
	}
	SMCResult ReadINI_RawLine(const char *line, unsigned int lineno, unsigned int *curtok)
	{
		if (raw_line != -1)
			return (SMCResult)executeForwards(raw_line, handle, line, lineno, *curtok);

		return SMCResult_Continue;
	}
public:
	int parse_start;
	int parse_end;
	int new_section;
	int key_value;
	int end_section;
	int raw_line;
	int handle;
	bool ini_format;
};

template <typename T>
class TextParserHandles
{
private:
	ke::Vector<T *> m_textparsers;

public:
	TextParserHandles() { }
	~TextParserHandles()
	{
		this->clear();
	}

	void clear()
	{
		for (size_t i = 0; i < m_textparsers.length(); i++)
		{
			if (m_textparsers[i] != NULL)
			{
				delete m_textparsers[i];
			}
		}

		m_textparsers.clear();
	}
	T *lookup(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_textparsers.length()))
		{
			return NULL;
		}

		return m_textparsers[handle];
	}
	int create()
	{
		for (size_t i = 0; i < m_textparsers.length(); i++)
		{
			if (m_textparsers[i] == NULL)
			{
				// reuse handle
				m_textparsers[i] = new T;

				return static_cast<int>(i)+1;
			}
		}
		m_textparsers.append(new T);
		return m_textparsers.length();
	}
	bool destroy(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_textparsers.length()))
		{
			return false;
		}

		if (m_textparsers[handle] == NULL)
		{
			return false;
		}
		delete m_textparsers[handle];
		m_textparsers[handle] = NULL;

		return true;
	}
};

extern TextParserHandles<ParseInfo> g_TextParsersHandles;

#endif // _INCLUDE_TEXTPARSE_H_