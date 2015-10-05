// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "libraries.h"
#include "sh_list.h"

List<Library *> g_libraries;

bool AddLibrary(const char *name, LibType type, LibSource src, void *parent)
{
	if (FindLibrary(name, type))
		return false;

	Library *lib = new Library;

	lib->name = name;
	lib->type = type;
	lib->src = src;
	lib->parent = parent;

	g_libraries.push_back(lib);
	
	return true;
}

bool DecodeLibCmdString(const char *str, LibDecoder *dec)
{
	if (dec->buffer)
	{
		free(dec->buffer);
		dec->buffer = NULL;
	}
	if (str[0] != '?')
	{
		return false;
	} else {
		str++;
		if (*str == 'r')
		{
			str++;
			if (*str == 'c')
				dec->cmd = LibCmd_ReqClass;
			else if (*str == 'l')
				dec->cmd = LibCmd_ReqLib;
			else
				return false;
			str++;
		} else if (*str == 'f') {
			str++;
			dec->cmd = LibCmd_ForceLib;
		} else if (*str == 'e') {
			str++;
			if (*str == 'c')
				dec->cmd = LibCmd_ExpectClass;
			else if (*str == 'l')
				dec->cmd = LibCmd_ExpectLib;
			else
				return false;
			str++;
		} else if (*str == 'd') {
			str++;
			dec->cmd = LibCmd_DefaultLib;
		}
		if (*str != '_')
			return false;
		str++;
		if (dec->cmd < LibCmd_ExpectLib)
		{
			dec->buffer = strdup(str);
			dec->param1 = dec->buffer;
			dec->param2 = NULL;
		} else {
			dec->buffer = strdup(str);
			char *p = strchr(dec->buffer, '_');
			while (p && (*(p+1) == '_'))
				p = strchr(p+2, '_');
			if (!p || !*(p+1))
				return false;
			*p = '\0';
			dec->param1 = dec->buffer;
			dec->param2 = p+1;
		}
	}

	return true;
}

size_t AddLibrariesFromString(const char *name, LibType type, LibSource src, void *parent)
{
	char buffer[255];
	char *ptr, *p, s;
	size_t count = 0;

	ke::SafeSprintf(buffer, sizeof(buffer), "%s", name);

	ptr = buffer;
	p = buffer;
	while (*p)
	{
		while (*p && (*p != ','))
			p++;
		s = *p;
		*p = '\0';
		if (AddLibrary(ptr, type, src, parent))
			count++;
		if (!s)
			break;
		p++;
		while (*p && (*p == ','))
			p++;
		ptr = p;
	}

	return count;
}

size_t ClearLibraries(LibSource src)
{
	List<Library *>::iterator iter;
	size_t count = 0;

	iter = g_libraries.begin();
	while (iter != g_libraries.end())
	{
		if ( (*iter)->src == src )
		{
			delete (*iter);
			iter = g_libraries.erase(iter);
			count++;
		} else {
			iter++;
		}
	}

	return count;
}

size_t RemoveLibraries(void *parent)
{
	List<Library *>::iterator iter;
	Library *lib;
	size_t count = 0;

	iter = g_libraries.begin();
	while (iter != g_libraries.end())
	{
		lib = (*iter);
		if (lib->parent == parent)
		{
			delete (*iter);
			iter = g_libraries.erase(iter);
			count++;
		} else {
			iter++;
		}
	}

	return count;
}

bool FindLibrary(const char *name, LibType type)
{
	List<Library *>::iterator iter;
	Library *lib;

	for (iter = g_libraries.begin(); iter != g_libraries.end(); iter++)
	{
		lib = (*iter);
		if (lib->type != type)
			continue;
		if (strcasecmp(lib->name.chars(), name) == 0)
		{
			return true;
		}
	}

	return false;
}

LibError RunLibCommand(const LibDecoder *enc)
{
	List<Library *>::iterator iter,end;
	Library *lib;
	
	iter = g_libraries.begin();
	end = g_libraries.end();

	if ( (enc->cmd == LibCmd_ReqLib) || (enc->cmd == LibCmd_ReqClass) )
	{
		LibType expect = LibType_Library;

		if (enc->cmd == LibCmd_ReqLib)
			expect = LibType_Library;
		else if (enc->cmd == LibCmd_ReqClass)
			expect = LibType_Class;

		/** see if it exists */
		for (; iter != end; iter++)
		{
			lib = (*iter);
			if (lib->type != expect)
				continue;
			if (strcasecmp(lib->name.chars(), enc->param1) == 0)
				return LibErr_None;
		}
		if (expect == LibType_Library)
			return LibErr_NoLibrary;
		else if (expect == LibType_Class)
			return LibErr_NoClass;

		return LibErr_NoLibrary;
	} else if (enc->cmd == LibCmd_ForceLib) {
		if (!LoadModule(enc->param1, PT_ANYTIME, true, true))
		{
			return LibErr_NoLibrary;
		}
	} else if ( (enc->cmd == LibCmd_DefaultLib) || 
				((enc->cmd == LibCmd_ExpectLib) || (enc->cmd == LibCmd_ExpectClass)) )
	{
		LibType expect;

		if (enc->cmd == LibCmd_ExpectLib)
			expect = LibType_Library;
		else
			expect = LibType_Class;

		/** see if it exists */
		for (; iter != end; iter++)
		{
			lib = (*iter);
			if (lib->type != expect)
				continue;
			if (strcasecmp(lib->name.chars(), enc->param1) == 0)
				return LibErr_None;
		}

		if (!LoadModule(enc->param2, PT_ANYTIME, true, true))
		{
			return LibErr_NoLibrary;
		}

		return LibErr_None;
	}

	return LibErr_None;
}
