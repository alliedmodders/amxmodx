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

#ifndef _INCLUDE_SOURCEMOD_TEXTPARSERS_H_
#define _INCLUDE_SOURCEMOD_TEXTPARSERS_H_

#include <ITextParsers.h>
#include <am-vector.h>

using namespace SourceMod;

/**
* @param void *			IN: Stream pointer
* @param char *			IN/OUT: Stream buffer
* @param size_t			IN: Maximum size of buffer
* @param unsigned int *	OUT: Number of bytes read (0 = end of stream)
* @return					True on success, false on failure
*/
typedef bool(*STREAMREADER)(void *, char *, size_t, unsigned int *);

class TextParsers :	public ITextParsers
{
public:
	TextParsers();
public:
	bool ParseFile_INI(const char *file,
		ITextListener_INI *ini_listener,
		unsigned int *line,
		unsigned int *col);

	SMCError ParseFile_SMC(const char *file,
		ITextListener_SMC *smc_listener,
		SMCStates *states);

	SMCError ParseSMCFile(const char *file,
		ITextListener_SMC *smc_listener,
		SMCStates *states,
		char *buffer,
		size_t maxsize);

	SMCError ParseSMCStream(const char *stream,
		size_t length,
		ITextListener_SMC *smc_listener,
		SMCStates *states,
		char *buffer,
		size_t maxsize);

	unsigned int GetUTF8CharBytes(const char *stream);

	const char *GetSMCErrorString(SMCError err);
	bool IsWhitespace(const char *stream);
private:
	SMCError ParseStream_SMC(void *stream,
		STREAMREADER srdr,
		ITextListener_SMC *smc,
		SMCStates *states);
};

extern TextParsers g_TextParser;

#endif //_INCLUDE_SOURCEMOD_TEXTPARSERS_H_

