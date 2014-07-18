/* AMX Mod X
 *   Regular Expressions Module
 *
 * by the AMX Mod X Development Team
 *
 * This file is part of AMX Mod X.
 *
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#ifndef _INCLUDE_CREGEX_H
#define _INCLUDE_CREGEX_H
 
#include <am-vector.h>
#include <am-string.h>

/**
 * Maximum number of sub-patterns, here 50 (this should be a multiple of 3).
 */
#define REGEX_MAX_SUBPATTERNS 150

/**
 * Flags to used with regex_replace, to control the replacement behavior.
 */
#define REGEX_FORMAT_DEFAULT   0  // Uses the standard formatting rules to replace matches.
#define REGEX_FORMAT_NOCOPY    1  // The sections that do not match the regular expression are not copied when replacing matches.
#define REGEX_FORMAT_FIRSTONLY 2  // Only the first occurrence of a regular expression is replaced.

class RegEx
{
public:
	struct RegExSub {
		int start, end;
	};

	struct NamedGroup {
		ke::AString name;
		size_t index;
	};

	RegEx();
	~RegEx();

	bool isFree(bool set=false, bool val=false);
	void Clear();

	int Compile(const char *pattern, const char* flags = NULL);
	int Compile(const char *pattern, int iFlags);
	int Match(const char *str);
	int MatchAll(const char *str);
	int Replace(char *text, size_t text_maxlen, const char *replace, size_t replaceLen, int flags = 0);
	void ClearMatch();
	const char *GetSubstring(size_t start, char buffer[], size_t max, size_t *outlen = NULL);
	void MakeSubpatternsTable(int numSubpatterns);

public:
	int mErrorOffset;
	const char *mError;
	int Count() { return mSubStrings.length(); }

private:
	pcre *re;
	bool mFree;
	int ovector[REGEX_MAX_SUBPATTERNS];
	char *subject;
	ke::Vector<RegExSub> mSubStrings;
	ke::Vector<size_t> mMatchesSubs;
	ke::Vector<NamedGroup> mSubsNameTable;
	int mNumSubpatterns;
};

#endif //_INCLUDE_CREGEX_H

