// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Regular Expressions Module
//

#ifndef _INCLUDE_CREGEX_H
#define _INCLUDE_CREGEX_H
 
#include <amtl/am-vector.h>
#include <amtl/am-string.h>

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

