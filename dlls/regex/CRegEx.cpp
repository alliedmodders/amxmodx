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
#include "pcre.h"
#include "CRegEx.h"
#include <string.h>
#include "amxxmodule.h"

RegEx::RegEx()
{
	mErrorOffset = 0;
	mError = NULL;
	re = NULL;
	mFree = true;
	subject = NULL;
	mSubStrings = 0;
}

void RegEx::Clear()
{
	mErrorOffset = 0;
	mError = NULL;
	if (re)
		pcre_free(re);
	re = NULL;
	mFree = true;
	if (subject)
		delete [] subject;
	subject = NULL;
	mSubStrings = 0;
}

RegEx::~RegEx()
{
	Clear();
}

bool RegEx::isFree(bool set, bool val)
{
	if (set)
	{
		mFree = val;
		return true;
	} else {
		return mFree;
	}
}

int RegEx::Compile(const char *pattern, const char* flags)
{
	if (!mFree)
		Clear();

		
	int iFlags = 0;
	
	if (flags != NULL)
	{
		for ( ; *flags != 0; flags++)
		{
			switch (*flags)
			{
				case 'i':
				{
					iFlags |= PCRE_CASELESS;
					break;
				}
				case 'm':
				{
					iFlags |= PCRE_MULTILINE;
					break;
				}
				case 's':
				{
					iFlags |= PCRE_DOTALL;
					break;
				}
				case 'x':
				{
					iFlags |= PCRE_EXTENDED;
					break;
				}
				default:
				{
					break;
				}
			}
		}
	}
		
	re = pcre_compile(pattern, iFlags, &mError, &mErrorOffset, NULL);

	if (re == NULL)
	{
		return 0;
	}

	mFree = false;

	return 1;
}

int RegEx::Compile(const char *pattern, int iFlags)
{
	if (!mFree)
		Clear();

	re = pcre_compile(pattern, iFlags, &mError, &mErrorOffset, NULL);

	if (re == NULL)
	{
		return 0;
	}

	mFree = false;

	return 1;
}

int RegEx::Match(const char *str)
{
	int rc = 0;

	if (mFree || re == NULL)
		return -1;
		
	this->ClearMatch();

	//save str
	subject = new char[strlen(str)+1];
	strcpy(subject, str);

	rc = pcre_exec(re, NULL, subject, (int)strlen(subject), 0, 0, ovector, 30);

	if (rc < 0)
	{
		if (rc == PCRE_ERROR_NOMATCH)
		{
			return 0;
		} else {
			mErrorOffset = rc;
			return -1;
		}
	}

	mSubStrings = rc;

	return 1;
}
void RegEx::ClearMatch()
{
	// Clears match results
	mErrorOffset = 0;
	mError = NULL;
	if (subject)
		delete [] subject;
	subject = NULL;
	mSubStrings = 0;
}

const char *RegEx::GetSubstring(int s, char buffer[], int max)
{
	int i = 0;
	if (s >= mSubStrings || s < 0)
		return NULL;

	char *substr_a = subject + ovector[2*s];
	int substr_l = ovector[2*s+1] - ovector[2*s];

	for (i = 0; i<substr_l; i++)
	{
		if (i >= max)
			break;
		buffer[i] = substr_a[i];
	}

	buffer[i] = '\0';

	return buffer;
}

