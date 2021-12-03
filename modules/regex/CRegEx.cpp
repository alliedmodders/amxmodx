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

#include "amxxmodule.h"
#include "pcre.h"
#include "CRegEx.h"
#include <string.h>
#include <ctype.h>
#include "utils.h"

RegEx::RegEx()
{
	mErrorOffset = 0;
	mError = NULL;
	re = NULL;
	mFree = true;
	subject = NULL;
	mSubStrings.clear();
	mMatchesSubs.clear();
	mSubsNameTable.clear();
	mNumSubpatterns = 0;
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
		delete[] subject;
	subject = NULL;
	mSubStrings.clear();
	mMatchesSubs.clear();
	mSubsNameTable.clear();
	mNumSubpatterns = 0;
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

	/**
	 * Retrieve the number of captured groups
	 * including the full match.
	 */
	pcre_fullinfo(re, NULL, PCRE_INFO_CAPTURECOUNT, &mNumSubpatterns);
	++mNumSubpatterns;

	/**
	 * Build the table with the named groups,
	 * which contain an index and a name per group.
	 */
	MakeSubpatternsTable(mNumSubpatterns);

	return 1;
}

int RegEx::Match(const char *str)
{
	int rc = 0;

	if (mFree || re == NULL)
		return -1;

	ClearMatch();

	//save str
	subject = new char[strlen(str) + 1];
	strcpy(subject, str);

	rc = pcre_exec(re, NULL, subject, (int)strlen(subject), 0, 0, ovector, REGEX_MAX_SUBPATTERNS);

	if (rc < 0)
	{
		if (rc == PCRE_ERROR_NOMATCH)
		{
			return 0;
		}
		else {
			mErrorOffset = rc;
			return -1;
		}
	}

	RegExSub res;
	mSubStrings.ensure(rc);

	for (int s = 0; s < rc; ++s)
	{
		res.start = ovector[2 * s];
		res.end = ovector[2 * s + 1];
		mSubStrings.append(res);
	}

	return 1;
}

int RegEx::MatchAll(const char *str)
{
	int rr = 0;
	int rc = 0;
	int startOffset = 0;
	int exoptions = 0;
	int notEmpty = 0;
	int sizeOffsets = mNumSubpatterns * 3;
	int subjectLen = strlen(str);

	if (mFree || re == NULL)
	{
		return -1;
	}

	ClearMatch();
	
	subject = new char[subjectLen + 1];
	strcpy(subject, str);

	RegExSub sub;

	while (1)
	{
		rr = pcre_exec(re, NULL, subject, (int)subjectLen, startOffset, exoptions | notEmpty, ovector, REGEX_MAX_SUBPATTERNS);

		/**
		 * The string was already proved to be valid UTF-8
		 */
		exoptions |= PCRE_NO_UTF8_CHECK;

		/**
		 * Too many substrings
		 */
		if (rr == 0)
		{
			rr = sizeOffsets / 3;
		}

		if (rr > 0)
		{
			mMatchesSubs.append(rr);

			for (int s = 0; s < rr; ++s)
			{
				sub.start = ovector[2 * s];
				sub.end = ovector[2 * s + 1];

				mSubStrings.append(sub);
			}
		}
		else if (rr == PCRE_ERROR_NOMATCH)
		{
			/**
			 * If we previously set PCRE_NOTEMPTY after a null match,
			 * this is not necessarily the end. We need to advance
			 * the start offset, and continue. Fudge the offset values
			 * to achieve this, unless we're already at the end of the string. 
			 */
			if (notEmpty && startOffset < (int)subjectLen) 
			{
				ovector[0] = startOffset;
				ovector[1] = startOffset + 1;
			}
			else
			{
				break;
			}
		}
		else
		{
			mErrorOffset = rr;

			if (mMatchesSubs.length())
			{
				ClearMatch();
			}

			return -1;
		}

		/**
		 * If we have matched an empty string, mimic what Perl's /g options does.
		 * This turns out to be rather cunning. First we set PCRE_NOTEMPTY and try
		 * the match again at the same point. If this fails (picked up above) we
		 * advance to the next character. 
		 */
		notEmpty = (ovector[1] == ovector[0]) ? PCRE_NOTEMPTY | PCRE_ANCHORED : 0;

		/** 
		 * Advance to the next piece. 
		 */
		startOffset = ovector[1];
	}

	if (!mMatchesSubs.length())
	{
		return 0;
	}

	return 1;
}

void RegEx::ClearMatch()
{
	// Clears match results
	mErrorOffset = 0;
	mError = NULL;
	if (subject)
		delete[] subject;
	subject = NULL;
	mSubStrings.clear();
	mMatchesSubs.clear();
}

const char *getSubstring(char *subject, size_t start, size_t end, char buffer[], size_t max, size_t *outlen)
{
	size_t i;
	char * substr_a = subject + start;
	size_t substr_l = end - start;

	for (i = 0; i < substr_l; i++)
	{
		if (i >= max)
			break;
		buffer[i] = substr_a[i];
	}

	buffer[i] = '\0';

	if (outlen)
	{
		*outlen = i;
	}

	return buffer;
}

const char *RegEx::GetSubstring(size_t start, char buffer[], size_t max, size_t *outlen)
{
	if (start >= mSubStrings.length())
	{
		return NULL;
	}

	RegExSub sub = mSubStrings.at(start);

	return getSubstring(subject, sub.start, sub.end, buffer, max, outlen);
}

void RegEx::MakeSubpatternsTable(int numSubpatterns)
{
	int nameCount = 0;
	int rc = pcre_fullinfo(re, NULL, PCRE_INFO_NAMECOUNT, &nameCount);
	
	if (rc < 0) 
	{
		return;
	}

	if (nameCount > 0) 
	{
		const char *nameTable;
		int nameSize = 0;
		int i = 0;

		int rc1 = pcre_fullinfo(re, NULL, PCRE_INFO_NAMETABLE, &nameTable);
		int rc2 = pcre_fullinfo(re, NULL, PCRE_INFO_NAMEENTRYSIZE, &nameSize);

		rc = rc2 ? rc2 : rc1;

		if (rc < 0)
		{
			mSubsNameTable.clear();
			return;
		}

		NamedGroup data;

		while (i++ < nameCount) 
		{
			data.index = 0xff * (unsigned char)nameTable[0] + (unsigned char)nameTable[1];
			data.name = nameTable + 2;

			mSubsNameTable.append(ke::Move(data));
			nameTable += nameSize;
		}
	}
}

int RegEx::Replace(char *text, size_t textMaxLen, const char *replace, size_t replaceLen, int flags)
{
	char *output = text;

	/**
	 * Retrieve all matches and store them in 
	 * mSubStrings list.
	 */
	if (MatchAll(output) == -1)
	{
		return -1;
	}

	size_t subjectLen = strlen(subject);
	size_t total = 0;
	size_t baseIndex = 0;
	size_t diffLength = 0;

	char *toReplace = new char[textMaxLen + 1];
	char *toSearch = NULL;

	/**
	 * All characters which is not matched are not copied when replacing matches.
	 * Then original text (output buffer) should be considerated as empty.
	 */
	if (flags & REGEX_FORMAT_NOCOPY)
	{
		*output = '\0';
	}
	else
	{
		/**
		 * This is used only when we do replace matches.
		 */
		toSearch  = new char[textMaxLen + 1];
	}

	/** 
	 * Loop over all matches found.
	 */
	for (size_t i = 0; i < mMatchesSubs.length(); ++i)
	{
		char *ptr = toReplace;

		size_t browsed = 0;
		size_t searchLen = 0;
		size_t length = 0;
	
		/**
		 * Build the replace string as it can contain backreference
		 * and this needs to be parsed.
		 */
		for (const char *s = replace, *end = s + replaceLen; s < end && browsed <= textMaxLen; ++s, ++browsed)
		{
			unsigned int c = *s;

			/**
			 * Supported format specifiers:
			 *
			 *   $number  : Substitutes the substring matched by group number.
			 *              n must be an integer value designating a valid backreference, greater than 0, and of two digits at most.
			 *   ${name}  : Substitutes the substring matched by the named group name (a maximum of 32 characters).
			 *   $&       : Substitutes a copy of the whole match.
			 *   $`       : Substitutes all the text of the input string before the match.
			 *   $'       : Substitutes all the text of the input string after the match.
			 *   $+       : Substitutes the last group that was captured.
			 *   $_       : Substitutes the entire input string.
			 *   $$       : Substitutes a literal "$".
			 */
			if (c == '$' || c == '\\')
			{
				switch (*++s)
				{
					case '\0':
					{
						/**
						 * End of string.
						 * Copy one character.
						 */
						 *(ptr + browsed) = c;
						 break;
					}
					case '&':
					{
						/**
						 * Concatenate retrieved full match sub-string.
						 * length - 1 to overwrite EOS.
						 */
						GetSubstring(baseIndex, ptr + browsed, textMaxLen, &length);
						browsed += length - 1;
						break;
					}
					case '`':
					{
						/**
						 * Concatenate part of original text up to
						 * first sub-string position.
						 */
						length = mSubStrings.at(baseIndex).start;
						memcpy(ptr + browsed, subject, length);
						browsed += length - 1;
						break;
					}
					case '\'':
					{
						/**
						 * Concatenate part of original text from
						 * last sub-string end position to EOS.
						 */
						length = mSubStrings.at(baseIndex).end;
						memcpy(ptr + browsed, subject + length, subjectLen - length);
						browsed += (subjectLen - length) - 1;
						break;
					}
					case '+':
					{
						/**
						 * Copy the last group that was captured.
						 */
						GetSubstring(baseIndex + mMatchesSubs.at(i) - 1, ptr + browsed, textMaxLen, &length);
						browsed += length - 1;
						break;
					}
					case '_':
					{
						/**
						 * Copy the entire input string.
						 */
						memcpy(ptr + browsed, subject, subjectLen);
						browsed += (subjectLen - 1);
						break;
					}
					case '$':
					case '\\':
					{
						/**
						 * Copy the single character $ or \.
						 */
						*(ptr + browsed) = c;
						break;
					}
					case '0': case '1':	case '2': case '3':	case '4': 
					case '5': case '6': case '7': case '8': case '9':
					case '{':
					{
						/**
						 * Checking backreference.
						 * Which can be either $n, ${n} or ${name}.
						 */
						int backref = -1;
						const char *walk = s;
						bool inBrace = false;
						bool nameCheck = false;

						/**
						 * ${nn}.
						 *  ^
						 */
						if (*walk == '{') 
						{
							inBrace = true;
							++walk;
						}

						/**
						 * Valid number.
						 * $nn or ${nn}
						 *  ^       ^
						 */
						if (*walk >= '0' && *walk <= '9')
						{
							backref = *walk - '0';
							++walk;
						}
						else if (inBrace)
						{
							nameCheck = true;

							/**
							 * Not a valid number.
							 * Checking as string.
							 * ${name}
							 *   ^
							 */
							if (*walk)
							{
								const char *pch = strchr(walk, '}');

								if (pch != NULL)
								{
									/**
									 * A named group maximum character is 32 (PCRE).
									 */
									char name[32];
									size_t nameLength = strncopy(name, walk, pch - walk + 1);

									int flags, num = 0;
									pcre_fullinfo(re, NULL, PCRE_INFO_OPTIONS, &flags);

									/**
									 * If PCRE_DUPNAMES is set, the pcre_copy_named_substring function should be used
									 * as pcre_get_stringnumber output order is not defined.
									 */
									if (flags & PCRE_DUPNAMES)
									{
										memset(ovector, 0, REGEX_MAX_SUBPATTERNS * sizeof(int));

										/**
										 * pcre_copy_named_substring needs a vector containing sub-patterns ranges
										 * for a given match.
										 */
										for (size_t j = 0; j < mMatchesSubs.at(i); ++j)
										{
											ovector[2 * j] = mSubStrings.at(baseIndex + j).start;
											ovector[2 * j + 1] = mSubStrings.at(baseIndex + j).end;
										}

										num = pcre_copy_named_substring(re, subject, ovector, mMatchesSubs.at(i), name, ptr + browsed, (int)textMaxLen);

										if (num != PCRE_ERROR_NOSUBSTRING)
										{
											browsed += num - 1;
											s = pch;
											break;
										}
										++pch;
									}
									else
									{
										/**
										 * Retrieve sub-pattern index from a give name.
										 */
										num = pcre_get_stringnumber(re, name);
										if (num != PCRE_ERROR_NOSUBSTRING)
										{
											backref = num;
											walk = ++pch;
										}
									}

									if (num == PCRE_ERROR_NOSUBSTRING || num >= (int)mMatchesSubs.at(i))
									{
										/**
										 * If a sub-string for a given match is not found,  or if > to
										 * number of sub-patterns we still need to check if this 
										 * group name is a valid one because if so we want to escape it. 
										 * Looking at the name table.
										 */
										bool found = false;
										for (size_t i = 0; i < mSubsNameTable.length(); ++i)
										{
											if (!mSubsNameTable.at(i).name.compare(name))
											{
												--browsed;
												s = --pch;
												found = true;
												break;
											}
										}

										if (found)
										{
											continue;
										}
									}
								}
							}
						}

						if (!nameCheck)
						{
							/**
							 * Valid second number.
							 * $nn or ${nn}
							 *   ^       ^
							 */
							if (*walk >= '0' && *walk <= '9')
							{
								backref = backref * 10 + *walk - '0';
								++walk;
							}

							if (inBrace)
							{
								/**
								 * Invalid specifier
								 * Either hit EOS or missing }.
								 * ${n  or ${nn  or ${nx or ${nnx
								 *    ^        ^       ^        ^
								 */
								if (*walk == '\0' || *walk != '}')
								{
									backref = -1;
								}
								else
								{
									++walk;
								}
							}
						}

						length = walk - s;
						s = --walk;

						/**
						 * We can't provide a capture number >= to total that pcre_exec has found.
						 * 0 is implicitly accepted, same behavior as $&.
						 */
						if (backref >= 0 && backref < mNumSubpatterns)
						{
							/**
							 * Valid available index for a given match.
							 */
							if ((size_t)backref < mMatchesSubs.at(i))
							{
								/**
								 * Concatenate retrieved sub-string.
								 * length - 1 to overwrite EOS.
								 */
								GetSubstring(baseIndex + backref, ptr + browsed, textMaxLen, &length);
								browsed += length - 1;
							}
							else
							{
								/**
								 * Valid unavailable index for a given match.
								 */
								--browsed;
							}
						}
						else
						{
							/**
							 * If we here it means the syntax is valid but sub-pattern doesn't exist. 
							 * So, copy as it is, including $.
							 */
							memcpy(ptr + browsed, s - length, length + 1);
							browsed += length;
						}

						break;
					}
					default:
					{
						/**
						 * Not a valid format modifier.
						 * So we copy characters as it is.
						 */
						*(ptr + browsed) = *s;
						break;
					}
				}
			}
			else
			{
				/**
				 * At this point, direct copy.
				 */
				*(ptr + browsed) = c;
			}
		}

		*(ptr + browsed) = '\0';

		/**
		 * Concatenate only replace string of each match, 
		 * as we don't want to copy unmatched characters.
		 */
		if (flags & REGEX_FORMAT_NOCOPY)
		{
			/**
			 * We want just the first occurrence.
			 */
			if (total++ && (flags & REGEX_FORMAT_FIRSTONLY))
			{
				break;
			}

			strncat(output, toReplace, textMaxLen + 1);
		}
		else
		{
			/**
			 * Retrieves full string of a given match.
			 */
			const char *search = GetSubstring(baseIndex, toSearch, textMaxLen, &searchLen);

			/**
			 * We get something to replace, but the sub-pattern to search is empty.
			 * We insert replacement either a the start end or string.
			 */
			if (*toReplace && !searchLen)
			{
				if (output - text > 0)
				{
					strncat(output, toReplace, textMaxLen);
				}
				else
				{
					strncat(toReplace, text, textMaxLen);
					strncopy(text, toReplace, strlen(toReplace) + 1);
				}

				++total;
			}
			else if ((output = UTIL_ReplaceEx(text + mSubStrings.at(baseIndex).start + diffLength, textMaxLen, search, searchLen, toReplace, browsed, false)) != NULL)
			{
				/**
				 * Then we simply do a replace.
				 * Probably not the most efficient, but this should be at least safe.
				 * To avoid issue where the function could find a string which is not at the expected index,
				 * We force the input string to start from index of the full match.
				 */
				++total;
			}

			if (total && (flags & REGEX_FORMAT_FIRSTONLY))
			{
				break;
			}
		}

		/**
		 * mMatchesSubs is a flat list containing all sub-patterns of all matches.
		 * A number of sub-patterns can vary per match. So we calculate the position in the list, 
		 * from where the first sub-pattern result of current match starts.
		 */
		baseIndex  += mMatchesSubs.at(i);
		diffLength += browsed - searchLen;
	}

	delete[] toReplace;
	
	if (toSearch != NULL)
	{
		delete[] toSearch;
	}

	/**
	 * Return the number of successful replacements.
	 */
	return total;
}
