/* AMX Assembler
 * Copyright (C)2004 David "BAILOPAN" Anderson
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#include "amxasm.h"

char isletter(char c)
{
	if (c >= 65 && c <= 90)
		return c;
	if (c >= 97 && c <= 122)
		return c;
	if (c == '_')
		return c;
	return 0;
}

char expr(char c)
{
	if (c == '(')
		return ')';
	if (c == ')')
		return '(';
	return 0;
}

char literal(char c)
{
	if (c == '"')
		return '"';
	if (c == '\'')
		return '\'';
	return 0;
}

void StripComments(std::string &text)
{
	size_t i = 0;
	char c = 0, l = 0;

	for (i=0; i<text.size(); i++)
	{
		c = text[i];

		if (literal(c))
		{
			if (!l)
			{
				l = c;
			} else {
				l = 0;
			}
		} else {
			if (!l)
			{
				if (c == ';')
				{
					text.erase(i, text.size()-i);
					return;
				}
			}
		}
	}
}

void StringBreak(std::string &Source, std::string &Left, std::string &Right)
{
	int done_flag = 0;
	int l=0;
	unsigned int i=0;

	Left.clear();
	Right.clear();

	for (i=0; i<Source.size(); i++)
	{
		if (isspace(Source[i]) && !done_flag)
		{
			while (isspace(Source[++i]));
			done_flag = 1;
		}
		if (!done_flag)
		{
			Left.push_back(Source[i]);
		} else {
			Right.push_back(Source[i]);
		}
	}
}

/* Strips whitespace from the beginning and end of a string */
void Strip(std::string &text)
{
	int i = 0;

	if (text.size() == 1)
	{
		if (isspace(text[0]))
		{
			text.clear();
			return;
		}
	}

	for (i=0; i<(int)text.size(); i++)
	{
		if (!isspace(text[i]))
		{
			if (i!=0)
			{
				text.erase(0, i);
			}
			break;
		}
	}

	if (text.size() < 1)
		return;

	for (i=(int)(text.size()-1); i>=0; i--)
	{
		if (!isspace(text[i]))
		{
			if (i != (int)(text.size() - 1))
			{
				text.erase(i+1, text.size()-1-i);
			}
			break;
		}
	}
}

/* This is a very simple symbol searcher
 * It only restricts the pattern location to outside of 
 *  string literals and other symbols.
 */
int FindSymbol(std::string &text, const std::string &sym, int startPos = 0)
{
	unsigned int i = 0;
	char c = 0, d = 0, l = 0;
	
	for (i=startPos; i<text.size(); i++)
	{
		d = text[i];
		/* If the string can't possibly fit, opt out */
		if (sym.size() > text.size() - i)
			break;

		/* Skip literal strings */
		if (l)
		{
			if (d == l)
				l = 0;
			c = d;
			continue;
		} else {
			l = literal(d);
			if (l)
			{
				c = d;
				continue;
			}
		}

		/* If the last character was a letter, we're in a symbol already */
		if (isletter(c))
		{
			c = d;
			continue;
		}

		/* If the current character is a letter, test for a symbol */
		if (text.compare(i, sym.size(), sym) == 0)
			return i;
        		
		c = d;
	}
	
	return -1;
}

