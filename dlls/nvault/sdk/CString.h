/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#ifndef _INCLUDE_CSTRING_H
#define _INCLUDE_CSTRING_H

#include <stdio.h>
#include <string.h>

//by David "BAILOPAN" Anderson
class String
{
public:
	String() 
	{
		v = NULL;
		mSize = 0;
		cSize = 0;
		Grow(2);
		assign("");
	}

	~String()
	{ 
		if (v) 
			delete [] v; 
	}

	String(const char *src) 
	{
		v = NULL; 
		mSize = 0; 
		cSize = 0; assign(src); 
	}

	String(String &src) 
	{
		v = NULL;
		mSize = 0;
		cSize = 0;
		assign(src.c_str()); 
	}

	const char *c_str() { return v?v:""; }
	const char *c_str() const { return v?v:""; }

	void append(const char *t)
	{
		Grow(cSize + strlen(t) + 1);
		strcat(v, t);
		cSize = strlen(v);
	}

	void append(const char c)
	{
		Grow(cSize + 2);
		v[cSize] = c;
		v[++cSize] = 0;
	}

	void append(String &d)
	{
		const char *t = d.c_str();
		Grow(cSize + strlen(t));
		strcat(v, t);
		cSize = strlen(v);
	}

	void assign(const String &src)
	{
		assign(src.c_str());
	}

	void assign(const char *d)
	{
		if (!d)
		{
			Grow(1);
			cSize = 0;
			strcpy(v, "");
			return;
		}
		Grow(strlen(d));
		if (v)
		{
			strcpy(v, d);
			cSize = strlen(v);
		} else {
			cSize = 0;
		}
	}

	void clear()
	{
		if (v)
		{
			v[0] = 0;
			cSize = 0;
		}
	}

	int compare (const char *d)
	{
		if (v) {
			if (d) {
				return strcmp(v, d);
			} else {
				return strlen(v);
			}
		} else {
			if (d) {
				return strlen(d);
			} else {
				return 0;
			}
		}
	}

	//Added this for amxx inclusion
	bool empty()
	{
		if (!v || !cSize)
			return true;

		return false;
	}

	int size()
	{
		if (!v)
			return 0;
		return cSize;
	}

	const char * _fread(FILE *fp)
	{
		Grow(512);
		char * ret = fgets(v, 511, fp);
		cSize = strlen(v);
		return ret;
	}

	int find(const char c, int index = 0)
	{
		if (!v)
			return npos;
		if (index >= (int)cSize || index < 0)
			return npos;
		unsigned int i = 0;
		for (i=index; i<cSize; i++)
		{
			if (v[i] == c)
			{
				return i;
			}
		}

		return npos;
	}

	bool is_space(int c)
	{
		if (c == '\f' || c == '\n' ||
			c == '\t' || c == '\r' ||
			c == '\v' || c == ' ')
		{
			return true;
		}

		return false;
	}
	
	void trim()
	{
		if (!v)
			return;
		unsigned int i = 0;
		unsigned int j = 0;

		if (cSize == 1)
		{
			if (is_space(v[i]))
			{
				clear();
				return;
			} 
		}

		unsigned char c0 = v[0];

		if (is_space(c0))
		{
			for (i=0; i<cSize; i++)
			{
				if (!is_space(v[i]) || (is_space(v[i]) && ((unsigned char)i==cSize-1)))
				{
					erase(0, i);
					break;
				}
			}
		}

		cSize = strlen(v);

		if (cSize < 1)
		{
			return;
		}

		if (is_space(v[cSize-1]))
		{
			for (i=cSize-1; i>=0; i--)
			{
				if (!is_space(v[i])
					|| (is_space(v[i]) && i==0))
				{
					erase(i+1, j);
					break;
				}
				j++;
			}
		}

		if (cSize == 1)
		{
			if (is_space(v[0]))
			{
				clear();
				return;
			}
		}
	}

	String & erase(unsigned int start, int num = npos)
	{
		if (!v)
			return (*this);
		unsigned int i = 0;
		//check for bounds
		if (num == npos || start+num > cSize-num+1)
			num = cSize - start;
		//do the erasing
		bool copyflag = false;
		for (i=0; i<cSize; i++)
		{
			if (i>=start && i<start+num)
			{
				if (i+num < cSize)
				{	
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
				copyflag = true;
			} else if (copyflag) {
				if (i+num < cSize)
				{
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
			}
		}
		cSize -= num;
		v[cSize] = 0;

		return (*this);
	}

	String substr(unsigned int index, int num = npos)
	{
		String ns;

		if (index >= cSize || !v)
			return ns;
		
		if (num == npos)
		{
			num = cSize - index;
		} else if (index+num >= cSize) {
			num = cSize - index;
		}

		unsigned int i = 0, j=0;
		char *s = new char[cSize+1];

		for (i=index; i<index+num; i++)
		{
			s[j++] = v[i];
		}
		s[j] = 0;

		ns.assign(s);

		delete [] s;

		return ns;
	}

	void toLower()
	{
		if (!v)
			return;
		unsigned int i = 0;
		for (i=0; i<cSize; i++)
		{
			if (v[i] >= 65 && v[i] <= 90)
				v[i] |= 32;
		}
	}

	String & operator = (const String &src)
	{
		assign(src);
		return *this;
	}

	String & operator = (const char *src)
	{
		assign(src);
		return *this;

	}

	char operator [] (unsigned int index)
	{
		if (index > cSize)
		{
			return -1;
		} else {
			return v[index];
		}
	}

	int at(int a)
	{
		if (a < 0 || a >= (int)cSize)
			return -1;

		return v[a];
	}

	bool at(int at, char c)
	{
		if (at < 0 || at >= (int)cSize)
			return false;

		v[at] = c;

		return true;
	}

private:
	void Grow(unsigned int d)
	{
		if (d<1)
			return;
		if (d > mSize)
		{
			mSize = d + 16;	// allocate a buffer
			char *t = new char[d+1];
			if (v) {
				strcpy(t, v);
				t[cSize] = 0;
				delete [] v;
			}
			v = t;
			mSize = d;
		}
	}

	char *v;
	unsigned int mSize;
	unsigned int cSize;
public:
	static const int npos = -1;
};

#endif //_INCLUDE_CSTRING_H
