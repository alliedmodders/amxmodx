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

#include <string.h>
#include <stdio.h>

//by David "BAILOPAN" Anderson
class String
{
public:
	String() 
	{
		v = NULL;
		a_size = 0;
		//assign("");
	}

	~String()
	{ 
		if (v) 
			delete [] v; 
	}

	String(const char *src) 
	{
		v = NULL; 
		a_size = 0;
		assign(src); 
	}

	const char * _fread(FILE *fp) 	 
	{ 	 
		Grow(512, false); 	 
		char *ret = fgets(v, 511, fp); 	 
		return ret; 	 
	}

	String(const String &src) 
	{
		v = NULL;
		a_size = 0;
		assign(src.c_str()); 
	}

	const char *c_str() { return v?v:""; }

	const char *c_str() const { return v?v:""; }

	void append(const char *t)
	{
		Grow(size() + strlen(t) + 1);
		strcat(v, t);
	}

	void append(const char c)
	{
		size_t len = size();
		Grow(len + 2);
		v[len] = c;
		v[len + 1] = '\0';
	}

	void append(String &d)
	{
		append(d.c_str());
	}

	void assign(const String &src)
	{
		assign(src.c_str());
	}

	void assign(const char *d)
	{
		if (!d)
		{
			clear();
		} else {
			size_t len = strlen(d);
			Grow(len + 1, false);
			memcpy(v, d, len);
			v[len] = '\0';
		}
	}

	void clear()
	{
		if (v)
			v[0] = '\0';
	}

	int compare (const char *d) const
	{
		if (!v)
			return strcmp("", d);
		else
			return strcmp(v, d);
	}

	//Added this for amxx inclusion
	bool empty() const
	{
		if (!v)
			return true;

		if (v[0] == '\0')
			return true;

		return false;
	}

	size_t size() const
	{
		if (v)
			return strlen(v);
		else
			return 0;
	}

	int find(const char c, int index = 0)
	{
		int len = static_cast<int>(size());
		if (len < 1)
			return npos;
		if (index >= len || index < 0)
			return npos;
		int i = 0;
		for (i=index; i<len; i++)
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

	void reparse_newlines()
	{
		size_t len = size();
		int offs = 0;
		char c;
		if (!len)
			return;
		for (size_t i=0; i<len; i++)
		{
			c = v[i];
			if (c == '^' && (i != len-1))
			{
				c = v[++i];
				if (c == 'n')
					c = '\n';
				else if (c == 't')
					c = '\t';
				offs++;
			}
			v[i-offs] = c;
		}
		v[len-offs] = '\0';
	}
	
	void trim()
	{
		if (!v)
			return;

		unsigned int i = 0;
		unsigned int j = 0;
		size_t len = strlen(v);

		if (len == 1)
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
			for (i=0; i<len; i++)
			{
				if (!is_space(v[i]) || (is_space(v[i]) && ((unsigned char)i==len-1)))
				{
					erase(0, i);
					break;
				}
			}
		}

		len = strlen(v);

		if (len < 1)
		{
			return;
		}

		if (is_space(v[len-1]))
		{
			for (i=len-1; i>=0; i--)
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

		if (len == 1)
		{
			if (is_space(v[0]))
			{
				clear();
				return;
			}
		}
	}

	void erase(unsigned int start, int num = npos)
	{
		if (!v)
			return;
		unsigned int i = 0;
		size_t len = size();
		//check for bounds
		if (num == npos || start+num > len-start)
			num = len - start;
		//do the erasing
		bool copyflag = false;
		for (i=0; i<len; i++)
		{
			if (i>=start && i<start+num)
			{
				if (i+num < len)
				{	
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
				copyflag = true;
			} else if (copyflag) {
				if (i+num < len)
				{
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
			}
		}
		len -= num;
		v[len] = 0;
	}

	String substr(unsigned int index, int num = npos)
	{
		if (!v)
		{
			String b("");
			return b;
		}

		String ns;

		size_t len = size();

		if (index >= len || !v)
			return ns;
		
		if (num == npos)
		{
			num = len - index;
		} else if (index+num >= len) {
			num = len - index;
		}

		unsigned int i = 0;
		unsigned int nslen = num + 2;

		ns.Grow(nslen);

		for (i=index; i<index+num; i++)
			ns.append(v[i]);

		return ns;
	}

	void toLower()
	{
		if (!v)
			return;
		unsigned int i = 0;
		size_t len = strlen(v);
		for (i=0; i<len; i++)
		{
			if (v[i] >= 65 && v[i] <= 90)
				v[i] &= ~(1<<5);
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
		if (index > size() || !v)
		{
			return -1;
		} else {
			return v[index];
		}
	}

	int at(int a)
	{
		if (a < 0 || a >= (int)size() || !v)
			return -1;

		return v[a];
	}

	bool at(int at, char c)
	{
		if (at < 0 || at >= (int)size() || !v)
			return false;

		v[at] = c;

		return true;
	}

private:
	void Grow(unsigned int d, bool copy=true)
	{
		if (d <= a_size)
			return;
		char *n = new char[d + 1];
		if (copy && v)
			strcpy(n, v);
		if (v)
			delete [] v;
		else
			strcpy(n, "");			
		v = n;
		a_size = d + 1;
	}

	char *v;
	unsigned int a_size;
public:
	static const int npos = -1;
};

#endif //_INCLUDE_CSTRING_H
