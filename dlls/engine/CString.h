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

//by David "BAILOPAN" Anderson
class CString
{
public:
	CString() { v = NULL; mSize = 0; }
	~CString() { if (v) delete [] v; }

	//added these for amxx
	CString(const char *src) { v = NULL; mSize = 0; assign(src); }
	CString(CString &src) { v = NULL; mSize = 0; assign(src.c_str()); }

	const char *c_str() { return v?v:""; }
	const char *c_str() const { return v?v:""; }

	void append(const char *t)
	{
		Grow(strlen(v) + strlen(t));
		strcat(v, t);
	}

	void append(CString &d)
	{
		const char *t = d.c_str();
		Grow(strlen(v) + strlen(t));
		strcat(v, t);
	}

	void assign(const char *d)
	{
		if (!d)
		{
			Grow(1);
			strcpy(v, "");
			return;
		}
		Grow(strlen(d));
		if (v)
			strcpy(v, d);
	}

	void clear()
	{
		if (v)
			delete [] v;
		v = NULL;
		mSize = 0;
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
		if (!v || !mSize)
			return true;

		return false;
	}

	int size()
	{
		if (!v)
			return 0;
		return strlen(v);
	}

private:
	void Grow(int d)
	{
		if (d<1)
			return;
		if (d > mSize)
		{
			char *t = new char[d+1];
			if (v) {
				strcpy(t, v);
				t[strlen(v)] = 0;
				delete [] v;
			}
			v = t;
			mSize = d;
		}
	}

	char *v;
	int mSize;
};

#endif //_INCLUDE_CSTRING_H