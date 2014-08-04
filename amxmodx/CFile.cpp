// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <ctype.h>
#include "amxmodx.h"
#include "CFile.h"

// *****************************************************
// class File
// *****************************************************

File::File(const char* n, const char* m)
{
	fp = fopen(n, m);
}

File::~File()
{
	if (fp)
		fclose(fp);
}

File::operator bool () const
{
	return fp && !feof(fp);
}

File& operator<<(File& f, const String& n)
{
	if (f) fputs(n.c_str(), f.fp);
	return f;
}

File& operator<<(File& f, const char* n)
{
	if (f) fputs(n, f.fp);
	return f;
}

File& operator<<(File& f, int n)
{
	if (f) fprintf(f.fp, "%d", n);
	return f;
}

File& operator<<(File& f, const char& c)
{
	if (f) fputc(c, f.fp);
	return f;
}

File& operator>>(File& f, String& n)
{
	if (!f) return f;
	char temp[1024];
	fscanf(f.fp, "%s", temp);
	n.assign(temp);
	return f;
}

File& operator>>(File& f, char* n)
{
	if (f) fscanf(f.fp, "%s", n);
	return f;
}

int File::getline(char* buf, int sz)
{
	int a = sz;
	char *origBuf = buf;
	
	if (*this)
	{
		int c;
		while (sz-- && (c = getc((*this).fp)) && c != EOF && c != '\n')
		*buf++ = c;
		*buf = 0;
	}

	// trim 0x0a and 0x0d characters at the end
	while (buf != origBuf)
	{
		if (*buf == 0x0a || *buf == 0x0d)
			*buf = 0;
		--buf;
	}

	return a - sz;
}

File& File::skipWs()
{
	if (!*this) return *this;
	int c;
	while (isspace(c = getc(fp))) {};
	ungetc(c, fp);
	return *this;
}
