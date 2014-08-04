// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <stdio.h>
#include "CString.h"

// *****************************************************
// class File
// *****************************************************

class File
{
	FILE* fp;

public:
	File(const char* n, const char* m);
	~File();
	
	operator bool () const;
	
	friend File& operator<<(File& f, const String& n);
	friend File& operator<<(File& f, const char* n);
	friend File& operator<<(File& f, const char& c);
	friend File& operator<<(File& f, int n);
	friend File& operator>>(File& f, String& n);
	friend File& operator>>(File& f, char* n);
	
	int getline(char* buf, int sz);
	
	File& skipWs();
};
