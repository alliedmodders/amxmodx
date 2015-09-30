// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#ifndef TITLEMANAGER_H
#define TITLEMANAGER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <amtl/am-string.h>
#include "Hash.h"


class TitleManager
{
private:
	int						m_Loaded;

	// Use a string pointer for the data type because the location manager
	// stores a direct pointer to the c_str() of the title
	Hash<ke::AString, ke::AString*>	m_Hash;

public:

	TitleManager()
	{
		m_Loaded=0;
	};

	inline const char *Lookup(ke::AString &input)
	{
		ke::AString** ret = m_Hash.find(input);

		if (ret == NULL || *ret == NULL)
		{
			// was not found
			return NULL;
		}

		return (*ret)->chars();
	};
	void LoadTitles(void);
};

extern TitleManager TitleMan;

#endif // TITLEMANAGER_H
