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

#ifndef LOCATIONMANAGER_H
#define LOCATIONMANAGER_H

#include <amtl/am-vector.h>
#include "GameManager.h"
#include "TitleManager.h"

typedef struct location_data_s
{
	char		 name[128];
	vec3_t		 mins;
	vec3_t		 maxs;

	const char	*titlelookup;
} location_data_t;


class LocationManager
{
private:
	ke::Vector<location_data_t>			m_LocationList;

public:
	LocationManager()
	{
		Clear();
	};

	inline void Clear(void)
	{
		m_LocationList.clear();
		m_LocationList.ensure(32);
	};

	inline void Add(const char *Name, edict_t *Entity)
	{
		location_data_t Temp;

		Temp.mins=Entity->v.mins;
		Temp.maxs=Entity->v.maxs;

		strncpy(Temp.name,Name,sizeof(Temp.name)-1);

		ke::AString NameString(UTIL_ToLowerCase(Name));

		Temp.titlelookup=TitleMan.Lookup(NameString);

		m_LocationList.append(Temp);
	};
	inline const char *Lookup(vec3_t origin, cell titlelookup)
	{
		unsigned int i=0;
		location_data_t Temp;
		while (i<m_LocationList.length())
		{
			Temp=m_LocationList[i++];

			if (origin.x <= Temp.maxs.x &&
				origin.y <= Temp.maxs.y &&
				origin.x >= Temp.mins.x &&
				origin.y >= Temp.mins.y)
			{
				if (titlelookup==0)
				{
					return &(m_LocationList[i-1].name[0]);
				}
				else
				{
					if (m_LocationList[i-1].titlelookup!=NULL)
					{
						return m_LocationList[i-1].titlelookup;
					}

					return &(m_LocationList[i-1].name[0]);
				}
			}
		}
		return "";
	};

};

extern LocationManager LocationMan;

#endif // LOCATIONMANAGER_H
