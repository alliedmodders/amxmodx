/* AMX Mod X 
 *   Natural Selection Module 
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

#ifndef LOCATIONMANAGER_H
#define LOCATIONMANAGER_H

#include "CVector.h"
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
	CVector<location_data_t>			m_LocationList;

public:
	LocationManager()
	{
		Clear();
	};

	inline void Clear(void)
	{
		m_LocationList.clear();
		m_LocationList.reserve(32);
	};

	inline void Add(const char *Name, edict_t *Entity)
	{
		location_data_t Temp;

		Temp.mins=Entity->v.mins;
		Temp.maxs=Entity->v.maxs;

		strncpy(Temp.name,Name,sizeof(Temp.name)-1);

		String NameString(Name);

		NameString.toLower();

		Temp.titlelookup=TitleMan.Lookup(NameString);

		m_LocationList.push_back(Temp);
	};
	inline const char *Lookup(vec3_t origin, cell titlelookup)
	{
		unsigned int i=0;
		location_data_t Temp;
		while (i<m_LocationList.size())
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
