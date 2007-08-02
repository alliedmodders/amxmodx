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

#ifndef SPAWNMANAGER_H
#define SPAWNMANAGER_H

#include "CVector.h"

typedef struct spawndata_s
{
	REAL		Location[3];
	REAL		Angle[3];
} SpawnData;

class SpawnManager
{
private:
	CVector<SpawnData>		TeamSpawns[5];

public:
	SpawnManager()
	{
		this->Clear();
	};

	inline void Clear(void)
	{
		TeamSpawns[0].clear();
		TeamSpawns[1].clear();
		TeamSpawns[2].clear();
		TeamSpawns[3].clear();
		TeamSpawns[4].clear();

		// Reserve data for a "typical" map layout
		// makes data entry faster on map load
		TeamSpawns[0].reserve(32);
		TeamSpawns[1].reserve(16);
		TeamSpawns[2].reserve(48);
	};

	inline void Insert(const edict_t *Entity)
	{
		// Bounds check team
		if (Entity->v.team<0 || Entity->v.team > 4)
		{
			return;
		}

		SpawnData TemporaryData;

		Entity->v.origin.CopyToArray(TemporaryData.Location);
		Entity->v.angles.CopyToArray(TemporaryData.Angle);

		TeamSpawns[Entity->v.team].push_back(TemporaryData);
	};
	inline void InsertReadyRoom(const edict_t *Entity)
	{
		SpawnData TemporaryData;

		Entity->v.origin.CopyToArray(TemporaryData.Location);
		Entity->v.angles.CopyToArray(TemporaryData.Angle);

		TeamSpawns[0].push_back(TemporaryData);
	};

	// ns_get_spawn(team,number=0,Float:ret[3]);
	inline cell Lookup(AMX *amx, cell *params)
	{
		if (params[1] < 0 || params[1] > 4)
		{
			return 0;
		}
		if (params[2]==0)
		{
			return static_cast<int>(TeamSpawns[params[1]].size());
		}

		if (params[2]>=(int)TeamSpawns[params[1]].size())
		{
			return 0;
		}

		cell *Return=MF_GetAmxAddr(amx,params[3]);

		SpawnData SpawnRet=TeamSpawns[params[1]][params[2]];

		Return[0]=amx_ftoc2(SpawnRet.Location[0]);
		Return[1]=amx_ftoc2(SpawnRet.Location[1]);
		Return[2]=amx_ftoc2(SpawnRet.Location[2]);

		return 1;

	};
};

extern SpawnManager SpawnMan;

#endif // SPAWNMANAGER_H
