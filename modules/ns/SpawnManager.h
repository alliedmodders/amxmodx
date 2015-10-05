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

#ifndef SPAWNMANAGER_H
#define SPAWNMANAGER_H

#include <amtl/am-vector.h>

typedef struct spawndata_s
{
	REAL		Location[3];
	REAL		Angle[3];
} SpawnData;

class SpawnManager
{
private:
	ke::Vector<SpawnData>		TeamSpawns[5];

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
		TeamSpawns[0].ensure(32);
		TeamSpawns[1].ensure(16);
		TeamSpawns[2].ensure(48);
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

		TeamSpawns[Entity->v.team].append(TemporaryData);
	};
	inline void InsertReadyRoom(const edict_t *Entity)
	{
		SpawnData TemporaryData;

		Entity->v.origin.CopyToArray(TemporaryData.Location);
		Entity->v.angles.CopyToArray(TemporaryData.Angle);

		TeamSpawns[0].append(TemporaryData);
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
			return static_cast<int>(TeamSpawns[params[1]].length());
		}

		if (params[2]>=(int)TeamSpawns[params[1]].length())
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
