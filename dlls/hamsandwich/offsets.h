// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#ifndef OFFSETS_H
#define OFFSETS_H

#include "ham_const.h"

// Just a singleton class that keeps pev/base/offset values managed.

class OffsetManager
{
private:
	size_t pev;
	size_t baseclass;
	int baseset;
	int pevset;

public:
	OffsetManager()
	{
		memset(this,0x0,sizeof(*this));
	}
	void SetPev(size_t value)
	{
		pevset=1;
		pev=value;
	};
	size_t GetPev(void)
	{
		return pev;
	};
	int IsPevSet()
	{
		return pevset;
	};
	int IsBaseSet()
	{
		return baseset;
	};
	void SetBase(size_t value)
	{
		baseset=1;
		baseclass=value;
	};
	size_t GetBase(void)
	{
		return baseclass;
	};
	bool IsValid()
	{
		return pevset != 0 && baseset != 0;
	}
};

extern OffsetManager Offsets;

#endif
