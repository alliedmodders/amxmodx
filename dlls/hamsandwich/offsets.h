/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

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
