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

#include "amxxmodule.h"

#ifndef FORWARD_H
#define FORWARD_H

#include <amtl/am-refcounting.h>

enum fwdstate
{
	FSTATE_INVALID = 0,
	FSTATE_OK,
	FSTATE_PAUSE,
	FSTATE_STOP,
	FSTATE_DESTROY
};

class Forward :public ke::Refcounted<Forward>
{
public:
	int      id;    // id of the forward
	fwdstate state;
	Forward(int id_) : id(id_), state(FSTATE_OK)
	{
		/* do nothing */
	};
	Forward() : id(-1), state(FSTATE_INVALID)
	{
		/* do nothing */
	}
	~Forward()
	{
		MF_UnregisterSPForward(id);
	}
	inline void Set(int i)
	{
		state=FSTATE_OK;
		id=i;
	};

};

#endif
