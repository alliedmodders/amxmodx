/* Ham Sandwich
 *   Copyright 2007-2014
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
#include "amxxmodule.h"

#ifndef FORWARD_H
#define FORWARD_H

enum fwdstate
{
	FSTATE_INVALID = 0,
	FSTATE_OK,
	FSTATE_PAUSE,
	FSTATE_STOP,
	FSTATE_DESTROY
};

class Forward
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
