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

#ifndef CELLTOTYPE_H
#define CELLTOTYPE_H

#include <extdll.h>
#include "sdk/amxxmodule.h"

#include "CVector.h"

#include "hook.h"
#include "forward.h"

#include "ham_const.h"	
#include "ham_utils.h"

inline void CellToType(const AMX*& amx, const cell& in, int& out)
{
	out=static_cast<int>(in);
}

inline void CellToType(const AMX*& amx, const cell& in, float& out)
{
	out=amx_ctof2(in);
}

inline void CellToType(const AMX*& amx, const cell& in, edict_t*& out)
{
	out=INDEXENT_NEW(in);
}

inline void CellToType(const AMX*& amx, const cell& in, entvars_t*& out)
{
	out=&(INDEXENT_NEW(in)->v);
}

inline void CellToType(const AMX*& amx, const cell& in, HLBaseEntity*& out)
{
	out=(HLBaseEntity *)(INDEXENT_NEW(in)->pvPrivateData);
}

inline void CellToType(const AMX*& amx, const cell& in, Vector& out)
{
	float *v=(float *)MF_GetAmxAddr(amx, in);

	out.x=v[0];
	out.y=v[1];
	out.z=v[2];
}

inline void CellToType(const AMX*& amx, const cell& in, TraceResult*& out)
{
	out=reinterpret_cast<TraceResult*>(in);
}

#endif
