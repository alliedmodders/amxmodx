//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#ifndef HL_TYPE_CONVERSION_H
#define HL_TYPE_CONVERSION_H

#include "amxxmodule.h"

struct OffsetHandler
{
	int pev;

	void search_pev();

	OffsetHandler()
	{
		search_pev();
	}
};

class HL_TypeConversion
{
	public:

		inline edict_t* INDEXENT2(int iEdictNum);

		edict_t* entvar_to_edict(entvars_t *pev);
		int entvar_to_id(entvars_t *pev);

		void* id_to_cbase(int index);
		int cbase_to_id(void *cbase);

		entvars_t* id_to_entvar(int index);
		entvars_t* cbase_to_entvar(void* cbase);
};

extern OffsetHandler*     G_OffsetHandler;
extern HL_TypeConversion  G_HL_TypeConversion;

#endif // HL_TYPE_CONVERSION_H
