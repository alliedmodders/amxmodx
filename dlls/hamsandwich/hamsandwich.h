/* Ham Sandwich
 *
 * by sawce
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

#ifndef HAMSANDWICH_H
#define HAMSANDWICH_H

#include "NEW_Util.h"

extern unsigned int HAM_pev;
extern unsigned int HAM_classbase;


inline edict_t *PrivateToEdict(const void *pdata)
{

	if (!pdata)
	{
		return NULL;
	}

	char *ptr=(char*)pdata + HAM_pev;
	entvars_t *pev=(entvars_t *)ptr;

	if (!pev)
	{
		return NULL;
	}
	return pev->pContainingEntity;
};

inline int PrivateToIndex(const void *pdata)
{

	if (pdata==NULL)
	{
		return -1;
	}
	char *ptr=(char*)pdata;

	ptr+=HAM_pev;

	entvars_t *pev=*(entvars_t **)ptr;


	if (pev==NULL)
	{
		return -1;
	}
	
	if (pev->pContainingEntity==NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(pev->pContainingEntity);
};

inline int EntvarToIndex(entvars_t *pev)
{
	if (pev==NULL)
	{
		return -1;
	}
	
	if (pev->pContainingEntity==NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(pev->pContainingEntity);
};

inline edict_t *EntvarToEdict(entvars_t *pev)
{
	if (pev==NULL)
	{
		return NULL;
	}
	
	return pev->pContainingEntity;
};
inline void **EdictToVTable(edict_t *ent)
{
	char *btbl=(char *)ent->pvPrivateData;
	btbl+=HAM_classbase;
	return *((void ***)btbl);
};




void RegisterKeySuffix(const char *suffix, void (*callback)(const char *,const char *));
void RegisterConfigCallback(void (*callback)(void));
void HAM_CallConfigDone(void);
void HAM_CallInitialization(void);
int HAM_StrToNum(const char *input);
bool HAM_GetKey(const char *key, const char *data);
void HAM_SetPev(const char *key, const char *data);
#ifdef __linux__
void HAM_SetClassBase(const char *key, const char *data);
#endif



#endif //HAMSANDWICH_H
