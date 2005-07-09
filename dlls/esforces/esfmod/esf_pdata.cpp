#include <extdll.h>
#include <meta_api.h>
#include <eiface.h>
#include <edict.h>
#include "esforces.h"
#include "esf_pdata.h"

#define GET_PDATA_D(v,o) ( *((int *)v + (o)) )
#define SET_PDATA_D(v,o,s) ( *((int *)v + (o)) = (s) )

static cell AMX_NATIVE_CALL esf_get_powerlevel(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	return GET_PDATA_D(pEdict->pvPrivateData, ESF_POWERLEVEL1);
}

static cell AMX_NATIVE_CALL esf_set_powerlevel(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	SET_PDATA_D(pEdict->pvPrivateData, ESF_POWERLEVEL1, params[2]);
	SET_PDATA_D(pEdict->pvPrivateData, ESF_POWERLEVEL2, params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL esf_get_speed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	return GET_PDATA_D(pEdict->pvPrivateData, ESF_SPEED1);
}

static cell AMX_NATIVE_CALL esf_set_speed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);
	
	int speed = params[2];

	SET_PDATA_D(pEdict->pvPrivateData, ESF_SPEED1, speed);
	SET_PDATA_D(pEdict->pvPrivateData, ESF_SPEED2, speed);

	return 1;
}

AMX_NATIVE_INFO g_PdataNatives[] = {
 {"esf_get_powerlevel",	esf_get_powerlevel},
 {"esf_set_powerlevel",	esf_set_powerlevel},
 {"esf_get_speed",		esf_get_speed},
 {"esf_get_speed",		esf_get_speed},
 {NULL,					NULL},
};
