#include "fun.h"

/*
	JGHG says:

	Ok this is what I use below, it may probably not be right with all natives etc but I try to maintain this style to natives.
	Note that this is still very much subject to change, regarding return values etc!
	(Ok I haven't checked all natives that they comply with this yet, this is just a model I'm working on and which I might implement soon.)

	static cell AMX_NATIVE_CALL nativename(AMX *amx, cell *params) // nativename(argument1, argument2); = 2 params
	{
		// Description what this native does.					<--- Description what this native does
		// params[1] = argument1								<--- Description of each argument, so we don't have to allocate new variables and can
		// params[2] = argument2								<--- use the ones in params[n] directly, to save some time.

		// Check receiver and sender validity.					<--- Check ents, maybe need to do this better and more proper later?
		if (params[1] < 1 || params[1] > gpGlobals->maxClients
		|| params[2] < 1 || params[2] > gpGlobals->maxClients)
		{
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);				<--- Call this, it will end up as RUN TIME ERROR 10 in server console.
			return 0;											<--- Standard return 0 with run time errors? (note in small only 0 returns false, everything else returns true)
		}

		// Get * pointer.
		edict_t *pPlayer = INDEXENT(params[1]);

		if (FNullEnt(pPlayer)) {								<--- Test this pointer this way, return 0...
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}

		return 1												<--- If native succeeded, return 1, if the native isn't supposed to return a specific value.		
																Note: Should be able to do: if (thenative()) and it should return false when it fails, and true when succeeds... is -1 treated as false, or is 0 a must?
	}
*/



static cell AMX_NATIVE_CALL get_client_listening(AMX *amx, cell *params) // get_client_listening(receiver, sender); = 2 params
{
	// Gets who can listen to who.
	// params[1] = receiver
	// params[2] = sender

	// Check receiver and sender validity.
	if (params[1] < 1 || params[1] > gpGlobals->maxClients
	|| params[2] < 1 || params[2] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// GET- AND SETCLIENTLISTENING returns "qboolean", an int, probably 0 or 1...
	return GETCLIENTLISTENING(params[1], params[2]);
}

static cell AMX_NATIVE_CALL set_client_listening(AMX *amx, cell *params) // set_client_listening(receiver, sender, listen); = 3 params
{
	// Sets who can listen to who.
	// params[1] = receiver
	// params[2] = sender
	// params[3] = listen

	// Check receiver and sender validity.
	if (params[1] < 1 || params[1] > gpGlobals->maxClients
	|| params[2] < 1 || params[2] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make a check on params[3] here later, and call run time error when it's wrong.
	// To do: find out the possible values to set (0, 1?)

	// GET- AND SETCLIENTLISTENING returns "qboolean", an int, probably 0 or 1...
	return SETCLIENTLISTENING(params[1], params[2], params[3]);
}

static cell AMX_NATIVE_CALL set_user_godmode(AMX *amx, cell *params) // set_user_godmode(index, godmode = 0); = 2 params
{
	/* Sets player godmode. If you want to disable godmode set only first parameter. */
	// params[1] = index
	// params[2] = godmode = 0

	// Check index.
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (params[2] == 1) {
		// Enable godmode
		pPlayer->v.takedamage = 0.0;	// 0.0, the player doesn't seem to be able to get hurt.
	}
	else {
		// Disable godmode
		pPlayer->v.takedamage = 2.0;	// 2.0 seems to be standard value?
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_godmode(AMX *amx, cell *params) // get_user_godmode(index); = 1 param
{
	/* Returns 1 if godmode is set. */
	// params[1] = index

	// Check index.
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int godmode = 0;

	if (pPlayer->v.takedamage == 0.0) {
		// God mode is enabled
		godmode = 1;
	}

	return godmode;
}

static cell AMX_NATIVE_CALL give_item(AMX *amx, cell *params) // native give_item(index, const item[]); = 2 params
{
	/* Gives item to player, name of item can start
	* with weapon_, ammo_ and item_. This event
	* is announced with proper message to all players. */
	// params[1] = index
	// params[2] = item...

	// Check index.
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Create item entity pointer
	edict_t	*pItemEntity;

	// Make an "intstring" out of 2nd parameter
	int length;
	char *szItem = GET_AMXSTRING(amx, params[2], 1, length);

	int item = MAKE_STRING(szItem);

	// Create the entity, returns to pointer
	pItemEntity = CREATE_NAMED_ENTITY(item);

	VARS(pItemEntity)->origin = VARS(pPlayer)->origin; // nice to do VARS(ent)->origin instead of ent->v.origin? :-I
	pItemEntity->v.spawnflags |= SF_NORESPAWN;

	MDLL_Spawn(pItemEntity);
	MDLL_Touch(pItemEntity, ENT(pPlayer)); // unnecessary to do ENT(pPlayer) maybe, could've just used params[1] perhaps???

	// HERE: ADD PROPER CHECKING OF pPlayer LATER

	return 1;
}

static cell AMX_NATIVE_CALL spawn(AMX *amx, cell *params) // spawn(id) = 1 param
{
	// Spawns an entity, this can be a user/player -> spawns at spawnpoints, or created entities seems to need this as a final "kick" into the game? :-)
	// params[1] = entity to spawn

    if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
    {
        AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
        return 0;
    }
	edict_t *pEnt = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pEnt)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	MDLL_Spawn(pEnt);

	return 1;
}

static cell AMX_NATIVE_CALL set_user_money(AMX *amx, cell *params) // set_user_money(index, money, flash = 1); = 3 arguments
{
	// Give money to user
	// params[1] = user
	// params[2] = money
	// params[3] = flash money?

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Give money
	(int)*((int *)pPlayer->pvPrivateData + CSMONEYOFFSET) = params[2];

	// Update display
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "Money", NULL), NULL, pPlayer);
	WRITE_LONG(params[2]);
	WRITE_BYTE(params[3] ? 1 : 0); // if params[3] is 0, there will be no +/- flash of money in display...
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL get_user_money(AMX *amx, cell *params) // get_user_money(index); = 1 argument
{
	// Give money to user
	// params[1] = user

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Return money
	return (int)*((int *)pPlayer->pvPrivateData + CSMONEYOFFSET);
}

static cell AMX_NATIVE_CALL set_user_deaths_cs(AMX *amx, cell *params) // set_user_deaths_cs(index, newdeaths); = 2 arguments
{
	// Sets user deaths in cs.
	// params[1] = user
	// params[2] = new deaths

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Set deaths
	(int)*((int *)pPlayer->pvPrivateData + CSDEATHSOFFSET) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_health(AMX *amx, cell *params) // set_user_health(index, health); = 2 arguments
{
	// Sets user health. If health is 0 and below, also kill...
	// params[1] = index
	// params[2] = health

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Kill if health too low.
	if (params[2] > 0)
		pPlayer->v.health = float(params[2]);
	else
		MDLL_ClientKill(pPlayer);

	return 1;
}

static cell AMX_NATIVE_CALL set_user_frags(AMX *amx, cell *params) // set_user_frags(index, frags); = 2 arguments
{
	// Sets user frags.
	// params[1] = index
	// params[2] = frags

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.frags = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_armor(AMX *amx, cell *params) // set_user_armor(index, armor); = 2 arguments
{
	// Sets user armor.
	// params[1] = index
	// params[2] = armor

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.armorvalue = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_origin(AMX *amx, cell *params) // set_user_origin(index, origin[3]); = 2 arguments
{
	// Sets user origin.
	// params[1] = index
	// params[2] = origin

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	cell *newVectorCell = GET_AMXADDR(amx, params[3]);

	float newX = *(float *)((void *)&newVectorCell[0]); // JGHG: I cast a spell on you.....
	float newY = *(float *)((void *)&newVectorCell[1]);
	float newZ = *(float *)((void *)&newVectorCell[2]);

	Vector newVector = Vector(newX, newY, newZ);

	SET_SIZE(pPlayer, pPlayer->v.mins, pPlayer->v.maxs);
	SET_ORIGIN(pPlayer, newVector);
	
	return 1;
}

static cell AMX_NATIVE_CALL set_user_rendering(AMX *amx, cell *params) // set_user_rendering(index, fx = kRenderFxNone, r = 255, g = 255, b = 255, render = kRenderNormal, amount = 16); = 7 arguments
{
	// Sets user rendering.
	// params[1] = index
	// params[2] = fx
	// params[3] = r
	// params[4] = g
	// params[5] = b
	// params[6] = render
	// params[7] = amount

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.renderfx = params[2];
	Vector newVector = Vector(float(params[3]), float(params[4]), float(params[5]));
	pPlayer->v.rendercolor = newVector;
	pPlayer->v.rendermode = params[6];
	pPlayer->v.renderamt = params[7];
	
	return 1;
}


static cell AMX_NATIVE_CALL set_user_maxspeed(AMX *amx, cell *params) // set_user_maxspeed(index, Float:speed = -1.0) = 2 arguments
{
	// Sets user maxspeed.
	// params[1] = index
	// params[2] = speed (should be -1.0 if not specified) (JGHG: unspecified parameters seems to always be -1.0!)

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.maxspeed = *(float *)((void *)&params[2]); // JGHG: Gotta love the way to get floats from parameters :-P

	return 1;
}

static cell AMX_NATIVE_CALL get_user_maxspeed(AMX *amx, cell *params) // Float:get_user_maxspeed(index) = 1 argument
{
	// Gets user maxspeed.
	// params[1] = index

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return *(cell*)((void *)&(pPlayer->v.maxspeed)); // The way to return floats... (sigh)
}

static cell AMX_NATIVE_CALL set_user_gravity(AMX *amx, cell *params) // set_user_gravity(index, Float:gravity = 1.0) = 2 arguments
{
	// Sets user gravity.
	// params[1] = index
	// params[2] = gravity (=-1.0)
	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.gravity = *(float *)((void *)&params[2]); // JGHG: Gotta love the way to get floats from parameters :-P

	return 1;
}

static cell AMX_NATIVE_CALL get_user_gravity(AMX *amx, cell *params) // Float:get_user_gravity(index) = 1 argument
{
	// Gets user gravity.
	// params[1] = index

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return *(cell*)((void *)&(pPlayer->v.gravity)); // The way to return floats... (sigh)
}

static cell AMX_NATIVE_CALL set_hitzones(AMX *amx, cell *params) // set_hitzones(body = 255) = 1 argument
{
	// Gets user gravity.
	// params[1] = body hitzones

	// Fetch player pointer
	g_body = params[1];

	return 1;
}

static cell AMX_NATIVE_CALL get_hitzones(AMX *amx, cell *params) // get_hitzones() = 0 arguments
{
	// Gets hitzones.
	return g_body;
}

static cell AMX_NATIVE_CALL set_user_noclip(AMX *amx, cell *params) // set_user_noclip(index, noclip = 0); = 2 arguments
{
	// Sets user to no clipping mode.
	// params[1] = index
	// params[2] = no clip or not...

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check validity.
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (params[2] == 1)
		pPlayer->v.movetype = MOVETYPE_NOCLIP;
	else
		pPlayer->v.movetype = MOVETYPE_WALK;

	return 1;
}

static cell AMX_NATIVE_CALL get_user_noclip(AMX *amx, cell *params) // get_user_noclip(index); = 1 argument
{
	// Gets user noclip.
	// params[1] = index

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check validity.
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return pPlayer->v.movetype == MOVETYPE_NOCLIP;
}


AMX_NATIVE_INFO fun_Exports[] = {
	{"get_client_listen",		get_client_listening},
	{"set_client_listen",		set_client_listening},
	{"set_user_godmode",		set_user_godmode},
	{"get_user_godmode",		get_user_godmode},
	{"set_user_health",			set_user_health},
	{"give_item",				give_item},
	{"spawn",					spawn},
	//{"user_spawn",				spawn}, // No, it's not a typo! Uses the same as "spawn", because they do the same stuff! This is made only to maintain comp. with old plugins!
	{"set_user_money",			set_user_money},
	{"get_user_money",			get_user_money},
	{"set_user_deaths_cs",		set_user_deaths_cs},
	{"set_user_frags",			set_user_frags},
	{"set_user_armor",			set_user_armor},
	{"set_user_origin",			set_user_origin},
	{"set_user_rendering",		set_user_rendering},
	{"set_user_maxspeed",		set_user_maxspeed},
	{"get_user_maxspeed",		get_user_maxspeed},
	{"set_user_gravity",		set_user_gravity},
	{"get_user_gravity",		get_user_gravity},
	{"set_hitzones",			set_hitzones},
	{"get_hitzones",			get_hitzones},
	{"set_user_noclip",			set_user_noclip},
	{"get_user_noclip",			get_user_noclip},
	  /////////////////// <--- 19 chars max in current small version
	{NULL,					NULL}
};

/******************************************************************************************/

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr)
{
	if (g_body == 255) {
		RETURN_META(MRES_IGNORED);
	}
	else {
		TRACE_LINE(v1, v2, fNoMonsters, pentToSkip, ptr);

		if ( !(g_body & (1 << ptr->iHitgroup)) )
			ptr->flFraction = 1.0;

		// pentToSkip seems to be the user that is aiming (shooting), so it doesn't accidentally hit himself?

		RETURN_META(MRES_SUPERCEDE);
	}
}

enginefuncs_t meta_engfuncs;
C_DLLEXPORT int GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion ) {
	meta_engfuncs.pfnTraceLine = TraceLine;
	memcpy(pengfuncsFromEngine, &meta_engfuncs, sizeof(enginefuncs_t));
	return(TRUE);
}

/******************************************************************************************/

C_DLLEXPORT int Meta_Query(char *ifvers, plugin_info_t **pPlugInfo, mutil_funcs_t *pMetaUtilFuncs) {
	*pPlugInfo = &Plugin_info;
	gpMetaUtilFuncs = pMetaUtilFuncs;

	return(TRUE);
}

C_DLLEXPORT int Meta_Attach(PLUG_LOADTIME now, META_FUNCTIONS *pFunctionTable, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs) {
	if(!pMGlobals) {
		LOG_ERROR(PLID, "Meta_Attach called with null pMGlobals");
		return(FALSE);
	}

	gpMetaGlobals = pMGlobals;

	if(!pFunctionTable) {
		LOG_ERROR(PLID, "Meta_Attach called with null pFunctionTable");
		return(FALSE);
	}

	memcpy(pFunctionTable, &gMetaFunctionTable, sizeof(META_FUNCTIONS));
	gpGamedllFuncs = pGamedllFuncs;


	// JGHG added stuff below
	g_body = (1<<HITGROUP_GENERIC) | (1<<HITGROUP_HEAD) | (1<<HITGROUP_CHEST) | (1<<HITGROUP_STOMACH) | (1<<HITGROUP_LEFTARM) | (1<<HITGROUP_RIGHTARM)| (1<<HITGROUP_LEFTLEG) | (1<<HITGROUP_RIGHTLEG); // init hs_body
	// generic is needed or bots go crazy... don't know what its for otherwise. You can still kill people.
	// these hitzones never affect CS knife? ie you can always hit with knife no matter what you set here
	//hs_body = (1<<HITGROUP_GENERIC) | (1<<HITGROUP_LEFTLEG) | (1<<HITGROUP_LEFTARM); // init hs_body
	// JGHG added stuff above

	/*
	#define HITGROUP_GENERIC	0 // none
	#define HITGROUP_HEAD		1
	#define HITGROUP_CHEST		2
	#define HITGROUP_STOMACH	3
	#define HITGROUP_LEFTARM	4
	#define HITGROUP_RIGHTARM	5
	#define HITGROUP_LEFTLEG	6
	#define HITGROUP_RIGHTLEG	7
	*/

	return(TRUE);
}

C_DLLEXPORT int Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason) {
	if(now && reason) {
		return(TRUE);
	} else {
		return(FALSE);
	}
}

void WINAPI GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals ) {
	memcpy(&g_engfuncs, pengfuncsFromEngine, sizeof(enginefuncs_t));
	gpGlobals = pGlobals;
}

C_DLLEXPORT int AMX_Query(module_info_s** info) {
	*info = &module_info;
	return 1;
}

C_DLLEXPORT int AMX_Attach(pfnamx_engine_g* amxeng,pfnmodule_engine_g* meng) {
	g_engAmxFunc = amxeng;
	g_engModuleFunc = meng;

	if(!gpMetaGlobals) REPORT_ERROR(1, "[AMX] Fun module is not attached to metamod (module \"%s\")\n", LOGTAG);

	ADD_AMXNATIVES(&module_info, fun_Exports);

	return(1);
}

C_DLLEXPORT int AMX_Detach() {
	return(1);
}