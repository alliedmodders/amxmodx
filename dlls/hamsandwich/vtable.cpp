
/**
 * These are the functions called by the trampolines
 * I explicitly declare them as cdecl so I know exactly
 * how to work the stack in the trampoline.
 */
/*
static cell AMX_NATIVE_CALL register_takedamage(AMX *amx, cell *params)
{
};
static cell AMX_NATIVE_CALL register_use(AMX *amx, cell *params)
{
	int funcid;
	char *function=MF_GetAmxString(amx,params[2],0,NULL);
	if (MF_AmxFindPublic(amx,function,&funcid)!=AMX_ERR_NONE)
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Can not find function \"%s\"",function);
		return 0;
	}
	// Get the classname
	char *classname=MF_GetAmxString(amx,params[1],0,NULL);

	edict_t *Entity=CREATE_ENTITY();

	CALL_GAME_ENTITY(PLID,classname,&Entity->v);

	if (Entity->pvPrivateData)
	{
		VTableUse::Hook(&VTMan,EdictToVTable(Entity),amx,funcid);
		REMOVE_ENTITY(Entity);
		return 1;
	}

	REMOVE_ENTITY(Entity);

	return 0;

};

static AMX_NATIVE_INFO tdhooks[] = {
	{ "register_takedamage",		register_takedamage },
	{ "register_use",				register_use },
	{ NULL,			NULL }
};

void VTH_Natives()
{
	MF_AddNatives(tdhooks);
};

*/
