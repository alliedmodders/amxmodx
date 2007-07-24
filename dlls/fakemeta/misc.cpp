#include "fakemeta_amxx.h"
#include <studio.h> // HLSDK, for the animation natives

static cell AMX_NATIVE_CALL copy_infokey_buffer(AMX *amx, cell *params)
{
	char *infobuffer = reinterpret_cast<char *>(params[1]);

	return MF_SetAmxString(amx, params[2], infobuffer, params[3]);
}

int UTIL_stricmp(const char *s1, const char *s2)
{
	unsigned char c1, c2;

	for (;;) {
		c1 = *s1++;
		c2 = *s2++;
		
		if (!c1 || !c2)
			break;
 
		if (c1 == c2)
 			continue;
 			
		if ((c1 = tolower(c1)) != (c2 = tolower(c2)))
			break;
	}
	return (int)c1 - (int)c2;
}

// lookup_sequence(entid, "sequence name", &Float:framerate = 0.0, &bool:loops = false, &Float:groundspeed = 0.0);
static cell AMX_NATIVE_CALL lookup_sequence(AMX* amx, cell* params)
{
	int index = params[1];

	CHECK_ENTITY(index);

	edict_t* ent = INDEXENT(index);

	studiohdr_t* pstudiohdr = static_cast<studiohdr_t*>(GET_MODEL_PTR(ent));

	if (pstudiohdr == NULL)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Could not retrieve the model pointer from the entity provided.");
		return 0;
	}

	mstudioseqdesc_t* pseqdesc;

	pseqdesc = reinterpret_cast<mstudioseqdesc_t*>(
					reinterpret_cast<char*>(pstudiohdr) + pstudiohdr->seqindex);

	char* label = MF_GetAmxString(amx, params[2], 0, NULL);

	for (int i = 0; i < pstudiohdr->numseq; i++)
	{
		if (UTIL_stricmp( pseqdesc[i].label, label ) == 0)
		{
			REAL* FrameRate = reinterpret_cast<REAL*>(MF_GetAmxAddr(amx, params[3]));
			cell* Loops = MF_GetAmxAddr(amx, params[4]);
			REAL* GroundSpeed = reinterpret_cast<REAL*>(MF_GetAmxAddr(amx, params[5]));

			// Taken from HLSDK: animation & animating.cpp
			pseqdesc = &pseqdesc[i];
			*FrameRate = 256 * pseqdesc->fps / (pseqdesc->numframes - 1);

			*GroundSpeed = sqrt( pseqdesc->linearmovement[0]*pseqdesc->linearmovement[0]+ pseqdesc->linearmovement[1]*pseqdesc->linearmovement[1]+ pseqdesc->linearmovement[2]*pseqdesc->linearmovement[2] );
			*GroundSpeed = *GroundSpeed * pseqdesc->fps / (pseqdesc->numframes - 1);

			*Loops = pseqdesc->flags & STUDIO_LOOPING;
			return i;
		}
	}

	return -1;

};
// Float:set_controller(entid, controllerid, Float:value);
static cell AMX_NATIVE_CALL set_controller(AMX* amx, cell* params)
{
// From animation.cpp from the HLSDK
//	SetController( void *pmodel, entvars_t *pev, int iController, float flValue )
	int entindex = params[1];
	CHECK_ENTITY(entindex);
	edict_t* entity = INDEXENT(entindex);

	int iController = params[2];

	if (iController < 0 || iController > 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid controller id passed. Expected 0 through 3, got %d.", iController);
		return 0;
	}
	entvars_t* pev = &entity->v;

	float flValue = amx_ctof(params[3]);
	
	studiohdr_t* pstudiohdr = static_cast<studiohdr_t*>(GET_MODEL_PTR(entity));

	if (! pstudiohdr)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Could not find the model pointer for the entity.");
		return amx_ftoc(flValue);
	}

	mstudiobonecontroller_t	*pbonecontroller = (mstudiobonecontroller_t *)((byte *)pstudiohdr + pstudiohdr->bonecontrollerindex);

	int i = 0;

	// find first controller that matches the index
	for (i = 0; i < pstudiohdr->numbonecontrollers; i++, pbonecontroller++)
	{
		if (pbonecontroller->index == iController)
			break;
	}
	if (i >= pstudiohdr->numbonecontrollers)
		return amx_ftoc(flValue);

	// wrap 0..360 if it's a rotational controller

	if (pbonecontroller->type & (STUDIO_XR | STUDIO_YR | STUDIO_ZR))
	{
		// ugly hack, invert value if end < start
		if (pbonecontroller->end < pbonecontroller->start)
			flValue = -flValue;

		// does the controller not wrap?
		if (pbonecontroller->start + 359.0 >= pbonecontroller->end)
		{
			if (flValue > ((pbonecontroller->start + pbonecontroller->end) / 2.0) + 180)
				flValue = flValue - 360;
			if (flValue < ((pbonecontroller->start + pbonecontroller->end) / 2.0) - 180)
				flValue = flValue + 360;
		}
		else
		{
			if (flValue > 360)
				flValue = flValue - (int)(flValue / 360.0) * 360.0;
			else if (flValue < 0)
				flValue = flValue + (int)((flValue / -360.0) + 1) * 360.0;
		}
	}

	int setting = static_cast<int>(255 * (flValue - pbonecontroller->start) / (pbonecontroller->end - pbonecontroller->start));

	if (setting < 0) setting = 0;
	if (setting > 255) setting = 255;
	pev->controller[iController] = setting;

	return amx_ftoc(setting * (1.0 / 255.0) * (pbonecontroller->end - pbonecontroller->start) + pbonecontroller->start);
}



AMX_NATIVE_INFO misc_natives[] = {
	{ "copy_infokey_buffer",		copy_infokey_buffer },
	{ "lookup_sequence",			lookup_sequence },
	{ "set_controller",				set_controller },

	{NULL,							NULL},
};
