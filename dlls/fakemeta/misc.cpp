#include "fakemeta_amxx.h"

static cell AMX_NATIVE_CALL copy_infokey_buffer(AMX *amx, cell *params)
{
	char *infobuffer = reinterpret_cast<char *>(params[1]);

	return MF_SetAmxString(amx, params[2], infobuffer, params[3]);
}

AMX_NATIVE_INFO misc_natives[] = {
	{ "copy_infokey_buffer",		copy_infokey_buffer },
	{NULL,							NULL},
};
