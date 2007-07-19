#include <stdio.h>
#include "geoip_amxx.h"

GeoIP *gi = NULL;

inline void StripPort(char *ip)
{
	char *tmp = strchr(ip, ':');
	if (!tmp)
		return;
	*tmp = '\0';
}

static cell AMX_NATIVE_CALL amx_geoip_code2(AMX *amx, cell *params)
{
	int len = 0;
	char *ip = MF_GetAmxString(amx, params[1], 0, &len);
	StripPort(ip);
	const char *ccode = GeoIP_country_code_by_addr(gi, ip);
	return MF_SetAmxString(amx, params[2], ccode?ccode:"error", 3);
}

static cell AMX_NATIVE_CALL amx_geoip_code3(AMX *amx, cell *params)
{
	int len = 0;
	char *ip = MF_GetAmxString(amx, params[1], 0, &len);
	StripPort(ip);
	const char *ccode = GeoIP_country_code3_by_addr(gi, ip);
	return MF_SetAmxString(amx, params[2], ccode?ccode:"error", 4);
}
static cell AMX_NATIVE_CALL amx_geoip_code2_ex(AMX *amx, cell *params)
{
	int len = 0;
	char *ip = MF_GetAmxString(amx, params[1], 0, &len);
	StripPort(ip);
	const char *ccode = GeoIP_country_code_by_addr(gi, ip);
	
	if (ccode == NULL)
	{
		return 0;
	}
	
	MF_SetAmxString(amx, params[2], ccode, 2);
	
	return 1;
}

static cell AMX_NATIVE_CALL amx_geoip_code3_ex(AMX *amx, cell *params)
{
	int len = 0;
	char *ip = MF_GetAmxString(amx, params[1], 0, &len);
	StripPort(ip);
	const char *ccode = GeoIP_country_code3_by_addr(gi, ip);

	if (ccode == NULL)
	{
		return 0;
	}
	
	MF_SetAmxString(amx, params[2], ccode, 3);
	
	return 1;
}

static cell AMX_NATIVE_CALL amx_geoip_country(AMX *amx, cell *params)
{
	int len = 0;
	char *ip = MF_GetAmxString(amx, params[1], 0, &len);
	StripPort(ip);
	const char *ccode = GeoIP_country_name_by_addr(gi, ip);
	return MF_SetAmxString(amx, params[2], ccode?ccode:"error", params[3]);
}

void OnAmxxAttach()
{
	char *path = MF_BuildPathname("%s/GeoIP.dat",MF_GetLocalInfo("amxx_datadir", "addons/amxmodx/data"));
	gi = GeoIP_open(path, GEOIP_MEMORY_CACHE);
	if (gi == NULL) {
		MF_Log("Failed to instantiate GeoIP!");
		return;
	}
	char *db_info = GeoIP_database_info(gi);
	MF_Log("Database info: %s", db_info);
	MF_AddNatives(geoip_natives);
}

void OnAmxxDetach()
{
	GeoIP_delete(gi);
	gi = NULL;
}

AMX_NATIVE_INFO geoip_natives[] = {
	{"geoip_code2",		amx_geoip_code2},
	{"geoip_code3",		amx_geoip_code3},
	
	{"geoip_code2_ex",	amx_geoip_code2_ex},
	{"geoip_code3_ex",	amx_geoip_code3_ex},
	
	{"geoip_country",	amx_geoip_country},
	{NULL,				NULL},
};
