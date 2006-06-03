#ifndef _INCLUDE_SQLHEADERS_H
#define _INCLUDE_SQLHEADERS_H

#include "ISQLDriver.h"

#define SQL_DRIVER_FUNC	"GetSqlFuncs"

typedef int (*SQLAFFINITY)(AMX *amx);

struct SqlFunctions
{
	SourceMod::ISQLDriver *driver;
	SQLAFFINITY set_affinity;
	SqlFunctions *prev;
};

#endif //_INCLUDE_SQLHEADERS_H
