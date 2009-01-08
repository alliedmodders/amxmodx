#ifndef _INCLUDE_SOURCEMOD_SQLITE_DRIVER_H
#define _INCLUDE_SOURCEMOD_SQLITE_DRIVER_H

#include "SqliteHeaders.h"

namespace SourceMod
{
	class SqliteDriver : public ISQLDriver
	{
	public:
		IDatabase *Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		IDatabase *Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		const char *NameString();
		bool IsCompatDriver(const char *namestr);
		int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize);
	};
};

#endif //_INCLUDE_SOURCEMOD_SQLITE_DRIVER_H
