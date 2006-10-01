#ifndef _INCLUDE_SOURCEMOD_MYSQL_DRIVER_H
#define _INCLUDE_SOURCEMOD_MYSQL_DRIVER_H

#include "MysqlHeaders.h"

namespace SourceMod
{
	class MysqlDriver : public ISQLDriver
	{
	public:
		IDatabase *Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		IDatabase *Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		const char *NameString();
		bool IsCompatDriver(const char *namestring);
	public:
		IDatabase *_Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength, bool do_timeout);
	};
};

#endif //_INCLUDE_SOURCEMOD_MYSQL_DRIVER_H
