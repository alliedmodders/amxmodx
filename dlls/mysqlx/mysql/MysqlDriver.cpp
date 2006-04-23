#include <stdio.h>
#include <string.h>
#include "MysqlDriver.h"
#include "MysqlDatabase.h"

#if defined WIN32
#define snprintf _snprintf
#define strncasecmp strnicmp
#endif

using namespace SourceMod;

bool MysqlDriver::IsCompatDriver(const char *namestring)
{
	return (strncasecmp(namestring, "mysql", 5) == 0);
}

const char *MysqlDriver::NameString()
{
	return "mysql";
}

IDatabase *MysqlDriver::Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength)
{
	MYSQL *mysql = mysql_init(NULL);

	if (!mysql)
	{
		if (errcode)
			*errcode = -1;
		if (error && maxlength)
		{
			snprintf(error, maxlength, "Initialization failed");
		}
		return false;
	}

	if (mysql_real_connect(mysql, 
							info->host, 
							info->user, 
							info->pass, 
							info->database,
							info->port,
							NULL,
							0) == NULL)
	{
		if (errcode)
			*errcode = mysql_errno(mysql);
		if (error && maxlength)
			snprintf(error, maxlength, "%s", mysql_error(mysql));
		return false;
	}

	MysqlDatabase *pMysql = new MysqlDatabase(mysql, this);

	return static_cast<IDatabase *>(pMysql);
}
