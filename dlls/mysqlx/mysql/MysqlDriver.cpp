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
	return _Connect(info, errcode, error, maxlength, false);
}

IDatabase *MysqlDriver::Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength)
{
	return _Connect(info, errcode, error, maxlength, true);
}

IDatabase *MysqlDriver::_Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength, bool do_timeout)
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
		return NULL;
	}

	if (do_timeout && info->max_timeout)
	{
		mysql_options(mysql, MYSQL_OPT_CONNECT_TIMEOUT, (const char *)&(info->max_timeout));
	}

	if (mysql_real_connect(mysql, 
							info->host, 
							info->user, 
							info->pass, 
							info->database,
							info->port,
							NULL,
							CLIENT_MULTI_STATEMENTS) == NULL)
	{
		if (errcode)
		{
			*errcode = mysql_errno(mysql);
		}
		if (error && maxlength)
		{
			snprintf(error, maxlength, "%s", mysql_error(mysql));
		}
		return NULL;
	}

	MysqlDatabase *pMysql = new MysqlDatabase(mysql, this);

	return static_cast<IDatabase *>(pMysql);
}

int MysqlDriver::QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize)
{
	unsigned long size = static_cast<unsigned long>(strlen(str));
	unsigned long needed = size*2 + 1;

	if (maxlen < needed)
	{
		return (int)needed;
	}

	needed = mysql_escape_string(buffer, str, size);
	if (newsize)
	{
		*newsize = static_cast<size_t>(needed);
	}

	return 0;
}

