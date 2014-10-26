// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// MySQL Module
//

#include "amxxmodule.h"
#include "MysqlDriver.h"
#include "MysqlDatabase.h"

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

		if (error && maxlength > 0)
			UTIL_Format(error, maxlength, "Initialization failed");

		return NULL;
	}

	if (do_timeout && info->max_timeout > 0)
		mysql_options(mysql, MYSQL_OPT_CONNECT_TIMEOUT, (const char *)&(info->max_timeout));

	/** Have MySQL automatically reconnect if it times out or loses connection.
	 * This will prevent "MySQL server has gone away" errors after a while.
	 * This is currently used for SourceMod.
	 */
	my_bool my_true = true;
	mysql_options(mysql, MYSQL_OPT_RECONNECT, (const char *)&my_true);

	if (mysql_real_connect(mysql, info->host, info->user, info->pass, info->database, info->port, \
		NULL, CLIENT_MULTI_STATEMENTS) == NULL)
	{
		if (errcode)
			*errcode = mysql_errno(mysql);

		if (error && maxlength > 0)
			UTIL_Format(error, maxlength, "%s", mysql_error(mysql));

		return NULL;
	}

	MysqlDatabase *pMysql = new MysqlDatabase(mysql, this);

	if (info->charset && *info->charset)
		pMysql->SetCharacterSet(info->charset);

	return static_cast<IDatabase *>(pMysql);
}

int MysqlDriver::QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize)
{
	unsigned long size = static_cast<unsigned long>(strlen(str));
	unsigned long needed = size*2 + 1;

	if (maxlen < needed)
		return (int)needed;

	needed = mysql_escape_string(buffer, str, size);

	if (newsize)
		*newsize = static_cast<size_t>(needed);

	return 0;
}
