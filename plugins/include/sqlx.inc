// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLX - Newer SQL Database API
//

#if defined _sqlx_included
	#endinput
#endif
#define _sqlx_included

//eh..
#define SQL_NumRows SQL_NumResults

#pragma reqclass sqlx
#if !defined AMXMODX_NOAUTOLOAD
	#pragma defclasslib sqlx mysql
#endif

enum Handle
{
   Empty_Handle
};

/**
 * Creates a connection information tuple. This tuple must be passed
 * into connection routines.
 * 
 * @note Freeing the tuple is not necessary, but is a good idea if you create
 *       many of them. You can cache these handles globally.
 * @note This does not connect to the DB; it only caches the connection information.
 * 
 * @param host          Database host
 * @param user          Database user
 * @param pass          Database password
 * @param db            Database name to use
 * @param timeout       Specifies how long connections should wait before giving up.
 *                      If <= 0, the default of 60s is used.
 * 	
 * @return              A newly created tuple handle to be used in connection routines.
 */
native Handle:SQL_MakeDbTuple(const host[], const user[], const pass[], const db[], timeout=0);


/**
 * Frees an SQL handle.
 * 
 * @note The handle can be to anything (tuple, connection, query, results, etc).
 * @note If you free the database connection handle, it closes the connection as well.
 * 
 * @param h             Handle to be freed.
 * 	
 * @noreturn
 */
native SQL_FreeHandle(Handle:h);


/**
 * Opens a database connection.
 * 
 * @param cn_tuple      Tuple handle, returned from SQL_MakeDbTuple().
 * @param errcode       An error code set by reference.
 * @param error         String where error string will be stored.
 * @param maxlength     Maximum length of the error buffer.
 * 	
 * @return              Returns an SQL connection handle, which must be freed.
 *                      Returns Empty_Handle on failure.
 * @error               Invalid info tuple handle.
 */
native Handle:SQL_Connect(Handle:cn_tuple, &errcode, error[], maxlength);


/**
 * Sets the character set of the current connection. 
 * Like SET NAMES .. in mysql, but stays after connection problems.
 * 
 * @note If a connection tuple is supplied, this should be called before SQL_Connect or SQL_ThreadQuery.
 * @note The change will remain until you call this function with another value.
 * @note This native does nothing in SQLite. 
 *
 * Example: "utf8", "latin1"
 *
 * @param h             Database or connection tuple Handle.
 * @param charset       The character set string to change to.
 * 
 * @return              True, if character set was changed, false otherwise.
 */
native bool:SQL_SetCharset(Handle:h, const charset[]);


/**
 * Prepares a query.
 * 
 * @note This does not actually do a query!
 * 
 * @param db            Connection handle, returned from SQL_Connect().
 * @param fmt           Query string. Can be formated with format specifiers.
 * @param ...           Additional format specifiers used to format the query.
 * 
 * @return              Returns an SQL query handle, which must always be freed.
 *                      Returns Empty_Handle on failure.
 */
native Handle:SQL_PrepareQuery(Handle:db, const fmt[], any:...);


/**
 * Back-quotes characters in a string for database querying.
 * 
 * @note The buffer's maximum size should be 2*strlen(string) to catch all scenarios.
 *
 * @param db            Database handle for localization, or Empty_Handle 
 *                      for when a handle is not available.
 * @param buffer        Buffer to copy to.
 * @param buflen        Maximum size of the buffer.
 * @param string        String to backquote (should not overlap buffer).
 * 
 * @return              Length of new string, or -1 on failure.
 * @error               Invalid database handle.
 */
native SQL_QuoteString(Handle:db, buffer[], buflen, const string[]);

/**
 * Back-quotes characters in a string for database querying.
 * Note: The buffer's maximum size should be 2*strlen(string) to catch
 * all scenarios.
 *
 * @param db            Database handle for localization, or Empty_Handle 
 *                      for when a handle is not available.
 * @param buffer        Buffer to copy to.
 * @param buflen        Maximum size of the buffer.
 * @param fmt           Format of string to backquote (should not overlap buffer).
 * @param ...           Format arguments.
 * 
 * @return              Length of new string, or -1 on failure.
 */
native SQL_QuoteStringFmt(Handle:db, buffer[], buflen, const fmt[], any:...);


/**
 * Threaded query states. Used to check the state of a complete threaded query.
 */
#define TQUERY_CONNECT_FAILED	-2
#define TQUERY_QUERY_FAILED	-1
#define TQUERY_SUCCESS		0

/**
 * Prepares and executes a threaded query.
 * @note The handler should look like:
 *       public QueryHandler(failstate, Handle:query, error[], errnum, data[], size, Float:queuetime)
 *       failstate - One of the three TQUERY_ defines.
 *       query     - Handle to the query, do not free it.
 *       error     - An error message, if any.
 *       errnum    - An error code, if any.
 *       data      - Data array you passed in.
 *       size      - Size of the data array you passed in.
 *       queuetime - Amount of gametime that passed while the query was resolving.
 * @note This will not interrupt gameplay in the event of a poor/lossed 
 *       connection, however, the interface is more complicated and 
 *       asynchronous. Furthermore, a new connection/disconnection is 
 *       made for each query to simplify driver support.
 * @note The handle does not need to be freed.
 *
 * @param db_tuple      Tuple handle, returned from SQL_MakeDbTuple().
 * @param handler       A function to be called when the query finishes. It has to be public.
 * @param query         The query string.
 * @param data          Additional data array that will be passed to the handler function.
 * @param dataSize      The size of the additional data array. 
 *
 * @noreturn
 * @error               Thread worker was unable to start.
 *                      Invalid info tuple handle.
 *                      Handler function not found.
 */
native SQL_ThreadQuery(Handle:db_tuple, const handler[], const query[], const data[]="", dataSize=0);


/**
 * Executes an already prepared query.
 * 
 * @note You can call this multiple times as long as its parent connection is kept open. 
 *       Each time the result set from the previous call will be freed.
 *
 * @param query         Handle of a prepared query to be executed.
 * 
 * @return              1 if the query succeeded, 0 if the query failed.
 * @error               Invalid query handle.
 */
native SQL_Execute(Handle:query);


/**
 * Gets information about a failed query error.
 *
 * @param query         Handle of a query to extract the error from.
 * @param error         Buffer where to store the error string.
 * @param maxlength     The maximum length of the output buffer.
 * 
 * @return              The error code.
 */
native SQL_QueryError(Handle:query, error[], maxlength);


/**
 * Checks whether there are more results to be read.
 *
 * @param query         Handle of a query to check.
 * 
 * @return              1 if there are more results, 0 otherwise.
 * @error               Invalid query handle.
 */
native SQL_MoreResults(Handle:query);


/**
 * Tells whether a specific column in the current row is NULL or not.
 *
 * @param query         Handle of a query to check.
 * @param column        Which column to check for NULL.
 * 
 * @return              1 if the column is NULL, 0 otherwise.
 * @error               Invalid query handle.
 *                      No result set in this query.
 *                      Invalid column.
 */
native SQL_IsNull(Handle:query, column);


/**
 * Retrieves the current result.
 * 
 * @note A successful query starts at the first result, so you should not call
 *       SQL_NextRow() first.
 * 
 * @note Example how to get different types of values:
 *         new num = SQL_ReadResult(query, 0)
 *         new Float:num2
 *         new string[32]
 *         SQL_ReadResult(query, 1, num2)
 *         SQL_ReadResult(query, 2, string, charsmax(string))
 *
 * @param query         Handle of a query to read results from.
 * @param column        Which column to get the value from.
 * @param ...           Passing no extra arguments - returns an integer.
 *                      Passing one extra argument - returns a float in the first extra argument
 *                      Passing two extra params - returns a string in the first argument
 *                      with a maximum string length in the second argument.
 * 
 * @return              If no extra arguments are passed, returns an integer value.
 * @error               Invalid query handle.
 */
native SQL_ReadResult(Handle:query, column, any:...);


/**
 * Advances to the next result (row).
 *
 * @param query         Handle of a query.
 * 
 * @noreturn
 * @error               Invalid query handle.
 *                      No result set in this query.
 */
native SQL_NextRow(Handle:query);


/**
 * Returns the number of affected rows by a query.
 *
 * @param query         Handle of a query to check.
 * 
 * @return              The number of affected rows.
 * @error               Invalid query handle.
 */
native SQL_AffectedRows(Handle:query);


/**
 * The number of retrieved rows (results) after a query.
 *
 * @param query         Handle of a query to check.
 * 
 * @return              The number of retrieved rows by the query.
 * @error               Invalid query handle.
 */
native SQL_NumResults(Handle:query);


/**
 * Returns the total number of columns.
 *
 * @param query         Handle of a query to check.
 * 
 * @return              The number of retrieved columns by the query.
 * @error               Invalid query handle.
 *                      No result set in this query.    
 */
native SQL_NumColumns(Handle:query);


/**
 * Retrieves the name of a column by its index.
 *
 * @param query         Handle of a query.
 * @param num           The number (index) of a column to retrieve the name from.
 * @param name          Buffer where to store the column's name.
 * @param maxlength     Maximum length of the output buffer.
 * 
 * @noreturn
 * @error               Invalid query handle.
 *                      No result set in this query.
 *                      Invalid column index.
 */
native SQL_FieldNumToName(Handle:query, num, name[], maxlength);


/**
 * Retrieves the number of a named column.
 *
 * @param query         Handle of a query.
 * @param name          Name to search for.
 * 
 * @return              Column index if found (>= 0); -1 otherwise.
 * @error               Invalid query handle.
 *                      No result set in this query.
 */
native SQL_FieldNameToNum(Handle:query, const name[]);


/**
 * Rewinds a result set to the first row.
 *
 * @param query         Handle of a query to rewind the result set of.
 * 
 * @noreturn
 * @error               Invalid query handle.
 *                      No result set in this query.   
 */
native SQL_Rewind(Handle:query);


/**
 * Retrieves the instert ID of the latest INSERT query.
 *
 * @param query         Handle of a query.
 * 
 * @return              The insert ID of the latest INSERT query.
 * @error               Invalid query handle.
 */
native SQL_GetInsertId(Handle:query);


/**
 * Retrieves which driver is this plugin currently bound to.
 *
 * @param driver        Buffer to store the driver name in.
 * @param maxlen        Maximum length of the output buffer.
 * 
 * @noreturn
 */
native SQL_GetAffinity(driver[], maxlen);


/**
 * Sets driver affinity. You can use this to force a particular driver implementation.
 * This will automatically change all SQL natives in your plugin to be "bound" to
 * the module in question.
 * 
 * @note Using this while you have open handles to another database type will
 *       cause problems. I.e., you cannot open a handle, switch affinity,
 *       then close the handle with a different driver.
 * @note Switching affinity is an O(n * m) operation, where n is the number of
 *       SQL natives and m is the number of used natives in total.
 * @note Intuitive programmers will note that this causes problems for
 *       threaded queries. You will have to either force your script to work
 *       under one affinity, or to pack the affinity type into the query data,
 *       check it against the current, then set the new affinity if necessary.
 *       Then, restore the old one for safety.
 *
 * @param driver        The name of a driver to use.
 * 
 * @return              If no module with the given name is found, returns 0.
 *                      Unless your plugin is bult to handle different driver
 *                      types at once, you should let this error pass.
 */
native SQL_SetAffinity(const driver[]);

/**
 * Returns the original query string that a query handle used.
 *
 * @param query         Handle of a query.
 * @param buffer        Buffer where to put the query string in.
 * @param maxlength     The maximum length of the output buffer.
 * 
 * @noreturn
 * @error               Invalid query handle.
 */
native SQL_GetQueryString(Handle:query, buffer[], maxlength);

/**
 * For queries which return multiple result sets, this advances to the next 
 * result set if one is available.  Otherwise, the current result set is 
 * destroyed and will no longer be accessible.
 *
 * @note This function will always return false on SQLite, and when using threaded
 *       queries in MySQL.  Nonetheless, it has the same effect of removing the last
 *       result set.
 *
 * @param query         Query Handle.
 * 
 * @return              True on success, false on failure.
 * @error               Invalid query handle.
 *                      No result set in this query.
 */
native bool:SQL_NextResultSet(Handle:query);


/**
 * This function can be used to find out if a table in a SQLite database exists.
 *
 * @param db            Connection handle returned from SQL_Connect().
 * @param table         The table name to check for.
 * 
 * @return              True if it exists, false otherwise.
 */
stock bool:sqlite_TableExists(Handle:db, const table[])
{
	new Handle:query = SQL_PrepareQuery(
					db,
					"SELECT name FROM sqlite_master WHERE type='table' AND name='%s' LIMIT 1;", 
					table);
					
	if (!SQL_Execute(query) || !SQL_NumResults(query))
	{
		SQL_FreeHandle(query);
		return false;
	}
	
	SQL_FreeHandle(query);

	return true;
}

/**
 * Use this for executing a query where you don't care about the result.
 *
 * @param db            Connection handle returned from SQL_Connect().
 * @param query         The query string.
 * @param error         If an error occurs, it will be placed into this buffer.
 * @param maxlength     Maximum length of the error buffer.
 * @param rows          Optional. If put, retrieves the number of rows the query returned.
 * 
 * @return              1 on success, 0 on failure.
 */
stock SQL_SimpleQuery(Handle:db, const query[], error[]="", maxlength=0, &rows=0)
{
	new Handle:hQuery = SQL_PrepareQuery(db, "%s", query);
	
	if (!SQL_Execute(hQuery))
	{
		SQL_QueryError(hQuery, error, maxlength);
		SQL_FreeHandle(hQuery);
		return 0;
	}
	
	rows = SQL_NumResults(hQuery);
	
	SQL_FreeHandle(hQuery);
	
	return 1;
}


/**
 * Use this for executing a query where you don't care about the result.
 * 
 * @note Differs from SQL_SimpleQuery() because the query can be formated.
 *
 * @param db            Connection handle returned from SQL_Connect().
 * @param error         If an error occurs, it will be placed into this buffer.
 * @param maxlength     The maximum length of the error buffer.
 * @param rows          Optional. If put, retrieves the number of rows the query returned.
 * @param fmt           The query string that can be formated with format specifiers.
 * @param ...           Additional arguments for formating the query.
 * 
 * @return              1 on success, 0 on failure.
 */
stock SQL_SimpleQueryFmt(Handle:db, error[]="", maxlength=0, &rows=0, const fmt[], any:...)
{
	static query_buf[2048];
	vformat(query_buf, 2047, fmt, 6);
	
	new Handle:hQuery = SQL_PrepareQuery(db, "%s", query_buf);
	
	if (!SQL_Execute(hQuery))
	{
		SQL_QueryError(hQuery, error, maxlength);
		SQL_FreeHandle(hQuery);
		return 0;
	}
	
	rows = SQL_NumResults(hQuery);
	
	SQL_FreeHandle(hQuery);
	
	return 1;
}

/**
 * Use this for executing a query and not caring about the error.
 *
 * @param db            A connection handle returned from SQL_Connect().
 * @param queryfmt      The query string that can be formated with format specifiers.
 * @pram ...            Additional arguments for formating the query.
 * 
 * @return              -1 on error.
 *                      >= 0 on success (with the number of affected rows).
 */
stock SQL_QueryAndIgnore(Handle:db, const queryfmt[], any:...)
{
	static query[4096];
	new Handle:hQuery;
	new ret;
	
	vformat(query, sizeof(query)-1, queryfmt, 3);
	
	hQuery = SQL_PrepareQuery(db, "%s", query);
	
	if (SQL_Execute(hQuery))
	{
		ret = SQL_AffectedRows(hQuery);
	} else {
		ret = -1;
	}
	
	SQL_FreeHandle(hQuery);
	
	return ret;
}

/**
 * Use this for making a standard DB Tuple, using AMXX's database info cvars.
 *
 * @param timeout       Specifies how long connections should wait before giving up.
 *                      If 0, the value is read from "amx_sql_timeout" cvar.
 * 
 * @return              A newly created tuple handle to be used in connection routines.
 */
stock Handle:SQL_MakeStdTuple(timeout = 0)
{
	static host[64], user[32], pass[32], db[128];
	static get_type[12], set_type[12];
	
	get_cvar_string("amx_sql_host", host, 63);
	get_cvar_string("amx_sql_user", user, 31);
	get_cvar_string("amx_sql_pass", pass, 31);
	get_cvar_string("amx_sql_type", set_type, 11);
	get_cvar_string("amx_sql_db", db, 127);

	if (timeout <= 0)
	{
		timeout = get_cvar_num("amx_sql_timeout");
	}
	
	SQL_GetAffinity(get_type, 12);
	
	if (!equali(get_type, set_type))
	{
		if (!SQL_SetAffinity(set_type))
		{
			log_amx("Failed to set affinity from %s to %s.", get_type, set_type);
		}
	}
	
	return SQL_MakeDbTuple(host, user, pass, db, timeout);
}
