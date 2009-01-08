/**
 * SQLX - Newer version of SQL stuff
 */

#if defined _sqlx_included
  #endinput
#endif
#define _sqlx_included

//eh..
#define SQL_NumRows SQL_NumResults

#if AMXX_VERSION_NUM >= 175
 #pragma reqclass sqlx
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma defclasslib sqlx mysql
 #endif //!defined AMXMODX_NOAUTOLOAD
#endif //AMXX_VERSION_NUM

enum Handle
{
   Empty_Handle
};

/**
 * Creates a connection information tuple.
 * This tuple must be passed into connection routines.
 * Freeing the tuple is not necessary, but is a good idea if you 
 *  create many of them.  You can cache these handles globally.
 * !!NOTE!! I have seen most people think that this connects to the DB.
 *   Nowhere does it say this, and in fact it does not.  It only caches
 *   the connection information, the host/user/pass/etc.
 *
 * The optional timeout parameter specifies how long connections should wait before
 * giving up.  If 0, the default (which is undefined) is used.
 *
 */
native Handle:SQL_MakeDbTuple(const host[], const user[], const pass[], const db[], timeout=0);


/**
 * Frees an SQL handle.
 * The handle can be to anything (tuple, connection, query, results, etc).
 * If you free a database connection, it closes the connection as well.
 */
native SQL_FreeHandle(Handle:h);


/**
 * Opens a database connection.
 * Returns an SQL handle, which must be freed.
 * Returns Empty_Handle on failure.
 */
native Handle:SQL_Connect(Handle:cn_tuple, &errcode, error[], maxlength);


/**
 * Prepares a query.
 * The query must always be freed.
 * This does not actually do the query!
 */
native Handle:SQL_PrepareQuery(Handle:db, const fmt[], any:...);


/**
 * Back-quotes characters in a string for database querying.
 * Note: The buffer's maximum size should be 2*strlen(string) to catch
 * all scenarios.
 *
 * @param db				Database handle for localization, or Empty_Handle 
 *                          for when a handle is not available.
 * @param buffer			Buffer to copy to.
 * @param buflen			Maximum size of the buffer.
 * @param string			String to backquote (should not overlap buffer).
 * @return					Length of new string, or -1 on failure.
 */
native SQL_QuoteString(Handle:db, buffer[], buflen, const string[]);

/**
 * Back-quotes characters in a string for database querying.
 * Note: The buffer's maximum size should be 2*strlen(string) to catch
 * all scenarios.
 *
 * @param db				Database handle for localization, or Empty_Handle 
 *                          for when a handle is not available.
 * @param buffer			Buffer to copy to.
 * @param buflen			Maximum size of the buffer.
 * @param fmt				Format of string to backquote (should not overlap buffer).
 * @param ...				Format arguments.
 * @return					Length of new string, or -1 on failure.
 */
native SQL_QuoteStringFmt(Handle:db, buffer[], buflen, const fmt[], any:...);


#define TQUERY_CONNECT_FAILED	-2
#define TQUERY_QUERY_FAILED	-1
#define TQUERY_SUCCESS		0
/**
 * Prepares and executes a threaded query.
 * This will not interrupt gameplay in the event of a poor/lossed 
 *  connection, however, the interface is more complicated and 
 *  asynchronous.  Furthermore, a new connection/disconnection is 
 *  made for each query to simplify driver support.
 * The handler should look like:
 *
 * @param failstate - One of the three TQUERY_ defines.
 * @param query - Handle to the query, do not free it.
 * @param error - An error message, if any.
 * @param errnum - An error code, if any.
 * @param data - Data array you passed in.
 * @param size - Size of the data array you passed in.
 * @param queuetime - Amount of gametime that passed while the query was resolving.
 * 
 * public QueryHandler(failstate, Handle:query, error[], errnum, data[], size, Float:queuetime)
 *
 * Note! The handle you pass in is a DB Tuple, NOT an active connection!
 * Note! The handle does not need to be freed.
 * Also note: This function is not guaranteed to be in another thread
 *  (in fact - it's not).  You're seeing data "after the fact", 
 *  and as such to execute another query you should run 
 *  SQL_ThreadQuery again with new data.
 */
native SQL_ThreadQuery(Handle:db_tuple, const handler[], const query[], const data[]="", dataSize=0);


/**
 * Executes a query.
 * Returns 1 if the query succeeded.
 * Returns 0 if the query failed.
 * NOTE: You can call this multiple times as long as its parent
 *  connection is kept open.  Each time the result set will be freed
 *  from the previous call.
 */
native SQL_Execute(Handle:query);

/**
 * Gets information about a failed query error.
 * Returns the errorcode.
 */
native SQL_QueryError(Handle:query, error[], maxlength);


/**
 * Returns 1 if there are more results to be read,
 *  0 otherwise.
 */
native SQL_MoreResults(Handle:query);


/**
 * Tells whether a specific column in the current row
 *  is NULL or not.
 */
native SQL_IsNull(Handle:query, column);

/**
 * Retrieves the current result.
 * A successful query starts at the first result,
 *  so you should not call SQL_NextRow() first.
 * Passing no extra params - return int
 * Passing one extra param - return float in 1st extra arg
 * Passing two extra params - return string in 1st arg, max length in 2nd
 * Example:
 *  new num = SQL_ReadResult(query, 0)
 *  new Float:num2
 *  new str[32]
 *  SQL_ReadResult(query, 1, num2)
 *  SQL_ReadResult(query, 2, str, 31)
 */
native SQL_ReadResult(Handle:query, column, {Float,_}:...);


/**
 * Advances to the next result (return value should be ignored).
 */
native SQL_NextRow(Handle:query);


/** 
 * Returns the number of affected rows.
 */
native SQL_AffectedRows(Handle:query);


/**
 * Returns the number of rows total.
 */
native SQL_NumResults(Handle:query);


/**
 * Returns the number of columns total.
 */
native SQL_NumColumns(Handle:query);


/** 
 * Returns the name of a column.
 * Errors on a bad field number.
 */
native SQL_FieldNumToName(Handle:query, num, name[], maxlength);


/**
 * Returns the number of a named column, or -1 if not found.
 */
native SQL_FieldNameToNum(Handle:query, const name[]);


/**
 * Rewinds a result set to the first row.
 */
native SQL_Rewind(Handle:query);


/**
 * Returns the insert id of the last INSERT query. 
 * Returns 0 otherwise.
 */
native SQL_GetInsertId(Handle:query);


/**
 * Returns which driver this plugin is currently bound to.
 */
native SQL_GetAffinity(driver[], maxlen);

/**
 * Sets driver affinity.  You can use this to force a particular 
 *  driver implementation.  This will automatically change all SQL
 *  natives in your plugin to be "bound" to the module in question.
 * If no such module is found, it will return 0.  This isn't necessarily bad - 
 *  the user might have typed the wrong driver.  Unless your plugin is built
 *  to handle different driver types at once, you should let this error pass.
 * Note, that using this while you have open handles to another database 
 *  type will cause problems.  I.e., you cannot open a handle, switch
 *  affinity, then close the handle with a different driver.
 * Switching affinity is an O(n*m) operation, where n is the number of
 *  SQL natives and m is the number of used natives in total.
 * Intuitive programmers will note that this causes problems for threaded queries.
 *  You will have to either force your script to work under one affinity, or to 
 *  pack the affinity type into the query data, check it against the current, then
 *  set the new affinity if necessary.  Then, restore the old for safety.
 */
native SQL_SetAffinity(const driver[]);

/**
 * Returns the original query string that a query handle used.
 */
native SQL_GetQueryString(Handle:query, buffer[], maxlength);

/**
 * For queries which return multiple result sets, this advances to the next 
 * result set if one is available.  Otherwise, the current result set is 
 * destroyed and will no longer be accessible.
 *
 * This function will always return false on SQLite, and when using threaded 
 * queries in MySQL.  Nonetheless, it has the same effect of removing the last 
 * result set.
 *
 * @param query		Query Handle.
 * @return			True on success, false on failure.
 */
native bool:SQL_NextResultSet(Handle:query);

/**
 * This function can be used to find out if a table in a Sqlite database exists.
 * (updated for newer API)
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
 * Returns 0 on failure, 1 on success
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
 * Returns 0 on failure, 1 on success
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
 * Returns -1 on error, >=0 on success (with number of affected rows)
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

stock Handle:SQL_MakeStdTuple(timeout = 0)
{
	static host[64], user[32], pass[32], db[128];
	static get_type[12], set_type[12];
	
	get_cvar_string("amx_sql_host", host, 63);
	get_cvar_string("amx_sql_user", user, 31);
	get_cvar_string("amx_sql_pass", pass, 31);
	get_cvar_string("amx_sql_type", set_type, 11);
	get_cvar_string("amx_sql_db", db, 127);
	
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
