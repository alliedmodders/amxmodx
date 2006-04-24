/**
 * SQLX - Newer version of SQL stuff
 */

#if defined _sqlx_included
  #endinput
#endif
#define _sqlx_included

#pragma library sqlx

enum Handle
{
   Empty_Handle
};

/**
 * Creates a connection tuple.
 * This tuple must be passed into connection routines.
 * Freeing the tuple is not necessary, but is a good idea if you 
 *  create many of them.  You can cache these handles globally.
 */
native Handle:SQL_MakeDbTuple(const host[], const user[], const pass[], const db[]);


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
native Handle:SQL_PrepareQuery(Handle:db, const fmt[], {Float,_}:...);


#define TQUERY_CONNECT_FAILED	-2
#define TQUERY_QUERY_FAILED	-1
#define TQUERY_SUCCESS		0
/**
 * Prepares and executes a threaded query.
 * This will not interrupt gameplay in the event of a poor/lossed 
 *  connection, however, the interface is more complicated and 
 *  asynchronous.  Furthermore, a new connection/disconnection is 
 *  made each time.
 * The handler should look like:
 *
 * @param failstate - One of the three TQUERY_ defines.
 * @param query - Handle to the query, do not free it.
 * @param error - An error message, if any.
 * @param errnum - An error code, if any.
 * @param data - Data array you passed in.
 * @param size - Size of the data array you passed in.
 * 
 * public QueryHandler(failstate, Handle:query, error[], errnum, data[], size)
 *
 * Note! The handle does not need to be freed.
 * Also note: This function is not guaranteed to be in another thread
 *  (in fact - it's not).  You're seeing data "after the fact", 
 *  and as such to execute another query you should run 
 *  SQL_ThreadQuery again.
 * 
 */
native SQL_ThreadQuery(Handle:cn_tuple, const handler[], const query[], const data[]="", dataSize=0);


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
native SQL_ReadResult(Handle:query, column, ...);


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
 * Sets driver affinity (NOT IMPLEMENTED).
 */
// native SQL_SetAffinity(Handle:connection, const driver[]);


