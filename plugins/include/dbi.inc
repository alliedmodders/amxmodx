/* SQL Database API
 * By the AMX Mod X Development Team
 * Notes - Read the comments! Make sure your plugins use 
 *  nice ANSI SQL and don't use database column names like "key"
 *  otherwise this API will be a nightmare
 * Never do error checking with the not operator! This is bad:
 *   if (!dbi_query())
 * You should do:
 *   ret = dbi_query()
 *   if (ret < 0)
 * This is because DBI functions can and will return negative numbers
 * Negative numbers evaluate to "true" in AMX.
 */

#if defined _dbi_included
  #endinput
#endif
#define _dbi_included

enum Sql
{
	SQL_FAILED=0,
	SQL_OK
}

enum Result
{
	RESULT_FAILED=-1,
	RESULT_NONE,
	RESULT_OK
}

/* This will return a number equal to or below 0 on failure.
 * If it does fail, the error will be mirrored in dbi_error()
 * The return value will otherwise be a resource handle, not an
 * OK code or cell pointer.
 */
native Sql:dbi_connect(_host[], _user[], _pass[], _dbname[], _error[]="", _maxlength=0);

/* This will do a simple query execution on the SQL server.
 * If it fails, it will return a number BELOW ZERO (0)
 * If zero, it succeeded with NO RETURN RESULT.
 * If greater than zero, make sure to call dbi_free_result() on it!
 *  The return is a handle to the result set
 */
native Result:dbi_query(Sql:_sql, _query[], {Float,_}:...);

/* Returns 0 on failure or End of Results.
 * Advances result pointer by one row.
 */
native dbi_nextrow(Result:_result);

/* Gets a field by number.  Returns 0 on failure.
 * Although internally fields always start from 0, 
 * This function takes fieldnum starting from 1.
 * No extra params: returns int
 * One extra param: returns Float: byref
 * Two extra param: Stores string with length
 */
native dbi_field(Result:_result, _fieldnum, {Float,_}:... );

/* Gets a field by name.  Returns 0 on failure.
 * One extra param: returns Float: byref
 * Two extra param: Stores string with length
 */
native dbi_result(Result:_result, _field[], {Float,_}:... );

/* Returns the number of rows returned from a query
 */
native dbi_num_rows(Result:_result);

/* Frees memory used by a result handle.  Do this or get memory leaks.
 */
native dbi_free_result(&Result:result);

/* Closes a database handle.  Internally, it will also 
 * mark the handle as free, so this particular handle may
 * be re-used in the future to save time.
 */
native dbi_close(&Sql:_sql);

/* Returns an error message set.  For PGSQL and MySQL,
 * this is a direct error return from the database handle/API.
 * For MSSQL, it returns the last error message found from a 
 * thrown exception.
 */
native dbi_error(Sql:_sql, _error[], _len);

/* Returns the type of database being used.  So far:
 * "mysql", "pgsql", "mssql", "sqlite"
 */
native dbi_type(_type[], _len);

/* This function can be used to found out if a table in a Sqlite database exists.
 */
stock bool:sqlite_table_exists(Sql:sql, table[]) {
	new bool:exists
	new query[128]
	format(query, 127, "SELECT name FROM sqlite_master WHERE type='table' AND name='%s' LIMIT 1;", table)

	new Result:result = dbi_query(sql, query)

	if (dbi_nextrow(result))
		exists = true
	else
		exists = false

	if (result > RESULT_NONE)
		dbi_free_result(result)

	return exists
}