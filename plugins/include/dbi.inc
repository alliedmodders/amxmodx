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

/* This will return a number equal to or below 0 on failure.
 * If it does fail, the error will be mirrored in dbi_error()
 * The return value will otherwise be a resource handle, not an
 * OK code or cell pointer.
 */
native dbi_connect(_host[], _user[], _pass[], _dbname[], _error[]="", _maxlength=0);

/* This will do a simple query execution on the SQL server.
 * If it fails, it will return a number BELOW ZERO (0)
 * If zero, it succeeded with NO RETURN RESULT.
 * If greater than zero, make sure to call dbi_free_result() on it!
 */
native dbi_query(_sql, _query[], {Float,_}:...);

/* Returns 0 on failure or End of Results.
 * Advances result pointer by one row.
 */
native dbi_nextrow(_sql, _result=1);

/* Gets a field by number.  Returns 0 on failure.
 * Although internally fields always start from 0, 
 * This function takes fieldnum starting from 1.
 */
native dbi_field(_sql, _result, _fieldnum, _dest[], _maxlen);

/* Returns a field by number, as a number.
 */
native dbi_field_num(_sql, _result, _fieldnum);

/* Gets a field by name.  Returns 0 on failure.
 * Although internally fields always start from 0, 
 * This function takes fieldnum starting from 1.
 */
native dbi_result(_sql, _result, _fieldnum, _dest[], _maxlen);

/* Returns a field by name, as a number.
 */
native dbi_result_num(_sql, _result, _fieldnum);

/* Returns the number of rows returned from a query
 */
native dbi_num_rows(_sql, _result);

/* Frees memory used by a result handle.  Do this or get memory leaks.
 */
native dbi_free_result(_sql, _result);

/* Closes a database handle.  Internally, it will also 
 * mark the handle as free, so this particular handle may
 * be re-used in the future to save time.
 */
native dbi_close(_sql);

/* Returns an error message set.  For PGSQL and MySQL,
 * this is a direct error return from the database handle/API.
 * For MSSQL, it returns the last error message found from a 
 * thrown exception.
 */
native dbi_error(_sql, _error[], _len);

/* Returns the type of database being used.  So far:
 * "mysql", "pgsql", "mssql"
 */
native dbi_type(_type[], _len);

/* Backwards compatibility */
stock dbi_getfield(_sql, _fieldnum, _dest[], _maxlen)
{
  return dbi_field(_sql, 1, _fieldnum, _dest, _maxlen)
}