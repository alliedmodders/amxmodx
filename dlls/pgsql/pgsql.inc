/* PostgreSQL functions
*
* by David "BAILOPAN" Anderson
* Under the GNU General Public License, version 2
*/

#if defined _pgsql_included
  #endinput
#endif
#define _pgsql_included

/* Opens connection. If already such exists then that will be used.
* Function returns sql id to use with other sql natives.
* Host can be plain ip or hostname, ports are not yet allowed. */
native pgsql_connect(host[],user[],pass[],dbname[]);

/* Uses an existing connection (sql) to perform a new query (query) (might close previous query if any).
   Will return the number of rows found.
 */
native pgsql_query(sql,query[], {Float,_}:...);

/* Advances to the next row in the query set. */
native pgsql_nextrow(sql);

/* Stores specified column (fieldnum) of current query (sql) in (dest) with (maxlength) characters maximum. */
native pgsql_getfield(sql,fieldnum,dest[],maxlength);

/* Clears query (sql) and closes connection (if any other plugin doesn't use it). */
native pgsql_close(sql);

/* Stores last error of current query/connection (sql) in (dest) with (maxlength) characters maximum. */
native pgsql_error(sql,dest[],maxlength);