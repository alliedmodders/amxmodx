/* SQL Database API
 * By the AMX Mod X Development Team
 */

#if defined _dbi_included
  #endinput
#endif
#define _dbi_included

native dbi_connect(host[], user[], pass[], dbname[], error[], maxlength);

native dbi_query(sql, query[], {Float,_}:...);

native dbi_nextrow(sql);

native dbi_getfield(sql, fieldnum, dest[], maxlen);

native dbi_close(sql);

native dbi_error(sql, error[], len);