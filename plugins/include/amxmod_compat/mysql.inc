
#if defined _mysql_included
  #endinput
#endif
#define _mysql_included

#include <sqlx>

native mysql_connect(host[], user[], pass[], dbname[], error[], maxlength);
native mysql_query(sql, query[], {Float,_}:... );
native mysql_error(sql, dest[], maxlength);
native mysql_close(sql);
native mysql_nextrow(sql);
native mysql_getfield(sql, fieldnum, {Float,_}:... );
native mysql_getresult(sql, field[], {Float,_}:... );
native mysql_affected_rows(sql);
native mysql_num_fields(sql);
native mysql_num_rows(sql);
native mysql_field_name(sql, field, name[], length);
native mysql_insert_id(sql);
