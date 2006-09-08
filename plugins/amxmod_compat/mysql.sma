/**
 * AMX Mod Compatibility engine
 *  by the AMX Mod X Development Team
 */
 
#define MAX_CONNECTIONS		64

new Connections[MAX_CONNECTIONS+1] = {0}
new ConnectionTracker[MAX_CONNECTIONS+1] = {0}
new ConnectionErrors[MAX_CONNECTIONS+1][255]
new ConnectionQueries[MAX_CONNECTIONS+1] = {0}
new QueryPositions[MAX_CONNECTIONS+1]

MySQL_Natives()
{
	register_native("mysql_connect", 		"__mysql_connect")
	register_native("mysql_query",			"__mysql_query")
	register_native("mysql_error",			"__mysql_error")
	register_native("mysql_close",			"__mysql_close")
	register_native("mysql_nextrow",		"__mysql_nextrow")
	register_native("mysql_getfield",		"__mysql_getfield")
	register_native("mysql_getresult",		"__mysql_getresult")
	register_native("mysql_affected_rows",	"__mysql_affected_rows")
	register_native("mysql_num_fields",		"__mysql_num_fields")
	register_native("mysql_num_rows",		"__mysql_num_rows")
	register_native("mysql_field_name",		"__mysql_field_name")
	register_native("mysql_insert_id",		"__mysql_insert_id")
}

MakeConnectionIndex(Handle:cn)
{
	if (ConnectionTracker[0])
	{
		new idx = ConnectionTracker[ConnectionTracker[0]]
		ConnectionTracker[0]--
		Connections[idx] = _:cn
		return idx
	} else {
		Connections[0]++
		if (Connections[0] > MAX_CONNECTIONS)
		{
			return 0
		}
		
		Connections[Connections[0]] = _:cn
		return Connections[0]
	}
	
	return 0
}

Handle:GetConnectionIndex(idx)
{
	if (idx < 1 || idx > MAX_CONNECTIONS || !Connections[idx])
	{
		return Empty_Handle
	}
	
	return Handle:Connections[idx]
}

FreeConnectionIndex(idx)
{
	Connections[idx] = 0
	ConnectionTracker[0]++
	ConnectionTracker[ConnectionTracker[0]] = idx
	ConnectionErrors[idx][0] = 0
	ConnectionQueries[idx] = 0
	QueryPositions[idx] = 0
}

/*
 * Unlike the previous this does not check for a matching connection.
 * Unless a plugin breaks I'm not going to take that step.
 */

public __mysql_connect(plid, num)
{
	static host[255], user[128], pass[128], dbname[128], error[512]
	new errcode

	get_string(1, host, 254)
	get_string(2, user, 127)
	get_string(3, pass, 127)
	get_string(4, dbname, 127)
	
	new Handle:info = SQL_MakeDbTuple(host, user, pass, dbname)
	new Handle:cn = SQL_Connect(info, errcode, error, 511)
	
	if (cn == Empty_Handle)
	{
		set_string(5, error, get_param(6))
		return 0
	}
	
	SQL_FreeHandle(info)
	
	new idx = MakeConnectionIndex(cn)
	if (idx == 0)
	{
		set_string(5, "Reached max unclosed connections", get_param(6))
		return 0
	}
	
	ConnectionQueries[idx] = 0
	
	return idx
}

public __mysql_query(plid, num)
{
	static queryString[4096]
	new cn_idx = get_param(1)
	new Handle:cn
	
	if ((cn=GetConnectionIndex(cn_idx)) == Empty_Handle)
	{
		return 0
	}
	
	vdformat(queryString, 4095, 2, 3)
	
	new Handle:query = SQL_PrepareQuery(cn, "%s", queryString)
	
	if (!SQL_Execute(query))
	{
		SQL_QueryError(query, ConnectionErrors[cn_idx], 254)
		SQL_FreeHandle(query)
		return 0
	}
	
	if (ConnectionQueries[cn_idx])
	{
		SQL_FreeHandle(Handle:ConnectionQueries[cn_idx])
	}
	
	ConnectionQueries[cn_idx] = _:query
	QueryPositions[cn_idx] = 0
	
	return 1
}

public __mysql_error(plid, num)
{
	new cn_idx = get_param(1)
	
	if (Connections[cn_idx] < 1)
	{
		static error[255]
		format(error, 254, "Invalid connection index: %d", cn_idx)
		set_string(2, error, get_param(3))
		return 1
	}
	
	set_string(2, ConnectionErrors[cn_idx], get_param(3))
	
	return 1
}

public __mysql_close(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	
	if (query != Empty_Handle)
	{
		SQL_FreeHandle(query)
	}
	
	SQL_FreeHandle(cn)

	FreeConnectionIndex(cn_idx)	
	
	return 1
}

public __mysql_nextrow(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	if (QueryPositions[cn_idx] != 0)
	{
		SQL_NextRow(query)
	}
	
	if (SQL_MoreResults(query))
	{
		return ++QueryPositions[cn_idx]
	}
	
	return 0
}

public __mysql_getresult(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	if (!SQL_MoreResults(query))
	{
		return 0
	}
	
	static name[64]
	get_string(2, name, 63)
	new column = SQL_FieldNameToNum(query, name)
	if (column == -1)
	{
		log_error(AMX_ERR_NATIVE, "Invalid column name: %s", name)
		return 0
	}
	
	switch (num)
	{
		case 2:
		{
			return SQL_ReadResult(query, column)
		}
		case 3:
		{
			new Float:fma
			SQL_ReadResult(query, column, fma)
			set_param_byref(3, _:fma)
		}
		case 4:
		{
			static str[2048]
			SQL_ReadResult(query, column, str, 2047)
			set_string(3, str, get_param_byref(4))
		}
	}
	
	return 1
}

public __mysql_getfield(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	if (!SQL_MoreResults(query))
	{
		return 0
	}
	
	switch (num)
	{
		case 2:
		{
			return SQL_ReadResult(query, get_param(2)-1)
		}
		case 3:
		{
			new Float:fma
			SQL_ReadResult(query, get_param(2)-1, fma)
			set_param_byref(3, _:fma)
		}
		case 4:
		{
			static str[2048]
			SQL_ReadResult(query, get_param(2)-1, str, 2047)
			set_string(3, str, get_param_byref(4))
		}
	}
	
	return 1
}

public __mysql_affected_rows(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	return SQL_AffectedRows(query)
}

public __mysql_num_fields(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	return SQL_NumColumns(query)
}

public __mysql_insert_id(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	return SQL_GetInsertId(query)
}

public __mysql_num_rows(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	return SQL_NumResults(query)
}

public __mysql_field_name(plid, num)
{
	new cn_idx = get_param(1)
	
	new Handle:cn = GetConnectionIndex(cn_idx)
	if (cn == Empty_Handle)
	{
		return 0
	}
	
	new Handle:query = Handle:ConnectionQueries[cn_idx]
	if (query == Empty_Handle)
	{
		return 0
	}
	
	new column = get_param(2) - 1
	if (column < 0 || column >= SQL_NumColumns(query))
	{
		return 0
	}
	
	new field[64]
	SQL_FieldNumToName(query, column, field, 63)
	
	set_string(3, field, get_param(4))
	
	return 1
}
