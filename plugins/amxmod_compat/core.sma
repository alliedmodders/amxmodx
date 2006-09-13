/**
 * AMX Mod Compatibility engine
 *  by the AMX Mod X Development Team
 */

Core_Natives()
{
	/* implicit compatibility */
	register_native("VelocityByAim",		"__VelocityByAim")
	register_native("load_translations",	"__load_translations")
	register_native("is_user_authorized",	"__is_user_authorized")
	register_native("get_user_money",		"__get_user_money")
	register_native("set_user_money",		"__set_user_money")
	register_native("angle_to_vector",		"__angle_to_vector")
	register_native("fabs",					"__fabs")
	register_native("asin",					"__asin")
	register_native("sin",					"__sin")
	register_native("sinh",					"__sinh")
	register_native("acos",					"__acos")
	register_native("cos",					"__cos")
	register_native("cosh",					"__cosh")
	register_native("atan",					"__atan")
	register_native("atan2",				"__atan2")
	register_native("tan",					"__tan")
	register_native("tanh",					"__tanh")
	register_native("fsqroot",				"__fsqroot")
	register_native("fpower",				"__fpower")
	register_native("flog",					"__flog")
	register_native("get_cmdaccess",		"__get_cmdaccess")
	register_native("is_translated",		"__is_translated")
	register_native("get_plugincmdsnum",	"__get_plugincmdsnum")
	register_native("get_plugincmd",		"__get_plugincmd")
	register_native("get_plugincvarsnum",	"__get_plugincvarsnum")
	register_native("get_plugincvar",		"__get_plugincvar")
	register_native("is_module_running",	"__is_module_running")
	register_native("is_plugin_running",	"__is_plugin_running")
}

public __VelocityByAim(plid, num)
{
	new iIndex
	new iVelocity
	new Float:vRetValue[3]
	
	iIndex = get_param(1)
	iVelocity = get_param(2)
	
	new ret = velocity_by_aim(iIndex, iVelocity, vRetValue)
	set_array_f(3, vRetValue, 3)
	
	return ret
}

public __load_translations(plid, num)
{
	static file[255]
	
	get_string(1, file, 254)
	
	return load_translations(file)
}

public __is_user_authorized(plid, num)
{
	return is_user_authorized(get_param(1))
}

public __get_user_money(plid, num)
{
	return get_user_money(get_param(1))
}

public __set_user_money(plid, num)
{
	return set_user_money(get_param(1), get_param(2), get_param(3))
}

public __angle_to_vector(plid, num)
{
	new Float:angle[3]
	new Float:vRetValue[3]
	
	get_array_f(1, angle, 3)
	
	new ret = angle_vector(angle, get_param(2), vRetValue)
	set_array_f(3, vRetValue, 3)
	
	return ret
}

public Float:__fabs(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatabs(value)
}

public Float:__asin(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatasin(value, radian)
}

public Float:__sin(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatsin(value, radian)
}

public Float:__sinh(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatsinh(value, radian)
}

public Float:__acos(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatacos(value, radian)
}

public Float:__cos(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatcos(value, radian)
}

public Float:__cosh(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatcosh(value, radian)
}

public Float:__atan(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatatan(value, radian)
}

public Float:__atan2(plid, num)
{
	new Float:value1 = get_param_f(1)
	new Float:value2 = get_param_f(2)
	
	return floatatan2(value1, value2, radian)
}

public Float:__tan(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floattan(value, radian)
}

public Float:__tanh(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floattanh(value, radian)
}

public Float:__fsqroot(plid, num)
{
	new Float:value = get_param_f(1)
	
	return floatsqroot(value)
}

public Float:__fpower(plid, num)
{
	new Float:value = get_param_f(1)
	new Float:exponent = get_param_f(2)
	
	return floatpower(value, exponent)
}

public Float:__flog(plid, num)
{
	new Float:value = get_param_f(1)
	new Float:base = get_param_f(2)
	
	return floatlog(value, base)
}

//get_cmdaccess(cmd[], accessflags[], len)
public __get_cmdaccess(plid, num)
{
	static command[32], accessflags[32]
	new ret
	
	get_string(1, command, 31)

	if ((ret=get_cmdaccess(command, accessflags, 31)))
	{
		set_string(2, accessflags, get_param(3))
	}
	
	return ret
}

public __is_translated(plid, num)
{
	static string[512]
	
	get_string(1, string, 511)
	
	return is_translated(string)
}

public __get_plugincmdsnum(plid, num)
{
	static plugin[64]
	
	get_string(1, plugin, 63)
	
	return get_plugincmdsnum(plugin, get_param(2))
}

public __get_plugincmd(plid, num)
{
	static plugin[64]
	static command[32]
	static accessflags[32]
	static info[512]
	
	get_string(1, plugin, 63)
	
	if (get_plugincmd(plugin, 
					  get_param(2), 
					  command,
					  31,
					  accessflags,
					  31,
					  info,
					  511,
					  get_param(9),
					  get_param(10)))
	{
		set_string(3, command, get_param(4))
		set_string(5, accessflags, get_param(6))
		set_string(7, info, get_param(8))
		
		return 1
	}
	
	return 0
}

public __get_plugincvarsnum(plid, num)
{
	static plugin[64]
	
	get_string(1, plugin, 63)
	
	return get_plugincvarsnum(plugin, get_param(2))
}

//stock get_plugincvar(plugin[], index, cvar[], len1, value[], len2, flags=0)
public __get_plugincvar(plid, num)
{
	static plugin[64]
	static cvar[32]
	static value[512]
	
	get_string(1, plugin, 63)
	
	if (get_plugincvar(plugin, get_param(2), cvar, 31, value, 511, get_param(7)))
	{
		set_string(3, cvar, get_param(4))
		set_string(5, value, get_param(6))
		
		return 1
	}
	
	return 0
}

public __is_module_running(plid, num)
{
	static module[64]
	
	get_string(1, module, 63)
	
	return is_module_running(module)
}

public __is_plugin_running(plid, num)
{
	static plugin[64]
	
	get_string(1, plugin, 63)
	
	return is_plugin_running(plugin)
}
