#include <amxmodx>

new g_forward
new g_id

public plugin_init()
{
	g_id = register_plugin("Foward Test (Master)", "1.0", "Belsebub")
}

public plugin_natives()
{
	register_native("test_createforward", "test_createforward_handler")
	register_native("test_executeforward", "test_executeforward_handler")
}

//test_createforward(function[])
public test_createforward_handler(pluginid, numparams)
{
	server_print("(test_createforward_handler: %d,%d)", pluginid, numparams)
	
	new function[32]
	get_string(1, function, 31)
	
	if (g_forward > 0)
	{
		DestroyForward(g_forward)
	}

	g_forward = CreateOneForward(pluginid, function)
	if (g_forward < 0)
	{
		server_print("Failed to create forward!")
	}
}

//test_executeforward()
public test_executeforward_handler(pluginid, numparams)
{
	new retval
	
	server_print("(test_executeforward_handler: %d,%d)", pluginid, numparams)

	if (!ExecuteForward(g_forward, retval))
	{
		server_print("failed to execute forward (I'm %d)", g_id)
	}
	
	if (g_forward > 0)
	{
		DestroyForward(g_forward)
		g_forward = -1
	}
}
