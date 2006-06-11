#include <amxmodx>

new g_id

native test_createforward(function[])
native test_executeforward()

public plugin_init()
{
	g_id = register_plugin("Forward Test (Client)", "1.0", "Belsebub")
	
	register_srvcmd("fwd_test1", "Test_Forward1")
}

public Test_Forward1()
{
	server_print("Executing forward ^"gaben^" (I'm %d)", g_id)
	test_createforward("gaben")
	test_executeforward()	
}

public gaben()
{
	server_print("gaben executed (I'm %d)", g_id)
}
