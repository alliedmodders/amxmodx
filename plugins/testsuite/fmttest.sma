#include <amxmodx>

public plugin_init()
{
	register_plugin("Format Test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_format", "Command_TestFormat")
}

public Command_TestFormat()
{
	server_print("Printing -1 with d: %d", -1)
	server_print("Printing -1 with u: %u", -1)
	server_print("Printing (1<<31) with d: %d", (1<<31))
	server_print("Printing (1<<31) with u: %u", (1<<31))
	server_print("Printing 1 with d: %d", 1)
	server_print("Printing 1 with u: %u", 1)
}
