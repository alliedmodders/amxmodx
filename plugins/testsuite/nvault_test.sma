#include <amxmodx>
#include <nvault>

public plugin_init()
{
	register_plugin("nVault Test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_nvault", "Command_TestNvault")
}

public Command_TestNvault()
{
	new v = nvault_open("://:/1/R!?#@41345$%:$")
	server_print("Vault value: %d (expected: %d)", v, -1)
	
	if (v != -1)
	{
		nvault_close(v)
	}
}
