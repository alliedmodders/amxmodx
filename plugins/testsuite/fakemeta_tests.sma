#include <amxmodx>
#include <fakemeta>

public plugin_init()
{
	register_plugin("Fakemeta Tests", "1.0", "BAILOPAN")
	register_forward(FM_ServerDeactivate, "Hook_ServerDeactivate")
}

public Hook_ServerDeactivate()
{
	server_print("[FAKEMETA TEST] ServerDeactivate() at %f", get_gametime())
}

public plugin_end()
{
	server_print("[FAKEMETA TEST] plugin_end() at %f", get_gametime())
}
