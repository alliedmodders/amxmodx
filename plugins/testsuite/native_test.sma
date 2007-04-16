#include <amxmodx>

native Factorial(num)

public __Factorial(id, num)
{
	new num = get_param(1)
	if (num == 0)
	{
		return 1
	}
	
	return num * Factorial(num - 1)
}

public plugin_natives()
{
	register_native("Factorial", "__Factorial")
}

public plugin_init()
{
	register_plugin("Native Test", "1.0", "BAILOPAN")
	register_srvcmd("test_native1", "Command_TestNative1")
}

public Command_TestNative1()
{
	new num = Factorial(6)
	server_print("Factorial of 6 is: %d", num)
}
