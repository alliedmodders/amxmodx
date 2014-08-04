// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new FailCount;
new PassCount;

public plugin_init()
{
	register_plugin("Stack Tests", AMXX_VERSION_STR, "AMXX Dev Team");
	register_srvcmd("test_stack", "ServerCommand_TestStack");
}

assertEqual(const testname[], bool:pass)
{
	if (!pass)
	{
		server_print("[FAIL] %s", testname);
		FailCount++;
	}
	else
	{
		server_print("[PASS] %s", testname);
		PassCount++;
	}
}

done()
{
	server_print("Finished. %d tests, %d failed", FailCount + PassCount, FailCount);
}

public ServerCommand_TestStack()
{
	new Stack:stack;
	new test[20];
	new buffer[42];

	test[0] = 5;
	test[1] = 7;

	stack = CreateStack(30);
	{
		PushStackCell(stack, 50);
		PushStackArray(stack, test, 2);
		PushStackArray(stack, test, 2);
		PushStackString(stack, "space craaab");
		PushStackCell(stack, 12);
	}

	assertEqual("Size test #1", IsStackEmpty(stack) == false);

	PopStack(stack);
	PopStackString(stack, buffer, charsmax(buffer));
	assertEqual("String test", bool:equal(buffer, "space craaab"));

	test[0] = 0;
	test[1] = 0;
	assertEqual("Array test #1", test[0] == 0 && test[1] == 0);

	PopStackArray(stack, test, 2);
	assertEqual("Array test #1", test[0] == 5 && test[1] == 7);

	PopStackCell(stack, test[0], 1);
	assertEqual("Value test #1", test[0] == 7);

	PopStackCell(stack, test[0]);
	assertEqual("Value test #2", test[0] == 50);

	assertEqual("Size test #2", IsStackEmpty(stack) == true);

	DestroyStack(stack);

	done();
}
