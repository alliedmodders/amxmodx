// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new failcount;
new passcount;

public plugin_init()
{
	register_plugin("Datapack Test", AMXX_VERSION_STR, "AMXX Dev Team");
	register_srvcmd("datapacktest", "datapacktest");
}

test(const testname[], bool:pass)
{
	if (!pass)
	{
		server_print("[FAIL] %s", testname);
		failcount++;
	}
	else
	{
		server_print("[PASS] %s", testname);
		passcount++;
	}
}

stock done()
{
	server_print("Finished. %d tests, %d failed", failcount + passcount, failcount);
}

public datapacktest()
{
	failcount = 0;
	passcount = 0;

	new DataPack:pack = CreateDataPack();
	new DataPack:oldPack = pack; // Makes sure that the trie handle system recycles old handles

	new refCell = 23;
	new Float:refFloat = 42.42;
	new refString[] = "I'm a little teapot.";

	// Write
	new cellPos = GetPackPosition(pack);
	WritePackCell(pack, refCell);
	new floatPos = GetPackPosition(pack);
	WritePackFloat(pack, refFloat);
	new strPos = GetPackPosition(pack);
	WritePackString(pack, refString);
	new endPos = GetPackPosition(pack);

	test("Write position test",
		.pass = (cellPos != floatPos && cellPos != strPos && cellPos != endPos
				&& floatPos != strPos && floatPos != endPos && strPos != endPos));

	//resets the index to the beginning, necessary for read.
	ResetPack(pack);

	test("Position #1 test", .pass = (GetPackPosition(pack) == cellPos));
	test("Readable #1 test", .pass = !IsPackEnded(pack));

	new cellValue = ReadPackCell(pack);
	test("Cell test", .pass = (cellValue == refCell));

	test("Position #2 test", .pass = (GetPackPosition(pack) == floatPos));
	test("Readable #2 test", .pass = !IsPackEnded(pack));

	new Float:floatValue = ReadPackFloat(pack);
	test("Float test", .pass = (floatValue == refFloat));

	test("Position #3 test", .pass = (GetPackPosition(pack) == strPos));
	test("Readable #3 test", .pass = !IsPackEnded(pack));

	new buffer[1024];
	ReadPackString(pack, buffer, 1024);
	test("String test #1", .pass = bool:equal(buffer, refString));

	test("End test", .pass = IsPackEnded(pack));

	ResetPack(pack, .clear = true);
	test("Clear test", .pass = IsPackEnded(pack));

	DestroyDataPack(pack);

	test("Recycle handles", CreateDataPack() == oldPack);

	done();
}
