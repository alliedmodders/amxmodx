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
	WritePackCell(pack, refCell);		// 8 
	WritePackString(pack, refString);	// 25 (sizeof string + 4)
	WritePackFloat(pack, refFloat);		// 8
	 
	test("Position #1 test", .pass = GetPackPosition(pack) == 41);
	test("Readable #1 test", .pass = !IsPackReadable(pack, 41));
	
	//resets the index to the beginning, necessary for read.
	ResetPack(pack);
	
	test("Position #2 test", .pass = GetPackPosition(pack) == 0 );
	test("Readable #2 test", .pass = IsPackReadable(pack, 15));
	
	// Read
	new cellValue = ReadPackCell(pack);
	new buffer[1024];
	ReadPackString(pack, buffer, 1024);
	new Float:floatvalue = ReadPackFloat(pack);
	
	test("Cell test", .pass = cellValue == refCell);
	test("String test", .pass = bool:equal(buffer, refString));
	test("Float test #1", .pass = floatvalue == refFloat);
	
	SetPackPosition(pack, 33);
	test("Set Position test", .pass = GetPackPosition(pack) == 33);
	
	WritePackFloat(pack, refFloat + 1);
	SetPackPosition(pack, 33);
	test("Float test #2", .pass = ReadPackFloat(pack) == refFloat + 1);

	ResetPack(pack, .clear = true);
	test("Clear test", .pass = !IsPackReadable(pack, 15));
	
	DestroyDataPack(pack);
	
	test("Recycle handles", CreateDataPack() == oldPack);
	
	done();
}
