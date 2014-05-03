#include <amxmodx>

new failcount = 0;
new passcount = 0;
public plugin_init()
{
	register_plugin("Trie Test", AMXX_VERSION_STR, "AMXX Dev Team");
	register_srvcmd("trietest", "trietest");
}

stock fail(const testname[])
{
	server_print("[FAIL] %s", testname);

	failcount++;
}
stock pass(const testname[])
{
	server_print("[PASS] %s", testname);

	passcount++;
}
stock done()
{
	server_print("Finished. %d tests, %d failed", failcount + passcount, failcount);
}

public trietest()
{
	failcount = 0;
	passcount = 0;

	new bool:ok = true;
	new Trie:t = TrieCreate();

	new Trie:oldhandle = t; // Makes sure that the trie handle system recycles old handles

	new key[32];
	for (new i = 0; i < 100; i++)
	{
		formatex(key, charsmax(key), "K%dK", i);
		if (!TrieSetCell(t, key, i))
		{
			server_print("TrieGetCell(%d, '%s', %d) failed", t, key, i);
			ok = false;
		}
	}
	
	for (new i = 0; i < 100; i++)
	{
		formatex(key, charsmax(key), "K%dK", i);
		new val;
		if (!TrieGetCell(t, key, val))
		{
			server_print("TrieGetCell(%d, '%s', %d) failed", t, key, val);
			ok = false;
		}
		else if (val != i)
		{
			server_print("val mismatch, expected: %d got: %d", i, val);
			ok = false;
		}
	}
	
	// Check size is 100.
	if (TrieGetSize(t) != 100)
	{
		ok = false;
		server_print("Map size mismatch, expected: %d got: %d", 100, TrieGetSize(t));
	}
	
	if (ok)
		pass("Cell tests");
	else
		fail("Cell tests");

	TrieClear(t);
	TrieDestroy(t);

	t = TrieCreate();

	if (t == oldhandle)
		pass("Recycle handles");

	else
		fail("Recycle handles");

	ok = true;
	for (new i = 0; i < 100; i++)
	{
		static val[32];
		formatex(key, charsmax(key), "K%dK", i);
		formatex(val, charsmax(val), "V%dV", i);
		TrieSetString(t, key, val);
	}
	
	for (new i = 0; i < 100; i++)
	{
		formatex(key, charsmax(key), "K%dK", i);
		static val[32];
		static exp[32];
		formatex(exp, charsmax(exp), "V%dV", i);
		if (!TrieGetString(t, key, val, charsmax(val)))
		{
			server_print("TrieGetString(%d, '%s', %s) failed", t, key, val);
			ok = false;
		}

		else if (!equal(val, exp))
		{
			server_print("val mismatch, key: '%s' expected: '%s' got: '%s'", key, exp, val);
			ok = false;
		}

	}
	if (ok)
		pass("String tests");

	else
		fail("String tests");

	TrieDestroy(t);

	t = TrieCreate();
	ok = true;
	for (new i = 0; i < 1000; i++)
	{
		formatex(key, charsmax(key), "!%d!", i);
		TrieSetString(t, key, key);
	}
	for (new i = 0; i < 1000; i++)
	{
		formatex(key, charsmax(key), "!%d!", i);

		if (!TrieKeyExists(t, key))
		{
			ok = false;
			server_print("Key '%s' does not exist", key);
		}
		else
		{
			if (!TrieDeleteKey(t, key))
			{
				server_print("Key '%s' could not be deleted", key);
				ok = false;
			}
		}
	}
	if (ok)
		pass("Exists/Delete");

	else
		fail("Exists/Delete");

	TrieClear(t);
	TrieDestroy(t);

	done();
	
}

