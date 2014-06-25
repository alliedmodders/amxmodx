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

	// Setting K42K without replace should fail.
	new value;
	if (TrieSetCell(t, "K42K", 999, false))				{ ok = false; server_print("set trie K42K should fail"); }
	if (!TrieGetCell(t, "K42K", value) || value != 42)	{ ok = false; server_print("value at K42K not correct"); }
	if (!TrieSetCell(t, "K42K", 999))					{ ok = false; server_print("set trie K42K = 999 should succeed"); }
	if (!TrieGetCell(t, "K42K", value) || value != 999)	{ ok = false; server_print("value at K42K not correct"); }

	// Check size is 100.
	if (TrieGetSize(t) != 100)
	{
		server_print("Map size mismatch (1), expected: %d got: %d", 100, TrieGetSize(t)); ok = false;
	}

	if (TrieGetCell(t, "cat", value))
	{
		server_print("trie should not have a cat."); ok = false;
	}

	// Check that "K42K" is not a string or array.
	new array[32];
	new string[32];
	if (TrieGetArray(t, "K42K", array, sizeof(array)) ||
		TrieGetString(t, "K42K", string, charsmax(string)))
	{
		server_print("entry K42K should not be an array or string"); ok = false;
	}

	TrieClear(t);

	if (TrieGetSize(t) != 0)
	{
		server_print("Map size mismatch (2), expected: %d got: %d", 0, TrieGetSize(t)); ok = false;
	}

	TrieDestroy(t);

	if (ok)
		pass("Cell tests");
	else
		fail("Cell tests");


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
		new size;

		if (!TrieGetString(t, key, val, charsmax(val), size))
		{
			server_print("TrieGetString(%d, '%s', %s) failed", t, key, val);
			ok = false;
		}
		else if (!equal(val, exp))
		{
			server_print("val mismatch, key: '%s' expected: '%s' got: '%s'", key, exp, val);
			ok = false;
		}
		if (size != strlen(exp))
		{
			server_print("returned size mismatch, key: '%s' expected: '%s' got: '%s'", key, strlen(exp), size);
			ok = false;
		}
	}

	if (TrieGetCell(t, "K42K", value) ||
		TrieGetArray(t, "K42K", array, sizeof(array)))
	{
		server_print("entry K42K should not be an array or string"); ok = false;
	}

	if (TrieGetString(t, "cat", string, charsmax(string)))
	{
		server_print("trie should not have a cat."); ok = false;
	}

	if (ok)
		pass("String tests");
	else
		fail("String tests");


	ok = true;

	new data[5] = { 93, 1, 2, 3, 4 };

	if (!TrieSetArray(t, "K42K", data, sizeof data))
	{
		server_print("K42K should be a string."); ok = false;
	}
	if (!TrieGetArray(t, "K42K", array, sizeof(array)))
	{
		server_print("K42K should be V42V."); ok = false;
	}
	for (new i = 0; i < sizeof data; i++)
	{
		if (data[i] != array[i])
		{
			server_print("K42K slot %d should be %d, got %d", i, data[i], array[i]); ok = false;
		}
	}
	if (TrieGetCell(t, "K42K", value) ||
		TrieGetString(t, "K42K", string, charsmax(string)))
	{
		server_print("entry K42K should not be an array or string"); ok = false;
	}
	if (!TrieSetArray(t, "K42K", data, 1))
    {
		server_print("couldn't set K42K to 1-entry array"); ok = false;
	}
	if (!TrieGetArray(t, "K42K", array, sizeof(array), value))
	{
		server_print("couldn't fetch 1-entry array"); ok = false;
	}
	if (value != 1)
	{
		server_print("array size mismatch (%d, expected %d)", value, 1); ok = false;
	}

	if (ok)
		pass("Array tests");
	else
		fail("Array tests");


	ok = true;

	// Remove "K42K".
	if (!TrieDeleteKey(t, "K42K"))
	{
		server_print("K42K should have been removed"); ok = false;
	}
	if (TrieDeleteKey(t, "K42K"))
	{
		server_print("K42K should not exist"); ok =false;
	}
	if (TrieGetCell(t, "K42K", value) ||
		TrieGetArray(t, "K42K", array, sizeof(array)) ||
		TrieGetString(t, "K42K", string, charsmax(string)))
	{
		server_print("map should not have a K42K"); ok = false;
	}

	TrieDestroy(t);

	t = TrieCreate();

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
			server_print("Key '%s' does not exist", key); ok = false;
		}
		else if (!TrieDeleteKey(t, key))
		{
			server_print("Key '%s' could not be deleted", key); ok = false;
		}
	}

	if (ok)
		pass("Exists/Delete");
	else
		fail("Exists/Delete");

	ok = true;

	TrieClear(t);

	if (TrieGetSize(t))
	{
		server_print("size should be 0"); ok = false
	}

	TrieSetString(t, "adventure", "time!");
	TrieSetString(t, "butterflies", "bees");
	TrieSetString(t, "egg", "egg");

	new Snapshot:keys = TrieSnapshotCreate(t);
	{
		if (TrieSnapshotLength(keys) != 3)
		{
			server_print("trie snapshot length should be 3"); ok = false;
		}

		new bool:found[3], len = TrieSnapshotLength(keys);
		new buffer[32];
		for (new i = 0; i < len; i++)
		{
			new size = TrieSnapshotKeyBufferSize(keys, i); // Just to use it, otherwise you should use charsmax(buffer).
			TrieSnapshotGetKey(keys, i, buffer, size);

			if (strcmp(buffer, "adventure") == 0)			found[0] = true;
			else if (strcmp(buffer, "butterflies") == 0)	found[1] = true;
			else if (strcmp(buffer, "egg") == 0)			found[2] = true;
			else { server_print("unexpected key: %s", buffer); ok = false; }
		}

		if (!found[0] || !found[1] || !found[2])
		{
			server_print("did not find all keys"); ok = false;
		}
	}

	TrieSnapshotDestroy(keys);
	TrieDestroy(t);

	if (ok)
		pass("Snapshot");
	else
		fail("Snapshot");

	ok = true;

	t = TrieCreate();

	TrieSetString(t, "full", "throttle");
	TrieSetString(t, "brutal", "legend");
	TrieSetString(t, "broken", "age");

	new TrieIter:iter = TrieIterCreate(t);
	{
		if (TrieIterGetStatus(iter) != IterStatus_Valid)
		{
			server_print("Trie iterator should report itself as valid (create)");
			ok = false;
		}

		if (!TrieIterGetKey(iter))
		{
			server_print("Trie iterator should not be empty at this point (no key retrieval)");
			ok = false;
		}

		if (!TrieIterNext(iter))
		{
			server_print("Trie iterator should have a next key/value pair at this point");
			ok = false;
		}

		TrieIterRefresh(iter);

		new key[32], value[32], bool:valid[4], klen, vlen;
		if (!TrieIterGetKey(iter, key, charsmax(key), klen) || klen == 0)
		{
			server_print("Trie iterator should not be empty at this point (key retrieval)");
			ok = false;
		}
		else
		{
			// This tests calling IterGetKey first and then looping with TrieIterNext
			do
			{
				TrieIterGetString(iter, value, charsmax(value), vlen);

				if (strcmp(key, "full") == 0 && strcmp(value, "throttle") == 0)
					valid[0] = true;
				else if (strcmp(key, "brutal") == 0 && strcmp(value, "legend") == 0)
					valid[1] = true;
				else if (strcmp(key, "broken") == 0 && strcmp(value, "age") == 0)
					valid[2] = true;

				if (strlen(key) != klen)
				{
					server_print("Key string length does not match. %d != %d", strlen(key), klen);
					ok = false;
				}

				if (strlen(value) != vlen)
				{
					server_print("Value string length does not match. %d != %d", strlen(key), vlen);
					ok = false;
				}
			}
			while (TrieIterNext(iter, key, charsmax(key), klen))

			if (!valid[0] || !valid[1] || !valid[2])
			{
				server_print("Did not find all value pairs (1)");
				ok = false;
			}
		}

		TrieSetString(t, "monkey", "island");
		TrieSetString(t, "grim", "fandango");

		if (TrieIterGetStatus(iter) != IterStatus_Outdated)
		{
			server_print("Trie iterator should report itself as outdated (add)");
			ok = false;
		}

		TrieIterRefresh(iter);
		TrieDeleteKey(t, "full");

		if (TrieIterGetStatus(iter) != IterStatus_Outdated)
		{
			server_print("Trie iterator should report itself as outdated (remove)");
			ok = false;
		}

		if (TrieIterSetPos(iter, "monkey"))
		{
			if (TrieIterGetStatus(iter) != IterStatus_Valid)
			{
				server_print("Trie iterator should report itself as valid (setpos)");
				ok = false;
			}

			TrieIterGetKey(iter, key, charsmax(key));
			TrieIterGetString(iter, value, charsmax(value));

			if (strcmp(key, "monkey") != 0 || strcmp(value, "island") != 0)
			{
				server_print("Trie iterator key/value pair doesn't match after setpos |%s|%s|", key, value);
				ok = false;
			}
		}
		else
		{
			server_print("Trie iterator positioning failed");
			ok = false;
		}

		TrieIterRefresh(iter);
		arrayset(valid, false, sizeof(valid));

		if (TrieIterGetSize(iter) != 4)
		{
			server_print("Trie iterator size should be 4, is %d", TrieIterGetSize(iter));
			ok = false;
		}

		// This tests looping with IterGetKey and doing intermediate calls to TrieNextIter
		while (TrieIterGetKey(iter, key, charsmax(key)))
		{
			TrieIterGetString(iter, value, charsmax(value));

			if (strcmp(key, "monkey") == 0 && strcmp(value, "island") == 0)
				valid[0] = true;
			else if (strcmp(key, "brutal") == 0 && strcmp(value, "legend") == 0)
				valid[1] = true;
			else if (strcmp(key, "broken") == 0 && strcmp(value, "age") == 0)
				valid[2] = true;
			else if (strcmp(key, "grim") == 0 && strcmp(value, "fandango") == 0)
				valid[3] = true;

			TrieIterNext(iter);
		}

		if (!valid[0] || !valid[1] || !valid[2] || !valid[3])
		{
			server_print("Did not find all value pairs (2)");
			ok = false;
		}

 		// Overwriting an existing key should not invalidate our iterator
		TrieSetString(t, "monkey", "island 2");

		if (TrieIterGetStatus(iter) != IterStatus_Valid)
		{
			server_print("Trie iterator should report itself as valid (update)");
			ok = false;
		}

		TrieClear(t);

		if (TrieIterGetStatus(iter) != IterStatus_Outdated)
		{
			server_print("Trie iterator should report itself as outdated (clear)");
			ok = false;
		}

		TrieDestroy(t);

		if (TrieIterGetStatus(iter) != IterStatus_Closed)
		{
			server_print("Trie iterator should report itself as closed");
			ok = false;
		}

		TrieIterDestroy(iter);
	}

	if (ok)
		pass("Iterator");
	else
		fail("Iterator");

	done();
}

