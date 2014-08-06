// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>
#include <amxmisc>

#pragma ctrlchar '\'

public plugin_init()
{
	register_plugin("strbreak tests", "1.0", "BAILOPAN");
	register_concmd("test_strbreak", "test_strbreak", 0, "admin");
	register_concmd("test_argparse", "test_argparse", 0, "admin");
}

new TestCount = 0;
new FailCount = 0;

test_reset()
{
	TestCount = 0;
	FailCount = 0;
}

test_numequal(a, b)
{
	TestCount++;
	if (a == b) {
		server_print("[%d] PASS /%d/ == /%d/", TestCount, a, b);
	} else {
		server_print("[%d] FAIL /%d/ == /%d/", TestCount, a, b);
		FailCount++;
	}
}

test_equal(a[], b[])
{
	TestCount++;
	if (equal(a, b)) {
		server_print("[%d] PASS /%s/ == /%s/", TestCount, a, b);
	} else {
		server_print("[%d] FAIL /%s/ == /%s/", TestCount, a, b);
		FailCount++;
	}
}

public test_strbreak(id)
{
	test_reset();

	new left[8], right[8];

	// Test sort of normal behavior for strbreak().
	strbreak("a b c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b c");

	strbreak("a", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "");

	strbreak("a\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a\"");
	test_equal(right, "");

	strbreak("\"a\" yy", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "yy");

	strbreak("\"a x", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "\"a x");
	test_equal(right, "");

	strbreak("a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b  c");

	strbreak("\"a\"  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b  c");

	strbreak("a  \"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "\"b  c\"");

	strbreak("a  q\"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "q\"b  c\"");

	strbreak("q \"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q");
	test_equal(right, "\"b  c\"");

	strbreak("q \"b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q");
	test_equal(right, "\"b  c");

	// strbreak() functionality starts degrading here, but we test this to
	// preserve bug-for-bug compatibility.
	strbreak("q\"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "\"b  c");
	test_equal(right, "\"");

	strbreak("\"a  \"a  \"b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a  \"");
	test_equal(right, "\"b  c");

	strbreak("\"a  \"a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a  \"");
	test_equal(right, "b  c");

	strbreak("\"a\"a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a\"");
	test_equal(right, "b  c");

	strbreak("\"a\" x", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a\"");
	test_equal(right, "x");

	strbreak("\"a\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "\"");

	strbreak("\"a\"b", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "\"a\"b");
	test_equal(right, "");

	strbreak("q\" b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q\" b  c");
	test_equal(right, "");

	// Test truncation.
	strbreak("123456789A 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	strbreak("12345 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12345");
	test_equal(right, "1234567");

	strbreak("\"12345\" 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12345");
	test_equal(right, "1234567");

	strbreak("\"123456789A\" 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	strbreak("\"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	strbreak("\"123456789A\"               1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	strbreak("123456789A               1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	strbreak("123456789A               123456778923", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	// Bonkers - left-hand makes no sense. Does whitespace count toward the buffer?!
	strbreak("  123456789A               123456778923", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12345");
	test_equal(right, "1234567");

	strbreak("        \"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "");
	test_equal(right, "1234");

	strbreak("     \"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12");
	test_equal(right, "1234");

	new text_cmd[7], cheater[64];
	strbreak("  a ", text_cmd, 1, cheater, 31);
	test_equal(text_cmd, "");
	test_equal(cheater, "");

	server_print("%d/%d passed", TestCount - FailCount, TestCount);
}

public test_argparse(id)
{
	test_reset();

	new left[8], right[8];

	// Tests for behavior that we expect to work.
	argbreak("a b c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b c");

	argbreak("a", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "");

	argbreak("a\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "");

	argbreak("\"a\" yy", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "yy");

	argbreak("\"a x", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a x");
	test_equal(right, "");

	argbreak("a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b  c");

	argbreak("\"a\"  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "b  c");

	argbreak("a  \"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "\"b  c\"");

	argbreak("a  q\"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "q\"b  c\"");

	argbreak("q \"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q");
	test_equal(right, "\"b  c\"");

	argbreak("q \"b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q");
	test_equal(right, "\"b  c");

	argbreak("q\"b  c\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "qb  c");
	test_equal(right, "");

	argbreak("\"a  \"a  \"b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a  a");
	test_equal(right, "\"b  c");

	argbreak("\"a  \"a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a  a");
	test_equal(right, "b  c");

	argbreak("\"a\"a  b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "aa");
	test_equal(right, "b  c");

	argbreak("\"a\" x", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "x");

	argbreak("\"a\"", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "a");
	test_equal(right, "");

	argbreak("\"a\"b", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "ab");
	test_equal(right, "");

	argbreak("q\" b  c", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "q b  c");
	test_equal(right, "");

	// Test truncation.
	argbreak("123456789A 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	argbreak("12345 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12345");
	test_equal(right, "1234567");

	argbreak("\"12345\" 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "12345");
	test_equal(right, "1234567");

	argbreak("\"123456789A\" 123456789ABCDE", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	argbreak("\"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	argbreak("\"123456789A\"               1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	argbreak("123456789A               1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	argbreak("123456789A               123456778923", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	argbreak("  123456789A               123456778923", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234567");

	argbreak("        \"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	argbreak("     \"123456789A\" 1234", left, sizeof(left) - 1, right, sizeof(right) - 1);
	test_equal(left, "1234567");
	test_equal(right, "1234");

	new text_cmd[7], cheater[64];
	argbreak("  a ", text_cmd, 1, cheater, 31);
	test_equal(text_cmd, "a");
	test_equal(cheater, "");

	// Test no-finds.
	new pos = argparse("   \t\n \t", 0, left, sizeof(left));
	test_numequal(pos, -1);
	test_equal(left, "");

	// Loop for good measure.
	new argv[4][10], argc = 0;
	new text[] = "a \"b\" cd\"e\" \t f\"     ";
	pos = 0;
	while (argc < 4) {
		pos = argparse(text, pos, argv[argc++], 10 - 1);
		if (pos == -1)
			break;
	}
	test_numequal(argc, 4);
	test_equal(argv[0], "a");
	test_equal(argv[1], "b");
	test_equal(argv[2], "cde");
	test_equal(argv[3], "f     ");

	server_print("%d/%d passed", TestCount - FailCount, TestCount);
}

