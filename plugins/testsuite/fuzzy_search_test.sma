// vim: set ts=4 sw=4 tw=99 noet:

#include <amxmodx>

public plugin_init()
{
	register_plugin("Fuzzy string search Test", "1.0", "")

	register_srvcmd("test_lev", "Command_TestLevenshtein")
}

new TestCount = 0
new FailCount = 0

stock test_reset()
{
	TestCount = 0
	FailCount = 0
}

stock test_result()
{
	if (FailCount == 0) {
		server_print("All test passed");
	} else {
		server_print("%d failed", FailCount);
	}
}

stock test(const test_name[], expected, const str1[], const str2[])
{
	new actual = levenshtein(str1, str2)
	if (actual == expected) {
		server_print("[%d](%s) PASS %d == %d", ++TestCount, test_name, actual, expected);
	} else {
		server_print("[%d](%s) FAIL %d == %d", ++TestCount, test_name, actual, expected);
		FailCount++;
	}
}

public Command_TestLevenshtein()
{
	test_reset()

	test("equal strings", 0, "hello", "hello")
	test("str1 empty", 4, "", "test")
	test("str2 empty", 3, "tet", "")
	test("d(debug, debugg)", 1, "debug", "debugg")
	test("d(elephant, relevant)", 3, "elephant", "relevant")

	test_result()
}
