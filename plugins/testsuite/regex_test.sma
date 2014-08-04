// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>
#include <regex>

/**
 * Warning: To get expected result, file encoding must be UTF-8 without BOM.
 */

public plugin_init()
{
    register_plugin("Regex Test", AMXX_VERSION_STR, "AMXX Dev Team");
    register_srvcmd("regex_test", "OnServerCommand");
}

new FailedCount;
new PassedCount;

test(const regex[], const replace[], const string[], const expectedString[], expectedCount = -1, regexFlags = 0, formatFlags = 0, bufferlen = -1)
{
    new errorCode, error[128];
    new Regex:r = regex_compile_ex(regex, regexFlags, error, charsmax(error), errorCode);

    if (r == REGEX_PATTERN_FAIL || errorCode)
    {
        server_print("^t^t#%d. Pattern fail : ^"%s^"(%d)", ++FailedCount + PassedCount, error, errorCode);
    }
    else
    {
        new buffer[512];
        copy(buffer, charsmax(buffer), string);

        new errorCode;
        new count = regex_replace(r, buffer, bufferlen != -1 ?  bufferlen : charsmax(buffer), replace, formatFlags, errorCode);

        if (expectedCount != -1 && count != expectedCount)
        {
            server_print("^t^t#%d. Failed - count = %d, expected count = %d", ++FailedCount + PassedCount, count, expectedCount);
        }
        else if (!equal(buffer, expectedString))
        {
            server_print("^t^t#%d. Failed - output = %s, expected output = %s", ++FailedCount + PassedCount, buffer, expectedString);
        }
        else
        {
            ++PassedCount;
        }

        regex_free(r);
    }
}
end()
{
    server_print("Tests successful: %d/%d", PassedCount, PassedCount + FailedCount);
}

public OnServerCommand()
{
    server_print("Testing regex_replace()");

    server_print("^tChecking count...");
    {
        test( .regex          = "(([0-9a-z]+)-([0-9]+))-(([0-9]+)-([0-9]+))",
              .replace        = "xxxx",
              .string         = "1-2-3-4 a-2-3-4 1-a-3-4 1-2-a-4 1-2-3-a a-a-a-a 4-3-2-1 100-200-300-400-500-600-700-800",
              .expectedString = "xxxx xxxx 1-a-3-4 1-2-a-4 1-2-3-a a-a-a-a xxxx xxxx-xxxx",
              .expectedCount  = 5
            );

        test( .regex          = "([a-z]+)",
              .replace        = "xxxx",
              .string         = "Here must only number like 42 and 13 appear",
              .expectedString = "Hxxxx xxxx xxxx xxxx xxxx 42 xxxx 13 xxxx",
              .expectedCount  = 7
            );

        test( .regex          = "((V(I|1)(4|A)GR(4|A))|(V(I|1)C(0|O)D(I|1)(N|\/\\\/)))", .regexFlags = PCRE_CASELESS,
              .replace        = "...",
              .string         = "Viagra V14GR4 Vicodin V1C0D1/\/ v1c0d1/|/",
              .expectedString = "... ... ... ... v1c0d1/|/",
              .expectedCount  = 4
            );

        test( .regex          = "\[(right)\](((?R)|[^^[]+?|\[)*)\[/\\1\]", .regexFlags = PCRE_CASELESS | PCRE_UNGREEDY,
              .replace        = "",
              .string         = "[CODE]&lt;td align=&quot;$stylevar[right]&quot;&gt;[/CODE]",
              .expectedString = "[CODE]&lt;td align=&quot;$stylevar[right]&quot;&gt;[/CODE]",
              .expectedCount  = 0
            );

        test( .regex          = "- This is a string$",
              .replace        = "This shouldn\'t work",
              .string         = "123456789 - Hello, world -           This is a string.",
              .expectedString = "123456789 - Hello, world -           This is a string.",
              .expectedCount  = 0
            );

        test( .regex          = "[0-35-9]",
              .replace        = "4",
              .string         = "123456789 - Hello, world -           This is a string.",
              .expectedString = "444444444 - Hello, world -           This is a string.",
              .expectedCount  = 8
            );

        test( .regex          = "\b[hH]\w{2,4}",
              .replace        = "Bonjour",
              .string         = "123456789 - Hello, world -           This is a string.",
              .expectedString = "123456789 - Bonjour, world -           This is a string.",
              .expectedCount  = 1
            );

        test( .regex          = "(\w)\s*-\s*(\w)",
              .replace        = "$1. $2",
              .string         = "123456789 - Hello, world -           This is a string.",
              .expectedString = "123456789. Hello, world. This is a string.",
              .expectedCount  = 2
            );

        test( .regex          = "([a-z]\w+)@(\w+)\.(\w+)\.([a-z]{2,})",
              .replace        = "$1 at $2 dot $3 dot $4",
              .string         = "josmessa@uk.ibm.com",
              .expectedString = "josmessa at uk dot ibm dot com",
              .expectedCount  = 1
            );

        test( .regex          = "\b\w{1}s",
              .replace        = "test",
              .string         = "This is a string. (0-9) as well as parentheses",
              .expectedString = "This test a string. (0-9) test well test parentheses",
              .expectedCount  = 3
            );


        test( .regex          = "(\d{1})-(\d{1})",
              .replace        = "$1 to $2",
              .string         = "This is a string. It contains numbers (0-9) as well as parentheses and some other things!",
              .expectedString = "This is a string. It contains numbers (0 to 9) as well as parentheses and some other things!",
              .expectedCount  = 1
            );

        test( .regex          = "[\(!\)]",
              .replace        = "*",
              .string         = "This is a string. It contains numbers (0-9) as well as parentheses and some other things!",
              .expectedString = "This is a string. It contains numbers *0-9* as well as parentheses and some other things*",
              .expectedCount  = 3
            );
    }

    server_print("^tChecking edges cases...");
    {
        test(.regex = "[0-9]+",    .replace = "*",   .string = "",     .expectedString = "",    .expectedCount  = 0);
        test(.regex = "([0-9]+)",  .replace = "",    .string = "123",  .expectedString = "",    .expectedCount  = 1);
        test(.regex = "a",         .replace = "\",   .string = "a",    .expectedString = "\",   .expectedCount  = 1);
        test(.regex = "^^",        .replace = "x",   .string = "a",    .expectedString = "xa",  .expectedCount  = 1);
        test(.regex = "b",         .replace = "\",   .string = "b",    .expectedString = "\",   .expectedCount  = 1, .bufferlen = 1);
        test(.regex = "b",         .replace = "^^",  .string = "b",    .expectedString = "b",   .expectedCount  = 0, .bufferlen = 0);
        test(.regex = "\w+",       .replace = "123", .string = "abc",  .expectedString = "12",  .expectedCount  = 1, .bufferlen = 2);
    }

    server_print("^tChecking UTF-8 support...");
    {
        test(.regex = "(\w+)",  .replace = "*",  .string = "éà@É",  .expectedString = "éà@É",  .expectedCount = 0);
        test(.regex = "(\w+)",  .replace = "*",  .string = "éà@É",  .expectedString = "*@*",   .expectedCount = 2,  .regexFlags = PCRE_UCP | PCRE_UTF8);
        test(.regex = "(\w+)",  .replace = "字", .string = "éà@É",  .expectedString = "字@字",.expectedCount = 2,  .regexFlags = PCRE_UCP | PCRE_UTF8);
        test(.regex = "(\w+)",  .replace = "字", .string = "éà@É",  .expectedString = "字",   .expectedCount = 2,  .regexFlags = PCRE_UCP | PCRE_UTF8, .bufferlen = 3);
    }

    server_print("^tChecking substitutions...");
    {
        test(.regex = "x",            .replace = "y",             .string = "text",     .expectedString = "teyt"      );
        test(.regex = "x",            .replace = "$",             .string = "text",     .expectedString = "te$t"      );
        test(.regex = "x",            .replace = "$1",            .string = "text",     .expectedString = "te$1t"     );
        test(.regex = "x",            .replace = "${1",           .string = "text",     .expectedString = "te${1t"    );
        test(.regex = "x",            .replace = "${",            .string = "text",     .expectedString = "te${t"     );
        test(.regex = "x",            .replace = "${$0",          .string = "text",     .expectedString = "te${xt"    );
        test(.regex = "x",            .replace = "${1}",          .string = "text",     .expectedString = "te${1}t"   );
        test(.regex = "x",            .replace = "${1}",          .string = "text",     .expectedString = "te${1}t"   );
        test(.regex = "x",            .replace = "$5",            .string = "text",     .expectedString = "te$5t"     );
        test(.regex = "x",            .replace = "$5",            .string = "te(x)t",   .expectedString = "te($5)t"   );
        test(.regex = "x",            .replace = "${foo",         .string = "text",     .expectedString = "te${foot"  );
        test(.regex = "(x)",          .replace = "$5",            .string = "text",     .expectedString = "te$5t"     );
        test(.regex = "(x)",          .replace = "$1",            .string = "text",     .expectedString = "text"      );
        test(.regex = "e(x)",         .replace = "$1",            .string = "text",     .expectedString = "txt"       );
        test(.regex = "e(x)",         .replace = "$5",            .string = "text",     .expectedString = "t$5t"      );
        test(.regex = "e(x)",         .replace = "$4",            .string = "text",     .expectedString = "t$4t"      );
        test(.regex = "e(x)",         .replace = "$3",            .string = "text",     .expectedString = "t$3t"      );
        test(.regex = "e(x)",         .replace = "${1}",          .string = "text",     .expectedString = "txt"       );
        test(.regex = "e(x)",         .replace = "${3}",          .string = "text",     .expectedString = "t${3}t"    );
        test(.regex = "e(x)",         .replace = "${1}${3}",      .string = "text",     .expectedString = "tx${3}t"   );
        test(.regex = "e(x)",         .replace = "${1}${name}",   .string = "text",     .expectedString = "tx${name}t");
        test(.regex = "e(?<foo>x)",   .replace = "${1}${name}",   .string = "text",     .expectedString = "tx${name}t");
        test(.regex = "e(?<foo>x)",   .replace = "${1}${foo}",    .string = "text",     .expectedString = "txxt"      );
        test(.regex = "e(?<foo>x)",   .replace = "${goll}${foo}", .string = "text",     .expectedString = "t${goll}xt");
        test(.regex = "e(?<foo>x)",   .replace = "${goll${foo}",  .string = "text",     .expectedString = "t${gollxt" );
        test(.regex = "e(?<foo>x)",   .replace = "${goll${foo}}", .string = "text",     .expectedString = "t${gollx}t");
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "${${foo}}",     .string = "text",     .expectedString = "t${x}t"    );
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "$${bfoo}}",     .string = "text",     .expectedString = "t${bfoo}}t");
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}",       .string = "text",     .expectedString = "t${foo}t"  );
        test(.regex = "e(?<foo>x)",   .replace = "$$",            .string = "text",     .expectedString = "t$t"       );
        test(.regex = "(e)(?<foo>x)", .replace = "${foo}$1$2",    .string = "text",     .expectedString = "txext"     );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2",    .string = "text",     .expectedString = "teext"     );
        test(.regex = "(e)(?<foo>x)", .replace = "${foo}$1$2$+",  .string = "text",     .expectedString = "txexxt"    );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$+",  .string = "text",     .expectedString = "teexxt"    );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$_",  .string = "texts",    .expectedString = "teextextsts");
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$`",  .string = "texts",    .expectedString = "teextts"   ),
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$'",  .string = "texts",    .expectedString = "teextsts"  ),
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$&",  .string = "texts",    .expectedString = "teexexts"  ),
        test(.regex = "x",            .replace = "y",             .string = "text",     .expectedString = "teyt"      );
        test(.regex = "x",            .replace = "$",             .string = "text",     .expectedString = "te$t"      );
        test(.regex = "x",            .replace = "$1",            .string = "text",     .expectedString = "te$1t"     );
        test(.regex = "x",            .replace = "${1}",          .string = "text",     .expectedString = "te${1}t"   );
        test(.regex = "x",            .replace = "$5",            .string = "text",     .expectedString = "te$5t"     );
        test(.regex = "x",            .replace = "$5",            .string = "te(x)t",   .expectedString = "te($5)t"   );
        test(.regex = "x",            .replace = "${foo",         .string = "text",     .expectedString = "te${foot"  );
        test(.regex = "(x)",          .replace = "$5",            .string = "text",     .expectedString = "te$5t"     );
        test(.regex = "(x)",          .replace = "$1",            .string = "text",     .expectedString = "text"      );
        test(.regex = "e(x)",         .replace = "$1",            .string = "text",     .expectedString = "txt"       );
        test(.regex = "e(x)",         .replace = "$5",            .string = "text",     .expectedString = "t$5t"      );
        test(.regex = "e(x)",         .replace = "$4",            .string = "text",     .expectedString = "t$4t"      );
        test(.regex = "e(x)",         .replace = "$3",            .string = "text",     .expectedString = "t$3t"      );
        test(.regex = "e(x)",         .replace = "${1}",          .string = "text",     .expectedString = "txt"       );
        test(.regex = "e(x)",         .replace = "${3}",          .string = "text",     .expectedString = "t${3}t"    );
        test(.regex = "e(x)",         .replace = "${1}${3}",      .string = "text",     .expectedString = "tx${3}t"   );
        test(.regex = "e(x)",         .replace = "${1}${name}",   .string = "text",     .expectedString = "tx${name}t");
        test(.regex = "e(?<foo>x)",   .replace = "${1}${name}",   .string = "text",     .expectedString = "tx${name}t");
        test(.regex = "e(?<foo>x)",   .replace = "${1}${foo}",    .string = "text",     .expectedString = "txxt"      );
        test(.regex = "e(?<foo>x)",   .replace = "${goll}${foo}", .string = "text",     .expectedString = "t${goll}xt");
        test(.regex = "e(?<foo>x)",   .replace = "${goll${foo}",  .string = "text",     .expectedString = "t${gollxt" );
        test(.regex = "e(?<foo>x)",   .replace = "${goll${foo}}", .string = "text",     .expectedString = "t${gollx}t");
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "${${foo}}",     .string = "text",     .expectedString = "t${x}t"    );
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "$${bfoo}}",     .string = "text",     .expectedString = "t${bfoo}}t");
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}}",      .string = "text",     .expectedString = "t${foo}}t" );
        test(.regex = "e(?<foo>x)",   .replace = "$${foo}",       .string = "text",     .expectedString = "t${foo}t"  );
        test(.regex = "e(?<foo>x)",   .replace = "$$",            .string = "text",     .expectedString = "t$t"       );
        test(.regex = "(e)(?<foo>x)", .replace = "${foo}$1$2",    .string = "text",     .expectedString = "txext"     );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2",    .string = "text",     .expectedString = "teext"     );
        test(.regex = "(e)(?<foo>x)", .replace = "${foo}$1$2$+",  .string = "text",     .expectedString = "txexxt"    );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$+",  .string = "text",     .expectedString = "teexxt"    );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$_",  .string = "texts",    .expectedString = "teextextsts");
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$`",  .string = "texts",    .expectedString = "teextts"   );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$'",  .string = "texts",    .expectedString = "teextsts"  );
        test(.regex = "(?<foo>e)(x)", .replace = "${foo}$1$2$&",  .string = "texts",    .expectedString = "teexexts"  );
        test(.regex = "<(.+?)>",                .replace = "[$0:$1]",     .string = "<i>am not</i>", .expectedString = "[<i>:i]am not[</i>:/i]");
        test(.regex =  "(?<foo>e)(?<foo>x)",    .replace = "${foo}$1$2",  .string = "text",      .expectedString = "teext", .regexFlags = PCRE_DUPNAMES);
        test(.regex = "\b(\w+)(\s)(\w+)\b",     .replace = "$3$2$1",      .string = "one two",   .expectedString = "two one");
        test(.regex = "\b(\d+)\s?USD",          .replace = "$$$1",        .string = "103 USD",   .expectedString = "$103"   );
        test(.regex = "\b(?<w1>\w+)(\s)(?<w2>\w+)\b", .replace = "${w2} ${w1}", .string = "one two", .expectedString = "two one");
        test(.regex = "(\$*(\d*(\.+\d+)?){1})", .replace = "**$&",        .string = "$1.30",     .expectedString = "**$1.30**");
        test(.regex = "B+",                     .replace = "$`",          .string = "AABBCC",    .expectedString = "AAAACC");
        test(.regex = "B+",                     .replace = "$'",          .string = "AABBCC",    .expectedString = "AACCCC");
        test(.regex = "B+(C+)",                 .replace = "$+",          .string = "AABBCCDD",  .expectedString = "AACCDD");
        test(.regex = "B+",                     .replace = "$_",          .string = "AABBCC",    .expectedString = "AAAABBCCCC");
        test(.regex = "(F)(2)(3)(4)(5)(6)(7)(8)(9)(10)(L)\11",  .replace = "${S}$11$1", .string = "F2345678910L71", .expectedString = "F2345678910L71"),
        test(.regex = "(F)(2)(3)(4)(5)(6)(7)(8)(9)(10)(L)\11",  .replace = "${S}$11$1", .string = "F2345678910LL1", .expectedString = "${S}LF1");
    }

    server_print("^tChecking moar #1...");
    {
        test(.string = "(?(w)a|o)"       , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)|o)"        , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)a)"         , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)a|)"        , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)?|a|o)"     , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)||o)"       , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w)(a)"        , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(w))\a|)"      , .regex = "\(\?\(\w+\).*\|?.*\)"                              , .replace = "r", .expectedString = "r");
        test(.string = "(?(2)a|o)"       , .regex = "\(\?\([^^\)]+\).*\|?.*\)"                          , .replace = "r", .expectedString = "r");
        test(.string = "(?(|)a|o)"       , .regex = "\(\?\([^^\)]+\).*\|?.*\)"                          , .replace = "r", .expectedString = "r");
        test(.string = "a\3b"            , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\(\d+)"          , .replace = "\5", .expectedString = "a\5b");
        test(.string = "\3b"             , .regex = "(?=(?:\A|[^^\\])(?:[\\]{2}){0,3})\\(\d+)"          , .replace = "\5", .expectedString = "\5b");
        test(.string = "\\\3b"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\(\d+)"          , .replace = "\5", .expectedString = "\\\5b");
        test(.string = "\\\k<g>"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k<(\w+)>"       , .replace = "\5", .expectedString = "\\\5");
        test(.string = "\\\\k'g'"        , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w+)'"       , .replace = "\5", .expectedString = "\\\\k'g'");
        test(.string = "a\\\\k'g'"       , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w+)'"       , .replace = "\5", .expectedString = "a\\\\k'g'");
        test(.string = "\k'g'"           , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w+)'"       , .replace = "\5", .expectedString = "\5");
        test(.string = "(?<n1-n2>)"      , .regex = "\(\?<[A-Za-z]\w*-[A-Za-z]\w*>.*\)"                 , .replace = "r", .expectedString = "r");
        test(.string = "(?'n1-n2'a)"     , .regex = "\(\?'[A-Za-z]\w*-[A-Za-z]\w*'.*\)"                 , .replace = "r", .expectedString = "r");
        test(.string = "\p{Isa}"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\\[pP]\{)Is(?=\w+\})", .replace = "In", .expectedString = "\p{Ina}");
        test(.string = "\p{Is}"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\\[pP]\{)Is(?=\w+\})", .replace = "In", .expectedString = "\p{Is}");
        test(.string = "\p{Isa"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\\[pP]\{)Is(?=\w+\})", .replace = "In", .expectedString = "\p{Isa");
        test(.string = "a(?#|)"          , .regex = "\(\?#[^^\)]*\)"                                    , .replace = "", .expectedString = "a");
        test(.string = "(?#|)"           , .regex = "\(\?#[^^\)]*\)"                                    , .replace = "", .expectedString = "");
        test(.string = "(?#|)"           , .regex = "\#[^^\n\r]*"                                       , .replace = "", .expectedString = "(?");
        test(.string = "(?inm-xs:\#)"    , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?[imsx]*n[-imsx]*:[^^\)]+\)", .replace = "r", .expectedString = "(r");
        test(.string = "(?ni:())"        , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?[imsx]*n[-imsx]*:[^^\)]+\)", .replace = "r", .expectedString = "(r)");
        test(.string = "(?x-i:)"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?[imsx]*n[-imsx]*:[^^\)]+\)", .replace = "r", .expectedString = "(?x-i:)");
        test(.string = "(?n:))"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?[imsx]*n[-imsx]*:[^^\)]+\)", .replace = "r", .expectedString = "(?n:))");
        test(.string = "(?<n1>)"         , .regex = "\(\?<[A-Za-z]\w*>.*\)"                             , .replace = "r", .expectedString = "r");
        test(.string = "(?'n1'y)"        , .regex = "\(\?'[A-Za-z]\w*'.*\)"                             , .replace = "r", .expectedString = "r");
        test(.string = "(?<45>y)"        , .regex = "\(\?<\d+>.*\)"                                     , .replace = "r", .expectedString = "r");
        test(.string = "(?'7'o)"         , .regex = "\(\?'\d+'.*\)"                                     , .replace = "r", .expectedString = "r");
        test(.string = "\\\("            , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "\\r");
        test(.string = "a\\\("           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "a\\r");
        test(.string = "\\("             , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "\r");
        test(.string = "a\\("            , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "a\r");
        test(.string = "\("              , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "r");
        test(.string = "a\("             , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\\("             , .replace = "r", .expectedString = "ar");
        test(.string = "?:"              , .regex = "(?:^^\?[:imnsx=!>-]|^^\?<[!=])"                    , .replace = "r", .expectedString = "r");
        test(.string = "?<!"             , .regex = "(?:^^\?[:imnsx=!>-]|^^\?<[!=])"                    , .replace = "r", .expectedString = "r");
        test(.string = "?-"              , .regex = "(?:^^\?[:imnsx=!>-]|^^\?<[!=])"                    , .replace = "r", .expectedString = "r");
        test(.string = "\(?<n>"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?<[A-Za-z]\w*>", .replace = "r", .expectedString = "\(r");
        test(.string = "a\(?'n'"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?'[A-Za-z]\w*'", .replace = "r", .expectedString = "a\(r");
        test(.string = "\\(?<2>"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?<\d+>"        , .replace = "r", .expectedString = "\\(r");
        test(.string = "(?'2'"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3}\()\?'\d+'"        , .replace = "r", .expectedString = "(r");
        test(.string = "\[\b]"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\\b(?=[^^\[\]]*\])", .replace = "\\u8", .expectedString = "\[\u8]");
        test(.string = "\[a\bb]"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\\b(?=[^^\[\]]*\])", .replace = "\\u8", .expectedString = "\[a\u8b]");
        test(.string = "\[\b]"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\\b(?=[^^\[\]]*\])", .replace = "\\u8", .expectedString = "\[\u8]");
        test(.string = "\[\b]"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\\b(?=[^^\[\]]*\])", .replace = "\\u8", .expectedString = "\[\u8]");
        test(.string = "\[\\b]"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\\b(?=[^^\[\]]*\])", .replace = "\\u8", .expectedString = "\[\\u8]");
        test(.string = "[[]"             , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\[(?=[^^\[\]]*\])" , .replace = "\\[" , .expectedString = "[\[]");
        test(.string = "\[[]"            , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\[(?=[^^\[\]]*\])" , .replace = "\["  , .expectedString = "\[[]");
        test(.string = "\[\[]"           , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\[(?=[^^\[\]]*\])" , .replace = "\["  , .expectedString = "\[\[]");
        test(.string = "\[\[]"           , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2})*(?:\[|\[[^^\[\]]*[^^\[\]\\])(?:[\\]{2})*)\[(?=[^^\[\]]*\])" , .replace = "\["  , .expectedString = "\[\[]");
        test(.string = "\{"              , .regex = "(?=(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\{", .expectedString = "\\{");
        test(.string = "\{"              , .regex = "(?=(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\{", .expectedString = "\\{");
        test(.string = "\{1,2}"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\{", .expectedString = "\{1,2}");
        test(.string = "\{1}"            , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\{", .expectedString = "\{1}");
        test(.string = "\{1,}"           , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\{", .expectedString = "\{1,}");
        test(.string = "\{1"             , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2})*)\{(?!\d\d*(,(\d\d*)?)?\})", .replace = "\\{", .expectedString = "\{1");
        test(.string = "\\(?!{1}"        , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "\5", .expectedString = "?!");
        test(.string = "{1}"             , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "r", .expectedString = "r");
        test(.string = "({1}"            , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "r", .expectedString = "r");
        test(.string = "(?{1}"           , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "r", .expectedString = "(?{1}");
        test(.string = "(?:{1}"          , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "r", .expectedString = "r");
        test(.string = "\({1}"           , .regex = "(\A|((\A|[^^\\])([\\]{2})*\((\?([:>=!]|<([=!]|(\w+>))))?))\{\d+(,(\d+)?)?\}", .replace = "r", .expectedString = "\({1}");
        test(.string = "\p{Isa}"         , .regex = "(?!\\[pP]\{)Is(?=\w+\})"                       , .replace = "In", .expectedString = "\p{Ina}");
        test(.string = "\p{Is}"          , .regex = "(?!\\[pP]\{)Is(?=\w+\})"                       , .replace = "In", .expectedString = "\p{Is}");
        test(.string = "\p{Isa"          , .regex = "(?!\\[pP]\{)Is(?=\w+\})"                       , .replace = "In", .expectedString = "\p{Isa");
        test(.string = "\}"              , .regex = "(?!(\\A|[^^\\])(\\{2})*\\{\\d\\d*(,(\\d\\d*)?)?)\\}", .replace = "\\}", .expectedString = "\}");
        test(.string = "{\}"             , .regex = "(?!(\A|[^^\^^])(\^^{2})*\{\d\d*(,(\d\d*)?)?)\}", .replace = "\\}", .expectedString = "{\\}");
        test(.string = "{1,2}"           , .regex = "(?!(\A|[^^\\])(\\{2})*\{\d\d*(,(\d\d*)?)?)\}"  , .replace = "\\}", .expectedString = "{1,2\}");
        test(.string = "\{1}"            , .regex = "(?!(\A|[^^\\])(\\{2})*\{\d\d*(,(\d\d*)?)?)\}"  , .replace = "\\}", .expectedString = "\{1\}");
        test(.string = "\{1\}"           , .regex = "(?!(\A|[^^\\])(\\{2})*\{\d\d*(,(\d\d*)?)?)\}"  , .replace = "\\}", .expectedString = "\{1\\}");
        test(.string = "\{1}"            , .regex = "(?!(\A|[^^\\])(\\{2})*\{\d\d*(,(\d\d*)?)?)\}"  , .replace = "\\}", .expectedString = "\{1\}");
        test(.string = "{1,}"            , .regex = "(?!(\A|[^^\\])(\\{2})*\{\d\d*(,(\d\d*)?)?)\}"  , .replace = "\\}", .expectedString = "{1,\}");
        test(.string = "a(?<!b*c)"       , .regex = "\(\?\<[=!][^^\)]*(?:[\*\+]|\{\d+,\}).*\)"      , .replace = "r",   .expectedString = "ar");
        test(.string = "a(?<!b+c)"       , .regex = "\(\?\<[=!][^^\)]*(?:[\*\+]|\{\d+,\}).*\)"      , .replace = "r",   .expectedString = "ar");
        test(.string = "(?<!b{1}c))"     , .regex = "\(\?\<[=!][^^\)]*(?:[\*\+]|\{\d+,\}).*\)"      , .replace = "r",   .expectedString = "(?<!b{1}c))");
        test(.string = "(?<!b{1,}c)"     , .regex = "\(\?\<[=!][^^\)]*(?:[\*\+]|\{\d+,\}).*\)"      , .replace = "r",   .expectedString = "r");
        test(.string = "(?<!b{1,4}c)"    , .regex = "\(\?\<[=!][^^\)]*(?:[\*\+]|\{\d+,\}).*\)"      , .replace = "r",   .expectedString = "(?<!b{1,4}c)");
        test(.string = "a\3b"            , .regex = "\\(\d+)"                                       , .replace = "\5",  .expectedString = "a\5b");
        test(.string = "\3b"             , .regex = "\\(\d+)"                                       , .replace = "\5",  .expectedString = "\5b");
        test(.string = "\\3b"            , .regex = "(?!\\\\)\\(\d)"                                , .replace = "\5", . expectedString = "\\5b");
        test(.string = "a\\3b"           , .regex = "(?:(\\){0,3})\\(\d)"                           , .replace = "\5", . expectedString = "a\5b");
        test(.string = "\\k<g>"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k<(\w)>"    , .replace = "\5",  .expectedString = "\\5");
        test(.string = "a\\k<g>"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k<(\w)>"    , .replace = "\5",  .expectedString = "a\\5");
        test(.string = "\\k'g'"          , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w)'"    , .replace = "\5",  .expectedString = "\\5");
        test(.string = "a\\k'g'"         , .regex = "(?!(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w)'"    , .replace = "\5",  .expectedString = "a\\5");
        test(.string = "\k'g'"           , .regex = "(?:(?:\A|[^^\\])(?:[\\]{2}){0,3})\\k'(\w)'"    , .replace = "\5",  .expectedString = "\5");
    }

    server_print("^tChecking moar #2...");
    {
        test(.regex          = "^^((?>[a-zA-Z\d!#$%&'*+\-\/=?^^_`{|}~]+\x20*|^"((?=[\x01-\x7f])[^^^"\\]|\\[\x01-\x7f])*^"\x20*)*(?<angle><))?((?!\.)(?>\.?[a-zA-Z\d!#$%&'*+\-\/=?^^_`{|}~]+)+|^"((?=[\x01-\x7f])[^^^"\\]|\\[\x01-\x7f])*^")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d)){4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^^\\\[\]]|\\[\x01-\x7f])+)\])(?(angle)>)$"    ,
             .replace        = "$1$4@$7net>",
             .string         = "Name Surname <name.surname@blah.com>",
             .expectedString = "Name Surname <name.surname@blah.net>"
            );

        test(.regex         = "([A-Z])\w+",
             .replace       = "*snip*",
             .string        = "Welcome to RegExr v2.0 by gskinner.com!\
                                                                   \
                            Edit the Expression & Text to see matches. Roll over matches or the expression for details. Undo mistakes with ctrl-z. Save & Share expressions with friends or the Community. A full Reference & Help is available in the Library, or watch the video Tutorial.\
                                                                   \
                            Sample text for testing:\
                            abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            :0123456789 +-.,!@#$%^^&*();\/|<>^"'\
                            12345 -98.7 3.141 .6180 9,000 +42\
                            555.123.4567    +1-(800)-555-2468\
                            foo@demo.net    bar.ba@test.co.uk\
                            www.demo.com    http://foo.co.uk/\
                            http://regexr.com/foo.html?q=bar",

             .expectedString = "*snip* to *snip* v2.0 by gskinner.com!\
                                                                   \
                            *snip* the *snip* & *snip* to see matches. *snip* over matches or the expression for details. *snip* mistakes with ctrl-z. *snip* & *snip* expressions with friends or the *snip*. A full *snip* & *snip* is available in the *snip*, or watch the video *snip*.\
                                                                   \
                            *snip* text for testing:\
                            abcdefghijklmnopqrstuvwxyz *snip*\
                            :0123456789 +-.,!@#$%^^&*();\/|<>^"'\
                            12345 -98.7 3.141 .6180 9,000 +42\
                            555.123.4567    +1-(800)-555-2468\
                            foo@demo.net    bar.ba@test.co.uk\
                            www.de",
             .regexFlags = PCRE_EXTENDED
         );

        test(.regex          = "/\*(?>[^^*/]+|\*[^^/]|/[^^*]|/\*(?>[^^*/]+|\*[^^/]|/[^^*])*\*/)*\*/",
             .replace        = "",
             .string         = "/* comment */\
                                no comment\
                                /* comment\
                                   spanning\
                                   multiple\
                                   lines */\
                                /* comment /* nesting */ of /* two */ levels supported */\
                                /* comment /* nesting */ of /* /* more than */ two levels */ not supported */",
             .expectedString = "no comment\
                                /* comment  of  not supported */"
         );

        test(.regex          = "\b(?<protocol>https?|ftp)://(?<domain>[A-Z0-9.-]+)(?<file>/[A-Z0-9+&@#/%=~_|!:,.;-]*)?(?<parameters>\?[A-Z0-9+&@#/%=~_|!:,.;]*)?",
             .replace        = "${protocol}s://site.com${file}^n",
             .string         = "http://www.alliedmods.net http://www.alliedmods.net/ http://www.alliedmods.net/test.php http://www.alliedmods.net/index.php?secret=x Something interesting at http://www.alliedmods.net.",
             .expectedString = "https://site.com^nhttps://site.com/^nhttps://site.com/test.php^nhttps://site.com/index.php^nhttps://site.com^n",
             .regexFlags     = PCRE_CASELESS | PCRE_EXTENDED,
             .formatFlags    = REGEX_FORMAT_NOCOPY
         );

        test(.regex          = "\b(https?|ftp)://([A-Z0-9.-]+)(/[A-Z0-9+&@#/%=~_|!:,.;-]*)?(\?[A-Z0-9+&@#/%=~_|!:,.;]*)?",
             .replace        = "$1s://site.com$3^n",
             .string         = "http://www.alliedmods.net http://www.alliedmods.net/ http://www.alliedmods.net/test.php http://www.alliedmods.net/index.php?secret=x Something interesting at http://www.alliedmods.net.",
             .expectedString = "https://site.com^nhttps://site.com/^nhttps://site.com/test.php^nhttps://site.com/index.php^nhttps://site.com^n",
             .regexFlags     = PCRE_CASELESS | PCRE_EXTENDED,
             .formatFlags    = REGEX_FORMAT_NOCOPY
         );

        test(.regex          = "\b(https?|ftp)://([A-Z0-9.-]+)(/[A-Z0-9+&@#/%=~_|!:,.;-]*)?(\?[A-Z0-9+&@#/%=~_|!:,.;]*)?",
             .replace        = "$1s://site.com$3^n",
             .string         = "http://www.alliedmods.net http://www.alliedmods.net/ http://www.alliedmods.net/test.php http://www.alliedmods.net/index.php?secret=x Something interesting at http://www.alliedmods.net.",
             .expectedString = "https://site.com^n",
             .regexFlags     = PCRE_CASELESS | PCRE_EXTENDED,
             .formatFlags    = REGEX_FORMAT_NOCOPY | REGEX_FORMAT_FIRSTONLY
         );

        test(.regex          = "^^(.++)\r?\n(?=(?:^^(?!\1$).*+\r?\n)*+\1$)",
             .replace        = "",
             .string         = "one^n\
                                two^n\
                                three^n\
                                four^n\
                                two^n\
                                three^n\
                                four^n\
                                three^n\
                                four^n\
                                four",
             .expectedString = "one^n\
                                two^n\
                                three^n\
                                four",
             .regexFlags     = PCRE_EXTENDED | PCRE_MULTILINE
         );
    }

    end();
}
