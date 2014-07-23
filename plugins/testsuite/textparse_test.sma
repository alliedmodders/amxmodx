#include <amxmodx>
#include <textparse>

new SuccessCount;
new Trie:ExpectedKVData;

public plugin_init()
{
    register_concmd("textparse", "ConsoleCommand_TextParse");
}

public ConsoleCommand_TextParse()
{
    InitializeTextParseSMC();
    InitializeTextParseINI();
}

/**
 * SMC Config format
 */

InitializeTextParseSMC()
{
    SuccessCount = 0;

    server_print("Testing text parser with SMC config file format...");

    new const configFile[] = "addons/amxmodx/scripting/testsuite/textparse_test.cfg";

    ExpectedKVData = TrieCreate();
    TrieSetString(ExpectedKVData, "Logging", "on");
    TrieSetString(ExpectedKVData, "LogMode", "daily");
    TrieSetString(ExpectedKVData, "ServerLang", "en");
    TrieSetString(ExpectedKVData, "PublicChatTrigger", "!");
    TrieSetString(ExpectedKVData, "SilentChatTrigger", "/");
    TrieSetString(ExpectedKVData, "SilentFailSuppress", "no");
    TrieSetString(ExpectedKVData, "PassInfoVar", "_password");
    TrieSetString(ExpectedKVData, "MenuItemSound", "buttons/button14.wav");
    TrieSetString(ExpectedKVData, "MenuExitSound", "buttons/combine_button7.wav");
    TrieSetString(ExpectedKVData, "MenuExitBackSound", "buttons/combine_button7.wav");
    TrieSetString(ExpectedKVData, "AllowClLanguageVar", "On");
    TrieSetString(ExpectedKVData, "DisableAutoUpdate", "no");
    TrieSetString(ExpectedKVData, "ForceRestartAfterUpdate", "no");
    TrieSetString(ExpectedKVData, "AutoUpdateURL", "http://update.sourcemod.net/update/");
    TrieSetString(ExpectedKVData, "DebugSpew", "no");
    TrieSetString(ExpectedKVData, "SteamAuthstringValidation", "yes");
    TrieSetString(ExpectedKVData, "BlockBadPlugins", "yes");
    TrieSetString(ExpectedKVData, "SlowScriptTimeout", "8");

    new const expectedSectionCount  = 2; // Include start and end.
    new const expectedStartEndCount = 2;
    new const expectedKeyValueCount = TrieGetSize(ExpectedKVData);
    new const expectedLineCount     = file_size(configFile, .flag = 1) - 1;

    new TextParser:parser = SMC_CreateParser();

    SMC_SetReaders(parser, "ReadCore_NewSection", "ReadCore_KeyValue", "ReadCore_EndSection");
    SMC_SetParseStart(parser, "ReadCore_ParseStart");
    SMC_SetRawLine(parser, "ReadCore_CurrentLine");
    SMC_SetParseEnd(parser, "ReadCore_ParseEnd");

    new line, col;
    new SMCError:err = SMC_ParseFile(parser, configFile, line, col);

    if (err != SMCError_Okay)
    {
        new buffer[64];
        server_print("%s", SMC_GetErrorString(err, buffer, charsmax(buffer)) ? buffer : "Fatal parse error");
    }

    if (line == expectedLineCount + 1 && col == 2)
    {
        ++SuccessCount;
    }

    server_print("^tTests successful: %d/%d", SuccessCount, expectedStartEndCount + expectedSectionCount + expectedKeyValueCount + expectedLineCount + 1);

    SMC_DestroyParser(parser);
    TrieDestroy(ExpectedKVData);
    SuccessCount = 0;
}

public ReadCore_ParseStart(TextParser:handle)
{
    ++SuccessCount;
}

public ReadCore_NewSection(TextParser:handle, const name[])
{
    ++SuccessCount;
}

public ReadCore_KeyValue(TextParser:handle, const key[], const value[])
{
    new buffer[128];
    if (TrieGetString(ExpectedKVData, key, buffer, charsmax(buffer)) && equal(value, buffer))
    {
        ++SuccessCount;
    }
}

public ReadCore_EndSection(TextParser:handle)
{
    ++SuccessCount;
}

public ReadCore_CurrentLine(TextParser:handle, const line[], lineno)
{
   ++SuccessCount;
}

public ReadCore_ParseEnd(TextParser:handle, bool:halted, bool:failed)
{
    ++SuccessCount;
}


/**
 * INI Config format
 */

public InitializeTextParseINI()
{
    SuccessCount = 0;

    server_print("Testing text parser with INI config file format...");

    new const configFile[] = "addons/amxmodx/scripting/testsuite/textparse_test.ini";

    ExpectedKVData = TrieCreate();
    TrieSetString(ExpectedKVData, "settings", "");
    TrieSetString(ExpectedKVData, "enabled", "1");
    TrieSetString(ExpectedKVData, "strip_weapons", "1");
    TrieSetString(ExpectedKVData, "weapons_stay", "0");
    TrieSetString(ExpectedKVData, "spawnmode", "preset");
    TrieSetString(ExpectedKVData, "remove_bomb", "1");
    TrieSetString(ExpectedKVData, "spawn_wait_time", "0.75");
    TrieSetString(ExpectedKVData, "protection", "");
    TrieSetString(ExpectedKVData, "colors", "0 255 0 200");
    TrieSetString(ExpectedKVData, "time", "time");
    TrieSetString(ExpectedKVData, "secondary", "");
    TrieSetString(ExpectedKVData, "usp USP 1", "");
    TrieSetString(ExpectedKVData, "glock18 Glock 1", "");
    TrieSetString(ExpectedKVData, "deagle Deagle 1", "");
    TrieSetString(ExpectedKVData, "botsecondary", "");
    TrieSetString(ExpectedKVData, "deagle", "");
    TrieSetString(ExpectedKVData, "usp", "");

    new const expectedSectionCount  = 4;
    new const expectedStartEndCount = 2;
    new const expectedKeyValueCount = TrieGetSize(ExpectedKVData) - expectedSectionCount;
    new const expectedLineCount     = TrieGetSize(ExpectedKVData); // This doesn't include blanck/comments line.

    new TextParser:parser = SMC_CreateParser(.ini_format = true);

    SMC_SetReaders(parser, "ReadCSDM_NewSection", "ReadCSDM_KeyValue");
    SMC_SetParseStart(parser, "ReadCSDM_ParseStart");
    SMC_SetRawLine(parser, "ReadCSDM_CurrentLine");
    SMC_SetParseEnd(parser, "ReadCSDM_ParseEnd");

    new line, col;
    new SMCError:err = SMC_ParseFile(parser, configFile, line, col);

    if (err != SMCError_Okay)
    {
        new buffer[64];
        server_print("Error: %s", SMC_GetErrorString(err, buffer, charsmax(buffer)) ? buffer : "Fatal parse error");
    }

    if (line == expectedLineCount + 1)
    {
        ++SuccessCount;
    }

    server_print("^tTests successful: %d/%d", SuccessCount, expectedStartEndCount + expectedSectionCount + expectedKeyValueCount + expectedLineCount + 1);

    SMC_DestroyParser(parser);
    TrieDestroy(ExpectedKVData);
}

public ReadCSDM_ParseStart(TextParser:handle)
{
    ++SuccessCount;
}

public ReadCSDM_NewSection(TextParser:handle, const section[], bool:invalid_tokens, bool:close_bracket, bool:extra_tokens, curtok)
{
    if (TrieKeyExists(ExpectedKVData, section))
    {
        if ((equal(section, "secondary") && !extra_tokens) ||
            (equal(section, "botsecondary") && close_bracket))
        {
            return;
        }

        ++SuccessCount;
    }
}

public ReadCSDM_KeyValue(TextParser:handle, const key[], const value[], bool:invalid_tokens, bool:equal_token, bool:quotes, curtok)
{
    new buffer[128];
    if (TrieGetString(ExpectedKVData, key, buffer, charsmax(buffer)) && equal(value, buffer))
    {
        if (equal(value, "colors") && !quotes)
        {
            return;
        }

        ++SuccessCount;
    }
}

public ReadCSDM_CurrentLine(TextParser:handle, const line[], lineno, curtok)
{
   ++SuccessCount;
}

public ReadCSDM_ParseEnd(TextParser:handle, bool:halted, bool:failed)
{
    ++SuccessCount;
}