#include <amxmodx>
#include <textparse>

new SuccessCount;
new Trie:ExpectedKVData;

public plugin_init()
{
    register_concmd("textparse_vdf", "ConsoleCommand_TextParseVDF");
    register_clcmd("textparse_ini", "ServerCommand_TextParseINI");
}

/**
 * VDF Config format
 */

public ConsoleCommand_TextParseVDF()
{
    server_print("Testing text parser with VDF config file format...");

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
    new SMCError:err = SMC_ParseFile_VDF(parser, configFile, line, col);

    if (err != SMCError_Okay)
    {
        new buffer[64];
        server_print("%s", SMC_GetErrorString(err, buffer, charsmax(buffer)) ? buffer : "Fatal parse error");
    }

    if (line == expectedLineCount + 1 && col == 2)
    {
        ++SuccessCount;
    }

    SMC_DestroyParser(parser);

    server_print("^tTests successful: %d/%d", SuccessCount, expectedStartEndCount + expectedSectionCount + expectedKeyValueCount + expectedLineCount + 1);
}

public ReadCore_ParseStart(TextParser:smc)
{
    ++SuccessCount;
}

public ReadCore_NewSection(TextParser:smc, const name[])
{
    ++SuccessCount;
}

public ReadCore_KeyValue(TextParser:smc, const key[], const value[])
{
    new buffer[128];
    if (TrieGetString(ExpectedKVData, key, buffer, charsmax(buffer)) && equal(value, buffer))
    {
        ++SuccessCount;
    }
}

public ReadCore_EndSection(TextParser:smc)
{
    ++SuccessCount;
}

public ReadCore_CurrentLine(TextParser:smc, const line[], lineno)
{
   ++SuccessCount;
}

public ReadCore_ParseEnd(TextParser:smc)
{
    ++SuccessCount;
}



/**
 * INI Config format
 */
public ServerCommand_TextParseINI()
{

}
