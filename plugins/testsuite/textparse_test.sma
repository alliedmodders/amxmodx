/** 
 * vim: set ts=4 sw=4 tw=99 noet:
 *
 * AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
 * Copyright (C) The AMX Mod X Development Team.
 *
 * This software is licensed under the GNU General Public License, version 3 or higher.
 * Additional exceptions apply. For full license details, see LICENSE.txt or visit:
 *     https://alliedmods.net/amxmodx-license
 */
 
#include <amxmodx>

new SuccessCount;
new Trie:ExpectedKVData;
new bool:Debug;

public plugin_init()
{
    register_concmd("textparse", "ConsoleCommand_TextParse");

    Debug = !!(plugin_flags() & AMX_FLAG_DEBUG);
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

    new SMCParser:parser = SMC_CreateParser();

    SMC_SetReaders(parser, "ReadCore_KeyValue", "ReadCore_NewSection", "ReadCore_EndSection");
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

public ReadCore_ParseStart(SMCParser:handle)
{
    Debug && server_print("ReadCore_ParseStart");
    ++SuccessCount;
}

public ReadCore_NewSection(SMCParser:handle, const name[])
{
    Debug && server_print("^tReadCore_NewSection - %s", name);
    ++SuccessCount;
}

public ReadCore_KeyValue(SMCParser:handle, const key[], const value[])
{
    Debug && server_print("^t^tReadCore_KeyValue - %-32s %s", key, value);
    
    new buffer[128];
    if (TrieGetString(ExpectedKVData, key, buffer, charsmax(buffer)) && equal(value, buffer))
    {
        ++SuccessCount;
    }
}

public ReadCore_EndSection(SMCParser:handle)
{
    Debug && server_print("^tReadCore_EndSection");
    ++SuccessCount;
}

public ReadCore_CurrentLine(SMCParser:handle, const line[], lineno)
{
    //Debug && server_print("^t^tReadCore_CurrentLine - %s", line);
    ++SuccessCount;
}

public ReadCore_ParseEnd(SMCParser:handle, bool:halted, bool:failed)
{  
    Debug &&server_print("ReadCore_ParseEnd - halted: %s, failed: %s", halted ? "yes" : "no", failed ? "yes" : "no");
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

    new INIParser:parser = INI_CreateParser();

    INI_SetReaders(parser, "ReadCSDM_KeyValue", "ReadCSDM_NewSection");
    INI_SetParseStart(parser, "ReadCSDM_ParseStart");
    INI_SetRawLine(parser, "ReadCSDM_CurrentLine");
    INI_SetParseEnd(parser, "ReadCSDM_ParseEnd");

    new line, col;
    new bool:result = INI_ParseFile(parser, configFile, line, col);

    if (!result)
    {
        server_print("^tFatal parse error");
    }

    if (line == expectedLineCount + 1)
    {
        ++SuccessCount;
    }

    server_print("^tTests successful: %d/%d", SuccessCount, expectedStartEndCount + expectedSectionCount + expectedKeyValueCount + expectedLineCount + 1);

    INI_DestroyParser(parser);
    TrieDestroy(ExpectedKVData);
}

public ReadCSDM_ParseStart(INIParser:handle)
{
    Debug && server_print("ReadCSDM_ParseStart");
    ++SuccessCount;
}

public ReadCSDM_NewSection(INIParser:handle, const section[], bool:invalid_tokens, bool:close_bracket, bool:extra_tokens, curtok)
{
    Debug && server_print("^tReadCSDM_NewSection - [%s] (invalid_tokens: '%s', close_bracked: '%s', extra_tokens: '%s')", section, invalid_tokens ? "yes" : "no", close_bracket ? "yes" : "no", extra_tokens ? "yes" : "no");
    
    if (TrieKeyExists(ExpectedKVData, section))
    {
        if ((equal(section, "secondary") && !extra_tokens) ||
            (equal(section, "botsecondary") && close_bracket))
        {
            return true;
        }

        ++SuccessCount;
    }
    
    return true;
}

public bool:ReadCSDM_KeyValue(INIParser:handle, const key[], const value[], bool:invalid_tokens, bool:equal_token, bool:quotes, curtok)
{
    Debug && server_print("^t^tReadCSDM_KeyValue - %-32s %s", key, value);

    new buffer[128];
    if (TrieGetString(ExpectedKVData, key, buffer, charsmax(buffer)) && equal(value, buffer))
    {
        if (equal(key, "colors") && !quotes)
        {
            return true;
        }

        ++SuccessCount;
    }
    
    return true;
}

public bool:ReadCSDM_CurrentLine(INIParser:handle, const line[], curtok)
{
    //Debug && server_print("^t^tReadCSDM_CurrentLine - %s", line);
    ++SuccessCount;
    return true;
}

public ReadCSDM_ParseEnd(INIParser:handle, bool:halted, bool:failed)
{
    Debug && server_print("ReadCSDM_ParseStart");
    ++SuccessCount;
}