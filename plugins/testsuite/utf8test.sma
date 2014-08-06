// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

/**
 * Warning: To get expected result, file encoding must be UTF-8 without BOM.
 */
 
public plugin_init()
{
    register_plugin("UTF-8 Test", AMXX_VERSION_STR, "AMXX Dev Team");
    register_srvcmd("utf8test", "OnServerCommand");
}

new ErrorCount;
new TestNumber;

enum TestType
{
    TT_Equal = 0,
    TT_LessThan,
    TT_GreaterThan,
    TT_LessThanEqual,
    TT_GreaterThanEqual,
    TT_NotEqual
};

new const TestWords[TestType][] = 
{
    "==",
    "<",
    ">",
    "<=",
    ">=",
    "!="
};

test(any:a, any:b = true, TestType:type = TT_Equal)
{
    ++TestNumber;
    
    new passed = 0;
    
    switch (type)
    {
        case TT_Equal:              passed = a == b;
        case TT_LessThan:           passed = a < b;
        case TT_GreaterThan:        passed = a > b;
        case TT_LessThanEqual:      passed = a <= b;
        case TT_GreaterThanEqual:   passed = a >= b;
        case TT_NotEqual:           passed = a != b;
    }
    
    if (!passed)
    {
        server_print("^tFailed test #%d (%d %s %d)", TestNumber, a, bool:TestWords[type], b);
        ErrorCount++;
    }
}

showResult()
{
    if (!ErrorCount)
    {
        server_print("All tests passed (%d/%d).", TestNumber, TestNumber);
    }
    else
    {
        server_print("Test failed %d/%d, aborting.", TestNumber - ErrorCount, TestNumber);
    }
}
 
public OnServerCommand()  
{
    /**
     * Initiliaze some data.
     */
    new reference[] = "𤭢hi AMXX® Hello㋡ crab?ൠ";
    
    new Array:a = ArrayCreate(sizeof reference);
    ArrayPushString(a, reference);
    
    new Trie:t = TrieCreate();
    TrieSetString(t, "reference", reference);
    
    new DataPack:d = CreateDataPack();
    WritePackString(d, reference);
    ResetPack(d);
    
    set_localinfo("reference", reference);
    
    
    server_print("Counting character bytes...");
    {
        test(get_char_bytes("®") == 2);
        test(get_char_bytes("㋡") == 3);
        test(get_char_bytes("𤭢") == 4);
        test(get_char_bytes("ൠ") == 3);
    }
    
    server_print("Checking character bytes...");
    {
        /**
         * is_char_mb() returns also number of bytes if not 0.
         */
        test(is_char_mb(reference[0]) != 0);  // 𤭢
        test(is_char_mb(reference[11]) != 0); // ®
        test(is_char_mb(reference[19]) != 0); // ㋡
        test(is_char_mb(reference[29]) != 0); // ൠ
    }
    
    server_print("Checking truncated character bytes - atcprintf...");
    {
        /**
         * Truncating '𤭢' at different index. '𤭢' = 4 bytes
         * A buffer of 4 = 3 bytes + EOS.
         * Expected result: empty buffer.
         */
        new buffer1[4]; 
        for(new i = charsmax(buffer1), length1; i >= 0; --i)
        {
            length1 = formatex(buffer1, i, "%s", reference);
            test(buffer1[0] == EOS && length1 == 0); 
        }
        
        /**
         * Truncating inside '®'.
         * Expected result: '®' should be skipped.
         */
        new buffer2[12];
        new length2 = formatex(buffer2, charsmax(buffer2), "%s", reference);
        test(strcmp(buffer2, "𤭢hi AMXX") == 0 && length2 == strlen("𤭢hi AMXX"));
        
        /**
         * Truncating inside 'ൠ'.
         * Buffer of 14: Enough to hold "㋡ crab?ൠ"
         * Retrieve 11 characters using precision format from '㋡' to inside 'ൠ'..
         * Expected result: 'ൠ'. should be skipped.
         */
        new buffer3[14]; 
        new length3 = formatex(buffer3, charsmax(buffer3), "%.11s", reference[19]);
        test(strcmp(buffer3, "㋡ crab?") == 0 && length3 == get_char_bytes("㋡") + strlen(" crab?")); 
    }
    
    server_print("Checking truncated character bytes - set_amxstring_utf8..."); 
    {
        /**
         * Splits string at '㋡'.
         * Buffer can hold only 16 characters.
         * Expected result: '㋡' should not be included and returned position should be after '㋡'.
         */
        new buffer1[16];
        new index1 = split_string(reference, "㋡", buffer1, charsmax(buffer1));
        test(strcmp(buffer1, "𤭢hi AMXX® H") == 0 && index1 == strlen("𤭢hi AMXX® Hello") + get_char_bytes("㋡")); 
        
        /**
         * Splits string at '𤭢'.
         * Expected result: Empty string and returned position should be after '𤭢'.
         */
        new buffer2[5];
        new index2 = split_string(reference, "𤭢", buffer2, charsmax(buffer2));
        test(buffer2[0] == EOS && index2 == get_char_bytes("𤭢"));
        
        /**
         * Splits string at '\ൠ'.
         * Expected result: Empty string and returned position should -1 (not found).
         */
        new buffer3[12];
        new index3 = split_string(reference, "\ൠ", buffer3, charsmax(buffer3));
        test(buffer3[0] == EOS && index3 == -1); 
        
        /**
         * Truncating '𤭢' at different index. '𤭢' = 4 bytes
         * A buffer of 4 = 3 bytes + EOS.
         * Expected result: empty buffer.
         */
        new buffer4[4]; 
        for(new i = charsmax(buffer4), length4; i >= 0; --i)
        {
            length4 = get_localinfo("reference", buffer4, i);
            test(buffer4[0] == EOS && length4 == 0); 
        }
    }

    server_print("Checking truncated character bytes - direct copy...");
    {
        /**
         * Replaces '®' by '𤭢'.
         * Expected result: '𤭢' should eat '® He" which counts 4 bytes.
         */
        new count1 = replace_string(reference, charsmax(reference), "®", "𤭢");
        test(strcmp(reference, "𤭢hi AMXX𤭢ello㋡ crab?ൠ") == 0 && count1 == 1);
        
        /**
         * Replaces '®' by '𤭢'.
         * Expected result: not found.
         */
        new count2 = replace_string(reference, charsmax(reference), "®", "𤭢");
        test(strcmp(reference, "𤭢hi AMXX𤭢ello㋡ crab?ൠ") == 0 && count2 == 0);
        
        /**
         * Replaces 'ൠ' by '𤭢'.
         * Expected result: 'ൠ' = 3 bytes, '𤭢' = 4 bytes. Not enough spaces to hold '𤭢', skipping it.
         */
        new count3 = replace_string(reference, charsmax(reference), "ൠ", "𤭢");
        test(strcmp(reference, "𤭢hi AMXX𤭢ello㋡ crab?") == 0 && count3 == 1);
        
        /**
         * Gets reference string with limited buffer.
         * Expected result: '㋡' should be ignored as no spaces.
         */
        new buffer[charsmax(reference) - 9];
        ArrayGetString(a, 0, buffer, charsmax(buffer));
        test(strcmp(buffer, "𤭢hi AMXX® Hello") == 0);
        
        /**
         * Gets reference string with limited buffer.
         * Expected result: '㋡' should be ignored as no spaces.
         */
        TrieGetString(t, "reference", buffer, charsmax(buffer));
        test(strcmp(buffer, "𤭢hi AMXX® Hello") == 0);
        
        /**
         * Gets reference string with limited buffer.
         * Expected result: '㋡' should be ignored as no room.
         */
        new length = ReadPackString(d, buffer, charsmax(buffer));
        test(strcmp(buffer, "𤭢hi AMXX® Hello") == 0 && length == strlen("𤭢hi AMXX® Hello"));
    }
    
    ArrayDestroy(a);
    TrieDestroy(t);
    DestroyDataPack(d);

    showResult();
}
