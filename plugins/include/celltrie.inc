#if defined _celltrie_included
#endinput
#endif
#define _celltrie_included

enum Trie
{
	Invalid_Trie = 0
};


native Trie:TrieCreate();
native TrieClear(Trie:handle);

native TrieSetCell(Trie:handle, const key[], any:value);
native TrieSetString(Trie:handle, const key[], const value[]);
native TrieSetArray(Trie:handle, const key[], const any:buffer[], size);

native bool:TrieGetCell(Trie:handle, const key[], &any:value);
native bool:TrieGetString(Trie:handle, const key[], output[], outputsize);
native bool:TrieGetArray(Trie:handle, const key[], any:output[], outputsize);

native bool:TrieDeleteKey(Trie:handle, const key[]);
native bool:TrieKeyExists(Trie:handle, const key[]);
native TrieDestroy(&Trie:handle);

