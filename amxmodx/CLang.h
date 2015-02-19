// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_CLANG_H
#define _INCLUDE_CLANG_H

#include "sh_tinyhash.h"
#include "sm_stringhashmap.h"

#define LANG_SERVER 0
#define LANG_PLAYER -1

#define ERR_BADKEY	1	// Lang key not found
#define ERR_BADLANG 2	// Invalid lang

struct CRC32Pair
{
	String file;
	String val;
};

struct sKeyDef
{
	String *definition;
	int key;
};

struct lang_err
{
	lang_err() : last(0.0f)
	{
	};
	float last;
};

class defentry
{
public:
	defentry() : definition(NULL)
	{
	};
	defentry(const defentry &src)
	{
		definition = src.definition;
	}
	~defentry()
	{
	}
	String *definition;
};

struct keytbl_val
{
	keytbl_val() : index(-1)
	{
	};
	int index;
};

class CLangMngr
{
	class CLang
	{
	public:
		// Construct an empty CLang object
		CLang();
		// Construct a CLang object initialized with a language name
		CLang(const char *lang);
		// Destructor
		~CLang();

		// Get the definition
		const char *GetDef(int key, int &status);
		// Add definitions to this language
		void MergeDefinitions(CQueue <sKeyDef> & vec);
		// Reset this language
		void Clear();

		// compare this language to a language name
		friend bool operator == (const CLang &left, const char *right)
		{
			return strcmp(left.m_LanguageName, right) == 0 ? true : false;
		}
		
		// Get language name
		const char *GetName() { return m_LanguageName; }
		void SetMngr(CLangMngr *l) { m_LMan = l; }
		// Get number of entries
		int Entries();
	protected:
		typedef THash<int, defentry> LookUpVec;
		typedef LookUpVec::iterator	 LookUpVecIter;

		char m_LanguageName[3];

		// our lookup table
		LookUpVec m_LookUpTable;
		int m_entries;
		CLangMngr *m_LMan;
	public:
		void AddEntry(int key, const char *definition);
	};
public:
	// Merge definitions into a language
	void MergeDefinitions(const char *lang, CQueue <sKeyDef> &tmpVec);

private:
	// strip lowercase; make lower if needed
	static size_t strip(char *str, char *newstr, bool makelower = false);

	typedef CVector<CLang*> LangVec;
	typedef CVector<CLang*>::iterator LangVecIter;
	
	LangVec m_Languages;

	StringHashMap<time_t> FileList;
	CVector<String *> KeyList;
	THash<String, keytbl_val> KeyTable;

	// Get a lang object (construct if needed)
	CLang * GetLang(const char *name);

	CLang * GetLangR(const char *name);

	// Current global client-id for functions like client_print with first parameter 0
	int m_CurGlobId;
public:
	// Merge a definitions file
	int MergeDefinitionFile(const char *file);
	// Get a definition from a lang name and a key
	const char *GetDef(const char *langName, const char *key, int &status);
	// Format a string for an AMX plugin
	char *FormatAmxString(AMX *amx, cell *params, int parm, int &len);
	void InvalidateCache();
	// Get index
	int GetKeyEntry(String &key);
	int GetKeyEntry(const char *key);
	// Get key from index
	const char *GetKey(int key);
	// Add key
	int AddKeyEntry(String &key);
	int AddKeyEntry(const char *key);

	// Get the number of languages
	int GetLangsNum();
	// Get the name of a language
	const char *GetLangName(int langId);
	// Check if a language exists
	bool LangExists(const char *langName);

	// When a language id in a format string in FormatAmxString is LANG_PLAYER, the glob id decides which language to take.
	void SetDefLang(int id);

	inline int GetDefLang() const { return m_CurGlobId; }

	// Reset
	void Clear();

	CLangMngr();
	~CLangMngr();
};

#endif //_INCLUDE_CLANG_H
