/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#ifndef _INCLUDE_CLANG_H
#define _INCLUDE_CLANG_H

#include "sh_tinyhash.h"

#define LANG_SERVER 0
#define LANG_PLAYER -1

#define ERR_BADKEY	1	// Lang key not found
#define ERR_BADLANG 2	// Invalid lang

struct md5Pair
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
		// Save to file
		bool Save(FILE *fp, int &defOffset, uint32_t &curOffset);
		bool SaveDefinitions(FILE *fp, uint32_t &curOffset);
		// Load
		bool Load(FILE *fp);
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

	// Merge definitions into a language
	void MergeDefinitions(const char *lang, CQueue <sKeyDef> &tmpVec);
	// strip lowercase; make lower if needed
	static size_t strip(char *str, char *newstr, bool makelower = false);

	typedef CVector<CLang*> LangVec;
	typedef CVector<CLang*>::iterator LangVecIter;
	
	LangVec m_Languages;

	CVector<md5Pair *> FileList;
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
	// Format a string
	const char *Format(const char *src, ...);
	// Format a string for an AMX plugin
	char *FormatAmxString(AMX *amx, cell *params, int parm, int &len);
	char *FormatString(const char *fmt, va_list &ap);
	// Save
	bool Save(const char *filename);
	// Load
	bool Load(const char *filename);
	// Cache
	bool LoadCache(const char *filename);
	bool SaveCache(const char *filename);
	void InvalidateCache();
	// Get index
	int GetKeyEntry(String &key);
	int GetKeyEntry(const char *key);
	int GetKeyIndex(const char *key);
	// Get key from index
	const char *GetKey(int key);
	// Add key
	int AddKeyEntry(String &key);

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
