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

#define LANG_SERVER 0
#define LANG_PLAYER -1

struct md5Pair
{
	String file;
	String val;
};

struct keyEntry
{
	String key;
	uint32_t hash;
};

struct sKeyDef
{
	sKeyDef() { key = -1; def = 0; }
	~sKeyDef() { if (def) delete def; }
	int key;
	String *def;
};

class CLangMngr
{
	class CLang
	{
	public:
		CLang();
		CLang(const char *lang);
		~CLang();

		const char *GetDef(const char *key);
		void MergeDefinitions(CQueue <sKeyDef*> & vec);
		void Clear();

		friend bool operator == (const CLang &left, const char *right)
		{
			return strcmp(left.m_LanguageName, right)==0 ? true : false;
		}
		const char *GetName() { return m_LanguageName; }
		bool Save(FILE *fp, int &defOffset, uint32_t &curOffset);
		bool SaveDefinitions(FILE *fp, uint32_t &curOffset);
		bool Load(FILE *fp);
		void SetMngr(CLangMngr *l) { lman = l; }
		int Entries() { return m_LookUpTable.size(); }
	private:

		static uint32_t MakeHash(const char *src, bool makeLower = false);

		class LangEntry
		{
			uint32_t m_DefHash;
			int key;
			String m_pDef;
		public:
			void SetKey(int key);
			void SetDef(const char *pDef);
			uint32_t GetDefHash();

			int GetKey();
			const char *GetDef();
			int GetDefLength();

			LangEntry();
			LangEntry(int key);
			LangEntry(int key, const char *pDef);
			LangEntry(const LangEntry &other);
			LangEntry(int pKey, uint32_t defHash, const char *pDef);

			void Clear();
		};

		LangEntry * GetEntry(int key);
		typedef CVector<LangEntry*>	LookUpVec;
		typedef LookUpVec::iterator	LookUpVecIter;

		char m_LanguageName[3];

		LookUpVec m_LookUpTable;
		CLangMngr *lman;
	public:
				LangEntry *AddEntry(int pKey, uint32_t defHash, const char *def);
	};

	void MergeDefinitions(const char *lang, CQueue <sKeyDef*> &tmpVec);
	static size_t strip(char *str, char *newstr, bool makelower=false);

	typedef CVector<CLang*> LangVec;
	typedef CVector<CLang*>::iterator LangVecIter;
	
	LangVec m_Languages;
	CVector<md5Pair *> FileList;
	CVector<keyEntry*> KeyList;

	CLang * GetLang(const char *name);

	int m_CurGlobId;
public:
	int MergeDefinitionFile(const char *file);
	const char *GetDef(const char *langName, const char *key);
	const char *Format(const char *src, ...);
	char *FormatAmxString(AMX *amx, cell *params, int parm, int &len);
	bool Save(const char *filename);
	bool Load(const char *filename);
	bool LoadCache(const char *filename);
	bool SaveCache(const char *filename);
	int GetKeyEntry(String &key);
	int GetKeyEntry(const char *key);
	int GetKeyHash(int key);
	const char *GetKey(int key);
	int AddKeyEntry(String &key);
	uint32_t MakeHash(const char *src, bool makeLower);

	int GetLangsNum();
	const char *GetLangName(int langId);
	bool LangExists(const char *langName);

	// When a language id in a format string in FormatAmxString is 0, the glob id decides which language to take.
	void SetDefLang(int id);
	void Clear();

	CLangMngr();
	~CLangMngr();
};

#endif //_INCLUDE_CLANG_H
