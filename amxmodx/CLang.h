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

#define LANG_SERVER -1

class CLangMngr
{
	struct sKeyDef
	{
		const char *key;
		const char *def;
	};

	class CLang
	{
	public:
		CLang();
		CLang(const char *lang);
		~CLang();

		const char *GetDef(const char *key);
		void MergeDefinitions(CVector<sKeyDef> & vec);
		void Clear();

		friend bool operator == (const CLang &left, const char *right)
		{
			return strcmp(left.m_LanguageName, right)==0 ? true : false;
		}
		const char *GetName() { return m_LanguageName; }
		void Dump();
		bool Save(FILE *fp);
		bool Load(FILE *fp);
	private:

		static uint32_t MakeHash(const char *src, bool makeLower = false);

		class LangEntry
		{
			uint32_t m_DefHash;
			uint32_t m_KeyHash;
			char *m_pKey;
			char *m_pDef;
			void SetKey(const char *pKey);
			void SetDef(const char *pDef);
		public:
			uint32_t GetDefHash();
			uint32_t GetKeyHash();
			const char *GetKey();
			const char *GetDef();

			LangEntry();
			LangEntry(const char *pKey);
			LangEntry(const char *pKey, const char *pDef);
			LangEntry(const LangEntry &other);

			void operator= (const char *pNewDef);
			bool operator== (uint32_t hash);

			void Clear();
		};

		LangEntry & GetEntry(const char *key);
		typedef CVector<LangEntry>	LookUpVec;
		typedef LookUpVec::iterator	LookUpVecIter;

		char m_LanguageName[3];

		LookUpVec m_LookUpTable;
	};

	void MergeDefinitions(const char *lang, CVector<sKeyDef> &tmpVec);
	static size_t strip(char *str, char *newstr, bool makelower=false);

	typedef CVector<CLang> LangVec;
	typedef LangVec::iterator LangVecIter;
	
	LangVec m_Languages;

	CLang & GetLang(const char *name);

	int m_CurGlobId;
public:
	int MergeDefinitionFile(const char *file);
	void Dump();
	const char *GetDef(const char *langName, const char *key);
	const char *Format(const char *src, ...);
	char *FormatAmxString(AMX *amx, cell *params, int parm, int &len);
	bool Save(const char *filename);
	bool Load(const char *filename);

	int GetLangsNum();
	const char *GetLangName(int langId);
	bool LangExists(const char *langName);

	// When a language id in a format string in FormatAmxString is 0, the glob id decides which language to take.
	void SetDefLang(int id);
	void Clear();
};

#endif //_INCLUDE_CLANG_H
