// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <stdarg.h>
#include "amxmodx.h"
#include "CLang.h"
#include "format.h"
#include "ITextParsers.h"

#define LITIDX_NONE			0
#define LITIDX_BRACKET		1
#define LITIDX_DEFINITION	2

#define INSERT_NUMBER		1
#define INSERT_FLOAT		2
#define INSERT_STRING		3
#define INSERT_NEWLINE		4

template<>
int Compare<ke::AString>(const ke::AString &k1, const ke::AString &k2)
{
	return k1.compare(k2);
}

template<>
int CompareAlt<char const *, ke::AString>(char const * const &k1, ke::AString const &k2)
{
	return k2.compare(k1);
}
template<>
int CompareAlt<ke::AString, ke::AString>(ke::AString const &k1, ke::AString const &k2)
{
	return k1.compare(k2);
}

template<>
int HashFunction<ke::AString>(const ke::AString &k)
{
	unsigned long hash = 5381;
	register const char *str = k.chars();
	register char c;
	while ((c = *str++))
	{
		hash = ((hash << 5) + hash) + c; // hash*33 + c
	}
	return hash;
}

template<>
int HashAlt<const char *>(char const * const &k)
{
	unsigned long hash = 5381;
	register const char *str = k;
	register char c;
	while ((c = *str++))
	{
		hash = ((hash << 5) + hash) + c; // hash*33 + c
	}
	return hash;
}

template<>
int HashAlt<ke::AString>(ke::AString const &k)
{
	unsigned long hash = 5381;
	register const char *str = k.chars();
	register char c;
	while ((c = *str++))
	{
		hash = ((hash << 5) + hash) + c; // hash*33 + c
	}
	return hash;
}


template<>
int HashFunction<int>(const int &k)
{
	return k;
}

template<>
int Compare<int>(const int &k1, const int &k2)
{
	return (k1 - k2);
}

// strip the whitespaces at the beginning and the end of a string
// also convert to lowercase if needed
// return the number of written characters (including the terimating zero char)
size_t CLangMngr::strip(char *str, char *newstr, bool makelower)
{
	size_t i = 0;
	size_t pos = 0;
	int flag = 0;
	size_t strln = strlen(str);

	for (i = strln - 1; i < strln; i--)
	{
		if (str[i] == '\n' || str[i] == ' ' || str[i] == '\t')
		{
			str[i] = 0;
		} else {
			break;
		}
	}

	char *ptr = str;
	while (*ptr)
	{
		if (!flag)
		{
			if (*ptr != '\n' && *ptr != ' ' && *ptr != '\t')
			{
				flag = 1;
				newstr[pos++] = makelower ? tolower(*ptr) : *ptr;
			}
		} else {
			newstr[pos++] = makelower ? tolower(*ptr) : *ptr;
		}
		++ptr;
	}

	newstr[pos] = 0;
	
	return ptr - str + 1;
}


/******** CLangMngr::CLang *********/

CLangMngr::CLang::CLang()
{
	m_LookUpTable.clear();
	m_entries = 0;
}

CLangMngr::CLang::CLang(const char *lang)
{
	m_LookUpTable.clear();
	m_entries = 0;
	strncpy(m_LanguageName, lang, 2);
	m_LanguageName[2] = 0;
}

void CLangMngr::CLang::AddEntry(int key, const char *definition)
{
	defentry &d = m_LookUpTable[key];

	if (d.definition)
	{
		delete d.definition;
	} else {
		m_entries++;
	}

	d.definition = new ke::AString(definition);
}

CLangMngr::CLang::~CLang()
{
	Clear();
}

void CLangMngr::CLang::Clear()
{
	THash<int, defentry>::iterator iter;
	for (iter=m_LookUpTable.begin(); iter!=m_LookUpTable.end(); iter++)
	{
		if (iter->val.definition)
		{
			delete iter->val.definition;
			iter->val.definition = NULL;
		}
	}
	m_LookUpTable.clear();
	m_entries = 0;
}

void CLangMngr::CLang::MergeDefinitions(ke::Vector<sKeyDef> &vec)
{
	ke::AutoString *pDef;
	int key = -1;
	
	while (!vec.empty())
	{
		auto keydef = vec.popCopy();

		key = keydef.key;
		pDef = keydef.definition;

		AddEntry(key, pDef->ptr());

		delete pDef;
	}
}

const char * CLangMngr::CLang::GetDef(int key, int &status)
{
	defentry &def = m_LookUpTable[key];

	if (!def.definition)
	{
		status = ERR_BADKEY;
		return NULL;
	}

	status = 0;
	return def.definition->chars();
}

int CLangMngr::CLang::Entries()
{
	return m_entries;
}

/******** CLangMngr *********/

CLangMngr::CLangMngr()
{
	Clear();
}

const char * CLangMngr::GetKey(int key)
{
	if (key < 0 || key >= (int)KeyList.length())
		return NULL;

	return KeyList[key]->chars();
}

int CLangMngr::GetKeyEntry(const char *key)
{
	keytbl_val &val = KeyTable[ke::AString(key)];

	return val.index;
}

int CLangMngr::AddKeyEntry(const char *key)
{
	keytbl_val val;
	val.index = static_cast<int>(KeyList.length());

	KeyList.append(new ke::AString(key));

	KeyTable[ke::AString(key)] = val;

	return val.index;
}

int CLangMngr::AddKeyEntry(ke::AString &key)
{
	return AddKeyEntry(key.chars());
}

int CLangMngr::GetKeyEntry(ke::AString &key)
{
	keytbl_val &val = KeyTable[key];

	return val.index;
}

char * CLangMngr::FormatAmxString(AMX *amx, cell *params, int parm, int &len)
{
	//do an initial run through all this 
	static char outbuf[4096];
	cell *addr = get_amxaddr(amx, params[parm++]);

	len = atcprintf(outbuf, sizeof(outbuf)-1, addr, amx, params, &parm);

	return outbuf;
}

void CLangMngr::MergeDefinitions(const char *lang, ke::Vector<sKeyDef> &tmpVec)
{
	CLang * language = GetLang(lang);
	if (language)
		language->MergeDefinitions(tmpVec);
}

void reparse_newlines_and_color(char* def)
{
	size_t len = strlen(def);
	int offs = 0;
	int c;

	if (!len)
		return;

	for (size_t i = 0; i < len; i++)
	{
		c = def[i]; 

		if (c == '^' && (i != len - 1))
		{
			c = def[++i];

			if (c == 'n' || c == 't' || (c >= '1' && c <= '4'))
			{
				switch (c)
				{
					case '1': c = '\x01'; break;
					case '2': c = '\x02'; break;
					case '3': c = '\x03'; break;
					case '4': c = '\x04'; break;
					case 'n': c = '\n'; break;
					case 't': c = '\t'; break;
				}

				if (!g_bmod_cstrike && (c >= '1' && c <= '4')) // remove completely these two characters if not under CS
				{
					offs += 2;
					continue;
				}

				offs++;
			}
		}

		def[i - offs] = c;
	}

	def[len-offs] = '\0';
}

struct LangFileData
{
	void reset()
	{
		multiLine = false;

		*language    = '\0';
		*valueBuffer = '\0';

		clearEntry();
	}

	void clearEntry()
	{
		entry.key = -1;
		entry.definition = nullptr;
	}

	bool                multiLine;
	char                language[3];
	char                valueBuffer[512];
	ke::AString         currentFile;
	ke::AString         lastKey;
	ke::Vector<sKeyDef> defsQueue;
	sKeyDef             entry;

} Data;

void CLangMngr::ReadINI_ParseStart()
{
	Data.reset();
}

bool CLangMngr::ReadINI_NewSection(const char *section, bool invalid_tokens, bool close_bracket, bool extra_tokens, unsigned int *curtok)
{
	if (Data.multiLine)
	{
		AMXXLOG_Log("New section, unterminated block (file \"%s\" key \"%s\" lang \"%s\")", Data.currentFile.chars(), Data.lastKey.chars(), Data.language);

		Data.clearEntry();
	}

	if (!Data.defsQueue.empty())
	{
		MergeDefinitions(Data.language, Data.defsQueue);
	}

	Data.reset();

	Data.language[0] = section[0];
	Data.language[1] = section[1];
	Data.language[2] = '\0';

	return true;
}

bool CLangMngr::ReadINI_KeyValue(const char *key, const char *value, bool invalid_tokens, bool equal_token, bool quotes, unsigned int *curtok)
{
	bool colons_token = (key[strlen(key) - 1] == ':');

	if (!Data.multiLine)
	{
		Data.lastKey = key;

		if (colons_token || (equal_token && value))
		{
			int iKey = GetKeyEntry(key);

			if (iKey == -1)
			{
				iKey = AddKeyEntry(key);
			}

			if (equal_token)
			{
				strncopy(Data.valueBuffer, value, sizeof(Data.valueBuffer));

				reparse_newlines_and_color(Data.valueBuffer);

				Data.entry.key = iKey;
				Data.entry.definition = new ke::AutoString;
				*Data.entry.definition = Data.valueBuffer;

				Data.defsQueue.append(Data.entry);
				Data.clearEntry();
			}
			else if (!value && colons_token)
			{
				Data.entry.key = iKey;
				Data.entry.definition = new ke::AutoString;

				Data.multiLine = true;
			}
		}
		else
		{
			AMXXLOG_Log("Invalid multi-lingual line (file \"%s\" key \"%s\" lang \"%s\")", Data.currentFile.chars(), Data.lastKey.chars(), Data.language);
		}
	}
	else
	{
		if (!value && colons_token)
		{
			strncopy(Data.valueBuffer, Data.entry.definition->ptr(), sizeof(Data.valueBuffer));
			reparse_newlines_and_color(Data.valueBuffer);

			*Data.entry.definition = Data.valueBuffer;

			Data.defsQueue.append(Data.entry);
			Data.clearEntry();

			Data.multiLine = false;
		}
		else
		{
			if (!Data.entry.definition)
			{
				Data.entry.definition = new ke::AutoString();
			}

			*Data.entry.definition = *Data.entry.definition + key;
		}
	}

	return true;
}

void CLangMngr::ReadINI_ParseEnd(bool halted)
{
	if (!Data.defsQueue.empty())
	{
		MergeDefinitions(Data.language, Data.defsQueue);
	}
}

//this is the file parser for dictionary text files
// -- BAILOPAN
int CLangMngr::MergeDefinitionFile(const char *file)
{
	/** Tries to open the file. */
	struct stat fileStat;
	if (stat(file, &fileStat))
	{
		FileList.remove(file);
		AMXXLOG_Log("[AMXX] Failed to open dictionary file: %s", file);
		return 0;
	}

	/** Checks if there is an existing entry with same time stamp. */
	time_t timeStamp;
	if (FileList.retrieve(file, &timeStamp) && fileStat.st_mtime == timeStamp)
	{
		return -1;
	}

	/** If yes, it either means that the entry doesn't exist or the existing entry needs to be updated. */
	FileList.replace(file, fileStat.st_mtime);

	Data.currentFile = file;

	unsigned int line, col;
	bool result = textparsers->ParseFile_INI(file, static_cast<ITextListener_INI*>(this), &line, &col);

	if (!result)
	{
		AMXXLOG_Log("[AMXX] Failed to re-open dictionary file: %s", file);
		return 0;
	}

	return 1;
}

// Find a CLang by name, if not found, add it
CLangMngr::CLang * CLangMngr::GetLang(const char *name)
{
	for (size_t iter = 0; iter < m_Languages.length(); ++iter)
	{
		if (strcmp(m_Languages[iter]->GetName(), name) == 0)
			return m_Languages[iter];
	}

	CLang *p = new CLang(name);
	p->SetMngr(this);

	m_Languages.append(p);
	return p;
}

// Find a CLang by name, if not found, return NULL
CLangMngr::CLang * CLangMngr::GetLangR(const char *name)
{
	for (size_t iter = 0; iter < m_Languages.length(); ++iter)
	{
		if (strcmp(m_Languages[iter]->GetName(), name) == 0)
			return m_Languages[iter];
	}

	return nullptr;
}

const char *CLangMngr::GetDef(const char *langName, const char *key, int &status)
{
	CLang *lang = GetLangR(langName);

	keytbl_val &val = KeyTable.AltFindOrInsert(ke::AString(key)); //KeyTable[make_string(key)];
	if (lang == NULL)
	{
		status = ERR_BADLANG;
		return NULL;
	} else if (val.index == -1) {
		status = ERR_BADKEY;
		return NULL;
	} else {
		status = 0;
		return lang->GetDef(val.index, status);
	}
}

void CLangMngr::InvalidateCache()
{
	FileList.clear();
}

CLangMngr::~CLangMngr()
{
	Clear();
}

void CLangMngr::Clear()
{
	unsigned int i = 0;

	KeyTable.clear();
	
	for (i = 0; i < m_Languages.length(); i++)
	{
		if (m_Languages[i])
			delete m_Languages[i];
	}

	for (i = 0; i < KeyList.length(); i++)
	{
		if (KeyList[i])
			delete KeyList[i];
	}

	m_Languages.clear();
	KeyList.clear();
	FileList.clear();
}

int CLangMngr::GetLangsNum()
{
	return m_Languages.length();
}

const char *CLangMngr::GetLangName(int langId)
{
	for (size_t iter = 0; iter < m_Languages.length(); ++iter)
	{
		if (iter == langId)
		{
			return m_Languages[iter]->GetName();
		}
	}

	return "";
}

bool CLangMngr::LangExists(const char *langName)
{
	char buf[3] = {0};
	int i = 0;
	
	while ((buf[i] = tolower(*langName++)))
	{	
		if (++i == 2)
			break;
	}
	
	for (size_t iter = 0; iter < m_Languages.length(); ++iter)
	{
		if (strcmp(m_Languages[iter]->GetName(), buf) == 0)
		{
			return true;
		}
	}
	return false;
}

void CLangMngr::SetDefLang(int id)
{
	m_CurGlobId = id;
}
