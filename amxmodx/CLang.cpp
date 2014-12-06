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

#define LITIDX_NONE			0
#define LITIDX_BRACKET		1
#define LITIDX_DEFINITION	2

#define INSERT_NUMBER		1
#define INSERT_FLOAT		2
#define INSERT_STRING		3
#define INSERT_NEWLINE		4

template<>
int Compare<String>(const String &k1, const String &k2)
{
	return k1.compare(k2.c_str());
}

template<>
int CompareAlt<char const *, String>(char const * const &k1,  String const &k2)
{
	return strcmp(k1, k2.c_str());
}

template<>
int HashFunction<String>(const String &k)
{
	unsigned long hash = 5381;
	const char *str = k.c_str();
	char c;
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
	const char *str = k;
	char c;
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
	return (k1-k2);
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

	d.definition = new String(definition);
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

void CLangMngr::CLang::MergeDefinitions(CQueue<sKeyDef> &vec)
{
	String *pDef;
	int key = -1;
	
	while (!vec.empty())
	{
		key = vec.front().key;
		pDef = vec.front().definition;

		AddEntry(key, pDef->c_str());

		delete vec.front().definition;
		
		vec.pop();
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
	return def.definition->c_str();
}

int CLangMngr::CLang::Entries()
{
	return m_entries;
}

/******** CLangMngr *********/

inline String &make_string(const char *str)
{
	static String g_temp;

	g_temp.assign(str);

	return g_temp;
}

CLangMngr::CLangMngr()
{
	Clear();
}

const char * CLangMngr::GetKey(int key)
{
	if (key < 0 || key >= (int)KeyList.size())
		return NULL;

	return KeyList[key]->c_str();
}

int CLangMngr::GetKeyEntry(const char *key)
{
	keytbl_val &val = KeyTable[key];

	return val.index;
}

int CLangMngr::AddKeyEntry(const char *key)
{
	keytbl_val val;
	val.index = static_cast<int>(KeyList.size());

	String *pString = new String(key);
	KeyList.push_back(pString);

	KeyTable[key] = val;

	return val.index;
}

int CLangMngr::AddKeyEntry(String &key)
{
    return AddKeyEntry(key.c_str());
}

int CLangMngr::GetKeyEntry(String &key)
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

void CLangMngr::MergeDefinitions(const char *lang, CQueue<sKeyDef> &tmpVec)
{
	CLang * language = GetLang(lang);
	if (language)
		language->MergeDefinitions(tmpVec);
}

void reparse_color(String* def)
{
	size_t len = def->size();
	int offs = 0;
	int c;

	if (!len)
		return;

	for (size_t i = 0; i < len; i++)
	{
		c = def->at(i); 
		if (c == '^' && (i != len-1))
		{
			c = def->at(++i);
			if (c >= '1' && c <= '4')
			{
				switch(c)
				{
					case '1' : c = '\x01'; break;
					case '2' : c = '\x02'; break;
					case '3' : c = '\x03'; break;
					case '4' : c = '\x04'; break;
				}

				if (!g_bmod_cstrike) // remove completely these two characters if not under CS
				{
					offs += 2;
					continue;
				}

				offs++;
			}
		}
		def->at(i-offs, c);
	}

	def->at(len-offs, '\0');
}

//this is the file parser for dictionary text files
// -- BAILOPAN
int CLangMngr::MergeDefinitionFile(const char *file)
{
	FILE *fp = fopen(file, "rt");
	if (!fp)
	{
		CVector<md5Pair *>::iterator iter;
		for (iter = FileList.begin(); iter != FileList.end(); ++iter)
		{
			if ((*iter)->file.compare(file) == 0)
			{
				char buf[33] = {0};
				(*iter)->val.assign(buf);
				break;
			}	
		}
		AMXXLOG_Log("[AMXX] Failed to open dictionary file: %s", file);
		return 0;
	}
	
	MD5 md5;
	md5.update(fp);			// closes for us
	md5.finalize();
	char md5buffer[33];
	md5.hex_digest(md5buffer);
	bool foundFlag = false;
	
	CVector<md5Pair *>::iterator iter;
	for (iter = FileList.begin(); iter != FileList.end(); ++iter)
	{
		if ((*iter)->file.compare(file) == 0)
		{
			if ((*iter)->val.compare(md5buffer) == 0)
			{
				return -1;
			} else {
				(*iter)->val.assign(md5buffer);
				break;
			}
			foundFlag = true;
		}
	}

	if (!foundFlag)
	{
		md5Pair *p = new md5Pair;
		p->file.assign(file);
		p->val.assign(md5buffer);
		FileList.push_back(p);
	}

	fp = fopen(file, "rt");
	if (!fp)
	{
		AMXXLOG_Log("[AMXX] Failed to re-open dictionary file: %s", file);
		return 0;
	}

	// Allocate enough memory to store everything
	bool multiline = 0;
	int pos = 0, line = 0;
	CQueue<sKeyDef> Defq;
	String buf;
	char language[3];
	sKeyDef tmpEntry = {NULL, 0};

	while (!feof(fp))
	{
		line++;
		buf._fread(fp);
		buf.trim();
		if (buf[0] == 0)
			continue;
		if ((buf[0] == ';') || (buf[0] == '/' && buf[1] == '/'))
			continue;

		/* Check for BOM markings, which is only relevant on the first line.
		* Not worth it, but it could be moved out of the loop.
		*/
		if (line == 1 && (buf[0] == (char)0xEF && buf[1] == (char)0xBB && buf[2] == (char)0xBF))
		{
			buf.erase(0, 3);
		}

		if (buf[0] == '[' && buf.size() >= 3)
		{
			if (multiline)
			{
				AMXXLOG_Log("New section, multiline unterminated (file \"%s\" line %d)", file, line);
				tmpEntry.key = -1;
				tmpEntry.definition = NULL;
			}
			
			if (!Defq.empty())
			{
				MergeDefinitions(language, Defq);
			}
			
			language[0] = buf[1];
			language[1] = buf[2];
			language[2] = 0;
		} else {
			if (!multiline)
			{
				pos = buf.find('=');
				
				if (pos > String::npos)
				{
					String key;
					key.assign(buf.substr(0, pos).c_str());
					String def;
					def.assign(buf.substr(pos + 1).c_str());
					key.trim();
					key.toLower();
					int iKey = GetKeyEntry(key);
					if (iKey == -1)
						iKey = AddKeyEntry(key);
					tmpEntry.key = iKey;
					tmpEntry.definition = new String;
					tmpEntry.definition->assign(def.c_str());
					tmpEntry.definition->trim();
					reparse_color(tmpEntry.definition);
					tmpEntry.definition->reparse_newlines();
					Defq.push(tmpEntry);
					tmpEntry.key = -1;
					tmpEntry.definition = NULL;
				} else {
					pos = buf.find(':');
					
					if (pos > String::npos)
					{
						String key;
						key.assign(buf.substr(0, pos).c_str());;
						key.trim();
						key.toLower();
						int iKey = GetKeyEntry(key);
						if (iKey == -1)
							iKey = AddKeyEntry(key);
						tmpEntry.key = iKey;
						tmpEntry.definition = new String;
						multiline = true;
					} else {
						//user typed a line with no directives
						AMXXLOG_Log("Invalid multi-lingual line (file \"%s\" line %d)", file, line);
					}
				}
			} else {
				if (buf[0] == ':')
				{
					reparse_color(tmpEntry.definition);
					tmpEntry.definition->reparse_newlines();
					Defq.push(tmpEntry);
					tmpEntry.key = -1;
					tmpEntry.definition = NULL;
					multiline = false;
				} else {
					if (!tmpEntry.definition)
						tmpEntry.definition = new String();
					tmpEntry.definition->append(buf);
				}
			} // if !multiline
		} //if - main
	}
	
	// merge last section
	if (!Defq.empty())
	{
		MergeDefinitions(language, Defq);
	}
	fclose(fp);
	return 1;
}

// Find a CLang by name, if not found, add it
CLangMngr::CLang * CLangMngr::GetLang(const char *name)
{
	LangVecIter iter;
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		if (strcmp((*iter)->GetName(), name) == 0)
			return (*iter);
	}

	CLang *p = new CLang(name);
	p->SetMngr(this);

	m_Languages.push_back(p);
	return p;
}

// Find a CLang by name, if not found, return NULL
CLangMngr::CLang * CLangMngr::GetLangR(const char *name)
{
	LangVecIter iter;
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		if (strcmp((*iter)->GetName(), name) == 0)
			return (*iter);
	}

	return NULL;
}

const char *CLangMngr::GetDef(const char *langName, const char *key, int &status)
{
	CLang *lang = GetLangR(langName);
	keytbl_val &val = KeyTable.AltFindOrInsert(key); //KeyTable[make_string(key)];
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
	for (size_t i = 0; i < FileList.size(); i++)
	{
		if (FileList[i])
			delete FileList[i];
	}

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
	
	for (i = 0; i < m_Languages.size(); i++)
	{
		if (m_Languages[i])
			delete m_Languages[i];
	}

	for (i = 0; i < FileList.size(); i++)
	{
		if (FileList[i])
			delete FileList[i];
	}

	for (i = 0; i < KeyList.size(); i++)
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
	return m_Languages.size();
}

const char *CLangMngr::GetLangName(int langId)
{
	int i = 0;
	LangVecIter iter;
	
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		if (i == langId)
		{
			return (*iter)->GetName();
		}
		i++;
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
	
	LangVecIter iter;
	
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		if (strcmp((*iter)->GetName(), buf) == 0)
			return true;
	}
	
	return false;
}

void CLangMngr::SetDefLang(int id)
{
	m_CurGlobId = id;
}
