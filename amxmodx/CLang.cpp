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

#include <stdarg.h>
#include "amxmodx.h"
#include "CLang.h"
#include "format.h"

#ifdef __linux__
#define _snprintf snprintf
#endif

#define LITIDX_NONE			0
#define LITIDX_BRACKET		1
#define LITIDX_DEFINITION	2

#define INSERT_NUMBER		1
#define INSERT_FLOAT		2
#define INSERT_STRING		3
#define INSERT_NEWLINE		4

// dictionary format is Fast-Format-Hash-Lookup, v6
#define MAGIC_HDR			0x4646484C
#define FFHL_VERSION		6
#define FFHL_MIN_VERSION	6

/*version history:
	* 1 (BAILOPAN) - Simplest form possible, no reverse
	* 2 (BAILOPAN) - One language per file with full reverse
	* 3 (PM OnoTo) - 2^32 languages per file with full reverse
	* 4 (BAILOPAN) - Optimized by separating and relocating tables (normalization)
	* 5 (BAILOPAN) - Removed hash storage
	* 6 (BAILOPAN) - Arbitrary bump to force reparse.
FORMAT:
Magic					4bytes
Version					1byte
Number of Keys			4bytes
Number of Languages		4bytes
LANG INFO TABLE[]		
	Language Name		2bytes
	Offset				4bytes
KEY TABLE[]
	Key Lookup Offset	4bytes
LANGUAGES TABLE[]
	Language[]
		Definitions		4bytes
		Key	#			4bytes	(0-index into key table)
		Def Offset		4bytes
KEY LOOKUP TABLE[]
	Key length			1byte
	Key string			variable
DEF LOOKUP TABLE[]
	Def length			2bytes
	Def string			variable
	*/

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
	register const char *str = k.c_str();
	register char c;
	while (c = *str++) hash = ((hash << 5) + hash) + c; // hash*33 + c
	return hash;
}

template<>
int HashAlt<const char *>(char const * const &k)
{
	unsigned long hash = 5381;
	register const char *str = k;
	register char c;
	while (c = *str++) hash = ((hash << 5) + hash) + c; // hash*33 + c
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

	for (i = strln - 1; i >= 0; i--)
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

	return def.definition->c_str();
}

// Assumes fp is set to the right position
bool CLangMngr::CLang::SaveDefinitions(FILE *fp, uint32_t &curOffset)
{
	unsigned short defLen = 0;
	String *pdef;
	
	THash<int, defentry>::iterator iter;
	for (iter=m_LookUpTable.begin(); iter!=m_LookUpTable.end(); iter++)
	{
		pdef = iter->val.definition;
		if (!pdef)
			continue;
		defLen = pdef->size();
		fwrite((void *)&defLen, sizeof(unsigned short), 1, fp);
		curOffset += sizeof(unsigned short);
		fwrite(pdef->c_str(), sizeof(char), defLen, fp);
		curOffset += defLen;
	}

	return true;
}

int CLangMngr::CLang::Entries()
{
	return m_entries;
}

// Assumes fp is set to the right position
bool CLangMngr::CLang::Save(FILE *fp, int &defOffset, uint32_t &curOffset)
{
	uint32_t keynum = 0;
	uint32_t size = 0;
	String *pdef;

	//:TODO: remove this loop and assertion, use m_entries for size
	THash<int, defentry>::iterator iter;
	for (iter=m_LookUpTable.begin(); iter!=m_LookUpTable.end(); iter++)
	{
		if (iter->val.definition)
			size++;
	}

	assert(size == m_entries);

	fwrite((void*)&size, sizeof(uint32_t), 1, fp);
	curOffset += sizeof(uint32_t);

	for (iter=m_LookUpTable.begin(); iter!=m_LookUpTable.end(); iter++)
	{
		keynum = iter->key;
		pdef = iter->val.definition;
		if (!pdef)
			continue;
		fwrite((void *)&keynum, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		fwrite((void *)&defOffset, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		defOffset += sizeof(short);
		defOffset += pdef->size();
	}

	return true;
}

// assumes fp is set to the right position
bool CLangMngr::CLang::Load(FILE *fp)
{
	return true;
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

int CLangMngr::AddKeyEntry(String &key)
{
    keytbl_val val;
	val.index = static_cast<int>(KeyList.size());

	String *pString = new String(key);
	KeyList.push_back(pString);

	KeyTable[key] = val;

	return val.index;
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

const char *CLangMngr::Format(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	const char *retVal = FormatString(fmt, ap);
	va_end(ap);
	
	return retVal;
}

#undef CHECK_PTR
#undef CHECK_OUTPR
#undef ZEROTERM
#undef NEXT_PARAM

#define CHECK_PTR(ptr, start, bufsize) if ((ptr) - (start) >= (bufsize)) { \
	AMXXLOG_Log("[AMXX] Buffer overflow in formatting"); \
	outbuf[0] = 0; \
	return outbuf; }
#define CHECK_OUTPTR(offset) CHECK_PTR(outptr+offset, outbuf, sizeof(outbuf))
#define ZEROTERM(buf) buf[(sizeof(buf)/sizeof(buf[0]))-1]=0;
#define NEXT_PARAM()

//this is not implemented....
char *CLangMngr::FormatString(const char *fmt, va_list &ap)
{
	return "";
}

void CLangMngr::MergeDefinitions(const char *lang, CQueue<sKeyDef> &tmpVec)
{
	CLang * language = GetLang(lang);
	if (language)
		language->MergeDefinitions(tmpVec);
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
	sKeyDef tmpEntry;

	while (!feof(fp))
	{
		line++;
		buf._fread(fp);
		buf.trim();
		if (buf[0] == 0)
			continue;
		if ((buf[0] == ';') || (buf[0] == '/' && buf[1] == '/'))
			continue;

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

bool CLangMngr::Save(const char *filename)
{
	FILE *fp = fopen(filename, "wb");

	if (!fp)
		return false;

	uint32_t magic = MAGIC_HDR;
	unsigned char version = FFHL_VERSION;
	uint32_t langNum = m_Languages.size();
	const char *langName = 0;
	uint32_t curOffset = 0;
	uint32_t keyNum = KeyList.size();
	uint32_t ktbSize = KeyList.size() * sizeof(uint32_t);
	uint32_t ltbSize = m_Languages.size() * ((sizeof(char)*2) + sizeof(uint32_t));

	fwrite((void *)&magic, sizeof(uint32_t), 1, fp);
	fwrite((void *)&version, sizeof(unsigned char), 1, fp);
	fwrite((void *)&keyNum, sizeof(uint32_t), 1, fp);
	fwrite((void *)&langNum, sizeof(uint32_t), 1, fp);

	curOffset += sizeof(uint32_t);
	curOffset += sizeof(unsigned char);
	curOffset += sizeof(uint32_t);
	curOffset += sizeof(uint32_t);

	uint32_t langOffset = curOffset + ktbSize + ltbSize;
	for (unsigned int i = 0; i < m_Languages.size(); i++)
	{
		langName = m_Languages[i]->GetName();
		fwrite(langName, sizeof(char), 2, fp);
		curOffset += sizeof(char) * 2;
		fwrite((void *)&langOffset, sizeof(uint32_t), 1, fp);
		langOffset += sizeof(uint32_t) + (m_Languages[i]->Entries() * (sizeof(uint32_t) * 2));
		curOffset += sizeof(uint32_t);
	}
	
	//Note - langOffset now points to the start of key lookup table
	uint32_t keyHash = 0;
	uint32_t keyOffset = langOffset;
	for (unsigned int i = 0; i < KeyList.size(); i++)
	{
		fwrite((void*)&keyOffset, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		keyOffset += sizeof(char);
		keyOffset += KeyList[i]->size();
	}

	//Note - now keyOffset points toward the start of the def table
	int defOffset = keyOffset;
	for (unsigned int i = 0; i < m_Languages.size(); i++)
	{
		m_Languages[i]->Save(fp, defOffset, curOffset);
	}

	//Now, defOffset points toward the END of the file
	//curoffset should point toward the key table, so...
	unsigned char keyLen = 0;
	for (unsigned int i = 0; i < KeyList.size(); i++)
	{
		keyLen = KeyList[i]->size();
		fwrite((void*)&keyLen, sizeof(unsigned char), 1, fp);
		curOffset += sizeof(unsigned char);
		fwrite(KeyList[i]->c_str(), sizeof(char), keyLen, fp);
		curOffset += sizeof(char) * keyLen;
	}

	//Finally, write the def table
	// It's assumed no orders changed...
	for (unsigned int i = 0; i < m_Languages.size(); i++)
	{
		m_Languages[i]->SaveDefinitions(fp, curOffset);
	}

	fclose(fp);

	//done!
	return true;
}

bool CLangMngr::SaveCache(const char *filename)
{
	FILE *fp = fopen(filename, "wb");
	if (!fp)
	{
		return false;
	}

	CVector<md5Pair *>::iterator i;
	short dictCount = FileList.size();
	char len = 0;
	
	fwrite((void *)&dictCount, sizeof(short), 1, fp);

	for (i = FileList.begin(); i != FileList.end(); i++)
	{
		len = (*i)->file.size();
		fwrite((void *)&len, sizeof(char), 1, fp);
		fwrite((*i)->file.c_str(), sizeof(char), len, fp);
		fwrite((*i)->val.c_str(), sizeof(char), 32, fp);
	}

	fclose(fp);

	return true;
}

#define CACHEREAD(expr, type) \
	if (! (expr==1) ) { \
		FileList.clear(); \
		fclose(fp); \
		return false; \
	}
#define CACHEREAD_S(expr, size) \
	if (! (expr==size) ) { \
		FileList.clear(); \
		fclose(fp); \
		return false; \
	}

bool CLangMngr::LoadCache(const char *filename)
{
	FILE *fp = fopen(filename, "rb");
	if (!fp)
	{
		return false;
	}

	SetUnhandledExceptionFilter(NULL);

	short dictCount = 0;
	char len = 0;
	char buf[255];
	char md5[34];
	CACHEREAD(fread((void*)&dictCount, sizeof(short), 1, fp), short);
	md5Pair *p = 0;

	for (int i = 1; i <= dictCount; i++)
	{
		CACHEREAD(fread((void*)&len, sizeof(char), 1, fp), char);
		CACHEREAD_S(fread(buf, sizeof(char), len, fp), len);
		buf[len] = 0;
		CACHEREAD_S(fread(md5, sizeof(char), 32, fp), 32);
		md5[32] = 0;
		p = new md5Pair;
		p->file.assign(buf);
		p->val.assign(md5);
		FileList.push_back(p);
		p = 0;
	}

	fclose(fp);

	return true;
}

#define DATREAD(expr, type) \
	if (! (expr==1) ) { \
		Clear(); \
		fclose(fp); \
		return false; \
	}
#define DATREAD_S(expr, size) \
	if (! (expr==size) ) { \
		Clear(); \
		fclose(fp); \
		return false; \
	}

bool CLangMngr::Load(const char *filename)
{
	Clear();

	FILE *fp = fopen(filename, "rb");
	
	if (!fp)
		return false;

	uint32_t magic = 0;
	uint32_t langCount = 0;
	uint32_t keycount = 0;
	char version = 0;

	fseek(fp, 0, SEEK_END);
	long size = ftell(fp);
	rewind(fp);

	DATREAD(fread((void*)&magic, sizeof(uint32_t), 1, fp), uint32_t);
	if (magic != MAGIC_HDR)
		return false;

	DATREAD(fread((void*)&version, sizeof(char), 1, fp), char);
	if (version > FFHL_VERSION || version < FFHL_MIN_VERSION)
		return false;

	DATREAD(fread((void*)&keycount, sizeof(uint32_t), 1, fp), uint32_t);
	DATREAD(fread((void*)&langCount, sizeof(uint32_t), 1, fp), uint32_t);

	uint32_t *LangOffsets = new uint32_t[langCount];
	char langname[3];
	
	for (unsigned int i = 0; i < langCount; i++)
	{
		DATREAD_S(fread(langname, sizeof(char), 2, fp), 2);
		langname[2] = 0;
		GetLang(langname);	//this will initialize for us
		DATREAD(fread((void *)&(LangOffsets[i]), sizeof(uint32_t), 1, fp), uint32_t);
	}

	//we should now be at the key table
	int ktbOffset = ftell(fp);
	unsigned char keylen;
	char keybuf[255];
	uint32_t bogus;
	uint32_t keyoffset, save;
	String _tmpkey;
	
	for (unsigned i = 0; i < keycount; i++)
	{
		if (version == 4)
			fread((void*)&(bogus), sizeof(uint32_t), 1, fp);
		DATREAD(fread((void*)&keyoffset, sizeof(uint32_t), 1, fp), uint32_t);
		if (keyoffset > size-sizeof(uint32_t))
		{
			Clear();
			fclose(fp);
			return false;
		}
		save = ftell(fp);
		fseek(fp, keyoffset, SEEK_SET);
		DATREAD(fread((void*)&keylen, sizeof(char), 1, fp), char);
		//technically this isn't possible since
		// a char will never be more than 255
		// but it's good practice.
		if (keylen > sizeof(keybuf))
			return false;
		DATREAD_S(fread(keybuf, sizeof(char), keylen, fp), keylen);
		keybuf[keylen] = 0;
		_tmpkey.assign(keybuf);
		AddKeyEntry(_tmpkey);
		fseek(fp, save, SEEK_SET);		//bring back to next key
	}

	//we should now be at the languages table
	uint32_t numentries;
	uint32_t keynum;
	uint32_t defoffset;
	unsigned short deflen;
	char valbuf[4096];
	
	for (unsigned int i = 0; i < langCount; i++)
	{
		DATREAD(fread((void*)&numentries, sizeof(uint32_t), 1, fp), uint32_t);
		
		for (unsigned int j = 0; j < numentries; j++)
		{
			DATREAD(fread((void *)&keynum, sizeof(uint32_t), 1, fp), uint32_t);
			if (version == 4)
			{
				DATREAD(fread((void *)&bogus, sizeof(uint32_t), 1, fp), uint32_t);
			}
			DATREAD(fread((void *)&defoffset, sizeof(uint32_t), 1, fp), uint32_t);
			if (defoffset > size-sizeof(uint32_t))
			{
				Clear();
				fclose(fp);
				return false;
			}
			save = ftell(fp);
			fseek(fp, defoffset, SEEK_SET);
			DATREAD(fread((void *)&deflen, sizeof(unsigned short), 1, fp), short);
			if (deflen > sizeof(valbuf))
				return false;
			DATREAD_S(fread(valbuf, sizeof(char), deflen, fp), deflen);
			valbuf[deflen] = 0;
			m_Languages[i]->AddEntry(keynum, valbuf);
			fseek(fp, save, SEEK_SET);	//bring back to next entry
		}
	}

	fclose(fp);

	delete [] LangOffsets;
	
    //we're done!
	return true;
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
	
	while (buf[i] = tolower(*langName++))
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
