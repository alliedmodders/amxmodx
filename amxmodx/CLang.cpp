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

#define LITIDX_NONE			0
#define LITIDX_BRACKET		1
#define LITIDX_DEFINITION	2

#define INSERT_NUMBER		1
#define INSERT_FLOAT		2
#define INSERT_STRING		3
#define INSERT_NEWLINE		4

// dictionary format is Fast-Format-Hash-Lookup, v4
#define MAGIC_HDR			0x4646484C
#define FFHL_VERSION		4
#define FFHL_MIN_VERSION	4

/*version history:
	* 1 (BAILOPAN) - Simplest form possible, no reverse
	* 2 (BAILOPAN) - One language per file with full reverse
	* 3 (PM OnoTo) - 2^32 languages per file with full reverse
	* 4 (BAILOPAN) - Optimized by separating and relocating tables (normalization)
FORMAT:
Magic					4bytes
Version					1byte
Number of Keys			4bytes
Number of Languages		4bytes
LANG INFO TABLE[]		
	Language Name		2bytes
	Offset				4bytes
KEY TABLE[]
	Key Hash			4bytes
	Key Lookup Offset	4bytes
LANGUAGES TABLE[]
	Language[]
		Definitions		4bytes
		Key Hash #		4bytes (virtual # in hash table, 0 indexed)
		Def Hash		4bytes
		Def Offset		4bytes
KEY LOOKUP TABLE[]
	Key length			1byte
	Key string			variable
DEF LOOKUP TABLE[]
	Def length			2bytes
	Def string			variable
	*/

/******** CRC & Strip *********/
const uint32_t CRCTable[256] = {
  0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
  0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
  0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
  0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
  0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
  0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
  0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c,
  0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
  0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
  0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
  0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190, 0x01db7106,
  0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
  0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
  0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
  0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
  0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
  0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
  0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
  0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
  0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
  0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
  0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
  0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
  0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
  0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
  0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
  0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
  0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
  0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
  0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
  0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
  0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
  0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
  0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
  0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
  0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
  0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
  0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
  0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
  0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
  0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
  0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
  0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d};

uint32_t CLangMngr::CLang::MakeHash(const char *src, bool makeLower)
{
	uint32_t crc = 0xFFFFFFFF;
	
	while (*src)
		crc = ((crc>>8)&0xFFFFFFFF)^CRCTable[(crc^ (makeLower ? tolower(*src++) : *src++) )&0xFF];
	
	return ~crc;
}

uint32_t CLangMngr::MakeHash(const char *src, bool makeLower)
{
	uint32_t crc = 0xFFFFFFFF;
	
	while (*src)
		crc = ((crc>>8)&0xFFFFFFFF)^CRCTable[(crc^ (makeLower ? tolower(*src++) : *src++) )&0xFF];
	
	return ~crc;
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

	for (i=strln-1; i>=0; i--)
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

/******** CLangMngr::CLang::LangEntry *********/
uint32_t CLangMngr::CLang::LangEntry::GetDefHash()
{
	return m_DefHash;
}

void CLangMngr::CLang::LangEntry::SetCache(bool c)
{
	m_isCache = c;
}

bool CLangMngr::CLang::LangEntry::GetCache()
{
	return m_isCache;
}

const char *CLangMngr::CLang::LangEntry::GetDef()
{
	return m_pDef.c_str();
}

int CLangMngr::CLang::LangEntry::GetDefLength()
{
	return m_pDef.size();
}

int CLangMngr::CLang::LangEntry::GetKey()
{ 
	return key;
}

CLangMngr::CLang::LangEntry::LangEntry()
{
	Clear();
}

CLangMngr::CLang::LangEntry::LangEntry(int pKey)
{
	Clear();
	SetKey(pKey);
}

CLangMngr::CLang::LangEntry::LangEntry(int pKey, const char *pDef)
{
	Clear();
	SetKey(pKey);
	SetDef(pDef);
}

CLangMngr::CLang::LangEntry::LangEntry(const LangEntry &other)
{
	Clear();
	SetKey(other.key);
	SetDef(other.m_pDef.c_str());
}

CLangMngr::CLang::LangEntry::LangEntry(int pKey, uint32_t defHash, const char *pDef)
{
	Clear();
	key = pKey;
	m_DefHash = defHash;
	m_pDef.assign(pDef);
}

void CLangMngr::CLang::LangEntry::Clear()
{
	m_pDef.clear();
	key = -1;
}

void CLangMngr::CLang::LangEntry::SetKey(int pkey)
{
	key = pkey;
}

void CLangMngr::CLang::LangEntry::SetDef(const char *pDef)
{
	m_pDef.assign(pDef);
	m_DefHash = MakeHash(pDef);
}

/******** CLangMngr::CLang *********/

CLangMngr::CLang::CLang()
{
	m_LookUpTable.clear();
}

CLangMngr::CLang::CLang(const char *lang)
{
	m_LookUpTable.clear();
	strncpy(m_LanguageName, lang, 2);
	m_LanguageName[2]=0;
}

CLangMngr::CLang::LangEntry *CLangMngr::CLang::AddEntry(int pKey, uint32_t defHash, const char *def, bool cache)
{
	LangEntry *p = new LangEntry(pKey, defHash, def);

	p->SetCache(cache);

	m_LookUpTable.push_back(p);

	return p;
}

CLangMngr::CLang::~CLang()
{
	Clear();
}

void CLangMngr::CLang::Clear()
{
	for (unsigned int i=0; i<m_LookUpTable.size(); i++)
	{
		if (m_LookUpTable[i])
			delete m_LookUpTable[i];
	}
	m_LookUpTable.clear();
}

CLangMngr::CLang::LangEntry * CLangMngr::CLang::GetEntry(int pkey)
{
	unsigned int i;
	
	for (i=0; i<m_LookUpTable.size(); i++)
	{
		if (m_LookUpTable[i]->GetKey() == pkey)
		{
			return m_LookUpTable[i];
		}
	}

	LangEntry *e = new LangEntry(pkey);
	e->SetKey(pkey);
	e->SetCache(true);
	m_LookUpTable.push_back(e);

	return e;
}

void CLangMngr::CLang::MergeDefinitions(CQueue<sKeyDef*> &vec)
{
	const char *def = 0;
	int key = -1;
	while (!vec.empty())
	{
		key = vec.front()->key;
		def = vec.front()->def->c_str();
		LangEntry *entry = GetEntry(key);
		if (entry->GetDefHash() != MakeHash(def))
		{
			if (entry->GetCache())
			{
				entry->SetDef(def);
				entry->SetKey(key);
				entry->SetCache(false);
			} else {
				//AMXXLOG_Log("[AMXX] Language key %s[%s] defined twice", m_LMan->GetKey(key), m_LanguageName);
			}
		}
		delete vec.front();
		vec.pop();
	}
}

const char * CLangMngr::CLang::GetDef(const char *key)
{
	static char nfind[1024] = "ML_NOTFOUND(KEY)";
	int ikey = m_LMan->GetKeyEntry(key);
	if (ikey == -1)
	{
		sprintf(nfind, "ML_NOTFOUND: %s", key);
		return nfind;
	}
	for (unsigned int i = 0; i<m_LookUpTable.size(); i++)
	{
		if (m_LookUpTable[i]->GetKey() == ikey)
			return m_LookUpTable[i]->GetDef();
	}
	return NULL;
}


struct OffsetPair
{
	uint32_t defOffset;
	uint32_t keyOffset;
};

// Assumes fp is set to the right position
bool CLangMngr::CLang::SaveDefinitions(FILE *fp, uint32_t &curOffset)
{
	unsigned short defLen = 0;
	for (unsigned int i = 0; i<m_LookUpTable.size(); i++)
	{
		defLen = m_LookUpTable[i]->GetDefLength();
		fwrite((void *)&defLen, sizeof(unsigned short), 1, fp);
		curOffset += sizeof(unsigned short);
		fwrite(m_LookUpTable[i]->GetDef(), sizeof(char), defLen, fp);
		curOffset += defLen;
	}

	return true;
}

// Assumes fp is set to the right position
bool CLangMngr::CLang::Save(FILE *fp, int &defOffset, uint32_t &curOffset)
{
	uint32_t keynum = 0;
	uint32_t defhash = 0;
	uint32_t size = m_LookUpTable.size();

	fwrite((void*)&size, sizeof(uint32_t), 1, fp);
	curOffset += sizeof(uint32_t);

	for (unsigned int i = 0; i<m_LookUpTable.size(); i++)
	{
		keynum = m_LookUpTable[i]->GetKey();
		defhash = m_LookUpTable[i]->GetDefHash();
		fwrite((void *)&keynum, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		fwrite((void *)&defhash, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		fwrite((void *)&defOffset, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		defOffset += sizeof(short);
		defOffset += m_LookUpTable[i]->GetDefLength();
	}

	return true;
}

// assumes fp is set to the right position
bool CLangMngr::CLang::Load(FILE *fp)
{

	return true;
}

/******** CLangMngr *********/

CLangMngr::CLangMngr()
{
	Clear();
}

const char * CLangMngr::GetKey(int key)
{
	if (key < 0 || key >= (int)KeyList.size())
		return 0;

	return KeyList[key]->key.c_str();
}

int CLangMngr::GetKeyHash(int key)
{
	if (key < 0 || key >= (int)KeyList.size())
		return 0;

	return KeyList[key]->hash;
}

int CLangMngr::GetKeyEntry(const char *key)
{
	uint32_t hKey = MakeHash(key, true);
	uint32_t cmpKey = 0;
	unsigned int i = 0;

	if (hKey == 0)
	{
		return -1;
	}

	for (i = 0; i<KeyList.size(); i++)
	{
		cmpKey = KeyList[i]->hash;
		if (hKey == cmpKey)
		{
			return i;
		}
	}

	return -1;
}

int CLangMngr::AddKeyEntry(String &key)
{
	uint32_t hKey = MakeHash(key.c_str(), true);

	keyEntry *e = new keyEntry;
	e->key.assign(key);
	e->hash = hKey;
	KeyList.push_back(e);

	return (KeyList.size() - 1);
}

int CLangMngr::GetKeyEntry(String &key)
{
	uint32_t hKey = MakeHash(key.c_str(), true);
	unsigned int i = 0;

	for (i = 0; i<KeyList.size(); i++)
	{
		if (hKey == KeyList[i]->hash)
		{
			return i;
		}
	}

	return -1;
}

#define CHECK_PTR(ptr, start, bufsize) if ((ptr) - (start) >= (bufsize)) { \
	LogError(amx, AMX_ERR_STACKERR, "Buffer overflow in string formatting"); \
	outbuf[0] = 0; \
	len = 0; \
	return outbuf; }
#define CHECK_OUTPTR(offset) CHECK_PTR(outptr+offset, outbuf, sizeof(outbuf))
#define ZEROTERM(buf) buf[(sizeof(buf)/sizeof(buf[0]))-1]=0;
#define NEXT_PARAM()		\
	if (parm > paramCount) \
	{ \
		strcpy(outbuf, ""); \
		len = 0; \
		LogError(amx, AMX_ERR_PARAMS, "String formatted incorrectly - parameter %d (total %d)", parm, paramCount); \
		return outbuf; \
	} 

char * CLangMngr::FormatAmxString(AMX *amx, cell *params, int parm, int &len)
{
	// number of parameters ( for NEXT_PARAM macro )
	int paramCount = *params / sizeof(cell);
	// the output buffer
	static char outbuf[4096];
	char *outptr = outbuf;
	cell *src = get_amxaddr(amx, params[parm++]);

	while (*src)
	{
		if (*src == '%')
		{
			++src;
			if (*src=='L')
			{
				cell langName = params[parm];		// "en" case (langName contains the address to the string)
				NEXT_PARAM();
				cell *pAmxLangName = get_amxaddr(amx, params[parm++]);	// other cases
				const char *cpLangName=NULL;
				// Handle player ids (1-32) and server language
				if (*pAmxLangName == LANG_PLAYER)	// LANG_PLAYER
				{
					if ((int)CVAR_GET_FLOAT("amx_client_languages") == 0)
					{
						cpLangName = g_vault.get("server_language");
					} else {
						cpLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(m_CurGlobId)->pEdict, "lang");
					}
				} else if (*pAmxLangName == LANG_SERVER) {	// LANG_SERVER
					cpLangName = g_vault.get("server_language");
				} else if (*pAmxLangName >= 1 && *pAmxLangName <= 32) {	// Direct Client Id
					if ((int)CVAR_GET_FLOAT("amx_client_languages") == 0)
					{
						cpLangName = g_vault.get("server_language");
					} else {
						cpLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(*pAmxLangName)->pEdict, "lang");
					}
				} else {	// Language Name
					int tmplen = 0;
					cpLangName = get_amxstring(amx, langName, 2, tmplen);
				}
				if (!cpLangName || strlen(cpLangName) < 1)
					cpLangName = "en";
				int tmplen = 0;
				NEXT_PARAM();
				char *key = get_amxstring(amx, params[parm++], 1, tmplen);
				const char *def = GetDef(cpLangName, key);
				if (def == NULL)
				{
					if (*pAmxLangName != LANG_SERVER)
					{
						def = GetDef(g_vault.get("server_language"), key);
					}
					if (strcmp(cpLangName, "en")!=0 && strcmp(g_vault.get("server_language"), "en")!=0)
					{
						def = GetDef("en", key);
					}
					if (!def)
					{
						static char buf[512];
						CHECK_PTR((char*)(buf+17+strlen(key)), buf, sizeof(buf));
						sprintf(buf, "ML_LNOTFOUND: %s", key);
						def = buf;
					}
				}
				while (*def)
				{
					if (*def == '%')
					{
						++def;
						if (*def == '%' || *def == 0)
						{
							*outptr++ = '%';
							++def;
						}
						else
						{
							static char format[32];
							format[0] = '%';
							char *ptr = format+1;
							
							while (ptr-format<sizeof(format) && !isalpha(*ptr++ = *def++))
								/*nothing*/;
							ZEROTERM(format);

							*ptr = 0;
							switch ( *(ptr-1) )
							{
							case 's':
								{
									static char tmpString[4096];
									char *tmpPtr = tmpString;
									NEXT_PARAM();
									cell *tmpCell = get_amxaddr(amx, params[parm++]);
									while (tmpPtr-tmpString < sizeof(tmpString) && *tmpCell)
										*tmpPtr++ = *tmpCell++;

									*tmpPtr = 0;
									_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, tmpString);
									ZEROTERM(outbuf);
									break;
								}
							case 'g':
							case 'f':
								{
									NEXT_PARAM();
									_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, *(REAL*)get_amxaddr(amx, params[parm++]));
									ZEROTERM(outbuf);
									break;
								}
							case 'i':
							case 'd':
							case 'c':
								{
									NEXT_PARAM();
									_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, (int)*get_amxaddr(amx, params[parm++]));
									ZEROTERM(outbuf);
									break;
								}
							default:
								{
									CHECK_OUTPTR(strlen(format)+1);
									strcpy(outptr, format);
									break;
								}
							}
							outptr += strlen(outptr);
						}
					}
					else if (*def == '^')
					{
						++def;
						switch (*def)
						{
						case 'n':
							CHECK_OUTPTR(1);
							*outptr++ = '\n';
							break;
						case 't':
							CHECK_OUTPTR(1);
							*outptr++ = '\t';
							break;
						case '^':
							CHECK_OUTPTR(1);
							*outptr++ = '^';
							break;
						default:
							CHECK_OUTPTR(2);
							*outptr++ = '^';
							*outptr++ = *def;
							break;
						}
						++def;
					}
					else
					{
						CHECK_OUTPTR(1);
						*outptr++ = *def++;
					}
				}
			}
			else
			{
				static char tmpString[4096];
				char *tmpPtr = tmpString;
				int tmpLen = 0;
				static char format[32] = {'%'};
				char *ptr = format+1;
				if (*src != '%')
				{
					while (*src != 0 && ptr-format<sizeof(format) && !isalpha(*ptr++ = *src++))
						/*nothing*/;
					*ptr = 0;
					ZEROTERM(format);
					--src;
					switch ( *(ptr-1) )
					{
					case 's':
						{
							NEXT_PARAM();
							cell *tmpCell = get_amxaddr(amx, params[parm++]);
							while (tmpPtr-tmpString<sizeof(tmpString) && *tmpCell)
								*tmpPtr++ = *tmpCell++;
							*tmpPtr = 0;
							_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, tmpString);
							ZEROTERM(outbuf);
							break;
						}
					case 'g':
					case 'f':
						{
							NEXT_PARAM();
							_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, *(REAL*)get_amxaddr(amx, params[parm++]));
							break;
						}
					case 'i':
					case 'd':
					case 'c':
						{
							NEXT_PARAM();
							_snprintf(outptr, sizeof(outbuf)-(outptr-outbuf)-1, format, (int)*get_amxaddr(amx, params[parm++]));
							break;
						}
					default:
						{
							CHECK_OUTPTR(strlen(format)+1);
							strcpy(outptr, format);
							break;
						}
					}
					outptr += strlen(outptr);
				} else {
					CHECK_OUTPTR(1);
					*outptr++ = '%';
				}
			}
		}
		else
		{
			CHECK_OUTPTR(1);
			*outptr++ = *src;
		}
		++src;
	}
	len = outptr - outbuf;
	CHECK_OUTPTR(1);
	*outptr++ = 0;
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

char *CLangMngr::FormatString(const char *fmt, va_list &ap)
{
	// the output buffer
	static char outbuf[4096];
	char *outptr = outbuf;
	const char *src = fmt;

	while (*src)
	{
		if (*src == '%')
		{
			++src;
			if (*src=='L')
			{
				NEXT_PARAM();
				const char *pAmxLangName = va_arg(ap, const char*);
				const char *cpLangName=NULL;
				// Handle player ids (1-32) and server language
				if (pAmxLangName == (const char *)LANG_PLAYER)	// LANG_PLAYER
				{
					if ((int)CVAR_GET_FLOAT("amx_client_languages"))
					{
						cpLangName = g_vault.get("server_language");
					} else {
						cpLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(m_CurGlobId)->pEdict, "lang");
					}
				} else if (pAmxLangName == (const char *)LANG_SERVER) {	// LANG_SERVER
					cpLangName = g_vault.get("server_language");
				} else if (pAmxLangName >= (const char *)1 && pAmxLangName <= (const char *)32) {	// Direct Client Id
					if ((int)CVAR_GET_FLOAT("amx_client_languages"))
					{
						cpLangName = g_vault.get("server_language");
					} else {
						cpLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I((int)pAmxLangName)->pEdict, "lang");
					}
				} else {	// Language Name
					int tmplen = 0;
					cpLangName = pAmxLangName;
				}
				if (!cpLangName || strlen(cpLangName) < 1)
					cpLangName = "en";
				int tmplen = 0;
				const char *key = va_arg(ap, const char *);
				const char *def = GetDef(cpLangName, key);
				if (def == NULL)
				{
					if (pAmxLangName != LANG_SERVER)
					{
						def = GetDef(g_vault.get("server_language"), key);
					}
					if (strcmp(cpLangName, "en")!=0 && strcmp(g_vault.get("server_language"), "en")!=0)
					{
						def = GetDef("en", key);
					}
					if (!def)
					{
						static char buf[512];
						CHECK_PTR((char*)(buf+17+strlen(key)), buf, sizeof(buf));
						sprintf(buf, "ML_LNOTFOUND: %s", key);
						def = buf;
					}
				}
				while (*def)
				{
					if (*def == '%')
					{
						++def;
						static char format[32];
						format[0] = '%';
						char *ptr = format+1;
						while (ptr-format<sizeof(format) && !isalpha(*ptr++ = *def++))
							/*nothing*/;
						ZEROTERM(format);

						*ptr = 0;
						vsprintf(outptr, format, ap);
						// vsprintf doesnt alter the ap, increment here
						switch (*(ptr-1))
						{
						case 'f':
							va_arg(ap, double);
							break;
						case 's':
							va_arg(ap, char *);
							break;
						case 'c':
						case 'd':
						case 'i':
						default:		// default: assume int-like parameter
							va_arg(ap, int);
							break;
						}
						outptr += strlen(outptr);
					}
					else if (*def == '^')
					{
						++def;
						switch (*def)
						{
						case 'n':
							CHECK_OUTPTR(1);
							*outptr++ = '\n';
							break;
						case 't':
							CHECK_OUTPTR(1);
							*outptr++ = '\t';
							break;
						case '^':
							CHECK_OUTPTR(1);
							*outptr++ = '^';
							break;
						default:
							CHECK_OUTPTR(2);
							*outptr++ = '^';
							*outptr++ = *def;
							break;
						}
						++def;
					}
					else
					{
						CHECK_OUTPTR(1);
						*outptr++ = *def++;
					}
				}
			}
			else
			{
				static char format[32] = {'%'};
				char *ptr = format+1;
				if (*src != '%')
				{
					while (*src != 0 && ptr-format<sizeof(format) && !isalpha(*ptr++ = *src++))
						/*nothing*/;
					*ptr = 0;
					ZEROTERM(format);
					--src;
					vsprintf(outptr, format, ap);
					// vsprintf doesnt alter the ap, increment here
					switch (*(ptr-1))
					{
					case 'f':
						va_arg(ap, double);
						break;
					case 's':
						va_arg(ap, char *);
						break;
					case 'c':
					case 'd':
					case 'i':
					default:		// default: assume int-like parameter
						va_arg(ap, int);
						break;
					}
					outptr += strlen(outptr);
				} else {
					CHECK_OUTPTR(1);
					*outptr++ = '%';
				}
			}
		}
		else
		{
			CHECK_OUTPTR(1);
			*outptr++ = *src;
		}
		++src;
	}
	CHECK_OUTPTR(1);
	*outptr++ = 0;
	return outbuf;
}
void CLangMngr::MergeDefinitions(const char *lang, CQueue<sKeyDef*> &tmpVec)
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
		for (iter=FileList.begin(); iter!=FileList.end(); ++iter)
		{
			if ( (*iter)->file.compare(file) == 0 )
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
	md5.update(fp);		// closes for us
	md5.finalize();
	char md5buffer[33];
	md5.hex_digest(md5buffer);
	bool foundFlag = false;
	
	CVector<md5Pair *>::iterator iter;
	for (iter=FileList.begin(); iter!=FileList.end(); ++iter)
	{
		if ( (*iter)->file.compare(file) == 0 )
		{
			if ( (*iter)->val.compare(md5buffer) == 0 )
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
	CQueue<sKeyDef*> Defq;
	String buf;
	char language[3];
	sKeyDef *tmpEntry = NULL;

	while (!feof(fp))
	{
		line++;
		buf._fread(fp);
		buf.trim();
		if (buf[0] == 0)
			continue;
		if (buf[0] == '[' && buf.size() >= 3)
		{
			if (multiline)
			{
				AMXXLOG_Log("New section, multiline unterminated (file \"%s\" line %d)", file, line);
				if (tmpEntry)
					delete tmpEntry;
				tmpEntry = 0;
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
					tmpEntry = new sKeyDef;
					String key;
					key.assign(buf.substr(0, pos).c_str());
					String def;
					def.assign(buf.substr(pos+1).c_str());
					key.trim();
					key.toLower();
					int iKey = GetKeyEntry(key);
					if (iKey == -1)
						iKey = AddKeyEntry(key);
					tmpEntry->key = iKey;
					tmpEntry->def = new String;
					tmpEntry->def->assign(def.c_str());
					tmpEntry->def->trim();
					Defq.push(tmpEntry);
					tmpEntry = 0;
				} else {
					pos = buf.find(':');
					if (pos > String::npos)
					{
						tmpEntry = new sKeyDef;
						String key;
						key.assign(buf.substr(0, pos).c_str());;
						key.trim();
						key.toLower();
						int iKey = GetKeyEntry(key);
						if (iKey == -1)
							iKey = AddKeyEntry(key);
						tmpEntry->key = iKey;
						tmpEntry->def = new String;
						multiline = true;
					} else {
						//user typed a line with no directives
						AMXXLOG_Log("Invalid multi-lingual line (file \"%s\" line %d)", file, line);
					}
				}
			} else {
				if (buf[0] == ':')
				{
					Defq.push(tmpEntry);
					tmpEntry = 0;
					multiline = false;
				} else {
					tmpEntry->def->append(buf);
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
	for (iter=m_Languages.begin(); iter!=m_Languages.end(); ++iter)
	{
		if ( strcmp((*iter)->GetName(), name)==0 )
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
	for (iter=m_Languages.begin(); iter!=m_Languages.end(); ++iter)
	{
		if ( strcmp((*iter)->GetName(), name)==0 )
			return (*iter);
	}

	return NULL;
}

const char *CLangMngr::GetDef(const char *langName, const char *key)
{
	CLang *lang = GetLangR(langName);
	if (lang)
		return lang->GetDef(key);
	return "ML_NOTFOUND(LANG)";
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
	uint32_t ktbSize = KeyList.size() * (sizeof(uint32_t) + sizeof(uint32_t));
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
	for (unsigned int i = 0; i<m_Languages.size(); i++)
	{
		langName = m_Languages[i]->GetName();
		fwrite(langName, sizeof(char), 2, fp);
		curOffset += sizeof(char) * 2;
		fwrite((void *)&langOffset, sizeof(uint32_t), 1, fp);
		langOffset += sizeof(uint32_t) + (m_Languages[i]->Entries() * (sizeof(uint32_t) * 3));
		curOffset += sizeof(uint32_t);
	}
	
	//Note - langOffset now points to the start of key lookup table
	uint32_t keyHash = 0;
	uint32_t keyOffset = langOffset;
	for (unsigned int i = 0; i<KeyList.size(); i++)
	{
		keyHash = KeyList[i]->hash;
		fwrite((void*)&keyHash, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		fwrite((void*)&keyOffset, sizeof(uint32_t), 1, fp);
		curOffset += sizeof(uint32_t);
		keyOffset += sizeof(char);
		keyOffset += KeyList[i]->key.size();
	}

	//Note - now keyOffset points toward the start of the def table
	int defOffset = keyOffset;
	for (unsigned int i = 0; i<m_Languages.size(); i++)
	{
		m_Languages[i]->Save(fp, defOffset, curOffset);
	}

	//Now, defOffset points toward the END of the file
	//curoffset should point toward the key table, so...
	unsigned char keyLen = 0;
	for (unsigned int i = 0; i<KeyList.size(); i++)
	{
		keyLen = KeyList[i]->key.size();
		fwrite((void*)&keyLen, sizeof(unsigned char), 1, fp);
		curOffset += sizeof(unsigned char);
		fwrite(KeyList[i]->key.c_str(), sizeof(char), keyLen, fp);
		curOffset += sizeof(char) * keyLen;
	}

	//Finally, write the def table
	// It's assumed no orders changed...
	for (unsigned int i = 0; i<m_Languages.size(); i++)
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

	for (i=FileList.begin(); i!=FileList.end(); i++)
	{
		len = (*i)->file.size();
		fwrite((void *)&len, sizeof(char), 1, fp);
		fwrite((*i)->file.c_str(), sizeof(char), len, fp);
		fwrite((*i)->val.c_str(), sizeof(char), 32, fp);
	}

	fclose(fp);

	return true;
}

bool CLangMngr::LoadCache(const char *filename)
{
	FILE *fp = fopen(filename, "rb");
	if (!fp)
	{
		return false;
	}

	short dictCount = 0;
	char len = 0;
	char buf[255];
	char md5[34];
	fread((void*)&dictCount, sizeof(short), 1, fp);
	md5Pair *p = 0;


	for (int i=1; i<=dictCount; i++)
	{
		fread((void*)&len, sizeof(char), 1, fp);
		fread(buf, sizeof(char), len, fp);
		buf[len] = 0;
		fread(md5, sizeof(char), 32, fp);
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

	fread((void*)&magic, sizeof(uint32_t), 1, fp);
	if (magic != MAGIC_HDR)
		return false;

	fread((void*)&version, sizeof(char), 1, fp);
	if (version > FFHL_VERSION || version < FFHL_MIN_VERSION)
		return false;

	fread((void*)&keycount, sizeof(uint32_t), 1, fp);
	fread((void*)&langCount, sizeof(uint32_t), 1, fp);

	uint32_t *LangOffsets = new uint32_t[langCount];
	char langname[3];
	for (unsigned int i=0; i<langCount; i++)
	{
		fread(langname, sizeof(char), 2, fp);
		langname[2] = 0;
		GetLang(langname);	//this will initialize for us
		fread((void *)&(LangOffsets[i]), sizeof(uint32_t), 1, fp);
	}

	//we should now be at the key table
	int ktbOffset = ftell(fp);
	keyEntry *e = 0;
	unsigned char keylen;
	uint32_t keyoffset, save;
	for (unsigned i=0; i<keycount; i++)
	{
		e = new keyEntry;
		fread((void*)&(e->hash), sizeof(uint32_t), 1, fp);
		fread((void*)&keyoffset, sizeof(uint32_t), 1, fp);
		save = ftell(fp);
		fseek(fp, keyoffset, SEEK_SET);
		fread((void*)&keylen, sizeof(char), 1, fp);
		char *data = new char[keylen+1];
		fread(data, sizeof(char), keylen, fp);
		data[keylen] = 0;
		e->key.assign(data);
		delete [] data;
		KeyList.push_back(e);
		fseek(fp, save, SEEK_SET);		//bring back to next key
	}

	//we should now be at the languages table
	uint32_t numentries;
	uint32_t keynum;
	uint32_t defhash;
	uint32_t defoffset;
	unsigned short deflen;
	for (unsigned int i=0; i<langCount; i++)
	{
		fread((void*)&numentries, sizeof(uint32_t), 1, fp);
		for (unsigned int j=0; j<numentries; j++)
		{
			fread((void *)&keynum, sizeof(uint32_t), 1, fp);
			fread((void *)&defhash, sizeof(uint32_t), 1, fp);
			fread((void *)&defoffset, sizeof(uint32_t), 1, fp);
			save = ftell(fp);
			fseek(fp, defoffset, SEEK_SET);
			fread((void *)&deflen, sizeof(unsigned short), 1, fp);
			char *data = new char[deflen+1];
			fread(data, sizeof(char), deflen, fp);
			data[deflen] = 0;
			m_Languages[i]->AddEntry(keynum, defhash, data, true);
			delete [] data;
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
	for (i=0; i<m_Languages.size(); i++)
	{
		if (m_Languages[i])
			delete m_Languages[i];
	}

	for (i=0; i<FileList.size(); i++)
	{
		if (FileList[i])
			delete FileList[i];
	}

	for (i=0; i<KeyList.size(); i++)
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
	for (iter=m_Languages.begin(); iter!=m_Languages.end(); ++iter)
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
	char buf[3] = { 0 };
	int i = 0;
	while (buf[i] = tolower(*langName++))
	{	
		if (++i == 2)
			break;
	}
	
	LangVecIter iter;
	for (iter=m_Languages.begin(); iter!=m_Languages.end(); ++iter)
	{
		if ( strcmp((*iter)->GetName(), buf)==0 )
			return true;
	}
	return false;
}

void CLangMngr::SetDefLang(int id)
{
	m_CurGlobId = id;
}
