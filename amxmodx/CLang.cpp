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

#include "amxmodx.h"
#include "CLang.h"


#define LITIDX_NONE			0
#define LITIDX_BRACKET		1
#define LITIDX_DEFINITION	2

#define INSERT_NUMBER		1
#define INSERT_FLOAT		2
#define INSERT_STRING		3
#define INSERT_NEWLINE		4

// dictionary format is Fast-Format-Hash-Lookup, v3
#define MAGIC_HDR			0x4646484C

/*
	FORMAT:
	Magic							(4 bytes)
	Number of languages				(4 bytes)
	LANGUAGE INFO TABLE
		For every language:
			Language Name			(2 bytes)
			Absolut offset			(4 bytes)
	LANGUAGES
		For every language:
			Number of entries				(4 bytes)

			Offset to Definition Table		(4 bytes)
			Length of Definition Table		(4 bytes)

			Offset to Reverse Lookup Table	(4 bytes)
			Size of Reverse Lookup Table	(4 bytes)


			LOOK UP TABLE
				For every entry:
					Key hash				(4 bytes)
					Definition hash			(4 bytes)
					Definition offset		(4 bytes)		(into the Def table)
					Key offset				(4 bytes)		(into the Rev table)
			DEFINITION TABLE
				For every entry:
					Definition length		(2 bytes)
					Definition				(variable size)
			REVERSE LOOKUP TABLE
				For every entry:
					Key length				(1 byte)
					Key						(variable size)
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

uint32_t CLangMngr::CLang::LangEntry::GetKeyHash()
{
	return m_KeyHash;
}

const char *CLangMngr::CLang::LangEntry::GetKey()
{
	return m_pKey;
}

const char *CLangMngr::CLang::LangEntry::GetDef()
{
	return m_pDef;
}


CLangMngr::CLang::LangEntry::LangEntry()
{
	Clear();
}

CLangMngr::CLang::LangEntry::LangEntry(const char *pKey)
{
	Clear();
	SetKey(pKey);
}

CLangMngr::CLang::LangEntry::LangEntry(const char *pKey, const char *pDef)
{
	Clear();
	SetKey(pKey);
	SetDef(pDef);
}

CLangMngr::CLang::LangEntry::LangEntry(const LangEntry &other)
{
	Clear();
	SetKey(other.m_pKey);
	SetDef(other.m_pDef);
}


void CLangMngr::CLang::LangEntry::operator= (const char *pNewDef)
{
	SetDef(pNewDef);
}

bool CLangMngr::CLang::LangEntry::operator== (uint32_t hash)
{
	return m_KeyHash == hash;
}

void CLangMngr::CLang::LangEntry::Clear()
{
	m_pKey = NULL;
	m_pDef = NULL;
	m_KeyHash = 0;
	m_DefHash = 0;
}

void CLangMngr::CLang::LangEntry::SetKey(const char *pKey)
{
	delete [] m_pKey;
	m_pKey = new char[strlen(pKey)+1];
	strcpy(m_pKey, pKey);
	m_KeyHash = MakeHash(pKey, true);
}

void CLangMngr::CLang::LangEntry::SetDef(const char *pDef)
{
	delete [] m_pDef;
	m_pDef = new char[strlen(pDef)+1];
	strcpy(m_pDef, pDef);
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

CLangMngr::CLang::~CLang()
{
	Clear();
}

void CLangMngr::CLang::Clear()
{
	m_LookUpTable.clear();
}

CLangMngr::CLang::LangEntry & CLangMngr::CLang::GetEntry(const char *key)
{
	LookUpVecIter iter;
	uint32_t hash = MakeHash(key, true);
	for (iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
		if (*iter == hash)
			break;
	if (iter != m_LookUpTable.end())
		return *iter;
	
	m_LookUpTable.push_back(LangEntry(key));
	return m_LookUpTable.back();
}

void CLangMngr::CLang::MergeDefinitions(CVector<sKeyDef> &vec)
{
	for (CVector<sKeyDef>::iterator iter = vec.begin(); iter != vec.end(); ++iter)
	{
		LangEntry & entry = GetEntry(iter->key);
		if (entry.GetDefHash() != MakeHash(iter->def))
			entry = iter->def;
	}
}

void CLangMngr::CLang::Dump()
{
	LookUpVecIter iter;
	for (iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
		printf(" %s(%d)=%s(%d)\n", iter->GetKey(), iter->GetKeyHash(), iter->GetDef(), iter->GetDefHash());
}


const char * CLangMngr::CLang::GetDef(const char *key)
{
	uint32_t hash = MakeHash(key, true);
	for (LookUpVecIter iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
		if (*iter == hash)
			return iter->GetDef();
	return "(not found)";
}


struct OffsetPair
{
	uint32_t defOffset;
	uint32_t keyOffset;
};
// Assumes fp is set to the right position
bool CLangMngr::CLang::Save(FILE *fp)
{
	uint32_t tmpu32;

	long startOffset = ftell(fp);

	tmpu32 = m_LookUpTable.size();
	fflush(fp);
	// number of entries
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);

	// write fields that will be filled later
	tmpu32 = 0;
	// offset to definition table
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	// size of definition table
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	// offset to rev lookup table
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	// size of rev lookup table
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	uint32_t dtb, rtb;

	fflush(fp);
	// Write the lookup table
	for (LookUpVecIter iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
	{
		uint32_t ltbE[4] = {
			iter->GetKeyHash(),
			iter->GetDefHash(),
			0,					// def offset
			0					// key offset
		};
		fwrite((void*)ltbE, sizeof(uint32_t), 4, fp);
	}

	// Write the definition table
	dtb = ftell(fp);
	CVector<OffsetPair> offsetVec;
	offsetVec.reserve(m_LookUpTable.size());
	OffsetPair tmp;
	tmp.keyOffset = 0;
	for (LookUpVecIter iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
	{
		tmp.defOffset = ftell(fp);
		offsetVec.push_back(tmp);
		uint16_t defLength = strlen(iter->GetDef());
		fwrite((void*)&defLength, sizeof(defLength), 1, fp);
		fwrite((void*)iter->GetDef(), 1, defLength, fp);
	}

	// Write the reverse lookup table
	rtb = ftell(fp);
	CVector<OffsetPair>::iterator offsetVecIter = offsetVec.begin();
	for (LookUpVecIter iter = m_LookUpTable.begin(); iter != m_LookUpTable.end(); ++iter)
	{
		offsetVecIter->keyOffset = ftell(fp);
		++offsetVecIter;

		char keyLength = strlen(iter->GetKey());
		fwrite((void*)&keyLength, 1, 1, fp);
		fwrite((void*)iter->GetKey(), 1, keyLength, fp);
	}

	// Write the offsets
	fflush(fp);
	int i = 0;
	for (CVector<OffsetPair>::iterator iter = offsetVec.begin(); iter != offsetVec.end(); ++iter)
	{
		fseek(fp, startOffset + 20 + i*16 + 8, SEEK_SET);

		tmpu32 = iter->defOffset;
		fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
		tmpu32 = iter->keyOffset;
		fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
		fflush(fp);
		++i;
	}

	fseek(fp, 0, SEEK_END);
	uint32_t dtbSize = rtb - dtb;
	uint32_t rtbSize = ftell(fp) - rtb;
	fseek(fp, startOffset + 4, SEEK_SET);
	fwrite((void*)&dtb, sizeof(uint32_t), 1,fp);
	fwrite((void*)&dtbSize, sizeof(uint32_t), 1,fp);
	fwrite((void*)&rtb, sizeof(uint32_t), 1,fp);
	fwrite((void*)&rtbSize, sizeof(uint32_t), 1,fp);
	return true;
}

// assumes fp is set to the right position
bool CLangMngr::CLang::Load(FILE *fp)
{
	uint32_t numOfEntries;
	uint32_t dtb, rtb, dtbSize, rtbSize;

	Clear();

	fread((void*)&numOfEntries, sizeof(uint32_t), 1, fp);
	fread((void*)&dtb, sizeof(uint32_t), 1, fp);
	fread((void*)&dtbSize, sizeof(uint32_t), 1, fp);
	fread((void*)&rtb, sizeof(uint32_t), 1, fp);
	fread((void*)&rtbSize, sizeof(uint32_t), 1, fp);

	char keyBuf[257];
	char defBuf[4096];

	for (unsigned int i = 0; i < numOfEntries; ++i)
	{
		uint32_t keyHash, defHash;
		uint32_t tmp1, tmp2, tmp3;
		fread((void*)&keyHash, sizeof(uint32_t), 1, fp);
		fread((void*)&defHash, sizeof(uint32_t), 1, fp);
		fread((void*)&tmp1, sizeof(uint32_t), 1, fp);
		fread((void*)&tmp2, sizeof(uint32_t), 1, fp);
		tmp3 = ftell(fp);
		unsigned char tmpu8;
		uint16_t tmpu16;
		// definition
		fseek(fp, tmp2, SEEK_SET);
		fread((void*)&tmpu8, 1, 1, fp);
		fread((void*)keyBuf, 1, tmpu8, fp);
		keyBuf[tmpu8] = 0;
		//key
		fseek(fp, tmp1, SEEK_SET);
		fread((void*)&tmpu16, 2, 1, fp);
		fread((void*)defBuf, 1, tmpu16, fp);
		defBuf[tmpu16] = 0;

		// add to the entries
		m_LookUpTable.push_back(LangEntry(keyBuf, defBuf));

		// seek back
		fseek(fp, tmp3, SEEK_SET);
	}
	return true;
}

/******** CLangMngr *********/

const char *CLangMngr::Format(const char *src, ...)
{
	va_list argptr;
	va_start(argptr, src);
	static char outbuf[4096];
	char *outptr = outbuf;
	enum State
	{
		S_Normal,
		S_PercentSign,
	};

	State curState = S_Normal;

	while (*src)
	{
		if (*src == '%' && curState == S_Normal)
			curState = S_PercentSign;
		else if (curState == S_PercentSign)
		{
			switch (*src)
			{
			case 's':
				{
					char *tmpArg = va_arg(argptr, char*);
					while (*tmpArg)
						*outptr++ = *tmpArg++;
					break;
				}
			case 'd':
				{
					itoa(va_arg(argptr, int), outptr, 10);
					outptr += strlen(outptr);
					break;
				}
			case 'f':
			case 'g':
				{
					double tmpArg = va_arg(argptr, double);
					sprintf(outptr, "%f", tmpArg);
					outptr += strlen(outptr);
					break;
				}
			case 'L':
				{
					char *langName = va_arg(argptr, char*);
					// Handle player ids (1-32)
					if ((int)langName >= 1 && (int)langName <= 32)
						langName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I((int)langName)->pEdict, "_language");

					char *key = va_arg(argptr, char*);
					const char *def = GetDef(langName, key);
					while (*def)
					{
						switch (*def)
						{
						case INSERT_NUMBER:
							{
								itoa(va_arg(argptr, int), outptr, 10);
								outptr += strlen(outptr);
								break;
							}
						case INSERT_STRING:
							{
								char *tmpArg = va_arg(argptr, char*);
								while (*tmpArg)
									*outptr++ = *tmpArg++;
								break;
							}
						case INSERT_FLOAT:
							{
								double tmpArg = va_arg(argptr, double);
								sprintf(outptr, "%f", tmpArg);
								outptr += strlen(outptr);
								break;
								break;
							}
						case INSERT_NEWLINE:
							*outptr++ = '\n';
							break;
						default:
							*outptr++ = *def;
						}
						++def;
					}
					break;
				}
			default:
				*outptr++= '%';
				*outptr++ = *src;
			}
			curState = S_Normal;
		}
		else
			*outptr++ = *src;
		++src;
	}
	*outptr++ = 0;
	return outbuf;
}

char * CLangMngr::FormatAmxString(AMX *amx, cell *params, int parm, int &len)
{
	cell *src = get_amxaddr(amx, params[parm++]);
	static char outbuf[4096];
	char *outptr = outbuf;
	enum State
	{
		S_Normal,
		S_PercentSign,
	};

	State curState = S_Normal;
	while (*src)
	{
		if (*src == '%' && curState == S_Normal)
			curState = S_PercentSign;
		else if (curState == S_PercentSign)
		{
			switch (*src)
			{
			case 's':
				{
					cell *tmpArg = get_amxaddr(amx, params[parm++]);;
					while (*tmpArg)
						*outptr++ = *tmpArg++;
					break;
				}
			case 'f':
			case 'g':
				{
					char format[16];
					format[0] = '%';
					char *ptr = format+1;
					while (!isalpha(*ptr++ = *src++))
						/*nothing*/;
					--src;
					*ptr = 0;
					sprintf(outptr, format, *(REAL*)get_amxaddr(amx, params[parm++]));
					outptr += strlen(outptr);
					break;
				}
			case 'L':
				{
					cell langName = params[parm];
					cell *pLangName = get_amxaddr(amx, params[parm++]);
					const char *cpLangName=NULL;
					// Handle player ids (1-32) and server language
					if (*pLangName == 0)
						*pLangName = m_CurGlobId;
					if (*pLangName == -1)
						cpLangName = g_vault.get("server_language");
					else if (*pLangName > 1 && *pLangName < 32)
						cpLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(*pLangName)->pEdict, "_language");
					else
					{
						int len = 0;
						cpLangName = get_amxstring(amx, langName, 2, len);
					}
					int len;
					char *key = get_amxstring(amx, params[parm++], 1, len);
					const char *def = GetDef(cpLangName, key);
					while (*def)
					{
						switch (*def)
						{
						case INSERT_NUMBER:
							{
								itoa((float)*(REAL*)get_amxaddr(amx, params[parm++]), outptr, 10);
								outptr += strlen(outptr);
								break;
							}
						case INSERT_STRING:
							{
								cell *tmpArg = get_amxaddr(amx, params[parm++]);;
								while (*tmpArg)
									*outptr++ = *tmpArg++;
								break;
							}
						case INSERT_FLOAT:
							{
								sprintf(outptr, "%f", *(REAL*)get_amxaddr(amx, params[parm++]));
								outptr += strlen(outptr);
								break;
							}
						case INSERT_NEWLINE:
							*outptr++ = '\n';
							break;
						default:
							*outptr++ = *def;
						}
						++def;
					}
					break;
				}
			default:
				{
					char format[16];
					format[0] = '%';
					char *ptr = format+1;
					while (!isalpha(*ptr++ = *src++))
						/*nothing*/;
					--src;
					*ptr = 0;
					sprintf(outptr, format, *get_amxaddr(amx, params[parm++]));
					outptr += strlen(outptr);
					break;
				}
			}
			curState = S_Normal;
		}
		else
			*outptr++ = *src;
		++src;
	}
	len = outptr - outbuf;
	*outptr++ = 0;
	return outbuf;
}

void CLangMngr::MergeDefinitions(const char *lang, CVector<sKeyDef> &tmpVec)
{
	CLang & language = GetLang(lang);
	language.MergeDefinitions(tmpVec);
}

//this is the file parser for dictionary text files
// -- BAILOPAN
int CLangMngr::MergeDefinitionFile(const char *file)
{
	FILE *fp = fopen(file, "rt");
	if (!fp)
		return 0;
	char buf[4096];
	char language[3];
	char bufstk[4096];

	// Allocate enough memory to store everything
	fseek(fp, 0, SEEK_END);
	long fileSize = ftell(fp);
	char *tempStorage = new char[fileSize];
	fseek(fp, 0, SEEK_SET);
	char *ptrStorage = tempStorage;

	unsigned int i = 0, j = 0;
	int litidx = LITIDX_NONE;
	int stk = 0;
	int max = 0;
	int multiline = 0;
	CLang lang;
	CVector<sKeyDef> tmpVec;
	sKeyDef tmpEntry;
	while (fgets(buf, 4095, fp) != NULL)
	{
		j++;
		size_t len=strlen(buf);
		for (i=0; i<len; i++)
		{
			// be very careful not to mess up litidx assignments.
			//  also stk assignments.
			switch (litidx)
			{
			case LITIDX_NONE:
				{
					if (buf[i] == '[')
					{
						// Merge last language
						if (!tmpVec.empty())
						{
							MergeDefinitions(language, tmpVec);
							tmpVec.clear();
						}
						litidx = LITIDX_BRACKET;
						stk = 0;
					} else if (buf[i] == '=') {
						bufstk[stk] = 0;
						tmpEntry.key = ptrStorage;
						ptrStorage += strip(bufstk, ptrStorage, true);
						stk = 0;
						bufstk[stk] = 0;
						litidx = LITIDX_DEFINITION;
					} else if (buf[i] == ':') {
						bufstk[stk] = 0;
						tmpEntry.key = ptrStorage;
						ptrStorage += strip(bufstk, ptrStorage, true);
						stk = 0;
						bufstk[stk] = 0;
						multiline = 1;
						litidx = LITIDX_DEFINITION;
					} else {
						bufstk[stk++] = buf[i];
					}
					break;
				}
			case LITIDX_DEFINITION:
				{
					if (buf[i] == '\n' && !multiline)
					{
						bufstk[stk] = 0;
						tmpEntry.def = ptrStorage;
						tmpVec.push_back(tmpEntry);
						ptrStorage += strip(bufstk, ptrStorage);
						stk = 0;
						bufstk[stk] = 0;
						multiline = 0;
						litidx = LITIDX_NONE;
					} else if (buf[i] == '%') {
						i++;
						if (buf[i] == 's')
						{
							bufstk[stk++] = INSERT_STRING;
						} else if (buf[i] == 'f') {
							bufstk[stk++] = INSERT_FLOAT;
						} else if (buf[i] == 'd') {
							bufstk[stk++] = INSERT_NUMBER;
						} else if (buf[i] == 'n') {
							bufstk[stk++] = INSERT_NEWLINE;
						} else {
							bufstk[stk++] = '%';
							bufstk[stk++] = buf[i];
						}
					} else if (buf[i] == ':' && multiline && i == 0) {
						bufstk[stk] = 0;
						tmpEntry.def = ptrStorage;
						tmpVec.push_back(tmpEntry);
						ptrStorage += strip(bufstk, ptrStorage);
						stk = 0;
						bufstk[stk] = 0;
						//lang.MergeDefinition(key, def);
						litidx = LITIDX_NONE;
						multiline = 0;
					} else {
						if (stk < (sizeof(bufstk)-1))
							bufstk[stk++] = buf[i];
					}
					break;
				}
			case LITIDX_BRACKET:
				{
					if (buf[i] == ']')
					{
						language[stk] = 0;
						ptrStorage = tempStorage;			// reset storage pointer
						stk = 0;
						litidx = LITIDX_NONE;
					} else {
						if (stk < (sizeof(language)-1) && isalpha(buf[i]))
							language[stk++] = tolower(buf[i]);
					}
					break;
				}
			}
		}
	}
	// finish last definition if no newline is at the end of file
	if (litidx==LITIDX_DEFINITION)
	{
		bufstk[stk] = 0;
		tmpEntry.def = ptrStorage;
		tmpVec.push_back(tmpEntry);
		ptrStorage += strip(bufstk, ptrStorage);
	}
	// merge last language
	if (!tmpVec.empty())
		MergeDefinitions(language, tmpVec);
	delete [] tempStorage;
	return 1;
}

// Find a CLang by name, if not found, add it
CLangMngr::CLang & CLangMngr::GetLang(const char *name)
{
	LangVecIter iter;
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
		if (*iter == name)
			break;
	if (iter != m_Languages.end())
		return *iter;

	m_Languages.push_back(CLang(name));
	return m_Languages.back();
}

void CLangMngr::Dump()
{
	LangVecIter iter;
	for (iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		printf("LANGUAGE: %s\n", iter->GetName());
		iter->Dump();
	}
}

const char *CLangMngr::GetDef(const char *langName, const char *key)
{
	CLang & lang = GetLang(langName);
	return lang.GetDef(key);
}

bool CLangMngr::Save(const char *filename)
{
	FILE *fp = fopen(filename, "wb");
	if (!fp)
		return false;
	uint32_t tmpu32;
	// Magic
	tmpu32 = MAGIC_HDR;
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	tmpu32 = m_Languages.size();
	fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	// Write language info table
	tmpu32 = 0;
	for (LangVecIter iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
	{
		// name
		fwrite((void*)iter->GetName(), 2, 1, fp);
		// offset ( to be set later )
		fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
	}
	fflush(fp);
	CVector<uint32_t> langOffsets;
	langOffsets.reserve(m_Languages.size());

	// write the languages
	for (LangVecIter iter1 = m_Languages.begin(); iter1 != m_Languages.end(); ++iter1)
	{
		langOffsets.push_back((uint32_t)ftell(fp));
		if (!iter1->Save(fp))
		{
			fclose(fp);
			return false;
		}
		// Make sure we are at the end of the file again
		fseek(fp, 0, SEEK_END);
	}

	// write the offsets
	int i = 0;
	for (CVector<uint32_t>::iterator iter2 = langOffsets.begin(); iter2 != langOffsets.end(); ++iter2)
	{
		fseek(fp, 8 + i*6 + 2, SEEK_SET);
		tmpu32 = *iter2;
		fwrite((void*)&tmpu32, sizeof(tmpu32), 1, fp);
		++i;
	}
	fclose(fp);
	return true;
}

bool CLangMngr::Load(const char *filename)
{
	Clear();

	// open the file
	FILE *fp = fopen(filename, "rb");
	if (!fp)
		return false;

	uint32_t tmpu32;
	// Check magic
	fread((void*)&tmpu32, sizeof(uint32_t), 1, fp);
	if (tmpu32 != MAGIC_HDR)
		return false;

	uint32_t numOfLangs;
	fread((void*)&numOfLangs, sizeof(uint32_t), 1, fp);

	// Read language info table
	CVector<uint32_t> langOffsets;
	langOffsets.reserve(numOfLangs);
	for (uint32_t i = 0; i < numOfLangs; ++i)
	{
		char langName[3];
		fread((void*)langName, 2, 1, fp);
		langName[2] = 0;
		GetLang(langName);
		fread((void*)&tmpu32, sizeof(uint32_t), 1, fp);
		langOffsets.push_back(tmpu32);
	}

	// read languages
	for (uint32_t i = 0; i < numOfLangs; ++i)
	{
		fseek(fp, langOffsets[i], SEEK_SET);
		if (!m_Languages[i].Load(fp))
			return false;
	}
	fclose(fp);
	return true;
}

void CLangMngr::Clear()
{
	m_Languages.clear();
}

int CLangMngr::GetLangsNum()
{
	return m_Languages.size();
}

const char *CLangMngr::GetLangName(int langId)
{
	return m_Languages.at(langId).GetName();
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
	
	for (LangVecIter iter = m_Languages.begin(); iter != m_Languages.end(); ++iter)
		if (*iter == buf)
			return true;
	return false;
}

void CLangMngr::SetDefLang(int id)
{
	m_CurGlobId = id;
}