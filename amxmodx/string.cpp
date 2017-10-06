// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <ctype.h>
#include "amxmodx.h"
#include "format.h"
#include "binlog.h"
#include <utf8rewind.h>

const char* stristr(const char* str, const char* substr)
{
	register char *needle = (char *)substr;
	register char *prevloc = (char *)str;
	register char *haystack = (char *)str;

	while (*haystack)
	{
		if (tolower(*haystack) == tolower(*needle))
		{
			haystack++;
			if (!*++needle)
				return prevloc;
		} else {
			haystack = ++prevloc;
			needle = (char *)substr;
		}
	}

	return NULL;
}

char* format_amxstring(AMX *amx, cell *params, int parm, int& len)
{
#if !defined BINLOG_ENABLED
	return g_langMngr.FormatAmxString(amx, params, parm, len);
#else
	char *ans = g_langMngr.FormatAmxString(amx, params, parm, len);
	if (g_binlog_level & 4)
	{
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
		if (pl)
			g_BinLog.WriteOp(BinLog_FormatString, pl->getId(), parm, len, ans);
	}
	return ans;
#endif
}

int amxstring_len(cell* a)
{
	register int c = 0;

	while (a[c])
		++c;
	
	return c;
}

cell* get_amxaddr(AMX *amx, cell amx_addr)
{
	return (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
}

int set_amxstring_simple(cell *dest, const char *source, int max)
{
	cell* start = dest;

	while (max-- && *source)
	{
		*dest++ = (unsigned char)*source++;
	}

	*dest = 0;

	return dest - start;
}

int set_amxstring(AMX *amx, cell amx_addr, const char *source, int max)
{
	register cell* dest = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register cell* start = dest;

#if defined BINLOG_ENABLED
	if (g_binlog_level & 2)
	{
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
		if (pl)
			g_BinLog.WriteOp(BinLog_SetString, pl->getId(), amx_addr, max, source);
	}
#endif
	
	while (max-- && *source)
		*dest++ = (unsigned char)*source++;
	
	*dest = 0;
	
	return dest - start;
}

template int set_amxstring_utf8<cell>(AMX *, cell, const cell *, size_t, size_t);
template int set_amxstring_utf8<char>(AMX *, cell, const char *, size_t, size_t);

template <typename T>
int set_amxstring_utf8(AMX *amx, cell amx_addr, const T *source, size_t sourcelen, size_t maxlen)
{
	size_t len = sourcelen;
	bool needtocheck = false;

	register cell* dest = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register cell* start = dest;

	if (len > maxlen)
	{
		len = maxlen;
		needtocheck = true;
	}

	maxlen = len;

	while (maxlen-- && *source)
	{
		*dest++ = *(unsigned char*)source++;
	}

	if (needtocheck && (start[len - 1] & 1 << 7))
	{
		len -= UTIL_CheckValidChar(start + len - 1);
	}

	start[len] = '\0';

	return len;
}

int set_amxstring_utf8_char(AMX *amx, cell amx_addr, const char *source, size_t sourcelen, size_t maxlen)
{
	return set_amxstring_utf8(amx, amx_addr, source, sourcelen, maxlen);
}

int set_amxstring_utf8_cell(AMX *amx, cell amx_addr, const cell *source, size_t sourcelen, size_t maxlen)
{
	return set_amxstring_utf8(amx, amx_addr, source, sourcelen, maxlen);
}

extern "C" size_t get_amxstring_r(AMX *amx, cell amx_addr, char *destination, int maxlen)
{
	register cell *source = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register char *dest = destination;
	char *start = dest;

	while (maxlen-- && *source)
		*dest++=(char)(*source++);

	*dest = '\0';

#if defined BINLOG_ENABLED
	if (g_binlog_level & 2)
	{
		CPluginMngr::CPlugin *pl = g_plugins.findPluginFast(amx);
		if (pl)
			g_BinLog.WriteOp(BinLog_GetString, pl->getId(), amx_addr, destination);
	}
#endif

	return dest - start;
}

char *get_amxbuffer(int id)
{
	static char buffer[4][MAX_BUFFER_LENGTH];
	return buffer[id];
}

char *get_amxstring(AMX *amx, cell amx_addr, int id, int& len)
{
	auto buffer = get_amxbuffer(id);
	len = get_amxstring_r(amx, amx_addr, buffer, MAX_BUFFER_LENGTH - 1);
	return buffer;
}

char *get_amxstring_null(AMX *amx, cell amx_addr, int id, int& len)
{
	if (get_amxaddr(amx, amx_addr) == g_plugins.findPluginFast(amx)->getNullStringOfs())
	{
		return nullptr;
	}

	return get_amxstring(amx, amx_addr, id, len);
}

cell *get_amxvector_null(AMX *amx, cell amx_addr)
{
	cell *addr = get_amxaddr(amx, amx_addr);
	if (addr == g_plugins.findPluginFast(amx)->getNullVectorOfs())
	{
		return nullptr;
	}

	return addr;
}

void copy_amxmemory(cell* dest, cell* src, int len)
{
	while (len--)
		*dest++=*src++;
}

char* parse_arg(char** line, int& state)
{
	static char arg[3072];
	char* dest = arg;
	state = 0;
	
	while (**line)
	{
		if (isspace(**line))
		{
			if (state == 1)
				break;
			else if (!state)
			{
				(*line)++;
				continue;
			}
		}
		else if (state != 2)
			state = 1;
		
		if (**line == '"')
		{
			(*line)++;
			
			if (state == 2)
				break;
			
			state = 2;
			continue;
		}
		
		*dest++ = *(*line)++;
	}
	
	*dest = '\0';
	return arg;
}

bool fastcellcmp(cell *a, cell *b, cell len)
{
	while (len--)
	{
		if (*a++ != *b++)
			return false;
	}

	return true;
}

static cell AMX_NATIVE_CALL replace(AMX *amx, cell *params) /* 4 param */
{
	cell *text = get_amxaddr(amx, params[1]);
	cell len = params[2];
	cell *what = get_amxaddr(amx, params[3]);
	cell *with = get_amxaddr(amx, params[4]);
	cell *textptr = text;

	int withLen = amxstring_len(with);
	int whatLen = amxstring_len(what);
	int textLen = amxstring_len(text);

	if (whatLen > textLen)
		return 0;

	if (whatLen < 1)
	{
		LogError(amx, AMX_ERR_NATIVE, "No search string specified.");
		return 0;
	}

	if (textLen - whatLen + withLen > len)
	{
		LogError(amx, AMX_ERR_NATIVE, "replace() buffer not big enough (%d>=%d)", (textLen - whatLen + withLen), len);
		return 0;
	}

	cell browsed = 0;
	while (*text && (browsed <= (textLen-whatLen)))
	{
		if (*text == *what)
		{
			if (fastcellcmp(text, what, whatLen))
			{
				cell *saveptr = text + whatLen;
				cell restlen = textLen - (browsed + whatLen);
				textptr = text + withLen;
				memmove(textptr, saveptr, (restlen + 1) * sizeof(cell));
				memcpy(text, with, withLen * sizeof(cell));
				return (textLen - whatLen + withLen);
			}
		}
		text++;
		browsed++;
	}
	
	return 0;
}

// native replace_string(text[], maxlength, const search[], const replace[], bool:caseSensitive = true);
static cell AMX_NATIVE_CALL replace_string(AMX *amx, cell *params)
{
	enum args { arg_count, arg_text, arg_maxlength, arg_search, arg_replace, arg_casesensitive };

	auto textLength    = 0;
	auto searchLength  = 0;
	auto replaceLength = 0;

	auto text    = get_amxstring(amx, params[arg_text]   , 0, textLength);
	auto search  = get_amxstring(amx, params[arg_search] , 1, searchLength);
	auto replace = get_amxstring(amx, params[arg_replace], 2, replaceLength);

	auto textMaxLength = params[arg_maxlength];
	auto caseSensitive = params[arg_casesensitive] != 0;

	if (!*search)
	{
		LogError(amx, AMX_ERR_NATIVE, "Cannot replace searches of empty strings.");
		return -1;
	}

	auto count = UTIL_ReplaceAll(text, textMaxLength + 1, search, searchLength, replace, replaceLength, caseSensitive); // + EOS

	set_amxstring(amx, params[arg_text], text, textMaxLength);

	return count;
}

// native replace_stringex(text[], maxlength, const search[], const replace[], searchLen = -1, replaceLen = -1, bool:caseSensitive = true);
static cell AMX_NATIVE_CALL replace_stringex(AMX *amx, cell *params)
{
	enum args { arg_count, arg_text, arg_maxlength, arg_search, arg_replace, arg_searchlen, arg_replacelen, arg_casesensitive };

	auto textLength    = 0;
	auto searchLength  = 0;
	auto replaceLength = 0;

	auto text    = get_amxstring(amx, params[arg_text]   , 0, textLength);
	auto search  = get_amxstring(amx, params[arg_search] , 1, searchLength);
	auto replace = get_amxstring(amx, params[arg_replace], 2, replaceLength);

	auto textMaxLength = params[arg_maxlength];
	auto caseSensitive = params[arg_casesensitive] != 0;

	if (params[arg_searchlen]  != -1) { searchLength  = params[arg_searchlen]; }
	if (params[arg_replacelen] != -1) { replaceLength = params[arg_replacelen]; }

	if (searchLength <= 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Cannot replace searches of empty strings.");
		return -1;
	}

	auto ptr = UTIL_ReplaceEx(text, textMaxLength + 1, search, searchLength, replace, replaceLength, caseSensitive); // + EOS

	if (!ptr)
	{
		return -1;
	}

	set_amxstring(amx, params[arg_text], text, textMaxLength);

	return ptr - text;
}

static cell AMX_NATIVE_CALL contain(AMX *amx, cell *params) /* 2 param */
{
	register cell *a = get_amxaddr(amx, params[2]);
	register cell *b = get_amxaddr(amx, params[1]);
	register cell *c = b;
	cell* str = b;
	cell* substr = a;
	
	while (*c)
	{
		if (*c == *a)
		{
			c++;
			if (!*++a)
				return b - str;
		} else {
			c = ++b;
			a = substr;
		}
	}
	
	return -1;
}

// native containi(const source[], const string[]);
static cell AMX_NATIVE_CALL containi(AMX *amx, cell *params)
{
	enum args { arg_count, arg_source, arg_search };

	auto sourceLength = 0;
	auto searchLength = 0;

	auto source = get_amxstring(amx, params[arg_source], 0, sourceLength);
	auto search = get_amxstring(amx, params[arg_search], 1, searchLength);

	if (sourceLength && searchLength)
	{
		auto sourceFolded = get_amxbuffer(2);
		auto searchFolded = get_amxbuffer(3);

		sourceLength = utf8casefold(source, sourceLength, sourceFolded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
		searchLength = utf8casefold(search, searchLength, searchFolded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

		sourceFolded[sourceLength] = '\0';
		searchFolded[searchLength] = '\0';

		auto result = strstr(sourceFolded, searchFolded);

		if (result)
		{
			return result - sourceFolded;
		}
	}

	return -1;
}

static cell AMX_NATIVE_CALL strtonum(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	return atoi(get_amxstring(amx, params[1], 0, iLen));
}

static cell AMX_NATIVE_CALL amx_strtol(AMX *amx, cell *params) /* 3 param */
{
	int len;
	int base = params[3];

	if (base != 0 && (base < 2 || base > 36))
		base = 0;

	char *pString = get_amxstring(amx, params[1], 0, len);
	cell *endPos = get_amxaddr(amx, params[2]);

	char *pEnd = NULL;
	long result = strtol(pString, &pEnd, base);

	*endPos = pEnd - pString;

	return result;
}

static cell AMX_NATIVE_CALL amx_strtof(AMX *amx, cell *params) /* 2 param */
{
	int len;
	char *pString = get_amxstring(amx, params[1], 0, len);
	cell *endPos = get_amxaddr(amx, params[2]);

	char *pEnd = NULL;
	float result = strtod(pString, &pEnd);

	*endPos = pEnd - pString;

	return amx_ftoc(result);
}

static cell AMX_NATIVE_CALL numtostr(AMX *amx, cell *params) /* 3 param */
{
	char szTemp[32];
	sprintf(szTemp, "%d", (int)params[1]);

	return set_amxstring(amx, params[2], szTemp, params[3]);
}

static cell AMX_NATIVE_CALL str_to_float(AMX *amx, cell *params)
{
	cell *str = get_amxaddr(amx, params[1]);

	bool neg = false;
	unsigned long part1 = 0;

	if (*str == '-')
	{
		neg = true;
		++str;
	}
	else if (*str == '+')
		++str;

	while (*str)
	{
		if (*str == '.')
		{
			++str;
			break;
		}
		
		if (*str < '0' || *str > '9')
		{
			REAL fl = neg ? -static_cast<REAL>(part1) : static_cast<REAL>(part1);
			return amx_ftoc(fl);
		}

		part1 *= 10;
		part1 += *str - '0';

		++str;
	}

	unsigned long part2 = 0;
	unsigned long div = 1;
	
	while (*str)
	{
		if (*str < '0' || *str > '9')
			break;

		part2 *= 10;
		part2 += *str - '0';
		div *= 10;
		++str;
	}

	REAL fl = static_cast<REAL>(part1) + (static_cast<REAL>(part2) / div);
	
	if (neg)
		fl = -fl;
	
	return amx_ftoc(fl);
}

static cell AMX_NATIVE_CALL float_to_str(AMX *amx, cell *params)
{
	char szTemp[32];
	sprintf(szTemp, "%f", amx_ctof(params[1]));
	
	return set_amxstring(amx, params[2], szTemp, params[3]);
}

static cell AMX_NATIVE_CALL add(AMX *amx, cell *params) /* 4 param */
{
	cell *src = get_amxaddr(amx, params[3]);
	cell *dest = get_amxaddr(amx, params[1]);
	cell *start = dest;
	int c = params[2], d = params[4];
	
	while (*dest && c--)
		++dest;
	
	if (d)
	{
		while (c-- && d-- && *src)
			*dest++ =* src++;
		*dest = 0;
		
		return (dest - start);
	}

	while (c-- && *src)
		*dest++ =* src++;
	*dest = 0;
	
	return (dest-start);
}

static cell AMX_NATIVE_CALL copy(AMX *amx, cell *params) /* 4 param */
{
	cell *src = get_amxaddr(amx, params[3]);
	int c = params[2];

	cell *dest = get_amxaddr(amx, params[1]);
	cell *start = dest;

	while (c-- && *src)
	{
		*dest++ = *src++;
	}
	*dest = '\0';

	return (dest - start);
}

static cell AMX_NATIVE_CALL copyc(AMX *amx, cell *params) /* 4 param */
{
	cell *src = get_amxaddr(amx, params[3]);
	cell *dest = get_amxaddr(amx, params[1]);
	cell *start = dest;
	int c = params[2];
	cell ch = params[4];
	
	while (c-- && *src && *src != ch)
		*dest++ =* src++;
	*dest = 0;
	
	return (dest - start);
}

static cell AMX_NATIVE_CALL setc(AMX *amx, cell *params) /* 4 param */
{
	cell *src = get_amxaddr(amx, params[1]);
	int c = params[2];
	cell ch = params[3];
	
	while (c--)
		*src++ = ch;
	
	return 1;
}

static cell AMX_NATIVE_CALL equal(AMX *amx, cell *params) /* 3 param */
{
	cell *a = get_amxaddr(amx, params[1]);
	cell *b = get_amxaddr(amx, params[2]);
	int c = params[3];
	
	if (c)
	{
		while (--c && *a && (*a == *b))
			++a, ++b;
		return (*a-*b) ? 0 : 1;
	}
	
	int ret;
	
	while (!(ret = *a - *b) && *b)
		++a, ++b;
	
	return ret ? 0 : 1;
}

// native equali(const a[], const b[], c = 0);
static cell AMX_NATIVE_CALL equali(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string1, arg_string2, arg_numbytes };

	auto string1Length = 0;
	auto string2Length = 0;

	auto string1 = get_amxstring(amx, params[arg_string1], 0, string1Length);
	auto string2 = get_amxstring(amx, params[arg_string2], 1, string2Length);

	auto string1Folded = get_amxbuffer(2);
	auto string2Folded = get_amxbuffer(3);

	string1Length = utf8casefold(string1, string1Length, string1Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
	string2Length = utf8casefold(string2, string2Length, string2Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

	string1Folded[string1Length] = '\0';
	string2Folded[string2Length] = '\0';

	if (params[arg_numbytes] > 0)
	{
		return static_cast<cell>(strncmp(string1Folded, string2Folded, params[arg_numbytes]) == 0);
	}
	else
	{
		return static_cast<cell>(strcmp(string1Folded, string2Folded) == 0);
	}
}

static cell g_cpbuf[4096];

static cell AMX_NATIVE_CALL formatex(AMX *amx, cell *params)
{
	cell *buf = get_amxaddr(amx, params[1]);
	size_t maxlen = static_cast<size_t>(params[2]);
    cell *fmt = get_amxaddr(amx, params[3]);
	int param = 4;
	size_t total = atcprintf(buf, maxlen, fmt, amx, params, &param);
	return static_cast<cell>(total);
}

static cell AMX_NATIVE_CALL format(AMX *amx, cell *params) /* 3 param */
{
	cell *buf = get_amxaddr(amx, params[1]);
	cell *fmt = get_amxaddr(amx, params[3]);
	size_t maxlen = params[2];
	/** 
	 * SPECIAL CASE - check if the buffers overlap.
	 *  some users, for whatever reason, do things like:
	 *  format(buf, 255, buf....
	 *  this is considered "deprecated" but we have to support it.
	 * we do this by checking to see if reading from buf will overlap
	 */
	cell addr_start = params[1];
	cell addr_end = params[1] + maxlen * sizeof(cell);
	cell max = params[0] / sizeof(cell);
	bool copy = false;
	for (cell i = 3; i <= max; i++)
	{
		//does this clip the bounds?!?!? WELL, DOES IT!?!?! i am a loud dog
		if (params[i] >= addr_start && params[i] <= addr_end)
		{
			copy = true;
			break;
		}
	}
	if (copy)
		buf = g_cpbuf;
	int param = 4;
	size_t total = 0;

	total = atcprintf(buf, maxlen, fmt, amx, params, &param);

	if (copy)
	{
		/* copy back */
		cell *old = get_amxaddr(amx, params[1]);
		memcpy(old, g_cpbuf, (total+1) * sizeof(cell));
	}
	return total;
}

static cell AMX_NATIVE_CALL parse(AMX *amx, cell *params) /* 3 param */
{
	int inum = *params / sizeof(cell), iarg = 2, c;
	char* arg, *parse = get_amxstring(amx, params[1], 0, c);
	cell *cptr;
	int state;
	
	while (*parse)
	{
		arg = parse_arg(&parse,state);
		
		if (state)
		{
			if (inum <= iarg)
				return ((iarg - 2)>>1);
			
			cptr = get_amxaddr(amx, params[iarg++]);
			c = *get_amxaddr(amx, params[iarg++]);
			
			while (c-- && *arg)
				*cptr++ = (unsigned char)*arg++;
			*cptr = 0;
		}
	}

	return ((iarg - 2)>>1);
}

static cell AMX_NATIVE_CALL strtolower(AMX *amx, cell *params) /* 1 param */
{
	cell *cptr = get_amxaddr(amx, params[1]);
	cell *begin = cptr;
	
	while (*cptr)
	{
		*cptr = tolower(*cptr);
		cptr++;
	}

	return cptr - begin;
}

// native mb_strtolower(source[], maxlength = 0);
static cell AMX_NATIVE_CALL mb_strtolower(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string, arg_maxlength };

	auto sourceLength = 0;
	auto source = get_amxstring(amx, params[arg_string], 0, sourceLength);

	auto outputMaxLength = params[arg_maxlength];

	if (outputMaxLength <= 0)
	{
		outputMaxLength = sourceLength;
	}

	auto output = get_amxbuffer(1);
	auto outputLength = utf8tolower(source, sourceLength, output, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
	
	output[outputLength] = '\0';

	return set_amxstring_utf8(amx, params[arg_string], output, outputLength, outputMaxLength);
}

static cell AMX_NATIVE_CALL strtoupper(AMX *amx, cell *params) /* 1 param */
{
	cell *cptr = get_amxaddr(amx, params[1]);
	cell *begin = cptr;
	
	while (*cptr)
	{
		*cptr = toupper(*cptr);
		cptr++;
	}
	
	return cptr - begin;
}

// native mb_strtoupper(source[], maxlength = 0);
static cell AMX_NATIVE_CALL mb_strtoupper(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string, arg_maxlength };

	auto sourceLength = 0;
	auto source = get_amxstring(amx, params[arg_string], 0, sourceLength);
	
	auto outputMaxLength = params[arg_maxlength];

	if (outputMaxLength <= 0)
	{
		outputMaxLength = sourceLength;
	}

	auto output = get_amxbuffer(1);
	auto outputLength = utf8toupper(source, sourceLength, output, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

	output[outputLength] = '\0';

	return set_amxstring_utf8(amx, params[arg_string], output, outputLength, outputMaxLength);
}

int fo_numargs(AMX *amx)
{
	unsigned char *data = amx->base + (int)((AMX_HEADER *)amx->base)->dat;
	cell bytes= *(cell *)(data + (int)amx->frm + 2 * sizeof(cell));
	
	return (int)(bytes / sizeof(cell));
}

int fo_getargnum(AMX *amx, int pos)
{
	unsigned char *data = amx->base + (int)((AMX_HEADER *)amx->base)->dat;
	cell value = *(cell *)(data + (int)amx->frm + (pos + 3) * sizeof(cell));
	
	return *(cell *)(data + (int)value);
}

float fo_getargfloat(AMX *amx, int pos)
{
	unsigned char *data = amx->base + (int)((AMX_HEADER *)amx->base)->dat;
	cell value = *(cell *)(data + (int)amx->frm + (pos + 3) * sizeof(cell));
	cell number = *(cell *)(data + (int)value);
	
	return *(REAL *)((void *)&number);
}

char* fo_getargstr(AMX *amx, int swap, int pos)
{
	unsigned char *data = amx->base + (int)((AMX_HEADER *)amx->base)->dat;
	cell src_value= *(cell *)(data + (int)amx->frm + (pos + 3) * sizeof(cell));
	cell value;
	static char buffer[2][3072];
	char* b = buffer[swap];
	int a = 0;
	
	do
	{
		value = src_value + a++ * sizeof(cell);
		value = *(cell *)(data + (int)value);
		*b++ = static_cast<char>(value);
	} while (value);

	return buffer[swap];
}

char* format_arguments(AMX *amx, int parm, int& len)
{
	static char buffer[2][3072];
	static char format[16];
	char *ptr, *arg, *dest = *buffer;
	char *src = fo_getargstr(amx, 0, parm++);
	int numparam = fo_numargs(amx);

	while (*src)
	{
		if (*src == '%' && *(src + 1))
		{
			ptr = format;
			*ptr++ = *src++;
			
			if (*src == '%')
			{
				*dest++ = *src++;
				continue;
			}
			
			while (!isalpha(*ptr++ = *src++));
			
			*ptr='\0';
			if (numparam < parm) continue;
			arg = buffer[1];
			
			switch (*(ptr - 1))
			{
				case 's': sprintf(arg, format, fo_getargstr(amx, 1, parm++)); break;
				case 'f': case 'g': sprintf(arg, format, fo_getargfloat(amx, parm++)); break;
				default: sprintf(arg, format, fo_getargnum(amx, parm++));
			}
			
			while (*arg) *dest++ = *arg++;
			continue;
		}
		*dest++ = *src++;
	}

	*dest = '\0';
	len = dest - *buffer;
	
	return *buffer;
}

//added by BAILOPAN for jtp10181
//takes a string and breaks it into a 1st param and rest params
//different from strbreak because it's more crafted for control
static cell AMX_NATIVE_CALL amx_strtok(AMX *amx, cell *params)
{
	int left_pos = 0;
	int right_pos = 0;
	unsigned int i = 0;
	bool done_flag = false;
	int len = 0;

	//string[]
	char *string = get_amxstring(amx, params[1], 0, len);
	//left[]
	char *left = new char[len + 1];
	//right[]
	char *right = new char[len + 1];
	int leftMax = params[3];
	int rightMax = params[5];
	//token
	char token = static_cast<char>(params[6]);
	//trim
	int trim = params[7];
	
	for (i = 0; i < (unsigned int)len; i++)
	{
		if (trim && !done_flag)
		{
			if (isspace(string[i]))
			{
				while (isspace(string[++i]));
				done_flag = true;
			}
		}

		if (!done_flag && string[i] == token)
		{
			done_flag = true;
			i++;
		}

		if (done_flag)
		{
			right[right_pos++] = string[i];
		} else {
			left[left_pos++] = string[i];
		}
	}

	right[right_pos] = 0;
	left[left_pos] = 0;
	set_amxstring_utf8(amx, params[2], left, strlen(left), leftMax);
	set_amxstring_utf8(amx, params[4], right, strlen(right), rightMax);
	delete [] left;
	delete [] right;
	
	return 1;
}

// Same as amx_strtok but fixes and expands trim, returns token pos if token was found, -1 otherwise
static cell AMX_NATIVE_CALL amx_strtok2(AMX *amx, cell *params)
{
	int left_pos = 0, right_pos = 0, len, pos = -1;
	unsigned int i = 0;

	char *string = get_amxstring(amx, params[1], 0, len);
	char *left = new char[len + 1], *right = new char[len + 1];
	int left_max = params[3], right_max = params[5];
	char token = static_cast<char>(params[6]);
	
	/*	Trim flags:
			1 - ltrim left
			2 - rtrim left
			4 - ltrim right
			8 - rtrim right
	*/
	int trim = params[7];

	// ltrim left
	if (trim & 1 && isspace(string[i]))
	{
		while (isspace(string[++i]));
	}

	for (; i < (unsigned int) len; ++i)
	{
		if (string[i] == token)
		{
			pos = i;
			++i;
			break;
		}

		left[left_pos++] = string[i];
	}

	// rtrim left
	if (trim & 2 && left_pos && isspace(left[left_pos - 1]))
	{
		while (--left_pos >= 0 && isspace(left[left_pos]));
		
		++left_pos;
	}

	// ltrim right
	if (trim & 4 && isspace(string[i]))
	{
		while (isspace(string[++i]));
	}

	for (; i < (unsigned int) len; ++i)
	{
		right[right_pos++] = string[i];	
	}

	// rtrim right
	if (trim & 8 && right_pos && isspace(right[right_pos - 1]))
	{
		while (--right_pos >= 0 && isspace(right[right_pos]));

		++right_pos;
	}

	right[right_pos] = 0;
	left[left_pos] = 0;

	set_amxstring_utf8(amx, params[2], left, strlen(left), left_max);
	set_amxstring_utf8(amx, params[4], right, strlen(right), right_max);

	delete [] left;
	delete [] right;

	return pos;
}

// native argparse(const text[], pos, buffer, maxlen);
static cell AMX_NATIVE_CALL argparse(AMX *amx, cell *params)
{
	int temp;
	const char *input = get_amxstring(amx, params[1], 0, temp);
	size_t input_len = size_t(temp);
	size_t start_pos = size_t(params[2]);

	cell *buffer = get_amxaddr(amx, params[3]);
	size_t buflen = size_t(params[4]);

	// Strip all left-hand whitespace.
	size_t i = start_pos;
	while (i < input_len && isspace(input[i]))
		i++;

	if (i >= input_len) {
		*buffer = '\0';
		return -1;
	}

	cell *bufpos = buffer;

	bool in_quote = false;
	for (; i < input_len; i++) {
		// Ignore quotes, except as an indicator as to whether to stop
		// at a space.
		if (input[i] == '"') {
			in_quote = !in_quote;
			continue;
		}

		// If not in quotes, and we see a space, stop.
		if (isspace(input[i]) && !in_quote)
			break;

		if (size_t(bufpos - buffer) < buflen)
			*bufpos++ = (unsigned char)input[i];
	}

	*bufpos = '\0';
	return i;
}

//added by BAILOPAN :(
//Takes a string and breaks it into a 1st param and rest params
//strbreak(String[], First[], FirstLen, Rest[], RestLen)
static cell AMX_NATIVE_CALL strbreak(AMX *amx, cell *params)	/* 5 param */
{
	int _len;
	bool in_quote = false;
	bool had_quotes = false;
	size_t i = 0;
	size_t beg = 0;

	char *string = get_amxstring(amx, params[1], 0, _len);
	cell *right = get_amxaddr(amx, params[4]);
	int LeftMax = params[3];
	int RightMax = params[5];

	size_t len = (size_t)_len;

	while (isspace(string[i]) && i<len)
		i++;
	beg = i;
	for (; i<len; i++)
	{
		if (string[i] == '"' && !in_quote)
		{
			in_quote = (had_quotes = true);
		} else if (string[i] == '"' && in_quote) {
			in_quote = false;
			if (i == len-1)
				goto do_copy;
		} else {
			if (isspace(string[i]) && !in_quote)
			{
do_copy:
				size_t pos = i;
				while (isspace(string[i]))
					i++;
				const char *start = had_quotes ? &(string[beg+1]) : &(string[beg]);
				size_t _end = had_quotes ? (i==len-1 ? 1 : 2) : 0;
				size_t end = (pos - _end > (size_t)LeftMax) ? (size_t)LeftMax : pos - _end;
				
				// If there is anything to copy, make sure we copy min(maxlen, slicelen).
				size_t copylen = end >= beg
				                 ? ((end - beg > size_t(LeftMax))
				                    ? size_t(LeftMax)
				                    : end - beg
				                   )
				                 : 0;
				set_amxstring_utf8(amx, params[2], start, strlen(start), copylen);

				end = (len-i+1 > (size_t)RightMax) ? (size_t)RightMax : len-i+1;
				if (end)
				{
					start = &(string[i]);
					while (end--)
						*right++ = (unsigned char)*start++;
				}
				*right = '\0';
				return 1;
			}
		}
	}

	//if we got here, there was nothing to break
	set_amxstring_utf8(amx, params[2], &(string[beg]), strlen(&(string[beg])), LeftMax);
	if (RightMax)
		*right = '\0';

	return 1;
}

static cell AMX_NATIVE_CALL split_string(AMX *amx, cell *params)
{
	int textLen, splitLen;
	char *text = get_amxstring(amx, params[1], 0, textLen);
	const char *split = get_amxstring(amx, params[2], 1, splitLen);

	if (splitLen > textLen)
	{
		return -1;
	}

	int maxLen = params[4];

	/**
	* Note that it's <= ... you could also just add 1,
	* but this is a bit nicer
	*/
	for (int i = 0; i <= textLen - splitLen; i++)
	{
		if (strncmp(&text[i], split, splitLen) == 0)
		{
			/* Split hereeeee */
			if (i > maxLen)
			{
				set_amxstring_utf8(amx, params[3], text, textLen, maxLen);
			}
			else
			{
				set_amxstring_utf8(amx, params[3], text, textLen, i);
			}
			return i + splitLen;
		}
	}

	return -1;
}

static cell AMX_NATIVE_CALL format_args(AMX *amx, cell *params)
{
	int len;
	int pos = params[3];
	
	if (pos < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Pos has to be a positive number");
		return 0;
	}

	char* string = format_arguments(amx, pos, len); // indexed from 0
	
	return set_amxstring_utf8(amx, params[1], string, len, params[2]);
}

static cell AMX_NATIVE_CALL is_digit(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return isdigit(chr);
}

static cell AMX_NATIVE_CALL is_alnum(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return isalnum(chr);
}

static cell AMX_NATIVE_CALL is_space(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return isspace(chr);
}

static cell AMX_NATIVE_CALL is_alpha(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return isalpha(chr);
}

static cell AMX_NATIVE_CALL is_char_upper(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return isupper(chr);
}

static cell AMX_NATIVE_CALL is_char_lower(AMX *amx, cell *params)
{
	char chr = params[1];

	if (UTIL_GetUTF8CharBytes(&chr) != 1)
	{
		return 0;
	}

	return islower(chr);
}

static cell AMX_NATIVE_CALL is_char_mb(AMX *amx, cell *params)
{
	char chr = params[1];

	unsigned int bytes = UTIL_GetUTF8CharBytes(&chr);
	if (bytes == 1)
	{
		return 0;
	}

	return bytes;
}

static cell AMX_NATIVE_CALL get_char_bytes(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);

	return UTIL_GetUTF8CharBytes(str);
};

static cell AMX_NATIVE_CALL amx_ucfirst(AMX *amx, cell *params)
{
	cell *str = get_amxaddr(amx, params[1]);
	
	if (!isalpha((char)str[0]) || !(str[0] & (1<<5)))
		return 0;
	str[0] &= ~(1<<5);
	
	return 1;
}

// native mb_ucfirst(string[], maxlength = 0);
static cell AMX_NATIVE_CALL mb_ucfirst(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string, arg_maxlength };

	auto sourceLength = 0;
	auto source = get_amxstring(amx, params[arg_string], 0, sourceLength);

	auto outputMaxLength = params[arg_maxlength];

	if (outputMaxLength <= 0)
	{
		outputMaxLength = sourceLength;
	}

	// Retrieves the first character length in bytes.
	auto firstChLength = utf8seek(source, sourceLength, source, 1, SEEK_CUR) - source;

	if (firstChLength)
	{
		char output[8] = {};
		auto outputLength = utf8toupper(source, firstChLength, output, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

		// The converted character is either larger or smaller in bytes.
		if (firstChLength != outputLength)
		{
			// Calculates the new string length and makes sure we don't go over the buffer size (fairly unlikely).
			sourceLength = ke::Min<int>(sourceLength + (outputLength - firstChLength), outputMaxLength);

			// Move data forward or backward minus the first character (whathever its size).
			memmove(source + outputLength, source + firstChLength, (sourceLength - outputLength) * sizeof(char));	
		}

		// Copy the new character at the start of the string.
		memcpy(source, output, outputLength);
	}

	return set_amxstring_utf8(amx, params[arg_string], source, sourceLength, outputMaxLength);
}

static cell AMX_NATIVE_CALL amx_strlen(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);

	return strlen(str);
}

static cell AMX_NATIVE_CALL amx_trim(AMX *amx, cell *params)
{
	int len, newlen;
	char *str = get_amxstring(amx, params[1], 0, len);

	UTIL_TrimLeft(str);
	UTIL_TrimRight(str);

	newlen = strlen(str);
	len -= newlen;

	set_amxstring(amx, params[1], str, newlen);

	return len;
}

static cell AMX_NATIVE_CALL n_strcat(AMX *amx, cell *params)
{
	cell *cdest, *csrc;

	cdest = get_amxaddr(amx, params[1]);
	csrc = get_amxaddr(amx, params[2]);
	int num = params[3];
	
	while (*cdest && num)
	{
		cdest++;
		num--;
	}
	
	if (!num)
		return 0;
	
	while (*csrc && num)
	{
		*cdest++ = *csrc++;
		num--;
	}
	*cdest = 0;

	return params[3] - num;
}

// native strcmp(const string1[], const string2[], bool:ignorecase = false);
static cell AMX_NATIVE_CALL n_strcmp(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string1, arg_string2, arg_ignorecase };

	auto string1Length = 0;
	auto string2Length = 0;

	auto string1 = get_amxstring(amx, params[arg_string1], 0, string1Length);
	auto string2 = get_amxstring(amx, params[arg_string2], 1, string2Length);

	if (params[arg_ignorecase] != 0)
	{
		auto string1Folded = get_amxbuffer(2);
		auto string2Folded = get_amxbuffer(3);

		string1Length = utf8casefold(string1, string1Length, string1Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
		string2Length = utf8casefold(string2, string2Length, string2Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

		string1Folded[string1Length] = '\0';
		string2Folded[string2Length] = '\0';

		string1 = string1Folded;
		string2 = string2Folded;
	}

	return strcmp(string1, string2);
}

// native strncmp(const string1[], const string2[], num, bool:ignorecase = false);
static cell AMX_NATIVE_CALL n_strncmp(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string1, arg_string2, arg_numbytes, arg_ignorecase };

	auto string1Length = 0;
	auto string2Length = 0;

	auto string1 = get_amxstring(amx, params[arg_string1], 0, string1Length);
	auto string2 = get_amxstring(amx, params[arg_string2], 1, string2Length);

	if (params[arg_ignorecase] != 0)
	{
		auto string1Folded = get_amxbuffer(2);
		auto string2Folded = get_amxbuffer(3);

		string1Length = utf8casefold(string1, string1Length, string1Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
		string2Length = utf8casefold(string2, string2Length, string2Folded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

		string1Folded[string1Length] = '\0';
		string2Folded[string2Length] = '\0';

		string1 = string1Folded;
		string2 = string2Folded;
	}

	return strncmp(string1, string2, params[arg_numbytes]);
}

// native strfind(const string[], const sub[], bool:ignorecase = false, pos = 0);
static cell AMX_NATIVE_CALL n_strfind(AMX *amx, cell *params)
{
	enum args { arg_count, arg_source, arg_search, arg_ignorecase, arg_startpos };

	auto sourceLength = 0;
	auto searchLength = 0;

	auto source = get_amxstring(amx, params[arg_source], 0, sourceLength);
	auto search = get_amxstring(amx, params[arg_search], 1, searchLength);

	if (params[arg_ignorecase] != 0)
	{
		auto sourceFolded = get_amxbuffer(2);
		auto searchFolded = get_amxbuffer(3);

		sourceLength = utf8casefold(source, sourceLength, sourceFolded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
		searchLength = utf8casefold(search, searchLength, searchFolded, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

		sourceFolded[sourceLength] = '\0';
		searchFolded[searchLength] = '\0';

		source = sourceFolded;
		search = searchFolded;
	}

	auto position = params[arg_startpos];
	
	if (position < 0 || position > sourceLength)
	{
		return -1;
	}

	auto find = strstr(source + position, search);

	if (!find)
	{
		return -1;
	}

	return (find - source);
}

static cell AMX_NATIVE_CALL vformat(AMX *amx, cell *params)
{
	int vargPos = static_cast<int>(params[4]);

	/** get the parent parameter array */
	AMX_HEADER *hdr = (AMX_HEADER *)amx->base;
	cell *local_params = (cell *)(
		(char *)amx->base + (cell)hdr->dat +
		(cell)amx->frm + (2 * sizeof(cell))
		);

	cell max = local_params[0] / sizeof(cell);
	if (vargPos > (int)max + 1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid vararg parameter passed: %d", vargPos);
		return 0;
	}
	
	/**
	 * check for bounds clipping
	 */
	cell addr_start = params[1];
	cell addr_end = addr_start + params[2];
	bool copy = false;
	for (int i = vargPos; i <= max; i++)
	{
		//does this clip the bounds?
		if ( (local_params[i] >= addr_start)
			&& (local_params[i] <= addr_end) )
		{
			copy = true;
			break;
		}
	}

	/* get destination info */
	cell *fmt = get_amxaddr(amx, params[3]);
	cell *realdest = get_amxaddr(amx, params[1]);
	size_t maxlen = static_cast<size_t>(params[2]);
	cell *dest = realdest;

	/* if this is necessary... */
	static cell cpbuf[4096];
	if (copy)
		dest = cpbuf;

	/* perform format */
	size_t total = atcprintf(dest, maxlen, fmt, amx, local_params, &vargPos);

	/* copy back */
	if (copy)
	{
		memcpy(realdest, dest, (total+1) * sizeof(cell));
	}

	return total;
}


#define MAX_FMT_LENGTH 256

// native [MAX_FMT_LENGTH] fmt(const fmt[], any:...)
static cell AMX_NATIVE_CALL fmt(AMX *amx, cell *params)
{
	int length;
	const char *string = format_amxstring(amx, params, 1, length);

	size_t num_params = *params / sizeof(cell);

	set_amxstring_utf8_char(amx, params[num_params + 1], string, length, MAX_FMT_LENGTH - 1);

	return 1;
};

// native mb_strtotitle(source[], maxlength = 0);
static cell AMX_NATIVE_CALL mb_strtotitle(AMX *amx, cell *params)
{
	enum args { arg_count, arg_string, arg_maxlength };

	auto sourceLength = 0;
	auto source = get_amxstring(amx, params[arg_string], 0, sourceLength);

	auto outputMaxLength = params[arg_maxlength];

	if (outputMaxLength <= 0)
	{
		outputMaxLength = sourceLength;
	}

	auto output = get_amxbuffer(1);
	auto outputLength = utf8totitle(source, sourceLength, output, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

	output[outputLength] = '\0';

	return set_amxstring_utf8(amx, params[arg_string], output, outputLength, outputMaxLength);
}

// native bool:is_string_category(const input[], input_size, flags, &output_size = 0);
static cell AMX_NATIVE_CALL is_string_category(AMX *amx, cell *params)
{
	enum args { arg_count, arg_input, arg_inputsize, arg_flags, arg_outputsize };

	auto inputLength = 0;
	auto input = get_amxstring(amx, params[arg_input], 0, inputLength);

	auto inputMaxLength = ke::Min(params[arg_inputsize], inputLength);
	auto outputSize = get_amxaddr(amx, params[arg_outputsize]);

	// User wants to check only one character whatever its size.
	if (inputMaxLength <= 1)
	{
		// Gets the character length.
		inputMaxLength = utf8seek(input, inputLength, input, 1, SEEK_CUR) - input;

		// Truncated character.
		if (inputMaxLength > inputLength)
		{
			*outputSize = 0;
			return FALSE;
		}
	}

	// Checks input with the given flags.
	*outputSize = utf8iscategory(input, inputMaxLength, params[arg_flags]);

	// If function consumed input, then it's a success.
	return static_cast<cell>(*outputSize == inputMaxLength);
}

AMX_NATIVE_INFO string_Natives[] =
{
	{"add",				add},
	{"contain",			contain},
	{"containi",		containi},
	{"copy",			copy},
	{"copyc",			copyc},
	{"equal",			equal},
	{"equali",			equali},
	{"fmt",				fmt},
	{"format",			format},
	{"formatex",		formatex},
	{"format_args",		format_args},
	{"isdigit",			is_digit},
	{"isalnum",			is_alnum},
	{"isspace",			is_space},
	{"isalpha",			is_alpha},
	{"is_char_upper",	is_char_upper},
	{"is_char_lower",	is_char_lower},
	{"is_char_mb",		is_char_mb},
	{"is_string_category", is_string_category },
	{"get_char_bytes",	get_char_bytes},
	{"mb_strtotitle",	mb_strtotitle},
	{"mb_strtolower",	mb_strtolower},
	{"mb_strtoupper",	mb_strtoupper},
	{"mb_ucfirst",		mb_ucfirst},
	{"num_to_str",		numtostr},
	{"numtostr",		numtostr},
	{"parse",			parse},
	{"replace",			replace},
	{"replace_string",	replace_string},
	{"replace_stringex",replace_stringex},
	{"setc",			setc},
	{"strbreak",		strbreak},
	{"argparse",		argparse},
	{"split_string",	split_string},
	{"strtolower",		strtolower},
	{"strtoupper",		strtoupper},
	{"str_to_num",		strtonum},
	{"strtonum",		strtonum},
	{"strtol",			amx_strtol},
	{"strtof",			amx_strtof},
	{"trim",			amx_trim},
	{"ucfirst",			amx_ucfirst},
	{"strtok",			amx_strtok},
	{"strtok2",			amx_strtok2},
	{"strlen",			amx_strlen},
	{"strcat",			n_strcat},
	{"strfind",			n_strfind},
	{"strcmp",			n_strcmp},
	{"strncmp",			n_strncmp},
	{"str_to_float",	str_to_float},
	{"float_to_str",	float_to_str},
	{"vformat",			vformat},
	{NULL,				NULL}
};
