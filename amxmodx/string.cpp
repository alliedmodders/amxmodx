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

#include <ctype.h>
#include "amxmodx.h"
#include "format.h"

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
	return g_langMngr.FormatAmxString(amx, params, parm, len);
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

int set_amxstring(AMX *amx, cell amx_addr, const char *source, int max)
{
	register cell* dest = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register cell* start = dest;
	
	while (max-- && *source)
		*dest++ = (cell)*source++;
	
	*dest = 0;
	
	return dest - start;
}

extern "C" size_t get_amxstring_r(AMX *amx, cell amx_addr, char *destination, int maxlen)
{
	register cell *source = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register char *dest = destination;
	char *start = dest;
	
	while (maxlen-- && *source)
		*dest++=(char)(*source++);

	*dest = '\0';

	return dest - start;
}

char* get_amxstring(AMX *amx, cell amx_addr, int id, int& len)
{
	static char buffor[4][3072];
	register cell* source = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
	register char* dest = buffor[id];
	char* start = dest;
	
	while ((*dest++=(char)(*source++)));

	len = --dest - start;
	
	return start;
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

static cell AMX_NATIVE_CALL containi(AMX *amx, cell *params) /* 2 param */
{
	register cell *a = get_amxaddr(amx, params[2]);
	register cell *b = get_amxaddr(amx, params[1]);
	register cell *c = b;
	cell* str = b;
	cell* substr = a;
	
	while (*c)
	{
		if (tolower(*c) == tolower(*a))
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

static cell AMX_NATIVE_CALL strtonum(AMX *amx, cell *params) /* 1 param */
{
	int iLen;
	return atoi(get_amxstring(amx, params[1], 0, iLen));
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
	cell *dest = get_amxaddr(amx, params[1]);
	cell *start = dest;
	int c = params[2];
	
	while (c-- && *src)
		*dest++ =* src++;
	*dest = 0;
	
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

static cell AMX_NATIVE_CALL equali(AMX *amx, cell *params) /* 3 param */
{
	cell *a = get_amxaddr(amx, params[1]);
	cell *b = get_amxaddr(amx, params[2]);
	int f, l, c = params[3];
	
	if (c)
	{
		do
		{
			f = tolower(*a++);
			l = tolower(*b++);
		} while (--c && l && f && f == l);
		
		return (f - l) ? 0 : 1;
	}

	do
	{
		f = tolower(*a++);
		l = tolower(*b++);
	} while (f && f == l);
	
	return (f - l) ? 0 : 1;
}

static cell AMX_NATIVE_CALL format(AMX *amx, cell *params) /* 3 param */
{
	//int len;
	//return set_amxstring(amx, params[1], format_amxstring(amx, params, 3, len), params[2]);
	cell *buf = get_amxaddr(amx, params[1]);
	cell *fmt = get_amxaddr(amx, params[3]);
	size_t maxlen = params[2];
	int param = 4;
	size_t total = atcprintf(buf, maxlen, fmt, amx, params, &param);
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
				*cptr++ = (cell)*arg++;
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
	set_amxstring(amx, params[2], left, leftMax);
	set_amxstring(amx, params[4], right, rightMax);
	delete [] left;
	delete [] right;
	
	return 1;
}

//added by BAILOPAN
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
	cell *left = get_amxaddr(amx, params[2]);
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
				size_t end = (pos - _end > LeftMax) ? LeftMax : pos - _end;
				size_t to_go = end-beg;
				if (end && to_go)
				{
					while (to_go--)
						*left++ = (cell)*start++;
				}
				*left = '\0';
				end = (len-i+1 > RightMax) ? RightMax : len-i+1;
                if (end)
				{
					start = &(string[i]);
					while (end--)
						*right++ = (cell)*start++;
				}
				*right = '\0';
				return 1;
			}
		}
	}

	//if we got here, there was nothing to break
	set_amxstring(amx, params[2], &(string[beg]), LeftMax);
	if (RightMax)
		*right = '\0';

	return 1;
}

static cell AMX_NATIVE_CALL format_args(AMX *amx, cell *params)
{
	int len;
	int pos = params[3];
	
	if (pos < 0)
	{
		amx_RaiseError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	char* string = format_arguments(amx, pos, len); // indexed from 0
	
	return set_amxstring(amx, params[1], string, params[2]);
}

static cell AMX_NATIVE_CALL is_digit(AMX *amx, cell *params)
{
	return isdigit(params[1]);
}

static cell AMX_NATIVE_CALL is_alnum(AMX *amx, cell *params)
{
	return isalnum(params[1]);
}

static cell AMX_NATIVE_CALL is_space(AMX *amx, cell *params)
{
	return isspace(params[1]);
}

static cell AMX_NATIVE_CALL is_alpha(AMX *amx, cell *params)
{
	return isalpha(params[1]);
}

static cell AMX_NATIVE_CALL amx_ucfirst(AMX *amx, cell *params)
{
	int len = 0;
	cell *str = get_amxaddr(amx, params[1]);
	
	if (!isalpha((char)str[0]) || !(str[0] & (1<<5)))
		return 0;
	str[0] &= ~(1<<5);
	
	return 1;
}

static cell AMX_NATIVE_CALL amx_strlen(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);

	return strlen(str);
}

static cell AMX_NATIVE_CALL amx_trim(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);

	String toTrim;

	toTrim.assign(str);
	toTrim.trim();

	len -= toTrim.size();

	set_amxstring(amx, params[1], toTrim.c_str(), toTrim.size());

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

static cell AMX_NATIVE_CALL n_strcmp(AMX *amx, cell *params)
{
	int len;
	char *str1 = get_amxstring(amx, params[1], 0, len);
	char *str2 = get_amxstring(amx, params[2], 1, len);

	if (params[3])
		return stricmp(str1, str2);
	else
		return strcmp(str1, str2);
}

static cell AMX_NATIVE_CALL n_strfind(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	int sublen;
	char *sub = get_amxstring(amx, params[2], 1, sublen);

	bool found = false;
	bool igcase = params[3] ? true : false;
	
	if (igcase)
	{
		for (int i = 0; i < len; i++)
		{
			if (str[i] & (1<<5))
				str[i] &= ~(1<<5);
		}
		for (int i = 0; i < sublen; i++)
		{
			if (str[i] & (1<<5))
				str[i] &= ~(1<<5);			
		}
	}

	if (params[4] > len)
		return -1;

	char *pos = &(str[params[4]]);
	char *find = strstr(str, sub);

	if (!find)
		return -1;

	return (find - str);
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
	{"format",			format},
	{"format_args",		format_args},
	{"isdigit",			is_digit},
	{"isalnum",			is_alnum},
	{"isspace",			is_space},
	{"isalpha",			is_alpha},
	{"num_to_str",		numtostr},
	{"numtostr",		numtostr},
	{"parse",			parse},
	{"replace",			replace},
	{"setc",			setc},
	{"strbreak",		strbreak},
	{"strtolower",		strtolower},
	{"strtoupper",		strtoupper},
	{"str_to_num",		strtonum},
	{"strtonum",		strtonum},
	{"trim",			amx_trim},
	{"ucfirst",			amx_ucfirst},
	{"strtok",			amx_strtok},
	{"strlen",			amx_strlen},
	{"strcat",			n_strcat},
	{"strfind",			n_strfind},
	{"strcmp",			n_strcmp},
	{"str_to_float",	str_to_float},
	{"float_to_str",	float_to_str},
	{NULL,				NULL}
};
