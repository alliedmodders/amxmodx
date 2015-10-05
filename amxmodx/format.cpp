// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "format.h"
#include "datastructs.h"

//Adapted from Quake3's vsprintf
// thanks to cybermind for linking me to this :)
//I made the following changes:
// - Fixed spacing to be AMX Mod X standard
// - Added 'n' support, no buffer overflows
// - Templatized input/output buffers

#define ALT			0x00000001		/* alternate form */
#define HEXPREFIX	0x00000002		/* add 0x or 0X prefix */
#define LADJUST		0x00000004		/* left adjustment */
#define LONGDBL		0x00000008		/* long double */
#define LONGINT		0x00000010		/* long integer */
#define QUADINT		0x00000020		/* quad integer */
#define SHORTINT	0x00000040		/* short integer */
#define ZEROPAD		0x00000080		/* zero (as opposed to blank) pad */
#define FPT			0x00000100		/* floating point number */
#define UPPERDIGITS	0x00000200		/* make alpha digits uppercase */
#define to_digit(c)		((c) - '0')
#define is_digit(c)		((unsigned)to_digit(c) <= 9)
#define to_char(n)		((n) + '0')
#define CHECK_ARGS(n) \
	if ((arg+n) > args) { \
		LogError(amx, AMX_ERR_PARAMS, "String formatted incorrectly - parameter %d (total %d)", arg, args); \
		return 0; \
	}

template size_t atcprintf<cell, cell>(cell *, size_t, const cell *, AMX *, cell *, int *);
template size_t atcprintf<char, cell>(char *, size_t, const cell *, AMX *, cell *, int *);
template size_t atcprintf<cell, char>(cell *, size_t, const char *, AMX *, cell *, int *);
template size_t atcprintf<char, char>(char *, size_t, const char *, AMX *, cell *, int *);

THash<ke::AString, lang_err> BadLang_Table;

static cvar_t *amx_mldebug = NULL;
static cvar_t *amx_cl_langs = NULL;

const char *translate(AMX *amx, cell amxaddr, const char *key)
{
	const char *pLangName = NULL;
	const char *def = NULL;
	int status;
	cell *addr = get_amxaddr(amx, amxaddr);
	char name[4];
	if (addr[0] == LANG_PLAYER)
	{
		if (!amx_cl_langs)
			amx_cl_langs = CVAR_GET_POINTER("amx_client_languages");
		if ( (int)amx_cl_langs->value == 0 )
		{
			pLangName = amxmodx_language->string;
		} else {
			pLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(g_langMngr.GetDefLang())->pEdict, "lang");
		}
	} else if (addr[0] == LANG_SERVER) {
		pLangName = amxmodx_language->string;
	} else if (addr[0] >= 1 && addr[0] <= gpGlobals->maxClients) {
		if (!amx_cl_langs)
			amx_cl_langs = CVAR_GET_POINTER("amx_client_languages");
		if ( (int)amx_cl_langs->value == 0 )
		{
			pLangName = amxmodx_language->string;
		} else {
			pLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(addr[0])->pEdict, "lang");
		}
	} else {
		get_amxstring_r(amx, amxaddr, name, 3);
		pLangName = name;
	}

	if (!pLangName || !isalpha(pLangName[0]))
	{
		pLangName = amxmodx_language->string;
	}

	//next parameter!
	def = g_langMngr.GetDef(pLangName, key, status);
			
	if (!amx_mldebug)
		amx_mldebug = CVAR_GET_POINTER("amx_mldebug");

	bool debug = (amx_mldebug && amx_mldebug->string && (amx_mldebug->string[0] != '\0'));
		
	if (debug)
	{
		int debug_status;
		bool validlang = true;
		const char *testlang = amx_mldebug->string;
		if (!g_langMngr.LangExists(testlang))
		{
			AMXXLOG_Error("[AMXX] \"%s\" is an invalid debug language", testlang);
			validlang = false;
		}
				
		g_langMngr.GetDef(testlang, key, debug_status);
				
		if (validlang && debug_status == ERR_BADKEY)
			AMXXLOG_Error("[AMXX] Language key \"%s\" not found for language \"%s\", check \"%s\"", key, testlang, GetFileName(amx));
	}
				
	if (def == NULL)
	{
		if (debug && status == ERR_BADLANG)
		{
			ke::AString lang(pLangName);

			lang_err &err = BadLang_Table.AltFindOrInsert(ke::Move(lang));

			if (err.last + 120.0f < gpGlobals->time)
			{
				AMXXLOG_Error("[AMXX] Language \"%s\" not found", pLangName);
				err.last = gpGlobals->time;
			}
		}

		if (addr[0] != LANG_SERVER)
			def = g_langMngr.GetDef(amxmodx_language->string, key, status);
		
		if (!def && (strcmp(pLangName, "en") != 0 && strcmp(amxmodx_language->string, "en") != 0))
			def = g_langMngr.GetDef("en", key, status);
	}

	return def;
}

template <typename U, typename S>
void AddString(U **buf_p, size_t &maxlen, const S *string, int width, int prec)
{
	int		size = 0;
	U		*buf;
	static S nlstr[] = {'(','n','u','l','l',')','\0'};

	buf = *buf_p;

	if (string == NULL)
	{
		string = nlstr;
		prec = -1;
	}

	if (prec >= 0)
	{
		for (size = 0; size < prec; size++) 
		{
			if (string[size] == '\0')
				break;
		}
	} else {
		while (string[size++]) ;
		size--;
	}

	if (size > (int)maxlen)
		size = maxlen;

	/* If precision is provided, make sure we don't truncate a multi-byte character */
	if (prec >= size && (string[size - 1] & 1 << 7))
	{
		size -= UTIL_CheckValidChar((cell *)string + size - 1);
	}

	maxlen -= size;
	width -= size;

	while (size--)
		*buf++ = static_cast<U>(*string++);

	while (width-- > 0 && maxlen)
	{
		*buf++ = ' ';
		maxlen--;
	}

	*buf_p = buf;
}

template <typename U>
void AddFloat(U **buf_p, size_t &maxlen, double fval, int width, int prec, int flags)
{
	int digits;				// non-fraction part digits
	double tmp;				// temporary
	U *buf = *buf_p;			// output buffer pointer
	int val;				// temporary
	int sign = 0;				// 0: positive, 1: negative
	int fieldlength;			// for padding
	int significant_digits = 0;		// number of significant digits written
	const int MAX_SIGNIFICANT_DIGITS = 16;

	// default precision
	if (prec < 0)
	{
		prec = 6;
	}

	// get the sign
	if (fval < 0)
	{
		fval = -fval;
		sign = 1;
	}

	// compute whole-part digits count
	digits = (int)log10(fval) + 1;

	// Only print 0.something if 0 < fval < 1
	if (digits < 1)
	{
		digits = 1;
	}

	// compute the field length
	fieldlength = digits + prec + ((prec > 0) ? 1 : 0) + sign;

	// minus sign BEFORE left padding if padding with zeros
	if (sign && maxlen && (flags & ZEROPAD))
	{
		*buf++ = '-';
		maxlen--;
	}

	// right justify if required
	if ((flags & LADJUST) == 0)
	{
		while ((fieldlength < width) && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			width--;
			maxlen--;
		}
	}

	// minus sign AFTER left padding if padding with spaces
	if (sign && maxlen && !(flags & ZEROPAD))
	{
		*buf++ = '-';
		maxlen--;
	}

	// write the whole part
	tmp = pow(10.0, digits-1);
	while ((digits--) && maxlen)
	{
		if (++significant_digits > MAX_SIGNIFICANT_DIGITS)
		{
			*buf++ = '0';
		}
		else
		{
			val = (int)(fval / tmp);
			*buf++ = '0' + val;
			fval -= val * tmp;
			tmp *= 0.1;
		}
		maxlen--;
	}

	// write the fraction part
	if (maxlen && prec)
	{
		*buf++ = '.';
		maxlen--;
	}

	tmp = pow(10.0, prec);

	fval *= tmp;
	while (prec-- && maxlen)
	{
		if (++significant_digits > MAX_SIGNIFICANT_DIGITS)
		{
			*buf++ = '0';
		}
		else
		{
			tmp *= 0.1;
			val = (int)(fval / tmp);
			*buf++ = '0' + val;
			fval -= val * tmp;
		}
		maxlen--;
	}

	// left justify if required
	if (flags & LADJUST)
	{
		while ((fieldlength < width) && maxlen)
		{
			// right-padding only with spaces, ZEROPAD is ignored
			*buf++ = ' ';
			width--;
			maxlen--;
		}
	}

	// update parent's buffer pointer
	*buf_p = buf;
}

template <typename U>
void AddBinary(U **buf_p, size_t &maxlen, unsigned int val, int width, int flags)
{
	char text[32];
	int digits;
	U *buf;

	digits = 0;
	do
	{
		if (val & 1)
		{
			text[digits++] = '1';
		}
		else
		{
			text[digits++] = '0';
		}
		val >>= 1;
	} while (val);

	buf = *buf_p;

	if (!(flags & LADJUST))
	{
		while (digits < width && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			width--;
			maxlen--;
		}
	}

	while (digits-- && maxlen)
	{
		*buf++ = text[digits];
		width--;
		maxlen--;
	}

	if (flags & LADJUST)
	{
		while (width-- && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			maxlen--;
		}
	}

	*buf_p = buf;
}

template <typename U>
void AddUInt(U **buf_p, size_t &maxlen, unsigned int val, int width, int flags)
{
	U		text[32];
	int		digits;
	U		*buf;

	digits = 0;
	do {
		text[digits++] = '0' + val % 10;
		val /= 10;
	} while (val);

	buf = *buf_p;

	if( !(flags & LADJUST) )
	{
		while (digits < width && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			width--;
			maxlen--;
		}
	}

	while (digits-- && maxlen)
	{
		*buf++ = text[digits];
		width--;
		maxlen--;
	}

	if (flags & LADJUST)
	{
		while (width-- && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			maxlen--;
		}
	}

	*buf_p = buf;
}

template <typename U>
void AddInt(U **buf_p, size_t &maxlen, int val, int width, int flags)
{
	U		text[32];
	int		digits;
	int		signedVal;
	U		*buf;
	unsigned int unsignedVal;

	digits = 0;
	signedVal = val;
	if (val < 0)
	{
		/* we want the unsigned version */
		unsignedVal = abs(val);
	} else {
		unsignedVal = val;
	}
	do {
		text[digits++] = '0' + unsignedVal % 10;
		unsignedVal /= 10;
	} while (unsignedVal);

	if (signedVal < 0)
		text[digits++] = '-';

	buf = *buf_p;

	if( !(flags & LADJUST) )
	{
		while (digits < width && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			width--;
			maxlen--;
		}
	}

	while (digits-- && maxlen)
	{
		*buf++ = text[digits];
		width--;
		maxlen--;
	}

	if (flags & LADJUST)
	{
		while (width-- && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			maxlen--;
		}
	}

	*buf_p = buf;
}

template <typename U>
void AddHex(U **buf_p, size_t &maxlen, unsigned int val, int width, int flags)
{
	U		text[32];
	int		digits;
	U		*buf;
	U		digit;
	int		hexadjust;

	if (flags & UPPERDIGITS)
	{
		hexadjust = 'A' - '9' - 1;
	} else {
		hexadjust = 'a' - '9' - 1;
	}

	digits = 0;
	do
	{
		digit = ('0' + val % 16);
		if (digit > '9')
		{
			digit += hexadjust;
		}

		text[digits++] = digit;
		val /= 16;
	} while (val);

	buf = *buf_p;

	if( !(flags & LADJUST) )
	{
		while (digits < width && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			width--;
			maxlen--;
		}
	}

	while (digits-- && maxlen)
	{
		*buf++ = text[digits];
		width--;
		maxlen--;
	}

	if (flags & LADJUST)
	{
		while (width-- && maxlen)
		{
			*buf++ = (flags & ZEROPAD) ? '0' : ' ';
			maxlen--;
		}
	}

	*buf_p = buf;
}

template <typename D, typename S>
size_t atcprintf(D *buffer, size_t maxlen, const S *format, AMX *amx, cell *params, int *param)
{
	int		arg;
	int		args = params[0] / sizeof(cell);
	D		*buf_p;
	D		ch;
	int		flags;
	int		width;
	int		prec;
	int		n;
	//char	sign;
	const S	*fmt;
	size_t	llen = maxlen;

	buf_p = buffer;
	arg = *param;
	fmt = format;

	while (true)
	{
		// run through the format string until we hit a '%' or '\0'
		for (ch = static_cast<D>(*fmt); 
			llen && ((ch = static_cast<D>(*fmt)) != '\0' && ch != '%');
			fmt++)
		{
			*buf_p++ = static_cast<D>(ch);
			llen--;
		}
		if (ch == '\0' || llen <= 0)
			goto done;

		// skip over the '%'
		fmt++;

		// reset formatting state
		flags = 0;
		width = 0;
		prec = -1;
		//sign = '\0';

rflag:
		ch = static_cast<D>(*fmt++);
reswitch:
		switch(ch)
		{
		case '-':
			flags |= LADJUST;
			goto rflag;
		case '.':
			n = 0;
			while( is_digit( ( ch = static_cast<D>(*fmt++)) ) )
				n = 10 * n + ( ch - '0' );
			prec = n < 0 ? -1 : n;
			goto reswitch;
		case '0':
			flags |= ZEROPAD;
			goto rflag;
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			n = 0;
			do {
				n = 10 * n + ( ch - '0' );
				ch = static_cast<D>(*fmt++);
			} while( is_digit( ch ) );
			width = n;
			goto reswitch;
		case 'c':
			CHECK_ARGS(0);
			*buf_p++ = static_cast<D>(*get_amxaddr(amx, params[arg]));
			llen--;
			arg++;
			break;
		case 'b':
			CHECK_ARGS(0);
			AddBinary(&buf_p, llen, *get_amxaddr(amx, params[arg]), width, flags);
			arg++;
			break;
		case 'd':
		case 'i':
			CHECK_ARGS(0);
			AddInt(&buf_p, llen, *get_amxaddr(amx, params[arg]), width, flags);
			arg++;
			break;
		case 'u':
			CHECK_ARGS(0);
			AddUInt(&buf_p, llen, static_cast<unsigned int>(*get_amxaddr(amx, params[arg])), width, flags);
			arg++;
			break;
		case 'f':
			CHECK_ARGS(0);
			AddFloat(&buf_p, llen, amx_ctof(*get_amxaddr(amx, params[arg])), width, prec, flags);
			arg++;
			break;
		case 'X':
			CHECK_ARGS(0);
			flags |= UPPERDIGITS;
			AddHex(&buf_p, llen, static_cast<unsigned int>(*get_amxaddr(amx, params[arg])), width, flags);
			arg++;
			break;
		case 'x':
			CHECK_ARGS(0);
			AddHex(&buf_p, llen, static_cast<unsigned int>(*get_amxaddr(amx, params[arg])), width, flags);
			arg++;
			break;
		case 'a':
			{
				CHECK_ARGS(0);
				// %a is passed a pointer directly to a cell string.
				cell* ptr=reinterpret_cast<cell*>(*get_amxaddr(amx, params[arg]));
				if (!ptr)
				{
					LogError(amx, AMX_ERR_NATIVE, "Invalid vector string handle provided (%d)", *get_amxaddr(amx, params[arg]));
					return 0;
				}

				AddString(&buf_p, llen, ptr, width, prec);
				arg++;
				break;
			}
		case 's':
			CHECK_ARGS(0);
			AddString(&buf_p, llen, get_amxaddr(amx, params[arg]), width, prec);
			arg++;
			break;
		case 'L':
		case 'l':
			{
				cell target;
				if (ch == 'L')
				{
					CHECK_ARGS(1);
					target = params[arg++];
				}
				else
				{
					CHECK_ARGS(0);
					target = g_langMngr.GetDefLang();
				}
				int len;
				const char *key = get_amxstring(amx, params[arg++], 3, len);
				const char *def = translate(amx, target, key);
				if (!def)
				{
					static char buf[255];
					ke::SafeSprintf(buf, sizeof(buf), "ML_NOTFOUND: %s", key);
					def = buf;
				}
				size_t written = atcprintf(buf_p, llen, def, amx, params, &arg);
				buf_p += written;
				llen -= written;
				break;
			}
		case 'N':
			{
				CHECK_ARGS(0);
				cell *addr = get_amxaddr(amx, params[arg]);
				char buffer[255];
				if (*addr)
				{
					CPlayer *player = NULL;

					if (*addr >= 1 && *addr <= gpGlobals->maxClients)
					{
						player = GET_PLAYER_POINTER_I(*addr);
					}

					if (!player || !player->initialized)
					{
						LogError(amx, AMX_ERR_NATIVE, "Client index %d is invalid", *addr);
						return 0;
					}

					const char *auth = GETPLAYERAUTHID(player->pEdict);
					if (!auth || auth[0] == '\0')
					{
						auth = "STEAM_ID_PENDING";
					}

					int userid = GETPLAYERUSERID(player->pEdict);
					ke::SafeSprintf(buffer, sizeof(buffer), "%s<%d><%s><%s>", player->name.chars(), userid, auth, player->team.chars());
				}
				else
				{
					ke::SafeSprintf(buffer, sizeof(buffer), "Console<0><Console><Console>");
				}

				AddString(&buf_p, llen, buffer, width, prec);
				arg++;
				break;
			}
		case 'n':
			{
				CHECK_ARGS(0);
				cell *addr = get_amxaddr(amx, params[arg]);
				const char *name = "Console";

				if (*addr)
				{
					CPlayer *player = NULL;

					if (*addr >= 1 && *addr <= gpGlobals->maxClients)
					{
						player = GET_PLAYER_POINTER_I(*addr);
					}

					if (!player || !player->initialized)
					{
						LogError(amx, AMX_ERR_NATIVE, "Client index %d is invalid", *addr);
						return 0;
					}

					name = player->name.chars();
				}
			
				AddString(&buf_p, llen, name, width, prec);
				arg++;
				break;
			}
		case '%':
			*buf_p++ = static_cast<D>(ch);
			if (!llen)
				goto done;
			llen--;
			break;
		case '\0':
			*buf_p++ = static_cast<D>('%');
			if (!llen)
				goto done;
			llen--;
			goto done;
			break;
		default:
			*buf_p++ = static_cast<D>(ch);
			if (!llen)
				goto done;
			llen--;
			break;
		}
	}

done:
	*buf_p = static_cast<D>(0);
	*param = arg;

	/* if max buffer length consumed, make sure we don't truncate a multi-byte character */
	if (llen <= 0 && *(buf_p - 1) & 1 << 7)
	{
		llen += UTIL_CheckValidChar(buf_p - 1);
		*(buf_p - llen) = static_cast<D>(0);
	}

	return maxlen-llen;
}

/**
 * HACKHACK: The compiler will generate code for each case we need.
 * Don't remove this, otherwise files that use certain code generations
 *  will have extern problems.  For each case you need, add dummy code
 *  here.
 */
void __WHOA_DONT_CALL_ME_PLZ_K_lol_o_O()
{
	//acsprintf
	atcprintf((cell *)NULL, 0, (const char *)NULL, NULL, NULL, NULL);
	//accprintf
	atcprintf((cell *)NULL, 0, (cell *)NULL, NULL, NULL, NULL);
	//ascprintf
	atcprintf((char *)NULL, 0, (cell *)NULL, NULL, NULL, NULL);
}

