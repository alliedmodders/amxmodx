#include "amxmodx.h"
#include "format.h"

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
#define to_digit(c)		((c) - '0')
#define is_digit(c)		((unsigned)to_digit(c) <= 9)
#define to_char(n)		((n) + '0')
#define CHECK_ARGS(n) \
	if ((arg+n) > args) { \
		LogError(amx, AMX_ERR_PARAMS, "String formatted incorrectly - parameter %d (total %d)", arg, args); \
		return 0; \
	}

THash<String, lang_err> BadLang_Table;

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
			pLangName = g_vault.get("server_language");
		} else {
			pLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(g_langMngr.GetDefLang())->pEdict, "lang");
		}
	} else if (addr[0] == LANG_SERVER) {
		pLangName = g_vault.get("server_language");
	} else if (addr[0] >= 1 && addr[0] <= gpGlobals->maxClients) {
		if (!amx_cl_langs)
			amx_cl_langs = CVAR_GET_POINTER("amx_client_languages");
		if ( (int)amx_cl_langs->value == 0 )
		{
			pLangName = g_vault.get("server_language");
		} else {
			pLangName = ENTITY_KEYVALUE(GET_PLAYER_POINTER_I(addr[0])->pEdict, "lang");
		}
	} else {
		get_amxstring_r(amx, amxaddr, name, 3);
		pLangName = name;
	}
	if (!pLangName || !isalpha(pLangName[0]))
		pLangName = "en";
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
		if (debug)
		{
			if (status == ERR_BADLANG && (BadLang_Table.AltFindOrInsert(pLangName).last + 120.0f < gpGlobals->time))
			{
				AMXXLOG_Error("[AMXX] Language \"%s\" not found", pLangName);
				BadLang_Table.AltFindOrInsert(pLangName).last = gpGlobals->time;
			}
		}

		if (addr[0] != LANG_SERVER)
			def = g_langMngr.GetDef(g_vault.get("server_language"), key, status);
		
		if (!def && (strcmp(pLangName, "en") != 0 && strcmp(g_vault.get("server_language"), "en") != 0))
			def = g_langMngr.GetDef("en", key, status);
	}

	return def;
}

template <typename U>
void AddString(U **buf_p, size_t &maxlen, const cell *string, int width, int prec)
{
	int		size = 0;
	U		*buf;
	static cell nlstr[] = {'(','n','u','l','l',')','\0'};

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
void AddFloat(U **buf_p, size_t &maxlen, double fval, int width, int prec)
{
	U		text[32];
	int		digits;
	double	signedVal;
	U		*buf;
	int		val;

	// get the sign
	signedVal = fval;
	if (fval < 0)
		fval = -fval;

	// write the float number
	digits = 0;
	val = (int)fval;
	do {
		text[digits++] = '0' + val % 10;
		val /= 10;
	} while (val);

	if (signedVal < 0)
		text[digits++] = '-';

	buf = *buf_p;

	while (digits < width && maxlen)
	{
		*buf++ = ' ';
		width--;
		maxlen--;
	}

	while (digits-- && maxlen)
	{
		*buf++ = text[digits];
		maxlen--;
	}

	*buf_p = buf;

	if (prec < 0)
		prec = 6;
	// write the fraction
	digits = 0;
	while (digits < prec)
	{
		fval -= (int) fval;
		fval *= 10.0;
		val = (int) fval;
		text[digits++] = '0' + val % 10;
	}

	if (digits > 0 && maxlen)
	{
		buf = *buf_p;
		*buf++ = '.';
		maxlen--;
		for (prec = 0; maxlen && prec < digits; prec++)
		{
			*buf++ = text[prec];
			maxlen--;
		}
		*buf_p = buf;
	}
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
	char	sign;
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
		sign = '\0';

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
			AddFloat(&buf_p, llen, amx_ctof(*get_amxaddr(amx, params[arg])), width, prec);
			arg++;
			break;
		case 's':
			CHECK_ARGS(0);
			AddString(&buf_p, llen, get_amxaddr(amx, params[arg]), width, prec);
			arg++;
			break;
		case 'L':
			{
				CHECK_ARGS(1);
				cell addr = params[arg++];
				int len;
				const char *key = get_amxstring(amx, params[arg++], 3, len);
				const char *def = translate(amx, addr, key);
				if (!def)
				{
					static char buf[255];
					snprintf(buf, sizeof(buf)-1, "ML_NOTFOUND: %s", key);
					def = buf;
				}
				size_t written = atcprintf(buf_p, llen, def, amx, params, &arg);
				buf_p += written;
				llen -= written;
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

