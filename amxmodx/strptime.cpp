/**  strptime.c **********************************************************

     Locales' support for DOS / Win31 / Win32.
            Copyright (c) 1995-1997  by Timofei Bondarenko <tim@ipi.ac.ru>

     Localized strptime().
 *-----------------------------------------------------------------------*/
//#include "config.h"
#include <time.h>
#include <ctype.h>
#include <string.h>

#if defined(__linux__) || defined(__APPLE__)
#define strnicmp strncasecmp
#endif

#if _MSC_VER
	#if _MSC_VER >= 1400
		// MSVC8 - Replace POSIX stricmp with ISO C++ conformant one as it is deprecated
		#define stricmp _stricmp

		// Need this because of some stupid bug
		#pragma warning (disable : 4996)
	#endif
#endif

// this file does not include amxmodx.h, so we have to include the memory manager here
#ifdef MEMORY_TEST
#include "mmgr/mmgr.h"
#endif // MEMORY_TEST

const char *_lc_Wday_ [2][ 7],
                  *_lc_Month_[2][12],
                  *_lc_AmPm_ [2][ 2];

const char *_lc_fmt_c_[2],
                  *_lc_fmt_xD[2],
                  *_lc_fmt_XT[2];

int _lc_Txt_, /* 0="C", 1="Local Win"/"Rus DOS" Wday, Months, AmPm */
           _lc_Fmt_; /* 0="C", 1="Local", for formats %c %x %X */

//#include "_locale.h"

struct tm_int
      {
       int qS, /* Seconds (0...61) */
           qM, /* Minutes (0...59) */
           qH, /* Hour (0...23) */
           qI, /* Hour (1...12) */
           qp, /* 0 = AM, 1 = PM */
           qd, /* Day of month (1...31)=>[0...30] */
           qm, /* Month (1...12)=>[0...11] */
           qY, /* Year (0...9999) */
           qy, /* year (0...99) */
           qC, /* Century (0...99) */
           qw, /* Weekday (0...6; Sunday = 0) */
           qj, /* Day of year (1..366)=>[0...365] */
           qZ, /* 0 = STD, 1 = DST */
           qU, /* week in year (0...53) */
           qV; /* week in year mode: 'U', 'W', 'V' */
      };

/* skips spaces in `strp`. Returns 0 if no spaces skipped */

static void skip_sp(const unsigned char **strp)
{
 while(isspace(**strp)) (*strp)++;
}

/* scans no more decimal digits than contained in `max` as an integer,
   sign isn't allowed. Returns -1 if 0 digits scanned or
   if the scanned number is greater than `max`. */

static int scan_int(const unsigned char **strp, int max)
{
 int cc, val, pos;
 skip_sp(strp);
 for(val = pos = 0; pos < max && isdigit(cc = **strp); (*strp)++)
   {
    pos = pos * 10 + 9;
    if ((val = val * 10 + (cc - '0')) > max) return -1;
    /*val = val * 10 + (cc - '0');*/
   }
 return pos? val: -1;
}

static int scan_int2(const unsigned char **strp)
{
 int cc, val, pos;
 skip_sp(strp);
 for(val = pos = 0; isdigit(cc = **strp); (*strp)++)
   {
    pos = pos * 10 + 9;
    val = val * 10 + (cc - '0');
   }
 return pos? val: -1;
}

/* scans one word which is equivalence (case insensitive) to a
   word from list `n_full` or from list `n_short` or when
   n_short is NULL to first 3 characters from a word from `n_full`.
   `max` is number of words in each list `n_full` or `n_short`.
   Returns the index in a list >= 0 and < `max`, or -1 if no word found. */

static int scan_word_(const unsigned char **strp,
                      int max, const char *const *n_full
#if    USE_ABBR_NAMES
                             , const char *const *n_short
#endif
                                                   )
{
 int ix, l_max = 100;
 int found, found_len;
 const char *const *word_list;

 found = found_len = -1;
 skip_sp(strp); /* Required? Or Not? */
Scan0:
 word_list = n_full;
/*Scan1:*/
 for(ix = 0; ix < max; word_list++, ix++)
   {
    int len;
    const char *word = *word_list;
    skip_sp((const unsigned char**)&word);
    if (l_max < (len = strlen(word))) len = l_max;
    if (found_len < len && /* search for maximal lenth */
                  (!len || /* at least "" always founded */
        !strnicmp((const char*)*strp, word, len))) /* found */
      found_len = len, found = ix;
   }
 if (l_max >= 100) /* first pass: full names */
   {               /* go to second pass: short names */
#if    USE_ABBR_NAMES
    if (n_short)
      {
       l_max--; word_list = n_short; goto Scan1;
      }
#endif
    l_max = 3; goto Scan0;
   }
 if (found_len > 0) (*strp) += found_len;
 return found; /* -1 or index or first "" */
}

#if    USE_ABBR_NAMES
#define scan_word  scan_word_ /* pass all arguments as is */
#else
#define scan_word(str,max,n_full,n_short) scan_word_(str,max,n_full)
#endif

static int time_int(struct tm_int *ti,
                    const unsigned char **buf, const char *fmt, short addthem)
{
 int ii;

 for(; (ii = (unsigned char)*fmt); fmt++)
   if (ii != '%')
     {
Other:
      if (isspace(ii))
        {
/*SkipSp:*/  fmt++;  skip_sp((const unsigned char **)&fmt);
         fmt--;  skip_sp(buf);
        }       /* toupper(ii) != toupper(**buf) */
      /*else if (_lc_igncase[ii] != _lc_igncase[**buf]) return -1;*/
      else (*buf)++;
     }
   else
     {
      const char *fs;
/*Fmt:*/  switch(ii = (unsigned char)*++fmt)
        {
      case  0 : goto FmtEnd;
      case 'a':
      case 'A':
         ti->qw = ii = scan_word(buf, 7,
                                _lc_Wday_[_lc_Txt_],
                                _lc_Txt_? _lc_WdayS: NULL);  break;
      case 'h': /* STRFTIME_EXT */
      case 'b':
      case 'B':
         ti->qm = ii = scan_word(buf, 12,
                                _lc_Month_[_lc_Txt_],
                                _lc_Txt_? _lc_MonthS: NULL); break;
      case 'c':
         fs = _lc_fmt_c_[_lc_Fmt_];
strpt:   ii = time_int(ti, buf, fs, addthem);          break;
#if     STRFTIME_EXT
      case 'C': /* STRFTIME_EXT */
         ti->qC = ii = addthem ? scan_int2(buf) : scan_int(buf,99);
         if (ti->qy >= 0) goto SetYear;
         else if (ti->qY >= 0)
           {
            ti->qY = ti->qY % 100 + ii * 100;
            goto CleanCy;
           }
         break;
      case 'e': /* STRFTIME_EXT */
#endif
      case 'd':
         ti->qd = ii = (addthem ? scan_int2(buf) : scan_int(buf, 31)) - 1; break;
#if     STRFTIME_EXT
      case 'D': /* STRFTIME_EXT */
         fs = _lc_fmt_xD[0];             goto strpt;
/*    case 'E':    STRFTIME_EXT "Era specefic" see %'O' */
/*    case 'h':    STRFTIME_EXT see %'b' */
#endif
      case 'H':
         ti->qH = ii = addthem ? scan_int2(buf) : scan_int(buf, 23);
CleanIp: ti->qI = ti->qp = -1;                break;
      case 'I':
         ti->qI =
         ti->qH = ii = addthem ? scan_int2(buf) : scan_int(buf, 23);
         if (ii == 0 || ii > 12) goto CleanIp;
         else ti->qH = -1;
         break;
      case 'j':
         ti->qj = ii = (addthem ? scan_int2(buf) : scan_int(buf, 366)) - 1;
         ti->qU = -1;                         break;
      case 'm':
         ti->qm = ii = (addthem ? scan_int2(buf) : scan_int(buf, 12)) - 1; break;
      case 'M':
         ti->qM = ii = addthem ? scan_int2(buf) : scan_int(buf, 59);     break;
#if     STRFTIME_EXT
      case 'n': /* STRFTIME_EXT */
      case 't': /* STRFTIME_EXT */       goto SkipSp;
      case 'N': /* STRFTIME_EXT */
         fs = _lc_fmt_N_;                goto strpt;
      case 'E': /* STRFTIME_EXT "Era specefic"     */
#endif
#if     STRFTIME_EXT | STRFTIME_WIN
      case 'O': /* STRFTIME_EXT "Alternate digits" */
                                         goto Fmt;
#endif
      case 'p':
         ti->qp = /*ii =*/ scan_word(buf, 2, _lc_AmPm_[_lc_Txt_], NULL);
         break;
#if     STRFTIME_EXT
      case 'r': /* STRFTIME_EXT */
         fs = _lc_fmt_rI;                goto strpt;
      case 'R': /* STRFTIME_EXT */
         fs = _lc_fmt_RH;                goto strpt;
#endif
      case 'S':
         ti->qS = ii = addthem ? scan_int2(buf) : scan_int(buf, 61);     break;
#if     STRFTIME_EXT
      case 'T': /* STRFTIME_EXT */
         fs = _lc_fmt_XT[0];             goto strpt;
      case 'u': /* STRFTIME_EXT */
         ti->qw = ii = addthem ? scan_int2(buf) : scan_int(buf,  7);
         if (ii == 7) ti->qw = 0;
         else if (!ii) ii--;
         break;
#endif
#if     STRFTIME_EXT && 0
      case 'V': /* STRFTIME_EXT  0 = Wednesday ?(Thursday) */
#endif
      case 'U':
      case 'W':
         ti->qV = ii;
         ti->qU = ii = addthem ? scan_int2(buf) : scan_int(buf, 53);     break;
      case 'w':
         ti->qw = ii = addthem ? scan_int2(buf) : scan_int(buf,  6);     break;
      case 'x':
         fs = _lc_fmt_xD[_lc_Fmt_];      goto strpt;
      case 'X':
         fs = _lc_fmt_XT[_lc_Fmt_];      goto strpt;
      case 'y':
         ti->qy = ii = addthem ? scan_int2(buf) : scan_int(buf, 99);
#if     STRFTIME_EXT
         if (ti->qC >= 0)
           {
SetYear:    ti->qY = ti->qC * 100 + ti->qy;
            goto CleanCy;
           }
#endif
         if (ti->qY >= 0)
           {
            ti->qY = ti->qY - ti->qY % 100 + ii;
            goto CleanCy;
           }
         break;
      case 'Y':
         ti->qY = ii = addthem ? scan_int2(buf) : scan_int(buf, 9999);
CleanCy: ti->qC = ti->qy = -1;                break;
#if     STRFTIME_EXT
      case 'Z': /* STRFTIME_EXT */
         ti->qZ = ii = scan_word(buf, /*!daylight? 1:*/ 2, tzname, NULL) +1;
         if (!ii)
           while((ii = **buf) && !isspace(ii)
                 /*&& !isdigit(ii) && !strchr("+-,", ii)*/
                ) (*buf)++;
         break;
#endif
/*    case '%': */
      default: /**************************/ goto Other;
        } /* end of switch() */
      if (ii < 0) return -1;
     } /* end of else, for() */
FmtEnd:
 return 0;
}

typedef void (*specoper)(int* one, int two);
void justreplace(int* one, int two){
	*one = two;
}
void justadd(int* one, int two){
	*one += two;
}

char *strptime(const char *buf, const char *fmt, struct tm *tm, short addthem)
{
 specoper defoper = addthem ? justadd : justreplace;

 struct tm_int ti;
               ti.qS =     /* Seconds (0...61) */
               ti.qM =     /* Minutes (0...59) */
               ti.qH =     /* Hour (0...23) */
               ti.qI =     /* Hour (1...12) */
               ti.qp =     /* 0 = AM, 1 = PM */
               ti.qd =     /* Day of month (1...31)=>[0...30] */
               ti.qm =     /* Month (1...12)=>[0...11] */
               ti.qY =     /* Year (0...9999) */
               ti.qy =     /* year (0...99) */
               ti.qC =     /* Century (0...99) */
               ti.qw =     /* Weekday (0...6; Sunday = 0) */
               ti.qj =     /* Day of year (1..366)=>[0...365] */
               ti.qZ =     /* 0 = STD, 1 = DST */
               ti.qU =     /* week in year (0...53) */
               ti.qV = -1; /* week in year mode: 0=U, 1=W, 2=V */

 if (0 > time_int(&ti, (const unsigned char **)&buf, fmt, addthem)) buf = NULL;
 if (0 <= ti.qS) (*defoper) ( &tm->tm_sec ,  ti.qS );
 if (0 <= ti.qM) (*defoper) ( &tm->tm_min ,  ti.qM ); //tm->tm_min  = ti.qM;
 if (0 <= ti.qI)
 {
   if (0 <= ti.qp) ti.qH = ti.qI % 12 + ti.qp * 12;
   else (*defoper) ( &tm->tm_hour ,  ti.qI );  //tm->tm_hour = ti.qI;
 }
 if (0 <= ti.qH) (*defoper) ( &tm->tm_hour ,  ti.qH ); //tm->tm_hour = ti.qH;
 if (0 <= ti.qZ) (*defoper) ( &tm->tm_isdst ,  ti.qZ - 1 ); //tm->tm_isdst = ti.qZ - 1;
 if (0 <= ti.qy) ti.qY = ti.qy;
 if (0 <= ti.qY) (*defoper) ( &tm->tm_year ,
 						ti.qY +=
                              (ti.qY > 99? -1900:
                              (ti.qY < 70? 100: 0)) );
 					/*tm->tm_year = ti.qY +=
                              (ti.qY > 99? -1900:
                              (ti.qY < 70? 100: 0));*/
/* ti.qC = %C = Century without an Year - ignored */

 if (70 <= ti.qY && ti.qY < 200) /* a representable year */
   {
       /* 01-01-70 was Thursday,  1968 was leap */
    int day = (((ti.qY - 69) >> 2) + ti.qY - 70 + 4) % 7;
       /* 2100 wrongly assumed as leap!
          if (ti.qY > 200 && --day < 0) day = 6; */

    if (0 <= ti.qU && 0 <= ti.qw && 0 > ti.qj)
      {
       ti.qj = ti.qU * 7;
       switch(ti.qV)
         {
       case 'U': /* %U Sun first */
          ti.qj += ti.qw - (day == 0 ? 7: day);
          break;
       case 'W': /* %W Mon first */
          ti.qj += (ti.qw + 6) % 7
                         - (day == 1 ? 7: (day + 6) % 7);
          break;
#if     STRFTIME_EXT && 0
       case 'V': /* %V >= 4 day */
          if (ti.qU == 53) ti.qj = 0;
          /* Sun first: */
          ti.qj += ti.qw - (day < 4 ? 7: 0) - day;
          /* Mon first:
          ti.qj += (ti.qw + 6) % 7
                 - ((day + 6) % 7 < 5 ? 7: 0)
                  - (day + 6) % 7; */
          break;
#endif
       default: break;
         }
      }
#if 0 /* Advanced validating for yday<>m/d/y */
    if (0 <= ti.qj)
      {
       static int m_days[12] =
             { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
       int mon = (ti.qj + day) % 7;

       if (0 > ti.qw) ti.qw = mon;
       else if (ti.qw != mon) return NULL;

       for(mon = 11; 0 > (day =     /* not for 2100: && ti.qY != 200 */
                     ti.qj - (m_days[mon] + (mon > 1 && !(ti.qY & 3))))
         ; mon--);
       if (0 > ti.qd) ti.qd = day;
       else if (ti.qd != day) return NULL;
       if (0 > ti.qm) ti.qm = mon;
       else if (ti.qm != mon) return NULL;
      }
#endif
   }
 if (0 <= ti.qd) (*defoper) ( &tm->tm_mday ,  ti.qd + 1 );  //tm->tm_mday = ti.qd + 1;
 if (0 <= ti.qm) (*defoper) ( &tm->tm_mon ,  ti.qm );  //tm->tm_mon  = ti.qm;
 if (0 <= ti.qw) (*defoper) ( &tm->tm_wday ,  ti.qw );  //tm->tm_wday = ti.qw;
 if (0 <= ti.qj) (*defoper) ( &tm->tm_yday ,  ti.qj );  //tm->tm_yday = ti.qj;

 return (char*)buf;
}
/* end of strftime.c */
