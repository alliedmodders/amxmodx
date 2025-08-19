/*  Pawn compiler - Error message system
 *  In fact a very simple system, using only 'panic mode'.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2005
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 */

#include <assert.h>
#if defined __WIN32__ || defined _WIN32 || defined __MSDOS__
  #include <io.h>
#endif
#if defined LINUX || defined __APPLE__ || defined __GNUC__
  #include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>     /* ANSI standardized variable argument list functions */
#include <string.h>
#if defined FORTIFY
  #include "fortify.h"
#endif
#include "sc.h"

#if defined _MSC_VER
  #pragma warning(push)
  #pragma warning(disable:4125)  /* decimal digit terminates octal escape sequence */
#endif

#include "sc5-in.scp"

#if defined _MSC_VER
  #pragma warning(pop)
#endif

#define NUM_WARNINGS    (sizeof warnmsg / sizeof warnmsg[0])
static struct s_warnstack {
  unsigned char disable[(NUM_WARNINGS + 7) / 8]; /* 8 flags in a char */
  struct s_warnstack *next;
} warnstack;

static int errflag;
static int errfile;
static int errstart;    /* line number at which the instruction started */
static int errline;     /* forced line number for the error message */

/*  error
 *
 *  Outputs an error message (note: msg is passed optionally).
 *  If an error is found, the variable "errflag" is set and subsequent
 *  errors are ignored until lex() finds a semicolumn or a keyword
 *  (lex() resets "errflag" in that case).
 *
 *  Global references: inpfname   (reffered to only)
 *                     fline      (reffered to only)
 *                     fcurrent   (reffered to only)
 *                     errflag    (altered)
 */
SC_FUNC int error(int number,...)
{
static char *prefix[3]={ "error", "fatal error", "warning" };
static int lastline,errorcount;
static short lastfile;
  char *msg,*pre,*filename;
  va_list argptr;
  int is_warning;

  is_warning = (number >= 200 && !sc_warnings_are_errors);

  /* errflag is reset on each semicolon.
   * In a two-pass compiler, an error should not be reported twice. Therefore
   * the error reporting is enabled only in the second pass (and only when
   * actually producing output). Fatal errors may never be ignored.
   */
  if ((errflag || sc_status!=statWRITE) && (number<100 || number>=200))
    return 0;

  /* also check for disabled warnings */
  if (number>=200) {
    int index=(number-200)/8;
    int mask=1 << ((number-200)%8);
    if ((warnstack.disable[index] & mask)!=0)
      return 0;
  } /* if */

  if (number<100){
    msg=errmsg[number-1];
    pre=prefix[0];
    errflag=TRUE;       /* set errflag (skip rest of erroneous expression) */
    errnum++;
  } else if (number<200){
    msg=fatalmsg[number-100];
    pre=prefix[1];
    errnum++;           /* a fatal error also counts as an error */
  } else {
    msg=warnmsg[number-200];
    if (sc_warnings_are_errors) {
      pre=prefix[0];
      errnum++;
    } else {
      pre=prefix[2];
      warnnum++;
    }
  } /* if */

 if (errline>0)
    errstart=errline;           /* forced error position, set single line destination */
  else
    errline=fline;              /* normal error, errstart may (or may not) have been marked, endpoint is current line */
  if (errstart>errline)
    errstart=errline;           /* special case: error found at end of included file */
  if (errfile>=0)
    filename=get_inputfile(errfile);/* forced filename */
  else
    filename=inpfname;          /* current file */
  assert(filename!=NULL);

  va_start(argptr,number);
  if (strlen(errfname)==0) {
    int start= (errstart==errline) ? -1 : errstart;
    if (pc_error((int)number,msg,filename,start,errline,argptr)) {
      if (outf!=NULL) {
        pc_closeasm(outf,TRUE);
        outf=NULL;
      } /* if */
      longjmp(errbuf,3);        /* user abort */
    } /* if */
  } else {
    FILE *fp=fopen(errfname,"a");
    if (fp!=NULL) {
      if (errstart>=0 && errstart!=errline)
        fprintf(fp,"%s(%d -- %d) : %s %03d: ",filename,errstart,errline,pre,number);
      else
        fprintf(fp,"%s(%d) : %s %03d: ",filename,errline,pre,number);
      vfprintf(fp,msg,argptr);
      fclose(fp);
    } /* if */
  } /* if */
  va_end(argptr);

  if ((number>=100 && number<200) || errnum>25){
    if (strlen(errfname)==0) {
      va_start(argptr,number);
      pc_error(0,"\nCompilation aborted.",NULL,0,0,argptr);
      va_end(argptr);
    } /* if */
    if (outf!=NULL) {
      pc_closeasm(outf,TRUE);
      outf=NULL;
    } /* if */
    longjmp(errbuf,2);          /* fatal error, quit */
  } /* if */

  errline=-1;
  errfile=-1;
  /* check whether we are seeing many errors on the same line */
  if ((errstart<0 && lastline!=fline) || lastline<errstart || lastline>fline || fcurrent!=lastfile)
    errorcount=0;
  lastline=fline;
  lastfile=fcurrent;
  if (!is_warning)
    errorcount++;
  if (errorcount>=3)
    error(107);         /* too many error/warning messages on one line */

  return 0;
}

SC_FUNC void errorset(int code,int line)
{
  switch (code) {
  case sRESET:
    errflag=FALSE;      /* start reporting errors */
    break;
  case sFORCESET:
    errflag=TRUE;       /* stop reporting errors */
    break;
  case sEXPRMARK:
    errstart=fline;     /* save start line number */
    break;
  case sEXPRRELEASE:
    errstart=-1;        /* forget start line number */
    errline=-1;
    errfile=-1;
    break;
  case sSETLINE:
    errstart=-1;        /* force error line number, forget start line */
    errline=line;
    break;
  case sSETFILE:
    errfile=line;
    break;
  } /* switch */
}

/* sc_enablewarning()
 * Enables or disables a warning (errors cannot be disabled).
 * Initially all warnings are enabled. The compiler does this by setting bits
 * for the *disabled* warnings and relying on the array to be zero-initialized.
 *
 * Parameter enable can be:
 *  o  0 for disable
 *  o  1 for enable
 *  o  2 for toggle
 */
int pc_enablewarning(int number,warnmode enable)
{
  int index;
  unsigned char mask;

  if (number<200)
    return FALSE;       /* errors and fatal errors cannot be disabled */
  number-=200;
  if (number>=NUM_WARNINGS)
    return FALSE;

  index=number/8;
  mask=(unsigned char)(1 << (number%8));
  switch (enable) {
  case warnDISABLE:
    warnstack.disable[index] |= mask;
    break;
  case warnENABLE:
    warnstack.disable[index] &= (unsigned char)~mask;
    break;
  case warnTOGGLE:
    warnstack.disable[index] ^= mask;
    break;
  } /* switch */

  return TRUE;
}

/* pc_pushwarnings()
 * Saves currently disabled warnings, used to implement #pragma warning push
 */
void pc_pushwarnings(void)
{
  void *p;
  p=calloc(sizeof(struct s_warnstack),1);
  if (p==NULL)
    error(103); /* insufficient memory */
  memmove(p,&warnstack,sizeof(struct s_warnstack));
  warnstack.next=p;
}

/* pc_popwarnings()
 * This function is the reverse of pc_pushwarnings()
 */
void pc_popwarnings(void)
{
  void *p=warnstack.next;
  if (p==NULL) { /* nothing to do */
    error(235,"#pragma warning pop","#pragma warning push");
    return;
  } /* if */

  memmove(&warnstack,p,sizeof(struct s_warnstack));
  free(p);
}

SC_FUNC void warnstack_init(void)
{
  memset(&warnstack,0,sizeof(warnstack));
}

SC_FUNC void warnstack_cleanup(void)
{
  struct s_warnstack *cur=warnstack.next,*next;
  if (cur!=NULL)
    error(1,"#pragma warning pop","-end of file-");

  for (;cur!=NULL;cur=next) {
    next=cur->next;
    free(cur);
  } /* for */
  warnstack.next=NULL;
}
#undef SCPACK_TABLE
