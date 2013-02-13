/*  LIBPAWNC.C
 *
 *  A "glue file" for building the Pawn compiler as a DLL or shared library.
 *
 *  Copyright (c) ITB CompuPhase, 2000-2005
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
 *
 *  Version: $Id: libpawnc.c 2969 2006-08-25 00:28:36Z dvander $
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sc.h"

#if defined PAWNC_DLL

#if !defined(__linux__) && !defined(__APPLE__)
#include "dllmain.c"
#endif

# define MAX_ARGS   100
# if !defined UNUSED_PARAM
#   define UNUSED_PARAM(p) ((void)(p))
# endif

#if PAWN_CELL_SIZE==32
#define EXCOMPILER	Compile32
#else
#define EXCOMPILER	Compile64
#endif

# if defined __WIN32__ || defined _WIN32 || defined WIN32 || defined __NT__
  __declspec (dllexport)
  void EXCOMPILER(int argc, char **argv)
# else
  void extern __attribute__((visibility("default"))) EXCOMPILER(int argc, char **argv)
# endif
  {
	  pc_compile(argc, argv);
  }
#endif /* PAWNC_DLL */


/* pc_printf()
 * Called for general purpose "console" output. This function prints general
 * purpose messages; errors go through pc_error(). The function is modelled
 * after printf().
 */
#if PAWN_CELL_SIZE==32
#if defined __WIN32__ || defined _WIN32 || defined WIN32
 __declspec (dllexport)
int pc_printf(const char *message,...)
#else
extern int __attribute__((visibility("default"))) pc_printf(const char *message,...)
#endif
#else
int pc_printf(const char *message, ...)
#endif
{
#if PAWN_CELL_SIZE==32
  int ret;
  va_list argptr;

  va_start(argptr,message);
  ret=vprintf(message,argptr);
  va_end(argptr);

  return ret;
#else
  return 1;
#endif
}

/* pc_error()
 * Called for producing error output.
 *    number      the error number (as documented in the manual)
 *    message     a string describing the error with embedded %d and %s tokens
 *    filename    the name of the file currently being parsed
 *    firstline   the line number at which the expression started on which
 *                the error was found, or -1 if there is no "starting line"
 *    lastline    the line number at which the error was detected
 *    argptr      a pointer to the first of a series of arguments (for macro
 *                "va_arg")
 * Return:
 *    If the function returns 0, the parser attempts to continue compilation.
 *    On a non-zero return value, the parser aborts.
 */
int pc_error(int number,char *message,char *filename,int firstline,int lastline,va_list argptr)
{
#if PAWN_CELL_SIZE==32
static char *prefix[3]={ "error", "fatal error", "warning" };

  if (number!=0) {
    char *pre;

    pre=prefix[number/100];
    if (firstline>=0)
      pc_printf("%s(%d -- %d) : %s %03d: ",filename,firstline,lastline,pre,number);
    else
      pc_printf("%s(%d) : %s %03d: ",filename,lastline,pre,number);
  } /* if */
  vprintf(message,argptr);
#endif
  return 0;
}

/* pc_opensrc()
 * Opens a source file (or include file) for reading. The "file" does not have
 * to be a physical file, one might compile from memory.
 *    filename    the name of the "file" to read from
 * Return:
 *    The function must return a pointer, which is used as a "magic cookie" to
 *    all I/O functions. When failing to open the file for reading, the
 *    function must return NULL.
 * Note:
 *    Several "source files" may be open at the same time. Specifically, one
 *    file can be open for reading and another for writing.
 */
void *pc_opensrc(char *filename)
{
  return fopen(filename,"rt");
}

/* pc_createsrc()
 * Creates/overwrites a source file for writing. The "file" does not have
 * to be a physical file, one might compile from memory.
 *    filename    the name of the "file" to create
 * Return:
 *    The function must return a pointer, which is used as a "magic cookie" to
 *    all I/O functions. When failing to open the file for reading, the
 *    function must return NULL.
 * Note:
 *    Several "source files" may be open at the same time. Specifically, one
 *    file can be open for reading and another for writing.
 */
void *pc_createsrc(char *filename)
{
  return fopen(filename,"wt");
}

/* pc_closesrc()
 * Closes a source file (or include file). The "handle" parameter has the
 * value that pc_opensrc() returned in an earlier call.
 */
void pc_closesrc(void *handle)
{
  assert(handle!=NULL);
  fclose((FILE*)handle);
}

/* pc_resetsrc()
 * "position" may only hold a pointer that was previously obtained from
 * pc_getpossrc()
 */
void pc_resetsrc(void *handle,void *position)
{
  assert(handle!=NULL);
  fsetpos((FILE*)handle,(fpos_t *)position);
}

/* pc_readsrc()
 * Reads a single line from the source file (or up to a maximum number of
 * characters if the line in the input file is too long).
 */
char *pc_readsrc(void *handle,unsigned char *target,int maxchars)
{
  return fgets((char*)target,maxchars,(FILE*)handle);
}

/* pc_writesrc()
 * Writes to to the source file. There is no automatic line ending; to end a
 * line, write a "\n".
 */
int pc_writesrc(void *handle,unsigned char *source)
{
  return fputs((char*)source,(FILE*)handle) >= 0;
}

void *pc_getpossrc(void *handle)
{
  static fpos_t lastpos;  /* may need to have a LIFO stack of such positions */

  fgetpos((FILE*)handle,&lastpos);
  return &lastpos;
}

int pc_eofsrc(void *handle)
{
  return feof((FILE*)handle);
}

/* should return a pointer, which is used as a "magic cookie" to all I/O
 * functions; return NULL for failure
 */
void *pc_openasm(char *filename)
{
  #if defined __MSDOS__ || defined SC_LIGHT
    return fopen(filename,"w+t");
  #else
    return mfcreate(filename);
  #endif
}

void pc_closeasm(void *handle, int deletefile)
{
  #if defined __MSDOS__ || defined SC_LIGHT
    if (handle!=NULL)
      fclose((FILE*)handle);
    if (deletefile)
      remove(outfname);
  #else
    if (handle!=NULL) {
      if (!deletefile)
        mfdump((MEMFILE*)handle);
      mfclose((MEMFILE*)handle);
    } /* if */
  #endif
}

void pc_resetasm(void *handle)
{
  assert(handle!=NULL);
  #if defined __MSDOS__ || defined SC_LIGHT
    fflush((FILE*)handle);
    fseek((FILE*)handle,0,SEEK_SET);
  #else
    mfseek((MEMFILE*)handle,0,SEEK_SET);
  #endif
}

int pc_writeasm(void *handle,char *string)
{
  #if defined __MSDOS__ || defined SC_LIGHT
    return fputs(string,(FILE*)handle) >= 0;
  #else
    return mfputs((MEMFILE*)handle,string);
  #endif
}

char *pc_readasm(void *handle, char *string, int maxchars)
{
  #if defined __MSDOS__ || defined SC_LIGHT
    return fgets(string,maxchars,(FILE*)handle);
  #else
    return mfgets((MEMFILE*)handle,string,maxchars);
  #endif
}

/* Should return a pointer, which is used as a "magic cookie" to all I/O
 * functions; return NULL for failure.
 */
void *pc_openbin(char *filename)
{
  return fopen(filename,"wb");
}

void pc_closebin(void *handle,int deletefile)
{
  fclose((FILE*)handle);
  if (deletefile)
    remove(binfname);
}

/* pc_resetbin()
 * Can seek to any location in the file.
 * The offset is always from the start of the file.
 */
void pc_resetbin(void *handle,long offset)
{
  fflush((FILE*)handle);
  fseek((FILE*)handle,offset,SEEK_SET);
}

int pc_writebin(void *handle,void *buffer,int size)
{
  return (int)fwrite(buffer,1,size,(FILE*)handle) == size;
}

long pc_lengthbin(void *handle)
{
  return ftell((FILE*)handle);
}
