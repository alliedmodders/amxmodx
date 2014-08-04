/*  Core module for the Pawn AMX
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

#if defined _UNICODE || defined __UNICODE__ || defined UNICODE
# if !defined UNICODE   /* for Windows */
#   define UNICODE
# endif
# if !defined _UNICODE  /* for C library */
#   define _UNICODE
# endif
#endif

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "amx.h"
#if defined __WIN32__ || defined _WIN32 || defined WIN32 || defined _Windows
  #include <windows.h>
#endif

/* A few compilers do not provide the ANSI C standard "time" functions */
#if !defined SN_TARGET_PS2 && !defined _WIN32_WCE
  #include <time.h>
#endif

#if defined _UNICODE
# include <tchar.h>
#elif !defined __T
  typedef char          TCHAR;
# define __T(string)    string
# define _tcschr        strchr
# define _tcscpy        strcpy
# define _tcsdup        strdup
# define _tcslen        strlen
#endif


#define CHARBITS        (8*sizeof(char))
typedef unsigned char   uchar;

#if !defined AMX_NOPROPLIST
typedef struct _property_list {
  struct _property_list *next;
  cell id;
  char *name;
  cell value;
  //??? safe AMX (owner of the property)
} proplist;

static proplist proproot = { NULL, 0, NULL, 0 };

static proplist *list_additem(proplist *root)
{
  proplist *item;

  assert(root!=NULL);
  if ((item=(proplist *)malloc(sizeof(proplist)))==NULL)
    return NULL;
  item->name=NULL;
  item->id=0;
  item->value=0;
  item->next=root->next;
  root->next=item;
  return item;
}
static void list_delete(proplist *pred,proplist *item)
{
  assert(pred!=NULL);
  assert(item!=NULL);
  pred->next=item->next;
  assert(item->name!=NULL);
  free(item->name);
  free(item);
}
static void list_setitem(proplist *item,cell id,char *name,cell value)
{
  char *ptr;

  assert(item!=NULL);
  if ((ptr=(char *)malloc(strlen(name)+1))==NULL)
    return;
  if (item->name!=NULL)
    free(item->name);
  strcpy(ptr,name);
  item->name=ptr;
  item->id=id;
  item->value=value;
}
static proplist *list_finditem(proplist *root,cell id,char *name,cell value,
                               proplist **pred)
{
  proplist *item=root->next;
  proplist *prev=root;

  /* check whether to find by name or by value */
  assert(name!=NULL);
  if (strlen(name)>0) {
    /* find by name */
    while (item!=NULL && (item->id!=id || stricmp(item->name,name)!=0)) {
      prev=item;
      item=item->next;
    } /* while */
  } else {
    /* find by value */
    while (item!=NULL && (item->id!=id || item->value!=value)) {
      prev=item;
      item=item->next;
    } /* while */
  } /* if */
  if (pred!=NULL)
    *pred=prev;
  return item;
}
#endif

static cell AMX_NATIVE_CALL numargs(AMX *amx, cell *params)
{
  AMX_HEADER *hdr;
  uchar *data;
  cell bytes;

  (void)params;
  hdr=(AMX_HEADER *)amx->base;
  data=amx->data ? amx->data : amx->base+(int)hdr->dat;
  /* the number of bytes is on the stack, at "frm + 2*cell" */
  bytes= * (cell *)(data+(int)amx->frm+2*sizeof(cell));
  /* the number of arguments is the number of bytes divided
   * by the size of a cell */
  return bytes/sizeof(cell);
}

static cell AMX_NATIVE_CALL getarg(AMX *amx, cell *params)
{
  AMX_HEADER *hdr;
  uchar *data;
  cell value;

  hdr=(AMX_HEADER *)amx->base;
  data=amx->data ? amx->data : amx->base+(int)hdr->dat;
  /* get the base value */
  value= * (cell *)(data+(int)amx->frm+((int)params[1]+3)*sizeof(cell));
  /* adjust the address in "value" in case of an array access */
  value+=params[2]*sizeof(cell);
  /* get the value indirectly */
  value= * (cell *)(data+(int)value);
  return value;
}

static cell AMX_NATIVE_CALL setarg(AMX *amx, cell *params)
{
  AMX_HEADER *hdr;
  uchar *data;
  cell value;

  hdr=(AMX_HEADER *)amx->base;
  data=amx->data ? amx->data : amx->base+(int)hdr->dat;
  /* get the base value */
  value= * (cell *)(data+(int)amx->frm+((int)params[1]+3)*sizeof(cell));
  /* adjust the address in "value" in case of an array access */
  value+=params[2]*sizeof(cell);
  /* verify the address */
  if (value<0 || (value>=amx->hea && value<amx->stk))
    return 0;
  /* set the value indirectly */
  * (cell *)(data+(int)value) = params[3];
  return 1;
}

static cell AMX_NATIVE_CALL heapspace(AMX *amx,cell *params)
{
  (void)params;
  return amx->stk - amx->hea;
}

static cell AMX_NATIVE_CALL funcidx(AMX *amx,cell *params)
{
  char name[64];
  cell *cstr;
  int index,err,len;

  amx_GetAddr(amx,params[1],&cstr);

  /* verify string length */
  amx_StrLen(cstr,&len);
  if (len>=64) {
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  } /* if */

  amx_GetString(name,cstr,0,UNLIMITED);
  err=amx_FindPublic(amx,name,&index);
  if (err!=AMX_ERR_NONE)
    index=-1;   /* this is not considered a fatal error */
  return index;
}

static cell AMX_NATIVE_CALL swapchars(AMX *amx,cell *params)
{
  union {
    cell c;
    #if PAWN_CELL_SIZE==16
      uchar b[2];
    #elif PAWN_CELL_SIZE==32
      uchar b[4];
    #elif PAWN_CELL_SIZE==64
      uchar b[8];
	#else
	  #error Unsupported cell size
    #endif
  } value;
  uchar t;

  (void)amx;
  assert((size_t)params[0]==sizeof(cell));
  value.c = params[1];
  #if PAWN_CELL_SIZE==16
    t = value.b[0];
    value.b[0] = value.b[1];
    value.b[1] = t;
  #elif PAWN_CELL_SIZE==32
    t = value.b[0];
    value.b[0] = value.b[3];
    value.b[3] = t;
    t = value.b[1];
    value.b[1] = value.b[2];
    value.b[2] = t;
  #elif PAWN_CELL_SIZE==64
    t = value.b[0];
    value.b[0] = value.b[7];
    value.b[7] = t;
	t = value.b[1];
	value.b[1] = value.b[6];
	value.b[6] = t;
	t = value.b[2];
	value.b[2] = value.b[5];
	value.b[5] = t;
	t = value.b[3];
    value.b[3] = value.b[4];
    value.b[4] = t;
  #else
    #error Unsupported cell size
  #endif
  return value.c;
}

static cell AMX_NATIVE_CALL core_tolower(AMX *amx,cell *params)
{
  (void)amx;
  #if defined __WIN32__ || defined _WIN32 || defined WIN32
    return (cell)CharLower((LPTSTR)params[1]);
  #elif defined _Windows
    return (cell)AnsiLower((LPSTR)params[1]);
  #else
    return tolower((int)params[1]);
  #endif
}

static cell AMX_NATIVE_CALL core_toupper(AMX *amx,cell *params)
{
  (void)amx;
  #if defined __WIN32__ || defined _WIN32 || defined WIN32
    return (cell)CharUpper((LPTSTR)params[1]);
  #elif defined _Windows
    return (cell)AnsiUpper((LPSTR)params[1]);
  #else
    return toupper((int)params[1]);
  #endif
}

static cell AMX_NATIVE_CALL core_min(AMX *amx,cell *params)
{
  (void)amx;
  return params[1] <= params[2] ? params[1] : params[2];
}

static cell AMX_NATIVE_CALL core_max(AMX *amx,cell *params)
{
  (void)amx;
  return params[1] >= params[2] ? params[1] : params[2];
}

static cell AMX_NATIVE_CALL core_clamp(AMX *amx,cell *params)
{
  cell value = params[1];
  if (params[2] > params[3])  /* minimum value > maximum value ! */
    amx_RaiseError(amx,AMX_ERR_NATIVE);
  if (value < params[2])
    value = params[2];
  else if (value > params[3])
    value = params[3];
  return value;
}

#if !defined AMX_NOPROPLIST
static char *MakePackedString(cell *cptr)
{
  int len;
  char *dest;

  amx_StrLen(cptr,&len);
  dest=(char *)malloc(len+sizeof(cell));
  amx_GetString(dest,cptr,0,UNLIMITED);
  return dest;
}

static int verify_addr(AMX *amx,cell addr)
{
  int err;
  cell *cdest;

  err=amx_GetAddr(amx,addr,&cdest);
  if (err!=AMX_ERR_NONE)
    amx_RaiseError(amx,err);
  return err;
}

static cell AMX_NATIVE_CALL getproperty(AMX *amx,cell *params)
{
  cell *cstr;
  char *name;
  proplist *item;

  amx_GetAddr(amx,params[2],&cstr);
  name=MakePackedString(cstr);
  item=list_finditem(&proproot,params[1],name,params[3],NULL);
  /* if list_finditem() found the value, store the name */
  if (item!=NULL && item->value==params[3] && strlen(name)==0) {
    int needed=(strlen(item->name)+sizeof(cell)-1)/sizeof(cell);     /* # of cells needed */
    if (verify_addr(amx,(cell)(params[4]+needed))!=AMX_ERR_NONE) {
      free(name);
      return 0;
    } /* if */
    amx_GetAddr(amx,params[4],&cstr);
    amx_SetString(cstr,item->name,1,0,UNLIMITED);
  } /* if */
  free(name);
  return (item!=NULL) ? item->value : 0;
}

static cell AMX_NATIVE_CALL setproperty(AMX *amx,cell *params)
{
  cell prev=0;
  cell *cstr;
  char *name;
  proplist *item;

  amx_GetAddr(amx,params[2],&cstr);
  name=MakePackedString(cstr);
  item=list_finditem(&proproot,params[1],name,params[3],NULL);
  if (item==NULL)
    item=list_additem(&proproot);
  if (item==NULL) {
    amx_RaiseError(amx,AMX_ERR_MEMORY);
  } else {
    prev=item->value;
    if (strlen(name)==0) {
      free(name);
      amx_GetAddr(amx,params[4],&cstr);
      name=MakePackedString(cstr);
    } /* if */
    list_setitem(item,params[1],name,params[3]);
  } /* if */
  free(name);
  return prev;
}

static cell AMX_NATIVE_CALL delproperty(AMX *amx,cell *params)
{
  cell prev=0;
  cell *cstr;
  char *name;
  proplist *item,*pred;

  amx_GetAddr(amx,params[2],&cstr);
  name=MakePackedString(cstr);
  item=list_finditem(&proproot,params[1],name,params[3],&pred);
  if (item!=NULL) {
    prev=item->value;
    list_delete(pred,item);
  } /* if */
  free(name);
  return prev;
}

static cell AMX_NATIVE_CALL existproperty(AMX *amx,cell *params)
{
  cell *cstr;
  char *name;
  proplist *item;

  amx_GetAddr(amx,params[2],&cstr);
  name=MakePackedString(cstr);
  item=list_finditem(&proproot,params[1],name,params[3],NULL);
  free(name);
  return (item!=NULL);
}
#endif

#if !defined AMX_NORANDOM
/* This routine comes from the book "Inner Loops" by Rick Booth, Addison-Wesley
 * (ISBN 0-201-47960-5). This is a "multiplicative congruential random number
 * generator" that has been extended to 31-bits (the standard C version returns
 * only 15-bits).
 */
#define INITIAL_SEED  0xcaa938dbL
static unsigned long IL_StandardRandom_seed = INITIAL_SEED; /* always use a non-zero seed */
#define IL_RMULT 1103515245L
#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
static cell AMX_NATIVE_CALL core_random(AMX *amx,cell *params)
{
    unsigned long lo, hi, ll, lh, hh, hl;
    unsigned long result;

    /* one-time initialization (or, mostly one-time) */
    #if !defined SN_TARGET_PS2 && !defined _WIN32_WCE
        if (IL_StandardRandom_seed == INITIAL_SEED)
            IL_StandardRandom_seed=(unsigned long)time(NULL);
    #endif

    lo = IL_StandardRandom_seed & 0xffff;
    hi = IL_StandardRandom_seed >> 16;
    IL_StandardRandom_seed = IL_StandardRandom_seed * IL_RMULT + 12345;
    ll = lo * (IL_RMULT  & 0xffff);
    lh = lo * (IL_RMULT >> 16    );
    hl = hi * (IL_RMULT  & 0xffff);
    hh = hi * (IL_RMULT >> 16    );
    result = ((ll + 12345) >> 16) + lh + hl + (hh << 16);
    result &= ~LONG_MIN;        /* remove sign bit */
    if (params[1]!=0)
        result %= params[1];
    return (cell)result;
}
#endif


AMX_NATIVE_INFO core_Natives[] = {
  { "numargs",       numargs },
  { "getarg",        getarg },
  { "setarg",        setarg },
  { "heapspace",     heapspace },
  { "funcidx",       funcidx },
  { "swapchars",     swapchars },
  { "tolower",       core_tolower },
  { "toupper",       core_toupper },
  { "min",           core_min },
  { "max",           core_max },
  { "clamp",         core_clamp },
  { "random",        core_random },
  { NULL, NULL }        /* terminator */
};

int AMXEXPORT amx_CoreInit(AMX *amx)
{
  return amx_Register(amx, core_Natives, -1);
}

int AMXEXPORT amx_CoreCleanup(AMX *amx)
{
  (void)amx;
  #if !defined AMX_NOPROPLIST
    //??? delete only the properties owned by the AMX
    while (proproot.next!=NULL)
      list_delete(&proproot,proproot.next);
  #endif
  return AMX_ERR_NONE;
}
