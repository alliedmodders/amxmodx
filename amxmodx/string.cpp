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

const char* stristr(const char* str,const char* substr)
{
   register char *needle = (char *)substr;
   register char *prevloc = (char *)str;
   register char *haystack = (char *)str;

   while (*haystack) {
       if (tolower(*haystack) == tolower(*needle)) {
           haystack++;
           if (!*++needle)
               return prevloc;
       }
       else {
           haystack = ++prevloc;
           needle = (char *)substr;
       }
   }

   return NULL;
}

char* format_amxstring(AMX *amx, cell *params, int parm,int& len)
{
  static char buffer[2][3072];
  static char format[16];
  char *ptr,*arg;
  char *dest = *buffer;
  cell *src = get_amxaddr(amx, params[parm++]);
  int numparam = *params/sizeof(cell);
  while(*src) {
    if (*src=='%'&&*(src+1)) {
      ptr = format;
      *ptr++ = *src++;
      if (*src=='%'){
        *dest++=*src++;
        continue;
      }
      while (!isalpha(*ptr++=*src++))
        ;
      *ptr=0;
      if (numparam < parm) continue;
      arg = buffer[1];
      switch(*(ptr-1)){
      case 's':  sprintf(arg,format,get_amxstring(amx, params[parm++],2,len)); break;
      case 'f': case 'g': sprintf(arg,format,(float)*(REAL*)get_amxaddr(amx, params[parm++])); break;
      default: sprintf(arg,format,(int)*get_amxaddr(amx, params[parm++]));
      }
      while(*arg) *dest++=*arg++;
      continue;
    }
    *dest++=*src++;

  }
  *dest=0;
  len = dest - *buffer;
  return *buffer;
}

int amxstring_len(cell* a)
{
  register int c = 0;

  while( a[ c ] )
    ++c;

  return c;
}

cell* get_amxaddr(AMX *amx,cell amx_addr)
{
  return (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
}

int set_amxstring(AMX *amx,cell amx_addr,const char *source,int max)
{
  cell* dest = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
  cell* start = dest;
  while (max--&&*source)
    *dest++=(cell)*source++;
  *dest = 0;
  return dest-start;
}

char* get_amxstring(AMX *amx,cell amx_addr,int id, int& len)
{
  static char buffor[4][3072];
  register cell* source = (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
  register char* dest = buffor[id];
  char* start = dest;
  while ((*dest++=(char)(*source++)))
    ;
  len = --dest - start;
  return start;
}

void copy_amxmemory(cell* dest,cell* src,int len)
{
	while (len--)
		*dest++=*src++;
}


char* parse_arg(char** line,int& state)
{
  static char arg[3072];
  char* dest = arg;
  state = 0;
  while(**line) {
    if ( isspace(**line) ) {
      if (state == 1)
        break;
      else if (!state) {
        (*line)++;
        continue;
      }
    }
    else if (state != 2)
      state = 1;
    if (**line=='"') {
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

static cell AMX_NATIVE_CALL replace(AMX *amx, cell *params) /* 4 param */
{
  static char buffor[3072];
  cell *a = get_amxaddr(amx,params[1]);
  cell *b = get_amxaddr(amx,params[3]);
  cell *c = get_amxaddr(amx,params[4]);
  int iMain = amxstring_len(a);
  int iWhat = amxstring_len(b);
  int iWith = amxstring_len(c);
  int iPot = iMain + iWith - iWhat;
  if (iPot>=params[2]){
    amx_RaiseError(amx,AMX_ERR_NATIVE);
    return 0;
  }
  char *d = buffor;
  cell *x, *y, *z = a, *l = a;
  int p = 0;
  while(*a){
    if (*a==*b){
      x=a+1;
      y=b+1;
      p=1;
      if (!*y) break;
      while(*x==*y){
        x++; y++; p++;
        if (!*y) break;
      }
      if (!*y) break;
      p = 0;
      *d++=(char)*a++;
      continue;
    }
    *d++=(char)*a++;
  }
  if (p){
    while(*c) *d++=(char)*c++;
    a+=p;
    while(*a) *d++=(char)*a++;
    *d=0;
    d = buffor;
    while(*d) *z++=*d++;
    *z=0;
    return (z-l);
  }
  return 0;
}

static cell AMX_NATIVE_CALL contain(AMX *amx, cell *params) /* 2 param */
{
  register cell *a = get_amxaddr(amx,params[2]);
  register cell *b = get_amxaddr(amx,params[1]);
  register cell *c = b;
  cell* str = b;
  cell* substr = a;
  while (*c) {
    if (*c == *a) {
      c++;
      if (!*++a)
        return b - str;
    }
    else {
      c = ++b;
      a = substr;
    }
  }
  return -1;
}

static cell AMX_NATIVE_CALL containi(AMX *amx, cell *params) /* 2 param */
{
  register cell *a = get_amxaddr(amx,params[2]);
  register cell *b = get_amxaddr(amx,params[1]);
  register cell *c = b;
  cell* str = b;
  cell* substr = a;
  while (*c) {
    if (tolower(*c) == tolower(*a)) {
      c++;
      if (!*++a)
        return b - str;
    }
    else {
      c = ++b;
      a = substr;
    }
  }
  return -1;
}

static cell AMX_NATIVE_CALL strtonum(AMX *amx, cell *params) /* 1 param */
{
  int iLen;
  return atoi(get_amxstring(amx,params[1],0,iLen));
}

static cell AMX_NATIVE_CALL numtostr(AMX *amx, cell *params) /* 3 param */
{
  char szTemp[32];
  sprintf(szTemp,"%d",(int)params[1]);
  return set_amxstring(amx,params[2],szTemp,params[3]);
}

static cell AMX_NATIVE_CALL add(AMX *amx, cell *params) /* 4 param */
{
  cell *src = get_amxaddr(amx,params[3]);
  cell *dest = get_amxaddr(amx,params[1]);
  cell *start = dest;
  int c = params[2], d = params[4];
  while(*dest&&c--)
    ++dest;
  if (d){
    while(c--&&d--&&*src)
    *dest++=*src++;
    *dest=0;
    return (dest-start);
  }
  while(c--&&*src)
    *dest++=*src++;
  *dest=0;
  return (dest-start);
}

static cell AMX_NATIVE_CALL copy(AMX *amx, cell *params) /* 4 param */
{
  cell *src = get_amxaddr(amx,params[3]);
  cell *dest = get_amxaddr(amx,params[1]);
  cell *start = dest;
  int c = params[2];
  while(c--&&*src)
    *dest++=*src++;
  *dest=0;
  return (dest-start);
}

static cell AMX_NATIVE_CALL copyc(AMX *amx, cell *params) /* 4 param */
{
  cell *src = get_amxaddr(amx,params[3]);
  cell *dest = get_amxaddr(amx,params[1]);
  cell *start = dest;
  int c = params[2];
  cell ch = params[4];
  while(c--&&*src&&*src!=ch)
    *dest++=*src++;
  *dest=0;
  return (dest-start);
}

static cell AMX_NATIVE_CALL setc(AMX *amx, cell *params) /* 4 param */
{
  cell *src = get_amxaddr(amx,params[1]);
  int c = params[2];
  cell ch = params[3];
  while(c--)
    *src++=ch;
  return 1;
}

static cell AMX_NATIVE_CALL equal(AMX *amx, cell *params) /* 3 param */
{
  cell *a = get_amxaddr(amx,params[1]);
  cell *b = get_amxaddr(amx,params[2]);
  int c = params[3];
  if (c) {
        while (--c&&*a&&(*a==*b))
            ++a, ++b;
    return (*a-*b)?0:1;
  }
  int ret;
  while(!(ret=*a-*b)&&*b)
    ++a, ++b;
  return ret?0:1;
}

static cell AMX_NATIVE_CALL equali(AMX *amx, cell *params) /* 3 param */
{
  cell *a = get_amxaddr(amx,params[1]);
  cell *b = get_amxaddr(amx,params[2]);
  int  f,l, c = params[3];
  if (c) {
    do {
      f = tolower(*a++);
      l = tolower(*b++);
    }
    while (--c &&l&&f&& f==l);
    return(f - l)?0:1;
  }
  do {
    f = tolower(*a++);
    l = tolower(*b++);
  } while (f && f == l);
  return (f - l)?0:1;
}

static cell AMX_NATIVE_CALL format(AMX *amx, cell *params) /* 3 param */
{
  int len;
  return set_amxstring(amx,params[1],format_amxstring(amx,params,3,len),params[2]);
}

static cell AMX_NATIVE_CALL parse(AMX *amx, cell *params) /* 3 param */
{
  int inum = *params/sizeof(cell), iarg = 2, c;
  char* arg, *parse = get_amxstring(amx,params[1],0,c);
  cell *cptr;
  int state;
  while(*parse){
  arg = parse_arg(&parse,state);
    if (state){
      if (inum <= iarg)
        return( (iarg-2)>>1 );
      cptr = get_amxaddr(amx,params[iarg++]);
      c = *get_amxaddr(amx,params[iarg++]);
      while(c--&&*arg)
        *cptr++=(cell)*arg++;
      *cptr=0;
    }
  }

  return( (iarg-2)>>1 );
}

static cell AMX_NATIVE_CALL strtolower(AMX *amx, cell *params) /* 1 param */
{
  cell *cptr = get_amxaddr(amx,params[1]);
  cell *begin = cptr;
  while(*cptr){
    *cptr = tolower(*cptr);
    cptr++;
  }
  return cptr - begin;
}

static cell AMX_NATIVE_CALL strtoupper(AMX *amx, cell *params) /* 1 param */
{
  cell *cptr = get_amxaddr(amx,params[1]);
  cell *begin = cptr;
  while(*cptr){
    *cptr = toupper(*cptr);
    cptr++;
  }
  return cptr - begin;
}

int fo_numargs(AMX *amx)
{
  unsigned char *data =amx->base+(int)((AMX_HEADER *)amx->base)->dat;
  cell bytes= * (cell *)(data+(int)amx->frm+2*sizeof(cell));
  return (int)(bytes/sizeof(cell));
}

int fo_getargnum(AMX *amx, int pos)
{
  unsigned char *data =amx->base+(int)((AMX_HEADER *)amx->base)->dat;
  cell value = * (cell *)(data+(int)amx->frm+(pos+3)*sizeof(cell));
  return *(cell *)(data+(int)value);
}

float fo_getargfloat(AMX *amx, int pos)
{
  unsigned char *data =amx->base+(int)((AMX_HEADER *)amx->base)->dat;
  cell value = * (cell *)(data+(int)amx->frm+(pos+3)*sizeof(cell));
  cell number = *(cell *)(data+(int)value);
  return *(REAL *)((void *)&number);
}

char* fo_getargstr(AMX *amx, int swap, int pos)
{
  unsigned char *data =amx->base+(int)((AMX_HEADER *)amx->base)->dat;
  cell src_value= * (cell *)(data+(int)amx->frm+(pos+3)*sizeof(cell));
  cell value;
  static char buffer[2][3072];
  char* b = buffer[swap];
  int a = 0;
  do {
  value = src_value + a++ * sizeof(cell);
  value = *(cell *)(data+(int)value);
  *b++ = value;
  } while (value);

  return buffer[swap];
}

char* format_arguments(AMX *amx, int parm,int& len)
{
  static char buffer[2][3072];
  static char format[16];
  char *ptr,*arg, *dest = *buffer;
  char *src = fo_getargstr(amx, 0,parm++);
  int numparam = fo_numargs(amx);
  while(*src) {
    if (*src=='%'&&*(src+1)) {
      ptr = format;
      *ptr++ = *src++;
      if (*src=='%'){
        *dest++=*src++;
        continue;
      }
      while (!isalpha(*ptr++=*src++))
        ;
      *ptr='\0';
      if (numparam < parm) continue;
      arg = buffer[1];
      switch(*(ptr-1)){
      case 's':  sprintf(arg,format,fo_getargstr(amx,1, parm++)); break;
      case 'f': case 'g': sprintf(arg,format,fo_getargfloat(amx, parm++)); break;
      default: sprintf(arg,format,fo_getargnum(amx, parm++));
      }
      while(*arg) *dest++=*arg++;
      continue;
    }
    *dest++=*src++;
  }
  *dest='\0';
  len = dest - *buffer;
  return *buffer;
}

//added by BAILOPAN
//Takes a string and breaks it into a 1st param and rest params
//strbreak(String[], First[], FirstLen, Rest[], RestLen)
static cell AMX_NATIVE_CALL strbreak(AMX *amx, cell *params)	/* 5 param */
{
	bool quote_flag = false;
	bool done_flag = false;
	int left_pos = 0;
	int right_pos = 0;
	int l=0;
	unsigned int i=0;
	char hold = '"';

	char *string = get_amxstring(amx, params[1], 0, l);
	char *left = new char[strlen(string)+1];
	char *right = new char[strlen(string)+1];
	int LeftMax = params[3];
	int RightMax = params[5];

	for (i=0; i<strlen(string); i++) {
		if (string[i] == '"' && !quote_flag) {
			quote_flag = true;
		} else if (string[i] == '"' && quote_flag) {
			quote_flag = false;
		}
		if ((string[i] == ' ') && !quote_flag && !done_flag) {
			done_flag = true;
			i++;
		}
		if (!done_flag && string[i]!='"') {
			if (left_pos < LeftMax) {
				left[left_pos] = string[i];
				if (left[left_pos] == '\'') {
					left[left_pos] = hold;
				}
				left_pos++;
			} else {
				done_flag = true;
			}
		} else {
			if (right_pos < RightMax && string[i]!='"') {
				right[right_pos] = string[i];
				if (right[right_pos] == '\'') {
					right[right_pos] = hold;
				}
				right_pos++;
			}
		}
	}
	left[left_pos] = '\0';
	right[right_pos] = '\0';
	set_amxstring(amx, params[2], left, params[3]);
	set_amxstring(amx, params[4], right, params[5]);
	delete [] left;
	delete [] right;

	return 1;
}

static cell AMX_NATIVE_CALL format_args(AMX *amx, cell *params)
{
  int len;
  int pos = params[3];
  if (pos < 0){
     amx_RaiseError(amx,AMX_ERR_NATIVE);
     return 0;
  }
  char* string = format_arguments(amx, pos ,len); // indexed from 0
  return set_amxstring(amx,params[1],string,params[2]);
}

static cell AMX_NATIVE_CALL is_digit(AMX *amx, cell *params)
{
  return isdigit( params[1] );
}

static cell AMX_NATIVE_CALL is_alnum(AMX *amx, cell *params)
{
  return isalnum( params[1] );
}

static cell AMX_NATIVE_CALL is_space(AMX *amx, cell *params)
{
  return isspace( params[1] );
}

static cell AMX_NATIVE_CALL is_alpha(AMX *amx, cell *params)
{
  return isalpha( params[1] );
}

static cell AMX_NATIVE_CALL amx_trim(AMX *amx, cell *params)
{
	int len;
	char *asdf = get_amxstring(amx, params[1], 0, len);
	int flag = 0, incr = 0;
	register int i = 0;
	for (i=strlen(asdf); i>=0; i--)
	{
		if (!isspace(asdf[i]))
		{
			break;
		} else {
			asdf[i] = 0;
		}
	}

	len = strlen(asdf);
	
	for (i=0; i<len; i++)
	{
		if (isspace(asdf[i]) && !flag)
		{
			incr++;
			if (incr+i<len)
				asdf[i] = asdf[incr+i];
		} else {
			if (!flag)
				flag = 1;
			if (incr)
				asdf[i] = asdf[incr+i];
		}
	}
	asdf[len] = 0;

	return incr;
}

AMX_NATIVE_INFO string_Natives[] = {
  { "add",    add },
  { "contain",  contain },
  { "containi", containi },
  { "copy",   copy },
  { "copyc",    copyc },
  { "equal",    equal },
  { "equali",   equali },
  { "format",   format },
  { "format_args",   format_args },
  { "isdigit", is_digit },
  { "isalnum", is_alnum },
  { "isspace", is_space },
  { "isalpha", is_alpha },
  { "num_to_str", numtostr },
  { "parse",    parse },
  { "replace",  replace },
  { "setc",   setc },
  { "strbreak", strbreak},
  { "strtolower", strtolower },
  { "strtoupper", strtoupper },
  { "str_to_num", strtonum },
  { "trim", amx_trim },
  { NULL, NULL }
};
