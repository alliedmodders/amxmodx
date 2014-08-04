/*  Pawn debugger interface
 *
 *  Support functions for debugger applications
 *
 *  Copyright (c) ITB CompuPhase, 2005
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "osdefs.h"     /* for _MAX_PATH */
#include "amx.h"
#include "amxdbg.h"

// this file does not include amxmodx.h, so we have to include the memory manager here
#ifdef MEMORY_TEST
#include "mmgr/mmgr.h"
#endif // MEMORY_TEST

int AMXAPI dbg_FreeInfo(AMX_DBG *amxdbg)
{
  assert(amxdbg != NULL);
  if (amxdbg->hdr != NULL)
    free(amxdbg->hdr);
  if (amxdbg->filetbl != NULL)
    free(amxdbg->filetbl);
  if (amxdbg->symboltbl != NULL)
    free(amxdbg->symboltbl);
  if (amxdbg->tagtbl != NULL)
    free(amxdbg->tagtbl);
  if (amxdbg->automatontbl != NULL)
    free(amxdbg->automatontbl);
  if (amxdbg->statetbl != NULL)
    free(amxdbg->statetbl);
  memset(amxdbg, 0, sizeof(AMX_DBG));
  return AMX_ERR_NONE;
}

void memread(void *dest, char **src, size_t size)
{
	void *ptr = *src;
	memcpy(dest, ptr, size);
	*src += size;
}

const char *ClipFileName(const char *inp)
{
	static char buffer[256];
	size_t len = strlen(inp);
	const char *ptr = inp;

	for (size_t i=0; i<len; i++)
	{
		if ((inp[i] == '\\' || inp[i] == '/') && (i != len-1))
			ptr = inp + i + 1;
	}
	strcpy(buffer, ptr);

	return buffer;
}

//Note - I changed this function to read from memory instead.
// -- BAILOPAN
int AMXAPI dbg_LoadInfo(AMX_DBG *amxdbg, void *dbg_addr)
{
  AMX_DBG_HDR dbghdr;
  unsigned char *ptr;
  int index, dim;
  AMX_DBG_SYMDIM *symdim;

  assert(amxdbg != NULL);

  char *addr = (char *)(dbg_addr);

  memset(&dbghdr, 0, sizeof(AMX_DBG_HDR));
  memread(&dbghdr, &addr, sizeof(AMX_DBG_HDR));

  //brabbby graa gragghty graaahhhh
  #if BYTE_ORDER==BIG_ENDIAN
    amx_Align32((uint32_t*)&dbghdr.size);
    amx_Align16(&dbghdr.magic);
    amx_Align16(&dbghdr.flags);
    amx_Align16(&dbghdr.files);
    amx_Align16(&dbghdr.lines);
    amx_Align16(&dbghdr.symbols);
    amx_Align16(&dbghdr.tags);
    amx_Align16(&dbghdr.automatons);
    amx_Align16(&dbghdr.states);
  #endif

  if (dbghdr.magic != AMX_DBG_MAGIC)
    return AMX_ERR_FORMAT;

  /* allocate all memory */
  memset(amxdbg, 0, sizeof(AMX_DBG));
  amxdbg->hdr = (AMX_DBG_HDR *)malloc((size_t)dbghdr.size);
  if (dbghdr.files > 0)
    amxdbg->filetbl = (AMX_DBG_FILE **)malloc(dbghdr.files * sizeof(AMX_DBG_FILE *));
  if (dbghdr.symbols > 0)
    amxdbg->symboltbl = (AMX_DBG_SYMBOL **)malloc(dbghdr.symbols * sizeof(AMX_DBG_SYMBOL *));
  if (dbghdr.tags > 0)
    amxdbg->tagtbl = (AMX_DBG_TAG **)malloc(dbghdr.tags * sizeof(AMX_DBG_TAG *));
  if (dbghdr.automatons > 0)
    amxdbg->automatontbl = (AMX_DBG_MACHINE **)malloc(dbghdr.automatons * sizeof(AMX_DBG_MACHINE *));
  if (dbghdr.states > 0)
    amxdbg->statetbl = (AMX_DBG_STATE **)malloc(dbghdr.states * sizeof(AMX_DBG_STATE *));
  if (amxdbg->hdr == NULL
      || (dbghdr.files > 0 && amxdbg->filetbl == NULL)
      || (dbghdr.symbols > 0 && amxdbg->symboltbl == NULL)
      || (dbghdr.tags > 0 && amxdbg->tagtbl == NULL)
      || (dbghdr.states > 0 && amxdbg->statetbl == NULL)
      || (dbghdr.automatons > 0 && amxdbg->automatontbl == NULL))
  {
    dbg_FreeInfo(amxdbg);
    return AMX_ERR_MEMORY;
  } /* if */

  /* load the entire symbolic information block into memory */
  memcpy(amxdbg->hdr, &dbghdr, sizeof dbghdr);
  ptr = (unsigned char *)(amxdbg->hdr + 1);
  memread(ptr, &addr, (size_t)(dbghdr.size-sizeof(dbghdr)));

  /* file table */
  for (index = 0; index < dbghdr.files; index++) {
    assert(amxdbg->filetbl != NULL);
    amxdbg->filetbl[index] = (AMX_DBG_FILE *)ptr;
    #if BYTE_ORDER==BIG_ENDIAN
      amx_AlignCell(&amxdbg->filetbl[index]->address);
    #endif
    for (ptr = ptr + sizeof(AMX_DBG_FILE); *ptr != '\0'; ptr++)
      /* nothing */;
    ptr++;              /* skip '\0' too */
  } /* for */

  //debug("Files: %d\n", amxdbg->hdr->files);
  for (index=0;index<amxdbg->hdr->files; index++)
  {
	  strcpy((char *)amxdbg->filetbl[index]->name, ClipFileName(amxdbg->filetbl[index]->name));
	  //debug(" [%d] %s\n", index, amxdbg->filetbl[index]->name);
  }

  /* line table */
  amxdbg->linetbl = (AMX_DBG_LINE*)ptr;
  #if BYTE_ORDER==BIG_ENDIAN
    for (index = 0; index < dbghdr.lines; index++) {
      amx_AlignCell(&amxdbg->linetbl[index].address);
      amx_Align32((uint32_t*)&amxdbg->linetbl[index].line);
    } /* for */
  #endif
  ptr += dbghdr.lines * sizeof(AMX_DBG_LINE);

  /* symbol table (plus index tags) */
  for (index = 0; index < dbghdr.symbols; index++) {
    assert(amxdbg->symboltbl != NULL);
    amxdbg->symboltbl[index] = (AMX_DBG_SYMBOL *)ptr;
    #if BYTE_ORDER==BIG_ENDIAN
      amx_AlignCell(&amxdbg->symboltbl[index]->address);
      amx_Align16((uint16_t*)&amxdbg->symboltbl[index]->tag);
      amx_AlignCell(&amxdbg->symboltbl[index]->codestart);
      amx_AlignCell(&amxdbg->symboltbl[index]->codeend);
      amx_Align16((uint16_t*)&amxdbg->symboltbl[index]->dim);
    #endif
    for (ptr = ptr + sizeof(AMX_DBG_SYMBOL); *ptr != '\0'; ptr++)
      /* nothing */;
    ptr++;              /* skip '\0' too */
    for (dim = 0; dim < amxdbg->symboltbl[index]->dim; dim++) {
      symdim = (AMX_DBG_SYMDIM *)ptr;
      amx_Align16((uint16_t*)&symdim->tag);
      amx_AlignCell(&symdim->size);
      ptr += sizeof(AMX_DBG_SYMDIM);
    } /* for */
  } /* for */

  /* tag name table */
  for (index = 0; index < dbghdr.tags; index++) {
    assert(amxdbg->tagtbl != NULL);
    amxdbg->tagtbl[index] = (AMX_DBG_TAG *)ptr;
    #if BYTE_ORDER==BIG_ENDIAN
      amx_Align16(&amxdbg->tagtbl[index]->tag);
    #endif
    for (ptr = ptr + sizeof(AMX_DBG_TAG) - 1; *ptr != '\0'; ptr++)
      /* nothing */;
    ptr++;              /* skip '\0' too */
  } /* for */

  /* automaton name table */
  for (index = 0; index < dbghdr.automatons; index++) {
    assert(amxdbg->automatontbl != NULL);
    amxdbg->automatontbl[index] = (AMX_DBG_MACHINE *)ptr;
    #if BYTE_ORDER==BIG_ENDIAN
      amx_Align16(&amxdbg->automatontbl[index]->automaton);
      amx_AlignCell(&amxdbg->automatontbl[index]->address);
    #endif
    for (ptr = ptr + sizeof(AMX_DBG_MACHINE) - 1; *ptr != '\0'; ptr++)
      /* nothing */;
    ptr++;              /* skip '\0' too */
  } /* for */

  /* state name table */
  for (index = 0; index < dbghdr.states; index++) {
    assert(amxdbg->statetbl != NULL);
    amxdbg->statetbl[index] = (AMX_DBG_STATE *)ptr;
    #if BYTE_ORDER==BIG_ENDIAN
      amx_Align16(&amxdbg->statetbl[index]->state);
      amx_Align16(&amxdbg->automatontbl[index]->automaton);
    #endif
    for (ptr = ptr + sizeof(AMX_DBG_STATE) - 1; *ptr != '\0'; ptr++)
      /* nothing */;
    ptr++;              /* skip '\0' too */
  } /* for */

  return AMX_ERR_NONE;
}

int AMXAPI dbg_LookupFile(AMX_DBG *amxdbg, ucell address, const char **filename)
{
  int index;

  assert(amxdbg != NULL);
  assert(filename != NULL);
  *filename = NULL;
  /* this is a simple linear look-up; a binary search would be possible too */
  for (index = 0; index < amxdbg->hdr->files && amxdbg->filetbl[index]->address <= address; index++)
    /* nothing */;
  /* reset for overrun */
  if (--index < 0)
    return AMX_ERR_NOTFOUND;

  *filename = amxdbg->filetbl[index]->name;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_LookupLine(AMX_DBG *amxdbg, ucell address, long *line)
{
  int index;

  assert(amxdbg != NULL);
  assert(line != NULL);
  *line = 0;
  /* this is a simple linear look-up; a binary search would be possible too */
  for (index = 0; index < amxdbg->hdr->lines && amxdbg->linetbl[index].address <= address; index++)
    /* nothing */;
  /* reset for overrun */
  if (--index < 0)
    return AMX_ERR_NOTFOUND;

  *line = (long)amxdbg->linetbl[index].line;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_LookupFunction(AMX_DBG *amxdbg, ucell address, const char **funcname)
{
  /* dbg_LookupFunction() finds the function a code address is in. It can be
   * used for stack walking, and for stepping through a function while stepping
   * over sub-functions
   */
  int index;

  assert(amxdbg != NULL);
  assert(funcname != NULL);
  *funcname = NULL;
  for (index = 0; index < amxdbg->hdr->symbols; index++) {
    if (amxdbg->symboltbl[index]->ident == iFUNCTN
        && amxdbg->symboltbl[index]->codestart <= address
        && amxdbg->symboltbl[index]->codeend > address)
      break;
  } /* for */
  if (index >= amxdbg->hdr->symbols)
    return AMX_ERR_NOTFOUND;

  *funcname = amxdbg->symboltbl[index]->name;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetTagName(AMX_DBG *amxdbg, int tag, const char **name)
{
  int index;

  assert(amxdbg != NULL);
  assert(name != NULL);
  *name = NULL;
  for (index = 0; index < amxdbg->hdr->tags && amxdbg->tagtbl[index]->tag != tag; index++)
    /* nothing */;
  if (index >= amxdbg->hdr->tags)
    return AMX_ERR_NOTFOUND;

  *name = amxdbg->tagtbl[index]->name;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetAutomatonName(AMX_DBG *amxdbg, int automaton, const char **name)
{
  int index;

  assert(amxdbg != NULL);
  assert(name != NULL);
  *name = NULL;
  for (index = 0; index < amxdbg->hdr->automatons && amxdbg->automatontbl[index]->automaton != automaton; index++)
    /* nothing */;
  if (index >= amxdbg->hdr->automatons)
    return AMX_ERR_NOTFOUND;

  *name = amxdbg->automatontbl[index]->name;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetStateName(AMX_DBG *amxdbg, int state, const char **name)
{
  int index;

  assert(amxdbg != NULL);
  assert(name != NULL);
  *name = NULL;
  for (index = 0; index < amxdbg->hdr->states && amxdbg->statetbl[index]->state != state; index++)
    /* nothing */;
  if (index >= amxdbg->hdr->states)
    return AMX_ERR_NOTFOUND;

  *name = amxdbg->statetbl[index]->name;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetLineAddress(AMX_DBG *amxdbg, long line, const char *filename, ucell *address)
{
  /* Find a suitable "breakpoint address" close to the indicated line (and in
   * the specified file). The address is moved up to the next "breakable" line
   * if no "breakpoint" is available on the specified line. You can use function
   * dbg_LookupLine() to find out at which precise line the breakpoint was set.
   *
   * The filename comparison is strict (case sensitive and path sensitive); the
   * "filename" parameter should point into the "filetbl" of the AMX_DBG
   * structure.
   */
  int file, index;
  ucell bottomaddr,topaddr;

  assert(amxdbg != NULL);
  assert(filename != NULL);
  assert(address != NULL);
  *address = 0;

  index = 0;
  for (file = 0; file < amxdbg->hdr->files; file++) {
    /* find the (next) mathing instance of the file */
    if (strcmp(amxdbg->filetbl[file]->name, filename) != 0)
      continue;
    /* get address range for the current file */
    bottomaddr = amxdbg->filetbl[file]->address;
    topaddr = (file + 1 < amxdbg->hdr->files) ? amxdbg->filetbl[file+1]->address : (ucell)(cell)-1;
    /* go to the starting address in the line table */
    while (index < amxdbg->hdr->lines && amxdbg->linetbl[index].address < bottomaddr)
      index++;
    /* browse until the line is found or until the top address is exceeded */
    while (index < amxdbg->hdr->lines
           && amxdbg->linetbl[index].line < line
           && amxdbg->linetbl[index].address < topaddr)
      index++;
    if (index >= amxdbg->hdr->lines)
      return AMX_ERR_NOTFOUND;
    if (amxdbg->linetbl[index].line >= line)
      break;
    /* if not found (and the line table is not yet exceeded) try the next
     * instance of the same file (a file may appear twice in the file table)
     */
  } /* for */

  if (strcmp(amxdbg->filetbl[file]->name, filename) != 0)
    return AMX_ERR_NOTFOUND;

  assert(index < amxdbg->hdr->lines);
  *address = amxdbg->linetbl[index].address;
  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetFunctionAddress(AMX_DBG *amxdbg, const char *funcname, const char *filename, ucell *address)
{
  /* Find a suitable "breakpoint address" close to the indicated line (and in
   * the specified file). The address is moved up to the first "breakable" line
   * in the function. You can use function dbg_LookupLine() to find out at which
   * precise line the breakpoint was set.
   *
   * The filename comparison is strict (case sensitive and path sensitive); the
   * "filename" parameter should point into the "filetbl" of the AMX_DBG
   * structure. The function name comparison is case sensitive too.
   */
  int index, err;
  const char *tgtfile;
  ucell funcaddr;

  assert(amxdbg != NULL);
  assert(funcname != NULL);
  assert(filename != NULL);
  assert(address != NULL);
  *address = 0;

  index = 0;
  for ( ;; ) {
    /* find (next) matching function */
    while (index < amxdbg->hdr->symbols
           && (amxdbg->symboltbl[index]->ident != iFUNCTN || strcmp(amxdbg->symboltbl[index]->name, funcname) != 0))
      index++;
    if (index >= amxdbg->hdr->symbols)
      return AMX_ERR_NOTFOUND;
    /* verify that this line falls in the appropriate file */
    err = dbg_LookupFile(amxdbg, amxdbg->symboltbl[index]->address, &tgtfile);
    if (err == AMX_ERR_NONE || strcmp(filename, tgtfile) == 0)
      break;
    index++;            /* line is the wrong file, search further */
  } /* for */

  /* now find the first line in the function where we can "break" on */
  assert(index < amxdbg->hdr->symbols);
  funcaddr = amxdbg->symboltbl[index]->address;
  for (index = 0; index < amxdbg->hdr->lines && amxdbg->linetbl[index].address < funcaddr; index++)
    /* nothing */;

  if (index >= amxdbg->hdr->lines)
    return AMX_ERR_NOTFOUND;
  *address = amxdbg->linetbl[index].address;

  return AMX_ERR_NONE;
}

int AMXAPI dbg_GetVariable(AMX_DBG *amxdbg, const char *symname, ucell scopeaddr, const AMX_DBG_SYMBOL **sym)
{
  ucell codestart,codeend;
  int index;

  assert(amxdbg != NULL);
  assert(symname != NULL);
  assert(sym != NULL);
  *sym = NULL;

  codestart = codeend = 0;
  index = 0;
  for ( ;; ) {
    /* find (next) matching variable */
    while (index < amxdbg->hdr->symbols
           && (amxdbg->symboltbl[index]->ident == iFUNCTN || strcmp(amxdbg->symboltbl[index]->name, symname) != 0)
           && (amxdbg->symboltbl[index]->codestart > scopeaddr || amxdbg->symboltbl[index]->codeend < scopeaddr))
      index++;
    if (index >= amxdbg->hdr->symbols)
      break;
    /* check the range, keep a pointer to the symbol with the smallest range */
    if (strcmp(amxdbg->symboltbl[index]->name, symname) == 0
        && ((codestart == 0 && codeend == 0)
            || (amxdbg->symboltbl[index]->codestart >= codestart && amxdbg->symboltbl[index]->codeend <= codeend)))
    {
      *sym = amxdbg->symboltbl[index];
      codestart = amxdbg->symboltbl[index]->codestart;
      codeend = amxdbg->symboltbl[index]->codeend;
    } /* if */
    index++;
  } /* for */

  return (*sym == NULL) ? AMX_ERR_NOTFOUND : AMX_ERR_NONE;
}

int AMXAPI dbg_GetArrayDim(AMX_DBG *amxdbg, const AMX_DBG_SYMBOL *sym, const AMX_DBG_SYMDIM **symdim)
{
  /* retrieves a pointer to the array dimensions structures of an array symbol */
  const char *ptr;

  assert(amxdbg != NULL);
  assert(sym != NULL);
  assert(symdim != NULL);
  *symdim = NULL;

  if (sym->ident != iARRAY && sym->ident != iREFARRAY)
    return AMX_ERR_PARAMS;
  assert(sym->dim > 0); /* array must have at least one dimension */

  /* find the end of the symbol name */
  for (ptr = sym->name; *ptr != '\0'; ptr++)
    /* nothing */;
  *symdim = (AMX_DBG_SYMDIM *)(ptr + 1);/* skip '\0' too */

  return AMX_ERR_NONE;
}
