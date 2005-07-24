/*  Pawn compiler
 *
 *  Routines to maintain a "text file" in memory.
 *
 *  Copyright (c) ITB CompuPhase, 2003-2005
 *
 *  This software is provided 'as-is', without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from the
 *  use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented; you must not
 *     claim that you wrote the original software. If you use this software in
 *     a product, an acknowledgment in the product documentation would be
 *     appreciated but is not required.
 *
 *  2. Altered source versions must be plainly marked as such, and must not be
 *     misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined FORTIFY
  #include "fortify.h"
#endif


#define BUFFERSIZE 512u

/* For every block, except the first:
 *   buffer points to a block that is BUFFERSIZE long that holds the data
 *   bufpos is the "used size" of the block
 * For the first block:
 *   buffer points to the "file name"
 *   bufpos is the current "file pointer"
 */
typedef struct tagMEMFILE {
  struct tagMEMFILE *next;
  unsigned char *buffer;
  long bufpos;
} MEMFILE;
#define tMEMFILE  1

#include "sc.h"


MEMFILE *mfcreate(char *filename)
{
  MEMFILE *mf;

  /* create a first block that only holds the name */
  mf=(MEMFILE*)malloc(sizeof(MEMFILE));
  if (mf==NULL)
    return NULL;
  memset(mf,0,sizeof(MEMFILE));
  mf->buffer=(unsigned char*)strdup(filename);
  if (mf->buffer==NULL) {
    free(mf);
    return NULL;
  } /* if */
  return mf;
}

void mfclose(MEMFILE *mf)
{
  MEMFILE *next;

  assert(mf!=NULL);
  while (mf!=NULL) {
    next=mf->next;
    assert(mf->buffer!=NULL);
    free(mf->buffer);
    free(mf);
    mf=next;
  } /* while */
}

int mfdump(MEMFILE *mf)
{
  FILE *fp;
  int okay;

  assert(mf!=NULL);
  /* create the file */
  fp=fopen((char*)mf->buffer,"wb");
  if (fp==NULL)
    return 0;

  okay=1;
  mf=mf->next;
  while (mf!=NULL) {
    assert(mf->buffer!=NULL);
    /* all blocks except the last should be fully filled */
    assert(mf->next==NULL || (unsigned long)mf->bufpos==BUFFERSIZE);
    okay=okay && fwrite(mf->buffer,1,(size_t)mf->bufpos,fp)==(size_t)mf->bufpos;
    mf=mf->next;
  } /* while */

  fclose(fp);
  return okay;
}

long mflength(MEMFILE *mf)
{
  long length;

  assert(mf!=NULL);
  /* find the size of the memory file */
  length=0L;
  mf=mf->next;          /* skip initial block */
  while (mf!=NULL) {
    assert(mf->next==NULL || (unsigned long)mf->bufpos==BUFFERSIZE);
    length+=mf->bufpos;
    mf=mf->next;
  } /* while */

  return length;
}

long mfseek(MEMFILE *mf,long offset,int whence)
{
  long length;

  assert(mf!=NULL);
  if (mf->next==NULL)
    return 0L;          /* early exit: not a single byte in the file */

  /* find the size of the memory file */
  length=mflength(mf);

  /* convert the offset to an absolute position */
  switch (whence) {
  case SEEK_SET:
    break;
  case SEEK_CUR:
    offset+=mf->bufpos;
    break;
  case SEEK_END:
    assert(offset<=0);
    offset+=length;
    break;
  } /* switch */

  /* clamp to the file length limit */
  if (offset<0)
    offset=0;
  else if (offset>length)
    offset=length;

  /* set new position and return it */
  mf->bufpos=offset;
  return offset;
}

unsigned int mfwrite(MEMFILE *mf,unsigned char *buffer,unsigned int size)
{
  long length;
  long numblocks;
  int blockpos,blocksize;
  unsigned int bytes;
  MEMFILE *block;

  assert(mf!=NULL);

  /* see whether more memory must be allocated */
  length=mflength(mf);
  assert(mf->bufpos>=0 && mf->bufpos<=length);
  numblocks=(length+BUFFERSIZE-1)/BUFFERSIZE;   /* # allocated blocks */
  while (mf->bufpos+size>numblocks*BUFFERSIZE) {
    /* append a block */
    MEMFILE *last;
    block=(MEMFILE*)malloc(sizeof(MEMFILE));
    if (block==NULL)
      return 0;
    memset(block,0,sizeof(MEMFILE));
    block->buffer=(unsigned char*)malloc(BUFFERSIZE);
    if (block->buffer==NULL) {
      free(block);
      return 0;
    } /* if */
    for (last=mf; last->next!=NULL; last=last->next)
      /* nothing */;
    assert(last!=NULL);
    assert(last->next==NULL);
    last->next=block;
    numblocks++;
  } /* while */

  if (size==0)
    return 0;

  /* find the block to start writing to */
  numblocks=mf->bufpos/BUFFERSIZE;      /* # blocks to skip */
  block=mf->next;
  while (numblocks-->0) {
    assert(block!=NULL);
    block=block->next;
  } /* while */
  assert(block!=NULL);

  /* copy into memory */
  bytes=0;
  blockpos=(int)(mf->bufpos % BUFFERSIZE);
  do {
    blocksize=BUFFERSIZE-blockpos;
    assert(blocksize>=0);
    if ((unsigned int)blocksize>size)
      blocksize=size;

    assert(block!=NULL);
    memcpy(block->buffer+blockpos,buffer,blocksize);
    buffer+=blocksize;
    size-=blocksize;
    bytes+=blocksize;

    if (blockpos+blocksize>block->bufpos)
      block->bufpos=blockpos+blocksize;
    assert(block->bufpos>=0 && (unsigned long)block->bufpos<=BUFFERSIZE);
    block=block->next;
    blockpos=0;
  } while (size>0);

  /* adjust file pointer */
  mf->bufpos+=bytes;

  return bytes;
}

unsigned int mfread(MEMFILE *mf,unsigned char *buffer,unsigned int size)
{
  long length;
  long numblocks;
  int blockpos,blocksize;
  unsigned int bytes;
  MEMFILE *block;

  assert(mf!=NULL);

  /* adjust the size to read */
  length=mflength(mf);
  assert(mf->bufpos>=0 && mf->bufpos<=length);
  if (mf->bufpos+size>(unsigned long)length)
    size=(int)(length-mf->bufpos);
  assert(mf->bufpos+size<=(unsigned long)length);
  if (size==0)
    return 0;

  /* find the block to start reading from */
  numblocks=mf->bufpos/BUFFERSIZE;      /* # blocks to skip */
  block=mf->next;
  while (numblocks-->0) {
    assert(block!=NULL);
    block=block->next;
  } /* while */
  assert(block!=NULL);

  /* copy out of memory */
  bytes=0;
  blockpos=(int)(mf->bufpos % BUFFERSIZE);
  do {
    blocksize=BUFFERSIZE-blockpos;
    if ((unsigned int)blocksize>size)
      blocksize=size;

    assert(block!=NULL);
    assert(block->bufpos>=0 && (unsigned long)block->bufpos<=BUFFERSIZE);
    assert(blockpos+blocksize<=block->bufpos);
    memcpy(buffer,block->buffer+blockpos,blocksize);
    buffer+=blocksize;
    size-=blocksize;
    bytes+=blocksize;

    block=block->next;
    blockpos=0;
  } while (size>0);

  /* adjust file pointer */
  mf->bufpos+=bytes;

  return bytes;
}

char *mfgets(MEMFILE *mf,char *string,unsigned int size)
{
  char *ptr;
  unsigned int read;
  long seek;

  assert(mf!=NULL);

  read=mfread(mf,(unsigned char *)string,size);
  if (read==0)
    return NULL;
  seek=0L;

  /* make sure that the string is zero-terminated */
  assert(read<=size);
  if (read<size) {
    string[read]='\0';
  } else {
    string[size-1]='\0';
    seek=-1;            /* undo reading the character that gets overwritten */
  } /* if */

  /* find the first '\n' */
  ptr=strchr(string,'\n');
  if (ptr!=NULL) {
    *(ptr+1)='\0';
    seek=(long)(ptr-string)+1-(long)read;
  } /* if */

  /* undo over-read */
  assert(seek<=0);      /* should seek backward only */
  if (seek!=0)
    mfseek(mf,seek,SEEK_CUR);

  return string;
}

int mfputs(MEMFILE *mf,char *string)
{
  unsigned int written,length;

  assert(mf!=NULL);

  length=strlen(string);
  written=mfwrite(mf,(unsigned char *)string,length);
  return written==length;
}
