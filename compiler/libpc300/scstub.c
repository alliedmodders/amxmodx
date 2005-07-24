#include <process.h>
#include <stdio.h>
#include <string.h>

static char filename[] = "pccdos.exe";

int main(int argc, char *argv[])
{
  int result;

  /* build a command line to pass on to the "DOS" program */
  char path[80], *ptr;
  strcpy(path,argv[0]);
  ptr=strrchr(path,'\\');
  if (ptr==NULL)
    ptr=strchr(path,':');
  if (ptr==NULL) {
    strcpy(path,filename);
  } else {
    strcpy(ptr+1,filename);
  } /* if */

  /* launch the DOS version of the tool */
  result=execv(path,argv);
  if (result==-1)
    printf("Error launching '%s'\n",path);
  return result;
}

