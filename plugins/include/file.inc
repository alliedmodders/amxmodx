/* Files functions
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _file_included
  #endinput
#endif
#define _file_included

/* Reads content from directory.
* Returns index of next element or 0 when end of dir. is reached. */
native read_dir(const dirname[],pos,output[],len,&outlen);

/* Reads line from file. Returns index of next line or 0 when end of file is reached. */
native read_file(const file[],line,text[],len,&txtlen);

/* Writes text to file. Function returns 0 on failure.
* When line is set to -1, the text is added at the end of file. */
native write_file(const file[],const text[],line = -1);

/* Deletes file. Function returns 1 on success, 0 on failure. */
native delete_file(const file[]);

/* Checks for file. If file exists function returns 1, in other case 0. */
native file_exists(const file[]);

/* Returns a file size in bytes if flag is set to 0.
* When flag is set to 1 returns number of lines in the file,
* and when flags is 2, function returns 1 if the file ends
* with line feed. If file doesn't exist returns -1. */
native file_size(const file[], flag=0);