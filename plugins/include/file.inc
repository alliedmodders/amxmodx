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

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

// These are C style file access functions
// Code ported from Sanji's File module

//Open a file
native fopen(filename[],mode[]);
//Close a file
native fclose(file);
//Read a file for ret_size length
native fread(file,ret[],ret_size);
//Write a file
native fwrite(file,const str[],{Float,Sql,Result,_}:...);
//Check if at the end of a file
native feof(file);
//Seek a file
native fseek(file,pos,type);
//Rewind a file (reset)
native rewind(file);
//Flush a file
native fflush(file);
//Scan a file by a format
native fscanf(file,const str[],{Float,Sql,Result,_}:...);
//Returns the current file position pointer
native ftell(file);
//Return the size of a file
native filesize(const filename[],{Float,Sql,Result,_}:...);
//Delete a file (delete_file macro)
native unlink(const filename[],{Float,Sql,Result,_}:...);

//These are type specific file getting and writing commands:
//c=char, s=short, l=long, i=int, f=float
native fgetc(file);		//read char (size 1)
native fgets(file);		//read short (size 2)
native fgetl(file);		//read long (size 4)
native fgeti(file);		//read int (size 4)
native Float:fgetf(file);	//read float (size 4)

native fputc(file,num);		//write char (size 1)
native fputs(file,num);		//write short (size 2)
native fputl(file,num);		//write long (size 4)
native fputi(file,num);		//write int (size 4)
native fputf(file,Float:num);	//write float (size 4)