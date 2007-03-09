#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "sdk/amxxmodule.h"

#include "FileParser.h"

#define FP_MAX_LENGTH			2048


/**
 * Given a string, this will remove whitespace at the beginning and end, and remove comments.
 *
 * @param data			The string to format.  It changes this data directly.
 * @return				Returns a pointer to the end of the newly formated string.
 */
static char *FP_FormatLine(char *data)
{
	char				*End;			/**< Pointer to the end of the string. */
	char				*Start;			/**< Pointer to the start of the string. */
	char				*Temp=Start;	/**< Temporary pointer for parsing. */

	Start=data;

	// Strip beginning whitespace
	while (*Start==' ' || *Start=='\t') Start++;

	// if the beginning non-whitespace character is a ';', then it's a comment
	// ignore the rest of the file
	if (*Start==';')
	{
		// just set data[0] to \0 and return out of here.
		*data='\0';
		return data;
	}

	// now strip comments from the end of a line
	Temp=Start;

	End=Start+(strlen(Start)-1);


	while (Temp<=End)
	{
		if (*Temp==';')
		{
			*Temp='\0';
			break;
		}
		++Temp;
	}



	// now go to the end of the line, and remove whitespace

	while ( (*End=='\n' ||
			*End=='\r' ||
			*End==' '  ||
			*End=='\t') &&
			End>=Start)
	{
		End--;
	}
	++End;
	*End='\0';

	// if Start==data, we're done
	if (Start==data)
	{
		return End;
	}

	// otherwise, copy from Start to data

	while ((*data++=*Start++)!='\0')/*do nothing*/;

	return End;
};

static const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}
/**
 * Reads the config file and parses keys.
 *
 * @param Category			The category (prefix) to look for.  Eg.: "cs_linux_", "dod_windows_"
 * @param Feedback			The function to call when a match is made.
 * @noreturn
 */
void FP_SetupOffsets(const char *Category, FP_ptrFeedback Feedback)
{
	char FileName[512];

	size_t CatLen=strlen(Category);

	MF_BuildPathnameR(FileName,sizeof(FileName)-1,"%s",get_localinfo("amxx_configsdir","addons/amxmodx/configs"));

	strncat(FileName,"/hamdata.ini",sizeof(FileName)-1);

	FILE *fp=fopen(FileName,"r");

	if (!fp)
	{
		MF_Log("Unable to open \"%s\" for reading",FileName);
		return;
	}

	char Data[FP_MAX_LENGTH + 1];


	while (!feof(fp))
	{

		Data[0]='\0';

		fgets(Data,FP_MAX_LENGTH,fp);

		FP_FormatLine(Data);

		if (strncmp(Data,Category,CatLen)==0)
		{
			// Find the first space, set it to NULL
			char *Param=&Data[0];

			while (*Param!=' ' && *Param!='\t' && *Param!='\0')
			{
				++Param;
			}
			if (*Param=='\0')
			{
				// special instance; if this is NULL get out of here
				continue;
			}

			// NULL this space, and then find the first non whitespace character and 
			// use that as the parameter field in the callback
			*Param++='\0';
			while (*Param==' ' || *Param=='\t')
			{
				++Param;
			}

			if (*Param=='\0')
			{
				// special instance; if this is NULL get out of here
				continue;
			}
			Feedback(&Data[0],Param);
		}
	}
	

	fclose(fp);
}
