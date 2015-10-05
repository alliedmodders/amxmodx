// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include "amxxmodule.h"

#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"
#include <amtl/am-string.h>

extern hook_t hooklist[];

enum
{
	LEX_INVALID = 0,

	LEX_UNKNOWN,

	LEX_START_SEC,
	LEX_END_SEC,

	LEX_MIRROR,

	LEX_PEV,
	LEX_BASE,

	LEX_END
};

const char *tokens[] =
{
	"", // LEX_INVALID

	"", // LEX_UNKNOWN

	"@section", // LEX_START_SEC
	"@end", // LEX_END_SEC

	"@mirror", // LEX_MIRROR

	"pev", // LEX_PEV
	"base", // LEX_BASE

	"", // LEX_END
};

static void trim_line(char *input);
static void read_mirror(char *input);
static void skip_to_end_of_section(FILE *fp);
static int lex(char*& buffer);

int lex(char*& buffer)
{
	trim_line(buffer);

	size_t len;

	for (int i=0; i<LEX_END; i++)
	{
		if (tokens[i]!=NULL && *(tokens[i])!='\0')
		{
			len=strlen(tokens[i]);
			if (strncmp(buffer,tokens[i],len)==0)
			{
				buffer+=len+1;
				return i;
			}
		}
	}
	return LEX_UNKNOWN;
}

// How we handle "mirrors"
// We just note down the current mod name, and every time
// we come across a mirror with the destination that matches
// the current mod name, we change the current mod name to
// the source for that mirror.

char CurrentModName[64];

static void read_mirror(char *input)
{
	char *data=input;
	char *data2;

	char source[64];
	char dest[64];

	char old;
	while ( *data!=' ' &&
			*data!='\t' &&
			*data!='\0')
	{

		data++;
	}

	old=*data;
	*data='\0';

	// mark down the source
	ke::SafeSprintf(source, sizeof(source), "%s", input);

	*data=old;

	while ( *data==' ' ||
			*data=='\t')
	{
		data++;
	}
	data2=data;

	while ( *data!=' ' &&
			*data!='\t' &&
			*data!='\0')
	{
		data++;
	}
	old=*data;
	*data='\0';

	ke::SafeSprintf(dest, sizeof(dest), "%s", data2);

	*data=old;

	if (strcmp(dest, CurrentModName)==0)
	{
		ke::SafeSprintf(CurrentModName, sizeof(CurrentModName), "%s", source);
	}

}

static void trim_line(char *input)
{
	char *oldinput=input;
	char *start=input;

	while ( *start==' ' ||
			*start=='\t' ||
			*start=='\r' ||
			*start=='\n')
	{
		start++;
	}

	// Overwrite the whitespace

	if (start != input)
	{
		while ((*input++=*start++)!='\0')
			/* do nothing */ ;
	}

	start=oldinput;

	start+=strlen(start) - 1;

	while ( start >= oldinput &&
			( *start == '\0' ||
			  *start == ' '  ||
			  *start == '\r' ||
			  *start == '\n' ||
			  *start == '\t'))
	{
		start--;
	}
	start++;
	*start='\0';

	// Now find any comments and cut off at the start

	while (*start != '\0')
	{
		if (*start == ';')
		{
			*start='\0';
			break;
		}

		start++;
	}
}

void skip_to_end_of_section(FILE *fp)
{
	char buffer[1024];

	while (!feof(fp))
	{
		buffer[0]='\0';

		fgets(buffer, sizeof(buffer)-1, fp);

		trim_line(buffer);

		char *b=&buffer[0];
		if (lex(b)==LEX_END_SEC)
		{
			break;
		}
	}
}
static const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}
int read_start_section(char *data)
{
	if (strncasecmp(data, CurrentModName, strlen(CurrentModName))==0)
	{
		data+=strlen(CurrentModName)+1;
		trim_line(data);

#ifdef _WIN32
		if (strcmp(data, "windows")==0)
#elif defined(__linux__)
		if (strcmp(data, "linux")==0)
#elif defined(__APPLE__)
		if (strcmp(data, "mac")==0)
#endif
		{
			return 1;
		}
	}
	return 0;
}
int read_number(char *input)
{
	char *end; /* Temporary pointer, needed for strtoul(). */

	// if begins with 0x or 0X it's to be interpretted as hex
	if (*input=='0' && 
		(*(input+1)=='x' || *(input+1)=='X'))
	{
		return strtoul(input,&end,16);
	}

	// otherwise it's to be interpretted as base 10
	return strtoul(input,&end,10);
}
void process_pev(char *data)
{
	trim_line(data);
	Offsets.SetPev(read_number(data));
}
void process_base(char *data)
{
	trim_line(data);
	Offsets.SetBase(read_number(data));
}
void process_key(char *data)
{
	size_t size=0;

	char *a=data;

	while (*a != ' ' && *a != '\t' && *a != '\0')
	{
		a++;
		size++;
	}

	if (size==0)
	{
		return;
	}
	int set=0;
	for (int i=0; i< HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
	{
		if (strncmp(data, hooklist[i].name, size)==0)
		{
			data+=size+1;

			trim_line(data);
			int value=read_number(data);

			hooklist[i].isset=1;
			hooklist[i].vtid=value;


			set=1;
			break;

		}
	}

	if (set==0)
	{
		printf("stray key in process_key: %s\n", data);
	}

}
int ReadConfig(void)
{
	char FileName[512];

	MF_BuildPathnameR(FileName,sizeof(FileName)-1,"%s",get_localinfo("amxx_configsdir","addons/amxmodx/configs"));

	strncat(FileName,"/hamdata.ini",sizeof(FileName)-1);

	FILE *fp=fopen(FileName,"r");


	ke::SafeSprintf(CurrentModName, sizeof(CurrentModName), "%s", MF_GetModname());

	if (!fp)
	{
		MF_Log("Unable to open \"%s\" for reading.", FileName);

		return -1;
	}

	char data[2048];

	int insec=0;

	while (!feof(fp))
	{
		data[0]='\0';

		fgets(data, sizeof(data)-1, fp);

		char *b=&data[0];

		switch(lex(b))
		{
		case LEX_PEV:
			{
				if (insec)
				{
					process_pev(b);
				}
				break;
			};
		case LEX_BASE:
			{
				if (insec)
				{
					process_base(b);
				}
				break;
			};
		case LEX_MIRROR:
			{
				read_mirror(b);
				break;
			};
		case LEX_START_SEC:
			{
				insec=read_start_section(b);

				if (!insec)
				{
					skip_to_end_of_section(fp);
				}
				break;
			};
		case LEX_END_SEC:
			{
				insec=0;
				break;
			};
		case LEX_UNKNOWN:
			{
				if (insec)
				{
					process_key(b);
				}
			};
		}


	}

	fclose(fp);

	return 1;
}
