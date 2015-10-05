// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#include "amxxmodule.h"
#include "ns.h"
#include "TitleManager.h"
#include "utilfunctions.h"

void TitleManager::LoadTitles(void)
{
	if (m_Loaded!=0) // already loaded?
	{
		return;
	}

	m_Loaded=1;

	char FileName[128];

	ke::SafeSprintf(FileName, sizeof(FileName), "%s/titles.txt", MF_GetModname());

	FILE *fp=fopen(FileName,"r");

	if (!fp)
	{
		MF_Log("Unable to load \"%s\": TitleManager will not work!",FileName);
		return;
	};

	//MF_Log("TitleManager Loading titles from \"%s\"",FileName);
	char KeyName[512];			// last known good keyname
	char Data[2048];			// data for the key
								// does not support multi line data, but for 
								// location namesthat is acceptable.
	char  TempBuffer[2048];		// currently read data
	char *TempPointer;
	char *TempPointerEnd;

	unsigned int Line=0;
scan_for_key:
	KeyName[0]='\0';

	while (!feof(fp))
	{
		Line++;
		fgets(TempBuffer,2047,fp);
		TempPointer=&TempBuffer[0];

		// get rid of white space at the front
		while (*TempPointer=='\0' ||	// terminator
			*TempPointer==' ' ||		// space
			*TempPointer=='\t')			// tab
		{
			++TempPointer;
		}
		if (*TempPointer=='\0' ||		// terminator
			*TempPointer=='/')			// comment
		{
			continue;
		}

		// get rid of \r\n at the end
		TempPointerEnd=TempPointer+strlen(TempPointer)-1;
		while (*TempPointerEnd=='\r' || 
			*TempPointerEnd=='\n' ||
			*TempPointerEnd=='\t' ||
			*TempPointerEnd==' ')
		{
			*TempPointerEnd--='\0';
		}

		if (*TempPointer=='{')			// start of data
		{
			if (KeyName[0]!='\0')		// do we have a keyname
			{
				goto scan_for_data;
			}
			else
			{
				MF_Log("TitleManager: titles.txt line %u: began a data section with no key name in use!",Line);
				goto scan_for_key;
			}
		}

		// have a valid key name here
		strncpy(KeyName,TempBuffer,sizeof(KeyName)-1);
		
		// keep looping (until we hit a '{')
	};

	// if we're out here then !feof() failed
	goto end_of_file;
scan_for_data:
	Data[0]='\0';

	while (!feof(fp))
	{
		Line++;
		fgets(TempBuffer,2047,fp);
		TempPointer=&TempBuffer[0];

		// get rid of white space at the front
		while (*TempPointer=='\0' ||	// terminator
			*TempPointer==' ' ||		// space
			*TempPointer=='\t')			// tab
		{
			++TempPointer;
		}
		if (*TempPointer=='\0' ||		// terminator
			*TempPointer=='/')			// comment
		{
			continue;
		}

		// get rid of trailing whitespace
		TempPointerEnd=TempPointer+strlen(TempPointer)-1;
		while (*TempPointerEnd=='\r' || 
			*TempPointerEnd=='\n' ||
			*TempPointerEnd=='\t' ||
			*TempPointerEnd==' ')
		{
			*TempPointerEnd--='\0';
		}

		if (*TempPointer=='}')			// end of data
		{
			// insert KeyName & Data into the hash
			ke::AString key(UTIL_ToLowerCase(KeyName));

			this->m_Hash.insert(key, new ke::AString(Data));

			goto scan_for_key;
		}

		// have valid data here
		strncpy(Data,TempBuffer,sizeof(Data)-1);
	};
end_of_file:

	fclose(fp);

	//MF_Log("TitleManager loaded %u entries from titles.txt (%u lines parsed)",m_List.size(),Line);
};
