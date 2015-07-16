// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "sh_list.h"
#include "amxmodx.h"

#include "CFlagManager.h"

void CFlagManager::SetFile(const char *Filename)
{
	m_strConfigFile = build_pathname("%s/%s", get_localinfo("amxx_configsdir", "addons/amxmodx/configs"), Filename);

	CreateIfNotExist();
}

const int CFlagManager::LoadFile(const int force)
{
	CheckIfDisabled();
	// If we're disabled get the hell out.  now.
	if (m_iDisabled)
	{
		return 0;
	}
	// if we're not forcing this, and NeedToLoad says we dont have to
	// then just stop
	if (!force && !NeedToLoad())
	{
		return 0;
	};

	this->Clear();


	// We need to load the file

	FILE *File;

	File=fopen(GetFile(),"r");

	if (!File)
	{
		AMXXLOG_Log("[AMXX] FlagManager: Cannot open file \"%s\" (FILE pointer null!)", GetFile());
		return -1;
	};

	// Trying to copy this almost exactly as other configs are read...
	char Line[512];
	char TempLine[512];

	char Command[256];
	char Flags[256];

	
	while (!feof(File) && fgets(Line, sizeof(Line), File))
	{
		char *nonconst= Line;

		// Strip out comments
		while (*nonconst)
		{
			if (*nonconst==';') // End the line at comments
			{
				*nonconst='\0';
			}
			else
			{
				nonconst++;
			}
		};

		Command[0]='\0';
		Flags[0]='\0';

		// Extract the command
		strncopy(TempLine, Line, sizeof(TempLine));

		nonconst = TempLine;

		char *start=NULL;
		char *end=NULL;

		// move up line until the first ", mark this down as the start
		// then find the second " and mark it down as the end
		while (*nonconst!='\0')
		{
			if (*nonconst=='"')
			{
				if (start==NULL)
				{
					start=nonconst+1;
				}
				else
				{
					end=nonconst;
					goto done_with_command;
				}
			}
			nonconst++;
		}
done_with_command:

		// invalid line?
		if (start==NULL || end==NULL)
		{
			// TODO: maybe warn for an invalid non-commented line?
			continue;
		}

		*end='\0';

		strncpy(Command,start,sizeof(Command)-1);

		// Now do the same thing for the flags
		nonconst=++end;

		start=NULL;
		end=NULL;

		// move up line until the first ", mark this down as the start
		// then find the second " and mark it down as the end
		while (*nonconst!='\0')
		{
			if (*nonconst=='"')
			{
				if (start==NULL)
				{
					start=nonconst+1;
				}
				else
				{
					end=nonconst;
					goto done_with_flags;
				}
			}
			nonconst++;
		}
done_with_flags:
		// invalid line?
		if (start==NULL || end==NULL)
		{
			// TODO: maybe warn for an invalid non-commented line?
			continue;
		}

		*end='\0';

		strncpy(Flags,start,sizeof(Flags)-1);

		//if (!isalnum(*Command))
		if (*Command == '"' || *Command == '\0')
		{
			continue;
		};

		// Done sucking the command and flags out of the line
		// now insert this command into the linked list

		AddFromFile(const_cast<const char*>(&Command[0]),&Flags[0]);

		nonconst = Line;
		*nonconst = '\0';
	};

	fclose(File);

	return 1;
}

/**
 * This gets called from LoadFile
 * Do NOT flag the entries as NeedToWrite
 * No comment is passed from the file because
 * this should never get written
 */
void CFlagManager::AddFromFile(const char *Command, const char *Flags)
{

	CFlagEntry *Entry=new CFlagEntry;

	Entry->SetName(Command);
	Entry->SetFlags(Flags);

	// Link it
	m_FlagList.push_back(Entry);

};


void CFlagManager::LookupOrAdd(const char *Command, int &Flags, AMX *Plugin)
{
	if (m_iDisabled) // if disabled in core.ini stop
	{
		return;
	}
	

	int TempFlags=Flags;
	if (TempFlags==-1)
	{
		TempFlags=0;
	}

	List<CFlagEntry *>::iterator	 iter;
	List<CFlagEntry *>::iterator	 end;

	iter=m_FlagList.begin();
	end=m_FlagList.end();

	while (iter!=end)
	{
		if (strcmp((*iter)->GetName()->chars(),Command)==0)
		{
			CFlagEntry *Entry=(*iter);

			if (Entry->IsHidden()) // "!" flag, exclude this function
			{
				return;
			}
			// Found, byref the new flags
			Flags=Entry->Flags();

			// Move it to the back of the list for faster lookup for the rest
			m_FlagList.erase(iter);

			m_FlagList.push_back(Entry);
			return;
		}
		iter++;
	}

	// was not found, add it

	CFlagEntry *Entry=new CFlagEntry;

	Entry->SetName(Command);
	Entry->SetFlags(TempFlags);

	if (Plugin)
	{
		CPluginMngr::CPlugin* a = g_plugins.findPluginFast(Plugin);

		if (a)
		{
			Entry->SetComment(a->getName());
		}
	}

	// This entry was added from a register_* native
	// it needs to be written during map change
	Entry->SetNeedWritten(1);

	// Link it
	m_FlagList.push_back(Entry);

}
void CFlagManager::WriteCommands(void)
{
	List<CFlagEntry *>::iterator	 iter;
	List<CFlagEntry *>::iterator	 end;
	FILE							*File;
	int								 NeedToRead=0;

	// First off check the modified time of this file
	// if it matches the stored modified time, then update
	// after we write so we do not re-read next map
	struct stat TempStat;

	stat(GetFile(), &TempStat);



	if (TempStat.st_mtime != m_Stat.st_mtime)
	{
		NeedToRead=1;
	};


	File = fopen(GetFile(), "a");
	
	if (!File)
	{
		return;
	}

	iter=m_FlagList.begin();
	end=m_FlagList.end();



	while (iter!=end)
	{
		if ((*iter)->NeedWritten())
		{
			if ((*iter)->GetComment()->length())
			{
				fprintf(File,"\"%s\" \t\"%s\" ; %s\n",(*iter)->GetName()->chars(),(*iter)->GetFlags()->chars(),(*iter)->GetComment()->chars());
			}
			else
			{
				fprintf(File,"\"%s\" \t\"%s\"\n",(*iter)->GetName()->chars(),(*iter)->GetFlags()->chars());
			}
			(*iter)->SetNeedWritten(0);
		}
		++iter;
	};

	fclose(File);


	// If NeedToRead was 0, then update the timestamp
	// that was saved so we do not re-read this file
	// next map
	if (!NeedToRead)
	{
		stat(GetFile(), &TempStat);

		m_Stat.st_mtime=TempStat.st_mtime;

	}

}

int CFlagManager::ShouldIAddThisCommand(const AMX *amx, const cell *params, const char *cmdname) const
{

	// If flagmanager is disabled then ignore this
	if (m_iDisabled)
	{
		return 0;
	}
	
	
	// If 5th param exists it was compiled after this change was made
	// if it does not exist, try our logic at the end of this function
	// 5th param being > 0 means explicit yes
	// < 0 means auto detect (default is -1), treat it like there was no 5th param
	// 0 means explicit no

	if ((params[0] / sizeof(cell)) >= 5)
	{
		if (params[5]>0) // This command was explicitly told to be included
		{
			return 1;
		}
		else if (params[5]==0) // this command was explicitly told to NOT be used
		{
			return 0;
		}
	}

	// auto detect if we should use this command

	// if command access is -1 (default, not set to ADMIN_ALL or any other access), then no
	if (params[3]==-1)
	{
		return 0;
	}


	// if command is (or starts with) "say", then no
	if (strncmp(cmdname,"say",3)==0)
	{
		return 0;
	}


	// else use it
	return 1;
};


void CFlagManager::Clear(void)
{
	List<CFlagEntry *>::iterator	 iter;
	List<CFlagEntry *>::iterator	 end;

	iter=m_FlagList.begin();
	end=m_FlagList.end();

	while (iter!=end)
	{
		delete (*iter);

		++iter;
	}

	m_FlagList.clear();
};

void CFlagManager::CheckIfDisabled(void)
{
	if (atoi(get_localinfo("disableflagman","0"))==0)
	{
		m_iDisabled=0;
	}
	else
	{
		m_iDisabled=1;
	}
};
