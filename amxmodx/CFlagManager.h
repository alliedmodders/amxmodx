// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef CFLAGMANAGER_H
#define CFLAGMANAGER_H

#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "sh_list.h"
#include "amxmodx.h"
#include "CLibrarySys.h"

class CFlagEntry
{
private:
	ke::AString		m_strName;			// command name ("amx_slap")
	ke::AString		m_strFlags;			// string flags ("a","b")
	ke::AString		m_strComment;		// comment to write ("; admincmd.amxx")
	int				m_iFlags;			// bitmask flags
	int				m_iNeedWritten;		// write this command on map change?
	int				m_iHidden;			// set to 1 when the command is set to "!" access in
										// the .ini file: this means do not process this command

public:

	CFlagEntry()
	{
		m_iNeedWritten=0;
		m_iFlags=0;
		m_iHidden=0;
	};
	const int NeedWritten(void) const
	{
		return m_iNeedWritten;
	};

	void SetNeedWritten(const int i=1)
	{
		m_iNeedWritten=i;
	};

	const ke::AString *GetName(void) const
	{
		return &m_strName;
	};

	const ke::AString *GetFlags(void) const
	{
		return &m_strFlags;
	};

	const ke::AString *GetComment(void) const
	{
		return &m_strComment;
	};

	const int Flags(void) const
	{
		return m_iFlags;
	};

	void SetName(const char *data)
	{
		m_strName = data;
	};
	void SetFlags(const char *flags)
	{
		// If this is a "!" entry then stop
		if (flags && flags[0]=='!')
		{
			SetHidden(1);
			return;
		}

		m_strFlags = flags;
		m_iFlags=UTIL_ReadFlags(flags);
	};
	void SetFlags(const int flags)
	{
		m_iFlags=flags;

		char FlagsString[32];
		UTIL_GetFlags(FlagsString, flags);

		m_strFlags = FlagsString;
	};
	void SetComment(const char *comment)
	{
		m_strComment = comment;
	};
	void SetHidden(int i=1)
	{
		m_iHidden=i;
	};
	int IsHidden(void) const
	{
		return m_iHidden;
	};
};
class CFlagManager
{
private:
	List<CFlagEntry *>		 m_FlagList;
	ke::AString				 m_strConfigFile;
	struct stat				 m_Stat;
	int						 m_iForceRead;
	int						 m_iDisabled;
	

	void CreateIfNotExist(void) const
	{
		FILE *fp;
		
		fp = fopen(GetFile(), "r");

		if (!fp)
		{
			// File does not exist, create the header
			fp = fopen(GetFile(), "a");

			if (fp)
			{
				fprintf(fp,"; This file will store the commands used by plugins, and their access level\n");
				fprintf(fp,"; To change the access of a command, edit the flags beside it and then\n");
				fprintf(fp,";   change the server's map.\n;\n");
				fprintf(fp,"; Example: If I wanted to change the amx_slap access to require\n");
				fprintf(fp,";          RCON access (flag \"l\") I would change this:\n");
				fprintf(fp,";          \"amx_slap\"  \"e\" ; admincmd.amxx\n");
				fprintf(fp,";          To this:\n");
				fprintf(fp,";          \"amx_slap\"  \"l\" ; admincmd.amxx\n;\n");
				fprintf(fp,"; To disable a specific command from being used with the command manager\n");
				fprintf(fp,";   and to only use the plugin-specified access set the flag to \"!\"\n;\n");
				fprintf(fp,"; NOTE: The plugin name at the end is just for reference to what plugin\n");
				fprintf(fp,";       uses what commands.  It is ignored.\n\n");
				fclose(fp);
			};
		}
	};
	/**
	 * Returns 1 if the timestamp for the file is different than the one we have loaded
	 * 0 otherwise
	 */
	inline int NeedToLoad(void)
	{
		struct stat TempStat;

		stat(GetFile(), &TempStat);

		// If the modified timestamp does not match the stored
		// timestamp than we need to re-read this file.
		// Otherwise, ignore the file.
		if (TempStat.st_mtime != m_Stat.st_mtime)
		{
			// Save down the modified timestamp
			m_Stat.st_mtime=TempStat.st_mtime;
			return 1;
		};

		return 0;

	};
public:

	CFlagManager()
	{
		memset(&m_Stat,0x0,sizeof(struct stat));
		m_iDisabled=0;
		m_iForceRead=0;
	};
	~CFlagManager()
	{
	};

	/**
	 * Sets the filename in relation to amxmodx/configs
	 */
	void SetFile(const char *Filename="cmdaccess.ini");

	const char *GetFile(void) const	{ return m_strConfigFile.chars(); };
	
	/**
	 * Parse the file, and load all entries
	 * Returns 1 on success, 0 on refusal (no need to), and -1 on error
	 */
	const int LoadFile(const int force=0);

	/**
	 * Checks if the command exists in the list
	 * If it does, it byrefs the flags for it
	 * If it does not, it adds it to the list
	 * These are added from register_*cmd calls
	 */
	void LookupOrAdd(const char *Command, int &Flags, AMX *Plugin);


	/**
	 * Write the commands back to the file
	 */
	void WriteCommands(void);

	/**
	 * Add this straight from the cmdaccess.ini file
	 */
	void AddFromFile(const char *Command, const char *Flags);

	/**
	 * Checks if this command should be added to flagman or not
	 * This is only checked when adding commands from the register_* natives
	 * If an admin manually adds a command to cmdaccess.ini it will be used
	 *   regardless of whatever this function would say should be done with it
	 */
	int ShouldIAddThisCommand(const AMX *amx, const cell *params, const char *cmdname) const;

	void Clear(void);

	void CheckIfDisabled(void);
};

#endif // CFLAGMANAGER_H
