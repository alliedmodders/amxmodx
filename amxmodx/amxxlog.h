// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef __AMXXLOG_H__
#define __AMXXLOG_H__

class CLog
{
private:
	ke::AString m_LogFile;
	int m_LogType;
	bool m_FoundError;
	bool m_LoggedErrMap;

	void GetLastFile(int &outMonth, int &outDay, ke::AString &outFilename);
	void UseFile(const ke::AString &fileName);
public:
	CLog();
	~CLog();
	
	void CreateNewFile();
	void CloseFile();
	void SetLogType(const char* localInfo);
	void MapChange();
	void Log(const char *fmt, ...);
	void LogError(const char *fmt, ...);
};

#endif // __AMXXLOG_H__
