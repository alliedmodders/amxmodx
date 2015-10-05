// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// amxx_logging localinfo:
//  0 = no logging
//  1 = one logfile / day
//  2 = one logfile / map
//  3 = HL Logs

#include <time.h>
#if defined(_WIN32)
	#include <io.h>
#endif
#include "amxmodx.h"

#if defined(_WIN32WIN32)
	#define	vsnprintf	_vsnprintf
#endif

#include <amxmodx_version.h>

CLog::CLog()
{
	m_LogType = 0;
	m_LogFile = nullptr;
	m_FoundError = false;
	m_LoggedErrMap = false;
}

CLog::~CLog()
{
	CloseFile();
}

void CLog::CloseFile()
{
	// log "log file closed" to old file, if any
	if (m_LogFile.length())
	{
		FILE *fp = fopen(m_LogFile.chars(), "r");
		
		if (fp)
		{
			fclose(fp);
			fp = fopen(m_LogFile.chars(), "a+");

			// get time
			time_t td;
			time(&td);
			tm *curTime = localtime(&td);

			char date[32];
			strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

			fprintf(fp, "L %s: %s\n", date, "Log file closed.");
			fclose(fp);
		}
		
		m_LogFile = nullptr;
	}
}

void CLog::CreateNewFile()
{
	CloseFile();
	
	// build filename
	time_t td;
	time(&td);
	tm *curTime = localtime(&td);

	char file[256];
	char name[256];
	int i = 0;
	
	while (true)
	{
		ke::SafeSprintf(name, sizeof(name), "%s/L%02d%02d%03d.log", g_log_dir.chars(), curTime->tm_mon + 1, curTime->tm_mday, i);
		build_pathname_r(file, sizeof(file)-1, "%s", name);
		FILE *pTmpFile = fopen(file, "r");			// open for reading to check whether the file exists
		
		if (!pTmpFile)
			break;
		
		fclose(pTmpFile);
		++i;
	}
	m_LogFile = file;
	
	// Log logfile start
	FILE *fp = fopen(m_LogFile.chars(), "w");
	
	if (!fp)
	{
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error. AMXX Logging disabled.\n");
		SET_LOCALINFO("amxx_logging", "0");
	} else {
		fprintf(fp, "AMX Mod X log file started (file \"%s\") (version \"%s\")\n", name, AMXX_VERSION);
		fclose(fp);
	}
}

void CLog::UseFile(const ke::AString &fileName)
{
	static char file[256];
	m_LogFile = build_pathname_r(file, sizeof(file) - 1, "%s/%s", g_log_dir.chars(), fileName.chars());
}

void CLog::SetLogType(const char* localInfo)
{
	m_LogType = atoi(get_localinfo(localInfo, "1"));

	if (m_LogType < 0 || m_LogType > 3)
	{
		SET_LOCALINFO(localInfo, "1");
		m_LogType = 1;

		print_srvconsole("[AMXX] Invalid amxx_logging value; setting back to 1...");
	}
}

void CLog::MapChange()
{
	// create dir if not existing
	char file[256];
#if defined(__linux__) || defined(__APPLE__)
	mkdir(build_pathname_r(file, sizeof(file)-1, "%s", g_log_dir.chars()), 0700);
#else
	mkdir(build_pathname_r(file, sizeof(file) - 1, "%s", g_log_dir.chars()));
#endif

	SetLogType("amxx_logging");

	m_LoggedErrMap = false;

	if (m_LogType == 2)
	{
		// create new logfile
		CreateNewFile();
	} else if (m_LogType == 1) {
		Log("-------- Mapchange to %s --------", STRING(gpGlobals->mapname));
	} else {
		return;
	}
}

void CLog::Log(const char *fmt, ...)
{
	static char file[256];
	
	if (m_LogType == 1 || m_LogType == 2)
	{
		// get time
		time_t td;
		time(&td);
		tm *curTime = localtime(&td);

		char date[32];
		strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

		// msg
		static char msg[3072];

		va_list arglst;
		va_start(arglst, fmt);
		vsnprintf(msg, 3071, fmt, arglst);
		va_end(arglst);

		FILE *pF = NULL;
		if (m_LogType == 2)
		{
			pF = fopen(m_LogFile.chars(), "a+");
			if (!pF)
			{
				CreateNewFile();
				pF = fopen(m_LogFile.chars(), "a+");
				
				if (!pF)
				{
					ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+). AMXX Logging disabled for this map.\n", m_LogFile.chars());
					m_LogType = 0;
					return;
				}
			}
		} else {
			build_pathname_r(file, sizeof(file) - 1, "%s/L%04d%02d%02d.log", g_log_dir.chars(), (curTime->tm_year + 1900), curTime->tm_mon + 1, curTime->tm_mday);
			pF = fopen(file, "a+");
		}
		
		if (pF)
		{
			fprintf(pF, "L %s: %s\n", date, msg);
			fclose(pF);
		} else {
			ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+). AMXX Logging disabled for this map.\n", file);
			m_LogType = 0;
			return;
		}

		// print on server console
		print_srvconsole("L %s: %s\n", date, msg);
	} else if (m_LogType == 3) {
		// build message
		static char msg_[3072];
		va_list arglst;
		va_start(arglst, fmt);
		vsnprintf(msg_, 3071, fmt, arglst);
		va_end(arglst);
		ALERT(at_logged, "%s\n", msg_);
	}
}

void CLog::LogError(const char *fmt, ...)
{
	static char file[256];
	static char name[256];

	if (m_FoundError)
	{
		return;
	}

	// get time
	time_t td;
	time(&td);
	tm *curTime = localtime(&td);

	char date[32];
	strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

	// msg
	static char msg[3072];

	va_list arglst;
	va_start(arglst, fmt);
	vsnprintf(msg, sizeof(msg)-1, fmt, arglst);
	va_end(arglst);

	FILE *pF = NULL;
	ke::SafeSprintf(name, sizeof(name), "%s/error_%04d%02d%02d.log", g_log_dir.chars(), curTime->tm_year + 1900, curTime->tm_mon + 1, curTime->tm_mday);
	build_pathname_r(file, sizeof(file)-1, "%s", name);
	pF = fopen(file, "a+");

	if (pF)
	{
		if (!m_LoggedErrMap)
		{
			fprintf(pF, "L %s: Start of error session.\n", date);
			fprintf(pF, "L %s: Info (map \"%s\") (file \"%s\")\n", date, STRING(gpGlobals->mapname), name);
			m_LoggedErrMap = true;
		}
		fprintf(pF, "L %s: %s\n", date, msg);
		fclose(pF);
	} else {
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+). AMXX Error Logging disabled for this map.\n", file);
		m_FoundError = true;
		return;
	}

	// print on server console
	print_srvconsole("L %s: %s\n", date, msg);
}

