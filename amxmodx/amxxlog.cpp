/* AMX Mod X
*
* by the AMX Mod X Development Team
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

// amxx_logging localinfo:
//  0 = no logging
//  1 = one logfile / day
//  2 = one logfile / map
//  3 = HL Logs

#include <time.h>
#ifndef __linux__
#include <io.h>
#endif
#include "amxmodx.h"

#ifndef __linux__
#define	vsnprintf	_vsnprintf
#endif

CLog::CLog()
{
	m_LogType = 0;
	m_LogFile.clear();
}

CLog::~CLog()
{
	CloseFile();
}

void CLog::CloseFile()
{
	// log "log file closed" to old file, if any
	if (!m_LogFile.empty())
	{
		FILE *fp = fopen(m_LogFile.c_str(), "r");
		if (fp)
		{
			fclose(fp);
			fp = fopen(m_LogFile.c_str(), "a+");

			// get time
			time_t td;
			time(&td);
			tm *curTime = localtime(&td);

			char date[32];
			strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

			fprintf(fp, "L %s: %s\n", date, "Log file closed.");
			fclose(fp);
		}
		m_LogFile.clear();
	}
}

void CLog::CreateNewFile()
{
	CloseFile();
	// build filename
	time_t td;
	time(&td);
	tm *curTime = localtime(&td);

	int i = 0;
	while (true)
	{
		m_LogFile.assign(build_pathname("%s/L%02d%02d%03d.log", g_log_dir.c_str(), curTime->tm_mon + 1, curTime->tm_mday, i));
		FILE *pTmpFile = fopen(m_LogFile.c_str(), "r");		// open for reading to check whether the file exists
		if (!pTmpFile)
			break;
		fclose(pTmpFile);
		++i;
	}
	// Log logfile start
	FILE *fp = fopen(m_LogFile.c_str(), "w");
	if (!fp)
	{
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error. AMXX Logging disabled.\n");
		SET_LOCALINFO("amxx_logging", "0");
	}
	else
	{
		fprintf(fp, "AMX Mod X log file started (file \"%s/L%02d%02d%03d.log\") (version \"%s\")\n", g_log_dir.c_str(), curTime->tm_mon + 1, curTime->tm_mday, i, AMX_VERSION);
		fclose(fp);
	}
}

void CLog::UseFile(const String &fileName)
{
	m_LogFile.assign(build_pathname("%s/%s", g_log_dir.c_str(), fileName.c_str()));
}

void CLog::MapChange()
{
	// create dir if not existing
#ifdef __linux
	mkdir(build_pathname("%s", g_log_dir.c_str()), 0700);
#else
	mkdir(build_pathname("%s", g_log_dir.c_str()));
#endif

	m_LogType = atoi(get_localinfo("amxx_logging", "1"));
	if (m_LogType < 0 || m_LogType > 3)
	{
		SET_LOCALINFO("amxx_logging", "1");
		m_LogType = 1;
		print_srvconsole("[AMXX] Invalid amxx_logging value; setting back to 1...");
	}

	if (m_LogType == 2)
	{
		// create new logfile
		CreateNewFile();
	}
	else if (m_LogType == 1)
	{
		Log("-------- Mapchange --------");
	}
	else
		return;
}

void CLog::Log(const char *fmt, ...)
{
	if (m_LogType == 1 || m_LogType == 2)
	{
		// get time
		time_t td;
		time(&td);
		tm *curTime = localtime(&td);

		char date[32];
		strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

		// msg
		char msg[3072];

		va_list arglst;
		va_start(arglst, fmt);
		vsnprintf(msg, 3071, fmt, arglst);
		va_end(arglst);

		FILE *pF;
		if (m_LogType == 2)
		{
			pF = fopen(m_LogFile.c_str(), "a+");
			if (!pF)
			{
				CreateNewFile();
				pF = fopen(m_LogFile.c_str(), "a+");
				if (!pF)
				{
					ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+). AMXX Logging disabled for this map.\n", m_LogFile.c_str());
					m_LogType = 0;
					return;
				}
			}
		}
		else
		{
			pF = fopen(build_pathname("%s/L%02d%02d.log", g_log_dir.c_str(), curTime->tm_mon + 1, curTime->tm_mday), "a+");
		}
		if (pF)
		{
			fprintf(pF, "L %s: %s\n", date, msg);
			fclose(pF);
		}
		else
		{
			ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+). AMXX Logging disabled for this map.\n", m_LogFile.c_str());
			m_LogType = 0;
			return;
		}

		// print on server console
		print_srvconsole("L %s: %s\n", date, msg);
	}
	else if (m_LogType == 3)
	{
		// build message
		// :TODO: Overflow possible here
		char msg[3072];
		va_list arglst;
		va_start(arglst, fmt);
		vsnprintf(msg, 3071, fmt, arglst);
		va_end(arglst);
		ALERT(at_logged, "%s\n", msg);
	}
}
