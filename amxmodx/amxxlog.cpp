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

#include <extdll.h>
#include <meta_api.h>
#include <time.h>

#include "amxmodx.h"

String g_AMXXLOG_LogFile;
cvar_t init_amx_logging = {"amx_logging", "", FCVAR_SPONLY};
cvar_t *amx_logging = NULL;

// Initialize cvar; Called from Meta_Attach
void AMXXLOG_Init()
{
	CVAR_REGISTER(&init_amx_logging);
	amx_logging = CVAR_GET_POINTER(init_amx_logging.name);
	CVAR_SET_STRING(init_amx_logging.name, "1");
}

void AMXXLOG_MakeNewLogFile()
{
	if (amx_logging && amx_logging->value == 0.0f)
		return;

	// build filename
	time_t td;
	time(&td);
	tm *curTime = localtime(&td);

	// create dir if not existing
#ifdef __linux
	mkdir(build_pathname("%s", g_log_dir.str()), 0700);
#else
	mkdir(build_pathname("%s", g_log_dir.str()));
#endif

	int i = 0;
	while (true)
	{
		g_AMXXLOG_LogFile.set(build_pathname("%s/L%02d%02d%03d.log", g_log_dir.str(), curTime->tm_mon + 1, curTime->tm_mday, i));
		FILE *pTmpFile = fopen(g_AMXXLOG_LogFile.str(), "r");		// open for reading to check whether the file exists
		if (!pTmpFile)
			break;
		fclose(pTmpFile);
		++i;
	}
	// Log logfile start
	AMXXLOG_Log("AMX Mod X log file started (file \"%s/L%02d%02d%03d.log\") (version \"%s\")", g_log_dir.str(), curTime->tm_mon + 1, curTime->tm_mday, i, AMX_VERSION);
}

void AMXXLOG_Log(const char *fmt, ...)
{
	if (!amx_logging)
		return;
	int logType = (int)amx_logging->value;
	if (logType == 0)
		return;
	else if (logType == 1)
	{
		// build message
		// :TODO: Overflow possible here
		char msg[3072];
		va_list arglst;
		va_start(arglst, fmt);
		vsprintf(msg, fmt, arglst);
		va_end(arglst);

		// get time
		time_t td;
		time(&td);
		tm *curTime = localtime(&td);

		char date[32];
		strftime(date, 31, "%m/%d/%Y - %H:%M:%S", curTime);

		static bool s_inCreatingLogFile = false;
		FILE *pF = fopen(g_AMXXLOG_LogFile.str(), "a+");
		if (!pF)
		{
			if (s_inCreatingLogFile)
			{
				ALERT(at_logged, "[AMXX] Unexpected fatal logging error. AMXX Logging disabled.");
				CVAR_SET_FLOAT(init_amx_logging.name, 0.0f);
				return;
			}
			// Create new logfile
			s_inCreatingLogFile = true;
			AMXXLOG_MakeNewLogFile();
			s_inCreatingLogFile = false;
		}

		// log msg now
		fprintf(pF, "L %s: %s\n", date, msg);
		fclose(pF);
		print_srvconsole("L %s: %s\n", date, msg);
	}
	else if (logType == 2)
	{
		// build message
		// :TODO: Overflow possible here
		char msg[3072];
		va_list arglst;
		va_start(arglst, fmt);
		vsprintf(msg, fmt, arglst);
		va_end(arglst);
		ALERT(at_logged, msg);
	}
	else
	{
		ALERT(at_logged, "[AMXX] Invalid %s value. Setting to 0", init_amx_logging.name);
		CVAR_SET_FLOAT(init_amx_logging.name, 0.0f);
	}
}
