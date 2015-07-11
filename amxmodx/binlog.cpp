// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined BINLOG_ENABLED

#include <time.h>
#include "amxmodx.h"
#include "binlog.h"
#include "debugger.h"

BinLog g_BinLog;
int g_binlog_level = 0;
int g_binlog_maxsize = 0;

// Helper function to get a filename index
#define USHR(x) ((unsigned int)(x)>>1)
int LookupFile(AMX_DBG *amxdbg, ucell address)
{
	int high, low, mid;

	high = amxdbg->hdr->files;
	low = -1;

	while (high - low > 1)
	{
		mid = USHR(low + high);
		if (amxdbg->filetbl[mid]->address <= address)
		{
			low = mid;
		} else {
			high = mid;
		}
	}

	if (low == -1)
	{
		return -1;
	}

	return low;
}

bool BinLog::Open()
{
	const char *data = get_localinfo("amxmodx_datadir", "addons/amxmodx/data");
	char path[255];
	build_pathname_r(path, sizeof(path)-1, "%s/binlogs", data);
	
	if (!DirExists(path))
	{
		mkdir(path
#if defined(__linux__) || defined(__APPLE__)
			, 0755
#endif
			);
		if (!DirExists(path))
			return false;
	}

	char file[255];
	build_pathname_r(file, sizeof(file)-1, "%s/binlogs/lastlog", data);

	unsigned int lastcntr = 0;
	FILE *lastlog = fopen(file, "rb");
	if (lastlog)
	{
		if (fread(&lastcntr, sizeof(int), 1, lastlog) != 1)
			lastcntr = 0;
		fclose(lastlog);
	}
	lastlog = fopen(file, "wb");
	if (lastlog)
	{
		lastcntr++;
		fwrite(&lastcntr, sizeof(int), 1, lastlog);
		fclose(lastlog);
	}
	build_pathname_r(file, sizeof(file)-1, "%s/binlogs/binlog%04d.blg", data, lastcntr);
	m_logfile = file;

	/**
	* it's now safe to create the binary log
	*/
	FILE *fp = fopen(m_logfile.chars(), "wb");
	if (!fp)
		return false;

	int magic = BINLOG_MAGIC;
	short vers = BINLOG_VERSION;
	char c = sizeof(time_t);
	fwrite(&magic, sizeof(int), 1, fp);
	fwrite(&vers, sizeof(short), 1, fp);
	fwrite(&c, sizeof(char), 1, fp);

	WritePluginDB(fp);
	fclose(fp);

	m_state = true;

	WriteOp(BinLog_Start, -1);

	return true;
}

void BinLog::Close()
{
	WriteOp(BinLog_End, -1);
	m_state = false;
}

void BinLog::WriteOp(BinLogOp op, int plug, ...)
{
	if (!m_state)
		return;

	FILE *fp = fopen(m_logfile.chars(), "ab");
	if (!fp)
		return;

	if (g_binlog_maxsize && op != BinLog_End)
	{
		fseek(fp, 0, SEEK_END);
		if (ftell(fp) > (g_binlog_maxsize * (1024 * 1024)))
		{
			fclose(fp);
			Close();
			Open();
			fp = fopen(m_logfile.chars(), "ab");
			if (!fp)
				return;
		}
	}

	unsigned char c = static_cast<char>(op);
	time_t t = time(NULL);
	float gt = gpGlobals->time;
	fwrite(&c, sizeof(char), 1, fp);
	fwrite(&t, sizeof(time_t), 1, fp);
	fwrite(&gt, sizeof(float), 1, fp);
	fwrite(&plug, sizeof(int), 1, fp);

	va_list ap;
	va_start(ap, plug);

	AMX *amx = NULL;
	bool debug = false;
	AMX_DBG *dbg = NULL;
	CPluginMngr::CPlugin *pl = NULL;

	if (plug != -1)
	{
		pl = g_plugins.findPlugin(plug);
		if ((debug = pl->isDebug()))
		{
			amx = pl->getAMX();
			dbg = static_cast<Debugger *>(amx->userdata[UD_DEBUGGER])->m_pAmxDbg;
		}
	}

	switch (c)
	{
	case BinLog_Registered:
		{
			const char *title = va_arg(ap, const char *);
			const char *vers = va_arg(ap, const char *);
			c = (char)strlen(title);
			fwrite(&c, sizeof(char), 1, fp);
			fwrite(title, sizeof(char), c+1, fp);
			c = (char)strlen(vers);
			fwrite(&c, sizeof(char), 1 ,fp);
			fwrite(vers, sizeof(char), c+1, fp);
			break;
		}
	case BinLog_NativeCall:
		{
			int file;
			int native = va_arg(ap, int);
			int params = va_arg(ap, int);
			fwrite(&native, sizeof(int), 1, fp);
			fwrite(&params, sizeof(int), 1, fp);
			if (debug)
			{
				file = LookupFile(dbg, amx->cip);
				fwrite(&file, sizeof(int), 1, fp);
			} else {
				file = 0;
				fwrite(&file, sizeof(int), 1, fp);
			}
			break;
		}
	case BinLog_NativeRet:
		{
			cell retval = va_arg(ap, cell);
			fwrite(&retval, sizeof(cell), 1, fp);
			break;
		}
	case BinLog_NativeError:
		{
			int err = va_arg(ap, int);
			const char *msg = va_arg(ap, const char *);
			short len = (short)strlen(msg);
			fwrite(&err, sizeof(int), 1, fp);
			fwrite(&len, sizeof(short), 1, fp);
			fwrite(msg, sizeof(char), len+1, fp);
			break;
		}
	case BinLog_CallPubFunc:
		{
			int file;
			int num = va_arg(ap, int);
			fwrite(&num, sizeof(int), 1, fp);
			if (debug)
			{
				file = LookupFile(dbg, amx->cip);
				fwrite(&file, sizeof(int), 1, fp);
			} else {
				file = 0;
				fwrite(&file, sizeof(int), 1, fp);
			}
			break;
		}
	case BinLog_SetLine:
		{
			int file;
			int line = va_arg(ap, int);
			fwrite(&line, sizeof(int), 1, fp);
			if (debug)
			{
				file = LookupFile(dbg, amx->cip);
				fwrite(&file, sizeof(int), 1, fp);
			} else {
				file = 0;
				fwrite(&file, sizeof(int), 1, fp);
			}
			break;
		}
	case BinLog_FormatString:
		{
			int param = va_arg(ap, int);
			int maxlen = va_arg(ap, int);
			const char *str = va_arg(ap, const char *);
			short len = (short)strlen(str);
			fwrite(&param, sizeof(int), 1, fp);
			fwrite(&maxlen, sizeof(int), 1, fp);
			fwrite(&len, sizeof(short), 1, fp);
			fwrite(str, sizeof(char), len+1, fp);
			break;
		}
	case BinLog_NativeParams:
		{
			cell *params = va_arg(ap, cell *);
			cell num = params[0] / sizeof(cell);
			fwrite(&num, sizeof(cell), 1, fp);
			for (cell i=1; i<=num; i++)
				fwrite(&(params[i]), sizeof(cell), 1, fp);
			break;
		}
	case BinLog_GetString:
		{
			cell addr = va_arg(ap, cell);
			const char *str = va_arg(ap, const char *);
			short len = (short)strlen(str);
			fwrite(&addr, sizeof(cell), 1, fp);
			fwrite(&len, sizeof(short), 1, fp);
			fwrite(str, sizeof(char), len+1, fp);
			break;
		}
	case BinLog_SetString:
		{
			cell addr = va_arg(ap, cell);
			int maxlen = va_arg(ap, int);
			const char *str = va_arg(ap, const char *);
			short len = (short)strlen(str);
			fwrite(&addr, sizeof(cell), 1, fp);
			fwrite(&maxlen, sizeof(int), 1, fp);
			fwrite(&len, sizeof(short), 1, fp);
			fwrite(str, sizeof(char), len+1, fp);
			break;
		}
	};

	va_end(ap);
	fclose(fp);
}

void BinLog::WritePluginDB(FILE *fp)
{
	int num = g_plugins.getPluginsNum();
	fwrite(&num, sizeof(int), 1, fp);

	CPluginMngr::CPlugin *pl;
	char c;
	unsigned char len;
	for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
	{
		pl = &(*iter);
		if (pl->isValid())
			c = 1;
		else
			c = 0;
		if (c && pl->isDebug())
			c = 2;
		fwrite(&c, sizeof(char), 1, fp);
		if (c)
		{
			Debugger *pd = NULL;
			len = (char)strlen(pl->getName());
			fwrite(&len, sizeof(char), 1, fp);
			len++;
			fwrite(pl->getName(), sizeof(char), len, fp);
			int natives, publics, files;
			AMX *amx = pl->getAMX();
			// Write the number of Filenames
			if (c == 2)
			{
				pd = static_cast<Debugger *>(amx->userdata[UD_DEBUGGER]);
				files = pd->m_pAmxDbg->hdr->files;
				fwrite(&files, sizeof(int), 1, fp);
			}
			amx_NumNatives(amx, &natives);
			amx_NumPublics(amx, &publics);
			fwrite(&natives, sizeof(int), 1, fp);
			fwrite(&publics, sizeof(int), 1, fp);
			char name[34];
			// Write the Filenames to the binfile
			if (c == 2)
			{
				AMX_DBG_FILE **ftable = pd->m_pAmxDbg->filetbl;
				for (int i=0; i<files; i++)
				{
					len = (char)strlen(ftable[i]->name);
					fwrite(&len, sizeof(char), 1, fp);
					len++;
					fwrite(ftable[i]->name, sizeof(char), len, fp);
				}
			}
			for (int i=0; i<natives; i++)
			{
				amx_GetNative(amx, i, name);
				len = (char)strlen(name);
				fwrite(&len, sizeof(char), 1, fp);
				len++;
				fwrite(name, sizeof(char), len, fp);
			}
			for (int i=0; i<publics; i++)
			{
				amx_GetPublic(amx, i, name);
				len = (char)strlen(name);
				fwrite(&len, sizeof(char), 1, fp);
				len++;
				fwrite(name, sizeof(char), len, fp);
			}
		} else {
			char empty[] = " ";
			len = 1;
			fwrite(&len, sizeof(char), 1, fp);
			fwrite(empty, sizeof(char), len, fp);
			int no = 0;
			fwrite(&no, sizeof(int), 1, fp);
			fwrite(&no, sizeof(int), 1, fp);
		}
	}
}

#endif //BINLOG_ENABLED
