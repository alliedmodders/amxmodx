#include <time.h>
#include "amxmodx.h"
#include "binlog.h"

#if defined BINLOG_ENABLED

BinLog g_BinLog;

bool BinLog::Open()
{
	const char *data = get_localinfo("amxmodx_datadir", "addons/amxmodx/data");
	char path[255];
	build_pathname_r(path, sizeof(path)-1, "%s/binlogs", data);
	
	if (!DirExists(path))
	{
		mkdir(path
#if defined __linux__
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
	m_logfile.assign(file);
	build_pathname_r(file, sizeof(file)-1, "%s/binlogs/bindb%04d.bdb", data, lastcntr);
	m_dbfile.assign(file);

	return true;
}

void BinLog::Close()
{
	//dummy function - logs are not kept open
}

void BinLog::WriteOp(BinLogOp op, int plug, ...)
{
	FILE *fp = fopen(m_logfile.c_str(), "ab");
	if (!fp)
		return;

	unsigned char c = static_cast<char>(op);
	time_t t = time(NULL);
	float gt = gpGlobals->time;
	fwrite(&c, sizeof(char), 1, fp);
	fwrite(&t, sizeof(time_t), 1, fp);
	fwrite(&gt, sizeof(float), 1, fp);
	fwrite(&plug, sizeof(int), 1, fp);

	va_list ap;
	va_start(ap, plug);

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
	};

	va_end(ap);
}

void BinLog::CacheAllPlugins()
{
	FILE *fp = fopen(m_dbfile.c_str(), "wb");
	if (!fp)
		return;

	unsigned int magic = BINDB_MAGIC;
	unsigned short vers = BINDB_VERSION;

	fwrite(&magic, sizeof(unsigned int), 1, fp);
	fwrite(&vers, sizeof(unsigned short), 1, fp);

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
		len = (char)strlen(pl->getName());
		fwrite(&len, sizeof(char), 1, fp);
		len++;
		fwrite(pl->getName(), sizeof(char), len, fp);
		int natives, publics;
		AMX *amx = pl->getAMX();
		amx_NumNatives(amx, &natives);
		amx_NumPublics(amx, &publics);
		fwrite(&natives, sizeof(int), 1, fp);
		fwrite(&publics, sizeof(int), 1, fp);
		char name[34];
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
	}
	fclose(fp);

	/**
	 * it's now safe to create the binary log
	 */
	fp = fopen(m_logfile.c_str(), "wb");
	if (!fp)
		return;

	magic = BINLOG_MAGIC;
	vers = BINLOG_VERSION;
	c = sizeof(time_t);
	fwrite(&magic, sizeof(int), 1, fp);
	fwrite(&vers, sizeof(short), 1, fp);
	fwrite(&c, sizeof(char), 1, fp);
	fclose(fp);

	WriteOp(BinLog_Start, -1);
}

#endif //BINLOG_ENABLED
