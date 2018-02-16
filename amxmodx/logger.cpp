#include <time.h>

#include "amxmodx.h"
#include "CLang.h"
#include "debugger.h"
#include "logger.h"

#if defined(_WIN32)
	#define PATH_SEP_CHAR		'\\'
	#define ALT_SEP_CHAR		'/'
#else
	#define PATH_SEP_CHAR		'/'
	#define ALT_SEP_CHAR		'\\'
#endif

//#define SHOW_PARSER_DEBUGGING
//#define SHOW_LOG_STRING_BUILDER
//#define SHOW_LOG_PATH
//#define SHOW_LOGGER_DETAILS
//#define SHOW_LOAD_LOGGER

NativeHandle<Logger> LoggerHandles;
int LoggerCreatedForward = -1;

char MapCounter[32];

const char* VERBOSITY[] = {
	"ERROR",
	"WARN",
	"INFO",
	"DEBUG"
};

int toIndex(int severity) {
	if (severity >= LOG_SEVERITY_ERROR) {
		return 0;
	} else if (severity >= LOG_SEVERITY_WARN) {
		return 1;
	} else if (severity >= LOG_SEVERITY_INFO) {
		return 2;
	} else {
		return 3;
	}
}

int Logger::m_MinLoggableVerbosity = LOG_SEVERITY_LOWEST;

int Logger::getPluginId() const {
	return m_PluginId;
}

bool Logger::isLogging() const {
	return m_Verbosity > LOG_SEVERITY_NONE && m_MinLoggableVerbosity > LOG_SEVERITY_NONE;
}

int Logger::getVerbosity() const {
	return m_Verbosity;
}

int Logger::setVerbosity(int verbosity) {
	int oldVerbosity = m_Verbosity;
	m_Verbosity = ke::Max(LOG_SEVERITY_NONE, verbosity);
	return oldVerbosity;
}

const char* Logger::getNameFormat() const {
	return m_NameFormat.chars();
}

void Logger::setNameFormat(const char* nameFormat) {
	m_NameFormat = nameFormat;
}

const char* Logger::getMessageFormat() const {
	return m_MessageFormat.chars();
}

void Logger::setMessageFormat(const char* messageFormat) {
	m_MessageFormat = messageFormat;
}

const char* Logger::getDateFormat() const {
	return m_DateFormat.chars();
}

void Logger::setDateFormat(const char* dateFormat) {
	m_DateFormat = dateFormat;
}

const char* Logger::getTimeFormat() const {
	return m_TimeFormat.chars();
}

void Logger::setTimeFormat(const char* timeFormat) {
	m_TimeFormat = timeFormat;
}

const char* Logger::getPathFormat() const {
	return m_PathFormat.chars();
}

void Logger::setPathFormat(const char* pathFormat) {
	m_PathFormat = pathFormat;
}

const char* Logger::getTraceFormat() const {
	return m_TraceFormat.chars();
}

void Logger::setTraceFormat(const char* traceFormat) {
	m_TraceFormat = traceFormat;
}

int strncpys(char *destination, const char *source, int len) {
	int count = 0;
	while (count < len && *source != '\0') {
		*destination++ = *source++;
		count++;
	}

	*destination = '\0';
	return count;
}

int strncpyc(char* destination, const char source, int len) {
	if (len > 0) {
		*destination = source;
		return 0;
	}

	return 1;
}

void pad(int len, int &offset, char* buffer, const int bufferLen) {
	for (; len > 0; len--, offset++) {
		strncpyc(buffer + offset, ' ', bufferLen - offset);
	}
}

void shift(char* str, int len, int right) {
	if (right <= 0) {
		return;
	}

	for (; len >= 0; len--) {
		*(str + len + right) = *(str + len);
	}
}

bool parseFormat(const char *&c, char &specifier, bool &lJustify, int &width, int &precision) {
	specifier = ' ';
	lJustify = false;
	width = -1;
	precision = -1;
	if (*c != '%') {
		return false;
	}

	int temp;
#ifdef SHOW_PARSER_DEBUGGING
	print_srvconsole("c=%c\n", *c);
#endif
	c++;
	switch (*c) {
		case '\0':
			return false;
		case '-':
#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole("- c=%c\n", *c);
#endif
			lJustify = true;
			c++;
			if (*c == '\0') {
				return false;
			}
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole("# c=%c", *c);
#endif
			if (0 <= (temp = *c - '0') && temp <= 9) {
				width = temp;
				c++;
				while (0 <= (temp = *c - '0') && temp <= 9) {
#ifdef SHOW_PARSER_DEBUGGING
					print_srvconsole("\n# c=%c", *c);
#endif
					width *= 10;
					width += temp;
					c++;
				}
			} else {
#ifdef SHOW_PARSER_DEBUGGING
				print_srvconsole("; next");
#endif
			}

#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole(";\n");
#endif

			if (*c == '\0') {
				return false;
			}
		case '.':
#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole(". c=%c", *c);
#endif
			if (*c == '.') {
				c++;
				if (0 <= (temp = *c - '0') && temp <= 9) {
#ifdef SHOW_PARSER_DEBUGGING
					print_srvconsole("\n# c=%c", *c);
#endif
					precision = temp;
					c++;
					while (0 <= (temp = *c - '0') && temp <= 9) {
#ifdef SHOW_PARSER_DEBUGGING
						print_srvconsole("\n# c=%c", *c);
#endif
						precision *= 10;
						precision += temp;
						c++;
					}
				} else {
					return false;
				}
			} else {
#ifdef SHOW_PARSER_DEBUGGING
				print_srvconsole("; next");
#endif
			}

#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole(";\n");
#endif

			if (*c == '\0') {
				return false;
			}

		case 'd': case 'f': case 'i': case 'l': case 'm':
		case 'n': case 'p': case 's': case 't': case 'v':
		case '%':
#ifdef SHOW_PARSER_DEBUGGING
			print_srvconsole("s c=%c\n", *c);
#endif
			switch (*c) {
				case 'd': case 'f': case 'i': case 'l': case 'm':
				case 'n': case 'p': case 's': case 't': case 'v':
				case '%':
					specifier = *c;
					return true;
			}
	}

	return false;
}

int parseLoggerString(const char *format,
	char *buffer, int bufferLen,
	const char *date,
	const char *message,
	const char *time,
	const char *severity,
	const char *script,
	const char *plugin,
	const char *function,
	const char *mapname,
	const char *line) {

#ifdef SHOW_LOG_STRING_BUILDER
	print_srvconsole("FORMAT: %s\n", format);
#endif

	int offset = 0;
	char specifier = ' ';
	bool lJustify = false;
	int len, width = -1, precision = -1;
	const char *c = format;
	for (; *c != '\0'; c++) {
#ifdef SHOW_LOG_STRING_BUILDER
		print_srvconsole("->%s|%s\n", buffer, c);
#endif
		if (*c != '%') {
			strncpyc(buffer + offset, *c, bufferLen - offset);
			offset++;
			continue;
		}

		bool result = parseFormat(c, specifier, lJustify, width, precision);
		assert(result);
		switch (specifier) {
			case 'd': len = strncpys(buffer + offset, date, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'f': len = strncpys(buffer + offset, function, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'i': len = strncpys(buffer + offset, MapCounter, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'l': len = strncpys(buffer + offset, line, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'm': len = strncpys(buffer + offset, mapname, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'n': len = strncpys(buffer + offset, script, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'p': len = strncpys(buffer + offset, plugin, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 's': len = strncpys(buffer + offset, message, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 't': len = strncpys(buffer + offset, time, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case 'v': len = strncpys(buffer + offset, severity, precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
			case '%': len = strncpyc(buffer + offset, '%', precision == -1 ? bufferLen - offset : ke::Min(bufferLen - offset, precision)); break;
		}

		if (lJustify) {
			offset += len;
			pad(width - len, offset, buffer, bufferLen);
		} else {
			shift(buffer + offset, len, width - len);
			pad(width - len, offset, buffer, bufferLen);
			offset += len;
		}
	}

#ifdef SHOW_LOG_STRING_BUILDER
	print_srvconsole("->%s|%s\n", buffer, c);
#endif
	strncpyc(buffer + offset, '\0', bufferLen - offset);
	return offset;
}

char* build_pathname_and_mkdir_r(char *buffer, size_t maxlen, const char *fmt, ...) {
	ke::SafeSprintf(buffer, maxlen, "%s%c", g_mod_name.chars(), PATH_SEP_CHAR);

	size_t len = strlen(buffer);
	char *ptr = buffer + len;

	va_list argptr;
	va_start(argptr, fmt);
	vsnprintf(ptr, maxlen - len, fmt, argptr);
	va_end(argptr);

	while (*ptr) {
		switch (*ptr) {
			case ALT_SEP_CHAR:
			case PATH_SEP_CHAR:
#if defined(__linux__) || defined(__APPLE__)
				mkdir(buffer, 0700);
#else
				*ptr = '\0';
				mkdir(buffer);
				*ptr = PATH_SEP_CHAR;
#endif
		}

		++ptr;
	}

	return buffer;
}

size_t getPluginFile(CPluginMngr::CPlugin *plugin, char *pluginFile) {
	strcpy(pluginFile, plugin->getName());
	size_t len = (strrchr(pluginFile, '.') - pluginFile);
	*(pluginFile + len) = '\0';
	return len;
}

void Logger::log(AMX* amx, int severity, const bool printStackTrace, const bool force, const char* msgFormat, ...) const {
	if (!force && (severity < Logger::getMinLoggableVerbosity() || severity < getVerbosity())) {
		return;
	}

	time_t td;
	time(&td);
	tm* curTime = localtime(&td);

	char date[16];
	int dateLen = strftime(date, sizeof date - 1, getDateFormat(), curTime);

	char time[16];
	int timeLen = strftime(time, sizeof time - 1, getTimeFormat(), curTime);

	static char message[4096];

	va_list arglst;
	va_start(arglst, msgFormat);
	ke::SafeVsprintf(message, sizeof message - 1, msgFormat, arglst);
	va_end(arglst);

	const char* severityStr = VERBOSITY[toIndex(severity)];

	long lLine;
	static char line[32];
	const char *scriptFile = "null", *function = "null";
	Debugger *pDebugger = (Debugger*)amx->userdata[UD_DEBUGGER];
	trace_info_t *pTrace = 0;
	/*if (pDebugger) {
		amx->error = AMX_ERR_NATIVE;
		pDebugger->SetTracedError(AMX_ERR_NATIVE);
		pDebugger->DisplayTrace("test trace");
		amx->error = -1;
	}*/

	if (pDebugger) {
		pTrace = pDebugger->GetTraceStart();
		if (pTrace) {
			pDebugger->GetTraceInfo(pTrace, lLine, function, scriptFile);
			int len = sprintf(line, "%ld", lLine + 1);
			line[len] = '\0';
		}
	}

	/*if (scriptFile[0] == '\0') {
		scriptFile = "null";
	}

	if (function[0] == '\0') {
		function = "null";
	}*/

	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);
	char pluginFile[64];
	getPluginFile(plugin, pluginFile);

	CPluginMngr::CPlugin *loggerPlugin = g_plugins.findPlugin(getPluginId());
	char loggerPluginFile[64];
	getPluginFile(loggerPlugin, loggerPluginFile);

	static char formattedMessage[4096];
	int messageLen = parseLoggerString(
		getMessageFormat(),
		formattedMessage, sizeof formattedMessage - 2,
		date,
		message,
		time,
		severityStr,
		scriptFile,
		pluginFile,
		function,
		STRING(gpGlobals->mapname),
		line);
	// Special case, append newline
	*(formattedMessage + messageLen) = '\n';
	*(formattedMessage + messageLen + 1) = '\0';

	static char fileName[256];
	int fileNameLen = parseLoggerString(
		getNameFormat(),
		fileName, sizeof fileName - 1,
		date,
		message,
		time,
		severityStr,
		scriptFile,
		loggerPluginFile,
		function,
		STRING(gpGlobals->mapname),
		line);

	static char path[256];
	int pathLen = parseLoggerString(
		getPathFormat(),
		path, sizeof path - 1,
		date,
		message,
		time,
		severityStr,
		scriptFile,
		loggerPluginFile,
		function,
		STRING(gpGlobals->mapname),
		line);

	static const char *amxxLogsDir;
	if (!amxxLogsDir) {
		amxxLogsDir = get_localinfo("amxx_logsdir", "addons/amxmodx/logs");
	}

	static char fullPath[256];
	if (getPathFormat()[0] != '\0') {
		build_pathname_and_mkdir_r(fullPath, sizeof fullPath - 1, "%s/%s/%s.log", amxxLogsDir, path, fileName);
	} else {
		build_pathname_and_mkdir_r(fullPath, sizeof fullPath - 1, "%s/%s.log", amxxLogsDir, fileName);
	}

#ifdef SHOW_LOG_PATH
	print_srvconsole("PATH=%s\n", fullPath);
#endif
	FILE *pF = NULL;
	pF = fopen(fullPath, "a+");
	if (!pF) {
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+).\n", fullPath);
	}

	fprintf(pF, formattedMessage);
	print_srvconsole(formattedMessage);

	static char trace[256];
	if (printStackTrace) {
		if (!pDebugger) {
			AMXXLOG_Log("Warning: Cannot print stack trace unless plugin is running in debug mode! "
				"To enable debug mode, add \"debug\" after the plugin name in plugins.ini (without quotes).");
		} else {
			while (pTrace) {
				pDebugger->GetTraceInfo(pTrace, lLine, function, scriptFile);
				int len = sprintf(line, "%ld", lLine + 1);
				line[len] = '\0';

				int traceLen = parseLoggerString(
					getTraceFormat(),
					trace, sizeof trace - 2,
					date,
					message,
					time,
					severityStr,
					scriptFile,
					pluginFile,
					function,
					STRING(gpGlobals->mapname),
					line);
				// Special case, append newline
				*(trace + traceLen) = '\n';
				*(trace + traceLen + 1) = '\0';

				fprintf(pF, trace);
				print_srvconsole(trace);
				pTrace = pDebugger->GetNextTrace(pTrace);
			}
		}
	}

	fclose(pF);
}

bool isValidLoggerFormat(const char *str, int &percentLoc, int &errorLoc) {
	percentLoc = -1;
	errorLoc = -1;

	char specifier;
	bool lJustify;
	int width, precision;
	const char *c = str;
	for (; *c != '\0'; c++) {
		if (*c != '%') {
			continue;
		}

		percentLoc = c - str;
		if (!parseFormat(c, specifier, lJustify, width, precision)) {
			errorLoc = c - str;
			return false;
		}
	}

	return true;
}

cell getOrCreateLogger(CPluginMngr::CPlugin *plugin) {
	cell loggerId = plugin->getLogger();
	if (loggerId == INVALID_LOGGER) {
#ifdef SHOW_LOGGER_DETAILS
		print_srvconsole("Creating logger for %s (%d)...\n", plugin->getName(), plugin->getId());
#endif
		loggerId = LoggerHandles.create(
			plugin->getId(),
			LOG_SEVERITY_WARN,
			"%p_%d",
			"[%5v] [%t] %n::%f - %s",
			"%Y-%m-%d",
			"%H:%M:%S",
			"",
			"    at %n::%f : %l");
		plugin->setLogger(loggerId);
		Logger* logger = LoggerHandles.lookup(loggerId);
		if (LoggerCreatedForward > 0) {
			char pluginFile[64];
			getPluginFile(plugin, pluginFile);
			executeForwards(LoggerCreatedForward,
				static_cast<cell>(plugin->getId()),
				loggerId,
				static_cast<cell>(LOG_SEVERITY_WARN),
				pluginFile,
				logger->getNameFormat(),
				logger->getMessageFormat(),
				logger->getDateFormat(),
				logger->getTimeFormat(),
				logger->getPathFormat(),
				logger->getTraceFormat());
		}

		return loggerId;
	}

	return loggerId;
}

cell getOrCreateLogger(AMX* amx) {
	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);
	return getOrCreateLogger(plugin);
}

void Logger::onMapChange() {
	char dataFile[256];
	const char* amxxDataDir = get_localinfo("amxx_datadir", "addons/amxmodx/data");
	build_pathname_r(dataFile, sizeof dataFile - 1, "%s/logger.dat", amxxDataDir);
	FILE *pF = NULL;
	pF = fopen(dataFile, "a+");
	if (pF) {
		rewind(pF);

		char timestamp[32], count[32];
		fgets(timestamp, sizeof timestamp - 1, pF);
		long long nextReset = atoll(timestamp);
		fgets(count, sizeof count - 1, pF);
		int counter = atoi(count);
		time_t t1;
		time(&t1);
		localtime(&t1);
		pF = freopen(dataFile, "w", pF);
		assert(pF);
		if (nextReset < t1) {
			time(&t1);
			t1 += 86400;
			tm* t2 = localtime(&t1);
			t2->tm_hour = 0;
			t2->tm_min = 0;
			t2->tm_sec = 0;
			time_t t3 = mktime(t2);
			counter = 1;
			fprintf(pF, "%lld\n", static_cast<long long>(t3));
			fprintf(pF, "%d", counter);
		} else {
			counter++;
			fprintf(pF, "%s", timestamp);
			fprintf(pF, "%d", counter);
		}

		fflush(pF);
		fclose(pF);
		ke::SafeSprintf(MapCounter, sizeof MapCounter - 1, "%d", counter);
	} else {
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+).\n", dataFile);
		return;
	}
}

// native Logger(const LoggerVerbosity: verbosity, const format[], any: ...);
static cell AMX_NATIVE_CALL LoggerLog(AMX* amx, cell* params) {
	if (params[1] < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	if (!logger->isLogging()) {
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 2, len);
	logger->log(amx, params[1], params[1] >= LOG_SEVERITY_WARN, false, buffer);
	return 1;
}

// native LoggerLogDebug(const format[], any: ...);
static cell AMX_NATIVE_CALL LoggerLogDebug(AMX* amx, cell* params) {
	if (LOG_SEVERITY_DEBUG < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	if (!logger->isLogging()) {
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 1, len);
	logger->log(amx, LOG_SEVERITY_DEBUG, false, false, buffer);
	return 1;
}

// native LoggerLogInfo(const format[], any: ...);
static cell AMX_NATIVE_CALL LoggerLogInfo(AMX* amx, cell* params) {
	if (LOG_SEVERITY_INFO < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	if (!logger->isLogging()) {
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 1, len);
	logger->log(amx, LOG_SEVERITY_INFO, false, false, buffer);
	return 1;
}

// native LoggerLogWarn(const format[], any: ...);
static cell AMX_NATIVE_CALL LoggerLogWarn(AMX* amx, cell* params) {
	if (LOG_SEVERITY_WARN < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	if (!logger->isLogging()) {
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 1, len);
	logger->log(amx, LOG_SEVERITY_WARN, true, false, buffer);
	return 1;
}

// native LoggerLogError(const format[], any: ...);
static cell AMX_NATIVE_CALL LoggerLogError(AMX* amx, cell* params) {
	if (LOG_SEVERITY_ERROR < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	if (!logger->isLogging()) {
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 1, len);
	logger->log(amx, LOG_SEVERITY_ERROR, true, false, buffer);
	return 1;
}

// native LoadLogger(const pluginId);
static cell AMX_NATIVE_CALL LoadLogger(AMX* amx, cell* params) {
	CPluginMngr::CPlugin *thisPlugin = g_plugins.findPluginFast(amx);
	CPluginMngr::CPlugin *otherPlugin = g_plugins.findPlugin((int)params[1]);

	cell loggerId = getOrCreateLogger(otherPlugin);
#ifdef SHOW_LOAD_LOGGER
	print_srvconsole("%s::LoadLogger(%s)=%d\n", thisPlugin->getName(), otherPlugin->getName(), loggerId);
#endif
	ASSERT(loggerId > 0);

	thisPlugin->setLogger(loggerId);
	return 1;
}

// native LoggerVerbosity: GetLoggerVerbosity();
static cell AMX_NATIVE_CALL GetLoggerVerbosity(AMX* amx, cell* params) {
	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	return logger->getVerbosity();
}

// native LoggerVerbosity: SetLoggerVerbosity(const LoggerVerbosity: verbosity);
static cell AMX_NATIVE_CALL SetLoggerVerbosity(AMX* amx, cell* params) {
	if (params[1] < LOG_SEVERITY_NONE) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger verbosity level provided (%d)", params[1]);
		return 0;
	}

	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	return logger->setVerbosity(params[1]);
}

// native GetLoggerFormat(const LoggerFormat: type, format[], const maxLength);
static cell AMX_NATIVE_CALL GetLoggerFormat(AMX* amx, cell* params) {
	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	switch (params[1]) {
		case LOG_FORMAT_NAME:
			return set_amxstring(amx, params[2], logger->getNameFormat(), params[3]);
		case LOG_FORMAT_MESSAGE:
			return set_amxstring(amx, params[2], logger->getMessageFormat(), params[3]);
		case LOG_FORMAT_DATE:
			return set_amxstring(amx, params[2], logger->getDateFormat(), params[3]);
		case LOG_FORMAT_TIME:
			return set_amxstring(amx, params[2], logger->getTimeFormat(), params[3]);
		case LOG_FORMAT_PATH:
			return set_amxstring(amx, params[2], logger->getPathFormat(), params[3]);
		case LOG_FORMAT_TRACE:
			return set_amxstring(amx, params[2], logger->getTraceFormat(), params[3]);
		default:
			LogError(amx, AMX_ERR_NATIVE, "Invalid logger format id provided (%d)", params[1]);
			return 0;
	}
}

// native SetLoggerFormat(const LoggerFormat: type, const format[]);
static cell AMX_NATIVE_CALL SetLoggerFormat(AMX* amx, cell* params) {
	cell loggerId = getOrCreateLogger(amx);
	ASSERT(loggerId > 0);

	Logger* logger = LoggerHandles.lookup(loggerId);
	ASSERT(logger);

	int len, percentLoc, errorLoc;
	const char* format = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(format, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, format + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger format provided: \"%s\" (position %d = \"%s\")", format, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	switch (params[1]) {
		case LOG_FORMAT_NAME:
			logger->setNameFormat(format);
			break;
		case LOG_FORMAT_MESSAGE:
			logger->setMessageFormat(format);
			break;
		case LOG_FORMAT_DATE:
			logger->setDateFormat(format);
			break;
		case LOG_FORMAT_TIME:
			logger->setTimeFormat(format);
			break;
		case LOG_FORMAT_PATH:
			logger->setPathFormat(format);
			break;
		case LOG_FORMAT_TRACE:
			logger->setTraceFormat(format);
			break;
		default:
			LogError(amx, AMX_ERR_NATIVE, "Invalid logger format id provided (%d)", params[1]);
			return 0;
	}
	return 1;
}

// native LoggerVerbosity: GetGlobalLoggerVerbosity();
static cell AMX_NATIVE_CALL GetGlobalLoggerVerbosity(AMX* amx, cell* params) {
	return Logger::getMinLoggableVerbosity();
}

// native LoggerVerbosity: SetGlobalLoggerVerbosity(const LoggerVerbosity: verbosity);
static cell AMX_NATIVE_CALL SetGlobalLoggerVerbosity(AMX* amx, cell* params) {
	if (params[1] < LOG_SEVERITY_NONE) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger verbosity level provided (%d)", params[1]);
		return 0;
	}

	return Logger::setMinLoggableVerbosity(params[1]);
}

AMX_NATIVE_INFO logger_Natives[] = {
	{ "Logger",						LoggerLog },

	{ "LoggerDebug",				LoggerLogDebug },
	{ "LoggerInfo",					LoggerLogInfo },
	{ "LoggerWarn",					LoggerLogWarn },
	{ "LoggerError",				LoggerLogError },

	{ "LoadLogger",					LoadLogger },

	{ "GetLoggerVerbosity",			GetLoggerVerbosity },
	{ "SetLoggerVerbosity",			SetLoggerVerbosity },

	{ "GetLoggerFormat",			GetLoggerFormat },
	{ "SetLoggerFormat",			SetLoggerFormat },

	{ "GetGlobalLoggerVerbosity",	GetGlobalLoggerVerbosity },
	{ "SetGlobalLoggerVerbosity",	SetGlobalLoggerVerbosity },

	{ nullptr,						nullptr }
};
