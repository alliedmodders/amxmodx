#include <algorithm>
#include <time.h>

#include <am-string.h>

#include "amxmodx.h"
#include "CLang.h"
#include "debugger.h"
#include "modules.h"
#include "logger.h"

//#define SHOW_PARSER_DEBUGGING
//#define SHOW_LOG_STRING_BUILDER
//#define SHOW_LOG_PATH
//#define SHOW_LOGGER_DETAILS
#define INVALID_LOGGER  0
#define ALL_LOGGERS    -1

using namespace std;

NativeHandle<Logger> LoggerHandles;
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
	}
	else if (severity >= LOG_SEVERITY_WARN) {
		return 1;
	}
	else if (severity >= LOG_SEVERITY_INFO) {
		return 2;
	}
	else {
		return 3;
	}
}

int Logger::m_MinLoggableVerbosity = LOG_SEVERITY_LOWEST;

int Logger::getVerbosity() const {
	return m_Verbosity;
}

int Logger::setVerbosity(int verbosity) {
	int oldVerbosity = m_Verbosity;
	m_Verbosity = max(LOG_SEVERITY_NONE, verbosity);
	return oldVerbosity;
}

const char* Logger::getNameFormat() const {
	return m_pNameFormat.chars();
}

void Logger::setNameFormat(const char* nameFormat) {
	m_pNameFormat = nameFormat;
}

const char* Logger::getMessageFormat() const {
	return m_pMessageFormat.chars();
}

void Logger::setMessageFormat(const char* messageFormat) {
	m_pMessageFormat = messageFormat;
}

const char* Logger::getDateFormat() const {
	return m_pDateFormat.chars();
}

void Logger::setDateFormat(const char* dateFormat) {
	m_pDateFormat = dateFormat;
}

const char* Logger::getTimeFormat() const {
	return m_pTimeFormat.chars();
}

void Logger::setTimeFormat(const char* timeFormat) {
	m_pTimeFormat = timeFormat;
}

const char* Logger::getPathFormat() const {
	return m_pPathFormat.chars();
}

void Logger::setPathFormat(const char* pathFormat) {
	m_pPathFormat = pathFormat;
}

const char* Logger::getTraceFormat() const {
	return m_pTraceFormat.chars();
}

void Logger::setTraceFormat(const char* traceFormat) {
	m_pTraceFormat = traceFormat;
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
		}
		else {
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
			}
			else {
				return false;
			}
		}
		else {
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
		case 'd': len = strncpys(buffer + offset, date, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'f': len = strncpys(buffer + offset, function, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'i': len = strncpys(buffer + offset, MapCounter, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'l': len = strncpys(buffer + offset, line, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'm': len = strncpys(buffer + offset, mapname, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'n': len = strncpys(buffer + offset, script, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'p': len = strncpys(buffer + offset, plugin, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 's': len = strncpys(buffer + offset, message, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 't': len = strncpys(buffer + offset, time, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case 'v': len = strncpys(buffer + offset, severity, precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		case '%': len = strncpyc(buffer + offset, '%', precision == -1 ? bufferLen - offset : min(bufferLen - offset, precision)); break;
		}

		if (lJustify) {
			offset += len;
			pad(width - len, offset, buffer, bufferLen);
		}
		else {
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
	UTIL_Format(buffer, maxlen, "%s%c", g_mod_name.chars(), PATH_SEP_CHAR);

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

void Logger::log(AMX* amx, int severity, const bool printStackTrace, const char* msgFormat, ...) const {
	if (severity < Logger::getMinLoggableVerbosity() || severity < getVerbosity()) {
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
	const char *scriptFile = "", *function = "";
	Debugger *pDebugger = (Debugger*)amx->userdata[UD_DEBUGGER];
	//pDebugger->BeginExec();
	trace_info_t *pTrace = pDebugger->GetTraceStart();
	//amx->error = AMX_ERR_NONE;
	//pDebugger->SetTracedError(amx->error);
	if (pTrace) {
		pDebugger->GetTraceInfo(pTrace, lLine, function, scriptFile);
		int len = sprintf(line, "%ld", lLine + 1);
		line[len] = '\0';
	}

	CPluginMngr::CPlugin *plugin = (CPluginMngr::CPlugin*)amx->userdata[UD_FINDPLUGIN];
	static char pluginFile[64];
	strcpy(pluginFile, plugin->getName());
	*strrchr(pluginFile, '.') = '\0';

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
		pluginFile,
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
		pluginFile,
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
	}
	else {
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

	fclose(pF);

	//amx->error = -1;
	//pDebugger->EndExec();
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

// native Logger:LoggerCreate(
//		const verbosity = DEFAULT_LOGGER_VERBOSITY,
//		const nameFormat[] = DEFAULT_LOGGER_NAME_FORMAT,
//		const msgFormat[] = DEFAULT_LOGGER_MSG_FORMAT,
//		const dateFormat[] = DEFAULT_LOGGER_DATE_FORMAT,
//		const timeFormat[] = DEFAULT_LOGGER_TIME_FORMAT,
//		const path[] = DEFAULT_LOGGER_PATH,
//		const traceFormat[] = DEFAULT_LOGGER_TRACE_FORMAT);
static cell AMX_NATIVE_CALL LoggerCreate(AMX* amx, cell* params) {
	int len, percentLoc, errorLoc;
	int verbosity = params[1];
	char* nameFormat = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(nameFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, nameFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger name format provided: \"%s\" (position %d = \"%s\")", nameFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	char* msgFormat = get_amxstring(amx, params[3], 1, len);
	if (!isValidLoggerFormat(msgFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, msgFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger message format provided: \"%s\" (position %d = \"%s\")", msgFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	char* dateFormat = get_amxstring(amx, params[4], 2, len);
	char* timeFormat = get_amxstring(amx, params[5], 3, len);
	char* path = get_amxstring(amx, params[6], 4, len);
	if (!isValidLoggerFormat(path, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, path + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger path format provided: \"%s\" (position %d = \"%s\")", path, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}
	char* traceFormat = get_amxstring(amx, params[7], 5, len);
	if (!isValidLoggerFormat(traceFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, traceFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger trace format provided: \"%s\" (position %d = \"%s\")", traceFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

#ifdef SHOW_LOGGER_DETAILS
	print_srvconsole("Creating logger...\n");
	print_srvconsole("  nameFormat=%s\n", nameFormat);
	print_srvconsole("  msgFormat=%s\n", msgFormat);
	print_srvconsole("  dateFormat=%s\n", dateFormat);
	print_srvconsole("  timeFormat=%s\n", timeFormat);
	print_srvconsole("  pathFormat=%s\n", path);
	print_srvconsole("  traceFormat=%s\n", traceFormat);
#endif

	int loggerHandle = LoggerHandles.create(
		verbosity,
		nameFormat,
		msgFormat,
		dateFormat,
		timeFormat,
		path,
		traceFormat);

	Logger *logger = LoggerHandles.lookup(loggerHandle);
	assert(logger);
	CPluginMngr::CPlugin *p = (CPluginMngr::CPlugin*)amx->userdata[UD_FINDPLUGIN];
	logger->log(amx, LOG_SEVERITY_INFO, false, "Logger initialized; map: %s", STRING(gpGlobals->mapname));
	return static_cast<cell>(loggerHandle);
}

// native bool:LoggerDestroy(&Logger:logger);
static cell AMX_NATIVE_CALL LoggerDestroy(AMX* amx, cell* params) {
	cell* ptr = get_amxaddr(amx, params[1]);
	Logger* logger = LoggerHandles.lookup(*ptr);
	if (!logger) {
		return 0;
	}

	if (LoggerHandles.destroy(*ptr)) {
		*ptr = 0;
		return 1;
	}

	return 0;
}

// native Severity:LoggerGetVerbosity(const Logger:logger);
static cell AMX_NATIVE_CALL LoggerGetVerbosity(AMX* amx, cell* params) {
	if (params[1] == ALL_LOGGERS) {
		return Logger::getMinLoggableVerbosity();
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return logger->getVerbosity();
}

// native Severity:LoggerSetVerbosity(const Logger:logger, const Severity:verbosity);
static cell AMX_NATIVE_CALL LoggerSetVerbosity(AMX* amx, cell* params) {
	if (params[2] < LOG_SEVERITY_NONE) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger verbosity level provided (%d)", params[2]);
		return 0;
	}

	if (params[1] == ALL_LOGGERS) {
		return Logger::setMinLoggableVerbosity(params[2]);
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return logger->setVerbosity(params[2]);
}

// native LoggerGetNameFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetNameFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getNameFormat(), params[3]);
}

// native LoggerSetNameFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetNameFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len, percentLoc, errorLoc;
	const char* nameFormat = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(nameFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, nameFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger name format provided: \"%s\" (position %d = \"%s\")", nameFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	logger->setNameFormat(nameFormat);
	return 1;
}

// native LoggerGetMessageFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetMessageFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getMessageFormat(), params[3]);
}

// native LoggerSetMessageFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetMessageFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len, percentLoc, errorLoc;
	const char* messageFormat = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(messageFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, messageFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger message format provided: \"%s\" (position %d = \"%s\")", messageFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	logger->setMessageFormat(messageFormat);
	return 1;
}

// native LoggerGetDateFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetDateFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getDateFormat(), params[3]);
}

// native LoggerSetDateFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetDateFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char* dateFormat = get_amxstring(amx, params[2], 0, len);
	logger->setDateFormat(dateFormat);
	return 1;
}

// native LoggerGetTimeFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetTimeFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getTimeFormat(), params[3]);
}

// native LoggerSetTimeFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetTimeFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char* timeFormat = get_amxstring(amx, params[2], 0, len);
	logger->setTimeFormat(timeFormat);
	return 1;
}

// native LoggerGetPathFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetPathFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getPathFormat(), params[3]);
}

// native LoggerSetPathFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetPathFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len, percentLoc, errorLoc;
	const char* pathFormat = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(pathFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, pathFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger path format provided: \"%s\" (position %d = \"%s\")", pathFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	logger->setPathFormat(pathFormat);
	return 1;
}

// native LoggerGetTraceFormat(const Logger:logger, format[], const len);
static cell AMX_NATIVE_CALL LoggerGetTraceFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	return 	set_amxstring(amx, params[2], logger->getTraceFormat(), params[3]);
}

// native LoggerSetTraceFormat(const Logger:logger, const format[]);
static cell AMX_NATIVE_CALL LoggerSetTraceFormat(AMX* amx, cell* params) {
	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len, percentLoc, errorLoc;
	const char* traceFormat = get_amxstring(amx, params[2], 0, len);
	if (!isValidLoggerFormat(traceFormat, percentLoc, errorLoc)) {
		char *error = new char[errorLoc - percentLoc + 2];
		strncpy(error, traceFormat + percentLoc, errorLoc - percentLoc + 1);
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger trace format provided: \"%s\" (position %d = \"%s\")", traceFormat, percentLoc + 1, error);
		delete[] error;
		return INVALID_LOGGER;
	}

	logger->setTraceFormat(traceFormat);
	return 1;
}

// native LoggerLog(const Logger:logger, const Severity:severity, const bool:printStackTrace, const format[], any:...);
static cell AMX_NATIVE_CALL LoggerLog(AMX* amx, cell* params) {
	if (params[2] < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 4, len);
	logger->log(amx, params[2], params[3] == 1, buffer);
	return 1;
}

// native LoggerLogError(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
static cell AMX_NATIVE_CALL LoggerLogError(AMX* amx, cell* params) {
	if (LOG_SEVERITY_ERROR < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 3, len);
	logger->log(amx, LOG_SEVERITY_ERROR, params[2] == 1, buffer);
	return 1;
}

// native LoggerLogWarn(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
static cell AMX_NATIVE_CALL LoggerLogWarn(AMX* amx, cell* params) {
	if (LOG_SEVERITY_WARN < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 3, len);
	logger->log(amx, LOG_SEVERITY_WARN, params[2] == 1, buffer);
	return 1;
}

// native LoggerLogInfo(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);
static cell AMX_NATIVE_CALL LoggerLogInfo(AMX* amx, cell* params) {
	if (LOG_SEVERITY_INFO < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 3, len);
	logger->log(amx, LOG_SEVERITY_INFO, params[2] == 1, buffer);
	return 1;
}

// native LoggerLogDebug(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);
static cell AMX_NATIVE_CALL LoggerLogDebug(AMX* amx, cell* params) {
	if (LOG_SEVERITY_DEBUG < Logger::getMinLoggableVerbosity()) {
		return 0;
	}

	Logger* logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* buffer = format_amxstring(amx, params, 3, len);
	logger->log(amx, LOG_SEVERITY_DEBUG, params[2] == 1, buffer);
	return 1;
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
			time_t t2;
			time(&t2);
			t2 += 86400;
			tm* t3 = localtime(&t2);
			t3->tm_hour = 0;
			t3->tm_min = 0;
			t3->tm_sec = 0;
			time_t t4 = mktime(t3);
			counter = 1;
			fprintf(pF, "%lld\n", static_cast<long long>(t4));
			fprintf(pF, "%d", counter);
		}
		else {
			counter++;
			fprintf(pF, "%s", timestamp);
			fprintf(pF, "%d", counter);
		}

		fflush(pF);
		fclose(pF);
		itoa(counter, MapCounter, 10);
	}
	else {
		ALERT(at_logged, "[AMXX] Unexpected fatal logging error (couldn't open %s for a+).\n", dataFile);
		return;
	}
}

AMX_NATIVE_INFO logger_Natives[] = {
	{ "LoggerCreate",			LoggerCreate },
	{ "LoggerDestroy",			LoggerDestroy },

	{ "LoggerGetVerbosity",		LoggerGetVerbosity },
	{ "LoggerSetVerbosity",		LoggerSetVerbosity },

	{ "LoggerGetNameFormat",	LoggerGetNameFormat },
	{ "LoggerSetNameFormat",	LoggerSetNameFormat },

	{ "LoggerGetMessageFormat",	LoggerGetMessageFormat },
	{ "LoggerSetMessageFormat",	LoggerSetMessageFormat },

	{ "LoggerGetDateFormat",	LoggerGetDateFormat },
	{ "LoggerSetDateFormat",	LoggerSetDateFormat },

	{ "LoggerGetTimeFormat",	LoggerGetTimeFormat },
	{ "LoggerSetTimeFormat",	LoggerSetTimeFormat },

	{ "LoggerGetPathFormat",	LoggerGetPathFormat },
	{ "LoggerSetPathFormat",	LoggerSetPathFormat },

	{ "LoggerGetTraceFormat",	LoggerGetTraceFormat },
	{ "LoggerSetTraceFormat",	LoggerSetTraceFormat },

	{ "LoggerLog",				LoggerLog },

	{ "LoggerLogError",			LoggerLogError },
	{ "LoggerLogWarn",			LoggerLogWarn },
	{ "LoggerLogInfo",			LoggerLogInfo },
	{ "LoggerLogDebug",			LoggerLogDebug },
	{ nullptr,				nullptr }
};