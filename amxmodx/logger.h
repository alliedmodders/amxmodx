#ifndef _LOGGER_H_
#define _LOGGER_H_

#include <amtl/am-string.h>

#include "CPlugin.h"
#include "modules.h"
#include "natives_handles.h"

#define INVALID_LOGGER  0
#define ALL_LOGGERS    -1

#define LOG_SEVERITY_HIGHEST LOG_SEVERITY_ERROR
#define LOG_SEVERITY_LOWEST  LOG_SEVERITY_DEBUG
#define LOG_SEVERITY_ERROR	 301
#define LOG_SEVERITY_WARN	 201
#define LOG_SEVERITY_INFO	 101
#define LOG_SEVERITY_DEBUG	 1
#define LOG_SEVERITY_NONE	 0

class Logger {
private:
	static int m_MinLoggableVerbosity;

public:
	static int getMinLoggableVerbosity() {
		return m_MinLoggableVerbosity;
	};

	static int setMinLoggableVerbosity(int verbosity) {
		assert(LOG_SEVERITY_NONE <= verbosity);
		int oldVerbosity = m_MinLoggableVerbosity;
		m_MinLoggableVerbosity = verbosity;
		return oldVerbosity;
	};

public:
	static void onMapChange();

private:
	int m_Verbosity;
	ke::AString m_NameFormat;
	ke::AString m_MessageFormat;
	ke::AString m_DateFormat;
	ke::AString m_TimeFormat;
	ke::AString m_PathFormat;
	ke::AString m_TraceFormat;

public:
	Logger(int verbosity,
		const char *nameFormat,
		const char *messageFormat,
		const char *dateFormat,
		const char *timeFormat,
		const char *pathFormat,
		const char *traceFormat)
		:
		m_NameFormat(nameFormat),
		m_MessageFormat(messageFormat),
		m_DateFormat(dateFormat),
		m_TimeFormat(timeFormat),
		m_PathFormat(pathFormat),
		m_TraceFormat(traceFormat) {
		setVerbosity(verbosity);
	};

public:
	bool isLogging() const;
	int getVerbosity() const;
	int setVerbosity(int verbosity);

	const char* getNameFormat() const;
	void setNameFormat(const char* nameFormat);

	const char* getMessageFormat() const;
	void setMessageFormat(const char* messageFormat);

	const char* getDateFormat() const;
	void setDateFormat(const char* dateFormat);

	const char* getTimeFormat() const;
	void setTimeFormat(const char* timeFormat);

	const char* getPathFormat() const;
	void setPathFormat(const char* pathFormat);

	const char* getTraceFormat() const;
	void setTraceFormat(const char* traceFormat);

	void log(AMX* amx, const int severity, const bool printStackTrace, const bool force, const char* format, ...) const;
};

extern NativeHandle<Logger> LoggerHandles;
extern AMX_NATIVE_INFO logger_Natives[];
extern int LoggerCreatedForward;

#endif // _LOGGER_H_
