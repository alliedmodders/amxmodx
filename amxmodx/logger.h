#ifndef _LOGGER_H_
#define _LOGGER_H_

#include <am-vector.h>
#include "natives_handles.h"
#include "CPlugin.h"

#define LOG_SEVERITY_HIGHEST LOG_SEVERITY_ERROR
#define LOG_SEVERITY_LOWEST  LOG_SEVERITY_DEBUG
#define LOG_SEVERITY_ERROR	 301
#define LOG_SEVERITY_WARN	 201
#define LOG_SEVERITY_INFO	 101
#define LOG_SEVERITY_DEBUG	 1
#define LOG_SEVERITY_NONE	 0

#define LOG_ARG_NONE     0
#define LOG_ARG_DATE     1
#define LOG_ARG_FUNCTION 2
#define LOG_ARG_MESSAGE  3
#define LOG_ARG_MAP      4
#define LOG_ARG_SCRIPT   5
#define LOG_ARG_SEVERITY 6
#define LOG_ARG_TIME     7

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
	ke::AString m_pNameFormat;
	ke::AString m_pMessageFormat;
	ke::AString m_pDateFormat;
	ke::AString m_pTimeFormat;
	ke::AString m_pPathFormat;
	ke::AString m_pTraceFormat;

public:
	Logger(int verbosity,
				const char *nameFormat,
				const char *messageFormat,
				const char *dateFormat,
				const char *timeFormat,
				const char *pathFormat,
				const char *traceFormat)
			: 
				m_pNameFormat(nameFormat),
				m_pMessageFormat(messageFormat),
				m_pDateFormat(dateFormat),
				m_pTimeFormat(timeFormat),
				m_pPathFormat(pathFormat),
				m_pTraceFormat(traceFormat) {
		m_Verbosity = max(LOG_SEVERITY_NONE, verbosity);
	};

public:
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

	void log(AMX* amx, const int severity, const bool printStackTrace, const char* format, ...) const;
};

extern NativeHandle<Logger> LoggerHandles;
extern AMX_NATIVE_INFO logger_Natives[];

#endif // _LOGGER_H_