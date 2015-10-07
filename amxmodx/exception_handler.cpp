#include "amxmodx.h"
#include "CLang.h"
#include "logger.h"
#include "exception_handler.h"

Logger *ExceptionHandler::m_Logger;
ke::AString ExceptionHandler::m_ExceptionString;
ke::AString ExceptionHandler::m_ExceptionMessage;

Logger* ExceptionHandler::getLogger() {
	return m_Logger;
}

void ExceptionHandler::Throw(AMX *amx, const char *exception, const char* format, ...) {
	if (*exception == '\0') {
		LogError(amx, AMX_ERR_NATIVE, "Throw() called with empty exception!");
		return;
	}

	m_ExceptionString = exception;

	static char message[256];

	va_list arglst;
	va_start(arglst, format);
	ke::SafeVsprintf(message, sizeof message - 1, format, arglst);
	va_end(arglst);

	m_ExceptionMessage = message;
}

void ExceptionHandler::TryBegin(AMX *amx, Logger *logger) {
	if (!m_ExceptionString.length()) {
		return;
	}
	
	m_Logger = logger;
	if (m_Logger) {
		m_Logger->log(amx, LOG_SEVERITY_WARN, true, false, "TryBegin() called with uncaught exception in buffer: %s", m_ExceptionString);
	} else {
		LogError(amx, AMX_ERR_NATIVE, "TryBegin() called with uncaught exception in buffer: %s", m_ExceptionString);
	}

	m_ExceptionString = "";
	m_ExceptionMessage = "";
}

bool ExceptionHandler::TryCatch(const char *exception) {
	if (*exception == '\0') {
		return m_ExceptionString.length() > 0;
	} else {
		return m_ExceptionString.compare(exception) == 0;
	}
}

void ExceptionHandler::TryHandle() {
	m_ExceptionString = "";
	m_ExceptionMessage = "";
}

void ExceptionHandler::TryEnd(AMX *amx) {
	if (m_ExceptionString.length() == 0) {
		return;
	}

	if (m_Logger) {
		m_Logger->log(amx, LOG_SEVERITY_ERROR, true, false, "%s: %s", m_ExceptionString, m_ExceptionMessage);
		m_Logger = nullptr;
	} else {
		LogError(amx, AMX_ERR_NATIVE, "%s: %s", m_ExceptionString, m_ExceptionMessage);
	}

	m_ExceptionString = "";
	m_ExceptionMessage = "";
}

static cell AMX_NATIVE_CALL Throw(AMX* amx, cell* params) {
	char exception[64];
	get_amxstring_r(amx, params[1], exception, sizeof exception - 1);

	int len;
	char* message = format_amxstring(amx, params, 2, len);
	ExceptionHandler::Throw(amx, exception, message);
	return 1;
}

static cell AMX_NATIVE_CALL TryBegin(AMX* amx, cell* params) {
	if (params[1] == INVALID_LOGGER) {
		ExceptionHandler::TryBegin(amx);
		return 1;
	}

	Logger *logger = LoggerHandles.lookup(params[1]);
	if (!logger) {
		LogError(amx, AMX_ERR_NATIVE, "Invalid logger handle provided (%d)", params[1]);
		return 0;
	}

	ExceptionHandler::TryBegin(amx, logger);
	return 1;
}

static cell AMX_NATIVE_CALL TryCatch(AMX* amx, cell* params) {
	char exception[64];
	get_amxstring_r(amx, params[1], exception, sizeof exception - 1);
	return static_cast<cell>(ExceptionHandler::TryCatch(exception));
}

static cell AMX_NATIVE_CALL TryHandle(AMX* amx, cell* params) {
	ExceptionHandler::TryHandle();
	return 1;
}

static cell AMX_NATIVE_CALL TryEnd(AMX* amx, cell* params) {
	ExceptionHandler::TryEnd(amx);
	return 1;
}

AMX_NATIVE_INFO exception_handler_Natives[] = {
	{ "Throw",		Throw },
	{ "TryBegin",	TryBegin },
	{ "TryCatch",	TryCatch },
	{ "TryHandle",	TryHandle },
	{ "TryEnd",		TryEnd },
	{ nullptr,		nullptr }
};