#if defined _logger_const_included
    #endinput
#endif
#define _logger_const_included

/*******************************************************************************
 * DATA / DEFINITIONS
 ******************************************************************************/

enum Logger {
    All_Loggers = -1,
    Invalid_Logger
};

enum Severity {
    Severity_Invalid = -1,
    Severity_None    = 0,
    Severity_Debug   = 1,
    Severity_Info    = 101,
    Severity_Warn    = 201,
    Severity_Error   = 301,
};

/*******************************************************************************
 * CONSTANTS
 ******************************************************************************/

stock const DEFAULT_LOGGER_NAME_FORMAT[]      = "%n_%d";
stock const DEFAULT_LOGGER_MSG_FORMAT[]       = "[%5s] [%t] %n::%f: %l";
stock const DEFAULT_LOGGER_DATE_FORMAT[]      = "%Y-%m-%d";
stock const DEFAULT_LOGGER_TIME_FORMAT[]      = "%H:%M:%S";
stock const Severity:DEFAULT_LOGGER_VERBOSITY = Severity_Warn;
stock const DEFAULT_LOGGER_PATH               = "";

stock const SEVERITY_INVALID_STRING[] = "INVALID";
stock const SEVERITY_ALL_STRING[]     = "ALL";
stock const SEVERITY_DEBUG_STRING[]   = "DEBUG";
stock const SEVERITY_INFO_STRING[]    = "INFO";
stock const SEVERITY_WARN_STRING[]    = "WARN";
stock const SEVERITY_ERROR_STRING[]   = "ERROR";
stock const SEVERITY_NONE_STRING[]    = "NONE";