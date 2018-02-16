#if defined _logger_const_included
    #endinput
#endif
#define _logger_const_included

/*******************************************************************************
 * DATA / DEFINITIONS
 ******************************************************************************/

/**
 * Defines the Logger tag and standard Logger constants
 */
enum Logger {
    /**
     * Constant used to represent all Logger
     * {@see logger.inc::#LoggerGetVerbosity()}
     */
    All_Loggers = -1,

    /**
     * Constant used to represent no Logger (i.e., the {@literal Invalid_Logger})
     */
    No_Logger = 0,

    /**
     * Constant used to represent an invalid Logger handle
     */
    Invalid_Logger = No_Logger
};

/**
 * Defines the Severity tag and standard Severity constants
 */
enum Severity {
    /**
     * Constant used to represent invalid Severities
	 * 
	 * @deprecated
     */
    Severity_Invalid = -1,

    /**
     * Constant used to represent no Severity (i.e., disables logging)
     */
    Severity_None    = 0,

    /**
     * Constant used to represent the debugging Severity
     */
    Severity_Debug   = 1,

    /**
     * Constant used to represent the lowest Severity (i.e., the most verbose)
     */
    Severity_Lowest  = Severity_Debug,

    /**
     * Constant used to represent the informational Severity
     */
    Severity_Info    = 101,

    /**
     * Constant used to represent the warn Severity
     */
    Severity_Warn    = 201,

    /**
     * Constant used to represent the warn Severity
	 * 
	 * @note duplicate of {@literal #Severity_Warn}
     */
    Severity_Warning = Severity_Warn,

    /**
     * Constant used to represent the error Severity
     */
    Severity_Error   = 301,

    /**
     * Constant used to represent the highest Severity (i.e., the least verbose)
	 * 
	 * @note this will not disable logging, it will just make only the most
	 *       important messages log
     */
    Severity_Highest = Severity_Error
};

/*******************************************************************************
 * CONSTANTS
 ******************************************************************************/

/**
 * Constant used to represent the default verbosity assigned to a Logger.
 */
stock const Severity: DEFAULT_LOGGER_VERBOSITY = Severity_Warn;

/**
 * Constant used to represent the default name format assigned to a Logger.
 */
stock const DEFAULT_LOGGER_NAME_FORMAT[]       = "%p_%d";

/**
 * Constant used to represent the default message format assigned to a Logger.
 */
stock const DEFAULT_LOGGER_MSG_FORMAT[]        = "[%5v] [%t] %n::%f - %s";

/**
 * Constant used to represent the default date format assigned to a Logger.
 */
stock const DEFAULT_LOGGER_DATE_FORMAT[]       = "%Y-%m-%d";

/**
 * Constant used to represent the default time format assigned to a Logger.
 */
stock const DEFAULT_LOGGER_TIME_FORMAT[]       = "%H:%M:%S";

/**
 * Constant used to represent the default path format assigned to a Logger.
 */
stock const DEFAULT_LOGGER_PATH_FORMAT[]       = "";

/**
 * Constant used to represent the default stack trace format assigned to a
 * Logger.
 */
stock const DEFAULT_LOGGER_TRACE_FORMAT[]      = "    at %n::%f : %l";

/**
 * String representation of {@link #Invalid_Severity}
 */
stock const SEVERITY_INVALID_STRING[] = "INVALID";

/**
 * String representation of {@link #Severity_Highest}
 */
stock const SEVERITY_ALL_STRING[]     = "ALL";

/**
 * String representation of {@link #Invalid_Debug}
 */
stock const SEVERITY_DEBUG_STRING[]   = "DEBUG";

/**
 * String representation of {@link #Invalid_Info}
 */
stock const SEVERITY_INFO_STRING[]    = "INFO";

/**
 * String representation of {@link #Invalid_Warn}
 */
stock const SEVERITY_WARN_STRING[]    = "WARN";

/**
 * String representation of {@link #Invalid_Error}
 */
stock const SEVERITY_ERROR_STRING[]   = "ERROR";

/**
 * String representation of {@link #Severity_Lowest}
 */
stock const SEVERITY_NONE_STRING[]    = "NONE";