#if defined _logger_included
    #endinput
#endif
#define _logger_included

#include <amxconst>
#include <logger_const>
#include <logger_stocks>

/*******************************************************************************
 * NATIVES
 ******************************************************************************/

/**
 * Called after a Logger is created.
 * 
 * @note Uses the strftime C function for {@param dateFormat} and {@param
 *       timeFormat}. For a list of valid format parameters, see:
 *       http://cplusplus.com/reference/clibrary/ctime/strftime.html
 *       Common examples for these format strings would be:
 *           "%Y-%m-%d" and
 *           "%H:%M:%S" respectively
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger      Value used to uniquely identify the Logger
 * @param verbosity   Lowest severity of messages which will be logged
 * @param name        Name of the registering plugin (without {@code *.amxx}
 *                        extension)
 * @param nameFormat  Logger format used to generate the name of log files of
 *                        the Logger
 * @param msgFormat   Logger format used to generate log messages for the
 *                        Logger
 * @param dateFormat  C time format used to generate the date of the Logger
 * @param timeFormat  C time format used to generate the time of the Logger
 * @param pathFormat  Logger format used to generate paths for log files
 *                        created by the Logger relative to
 *                        {@code amxx_logsdir}
 * @param traceFormat Logger format used to generate stack traces for messages
 *                        logged by the Logger
 * 
 * @noreturn
 */
forward OnLoggerCreated(
        const Logger: logger,
        const Severity: verbosity,
        const name[],
        const nameFormat[],
        const msgFormat[],
        const dateFormat[],
        const timeFormat[],
        const pathFormat[],
        const traceFormat[]);

/*******************************************************************************
 * NATIVES
 ******************************************************************************/

/**
 * Creates a new Logger with the specified configuration and formats.
 * 
 * @note Logger format specifiers are defined as follows:
 *           %d  datestamp
 *           %f  calling function, or current function within a trace
 *           %i  counter for the number of map changes for the current day
 *           %l  line # within the calling script, or current line of trace
 *           %m  name of the map
 *           %n  script logging the message (with {@code *.sma} extension)
 *           %p  plugin logging the message (without {@code *.amxx} extension)
 *           %s  message being logged
 *           %t  timestamp
 *           %v  string representation of verbosity/severity of the message
 *           %%  percent literal (i.e., {@code %})
 * 
 * @param verbosity   Verbosity of the Logger (i.e., lowest severity a
 *                        message can be for it to be logged)
 * @param nameFormat  Logger format used to generate the name of log files of
 *                        the Logger
 * @param msgFormat   Logger format used to generate log messages for the
 *                        Logger
 * @param dateFormat  C time format used to generate the date of the Logger
 * @param timeFormat  C time format used to generate the time of the Logger
 * @param pathFormat  Logger format used to generate paths for log files
 *                        created by the Logger relative to
 *                        {@code amxx_logsdir}
 * @param traceFormat Logger format used to generate stack traces for messages
 *                        logged by the Logger
 * 
 * @return New Logger handle, which must be freed via {@link #LoggerDestroy()}
 */
native Logger: LoggerCreate(
        const Severity: verbosity = Severity_Warn,
        const nameFormat[]  = "%p_%d",
        const msgFormat[]   = "[%5v] [%t] %n::%f - %s",
        const dateFormat[]  = "%Y-%m-%d",
        const timeFormat[]  = "%H:%M:%S",
        const pathFormat[]  = "",
        const traceFormat[] = "    at %n::%f : %l");

/**
 * Destroys the specified Logger, freeing resources it has allocated.
 * 
 * @note This function automatically sets the variable passed to it to
 *       {@literal Invalid_Logger} to aid in preventing accidental usage after
 *       destruction.
 * 
 * @return {@code 1} on success, {@code 0} if an invalid handle was passed
 */
native bool: LoggerDestroy(&Logger: logger);

/**
 * Retrieves the current verbosity of a Logger.
 * 
 * @param logger Logger handle or {@literal All_Loggers} for all loggers
 * 
 * @return current verbosity of the passed Logger
 * 
 * @error If an invalid handle is provided
 */
native Severity: LoggerGetVerbosity(const Logger: logger);

/**
 * Changes the current verbosity of a Logger to the specified value.
 * 
 * @note changing the verbosity of {@literal All_Loggers} will not actually
 *       change their values, only the global verbosity setting (i.e.,
 *       loggable messages must be within both this verbosity and the Logger's
 *       verbosity)
 * 
 * @param logger    Logger handle or {@literal All_Loggers} for all loggers
 * @param verbosity Verbosity to set the passed Logger to
 * 
 * @return previous verbosity of the passed Logger
 * 
 * @error If an invalid handle is provided
 */
native Severity: LoggerSetVerbosity(const Logger: logger, const Severity: verbosity);

/**
 * Retrieves the current Logger name format used to generate file names for
 * logs created by the passed Logger.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate the name
 *                   of log files of the Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetNameFormat(const Logger: logger, format[], const len);

/**
 * Changes the current name format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log file names
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetNameFormat(const Logger: logger, const format[]);

/**
 * Retrieves the current Logger message format used to generate log messages
 * created by the passed Logger.
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate log
 *                   messages for the Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetMessageFormat(const Logger: logger, format[], const len);

/**
 * Changes the current message format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log messages
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetMessageFormat(const Logger: logger, const format[]);

/**
 * Retrieves the current Logger datestamp format used to generate dates for
 * Logger formats created by the passed Logger.
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate the
 *                   datestamp format of the Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetDateFormat(const Logger: logger, format[], const len);

/**
 * Changes the current datestamp format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the C time format strings
 * 
 * @param logger Logger handle
 * @param format C time format which will be used to generate datestamps
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetDateFormat(const Logger: logger, const format[]);

/**
 * Retrieves the current Logger timestamp format used to generate times for
 * Logger formats created by the passed Logger.
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate the
 *                   timestamp format of the Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetTimeFormat(const Logger: logger, format[], const len);

/**
 * Changes the current timestamp format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the C time format strings
 * 
 * @param logger Logger handle
 * @param format C time format which will be used to generate timestamps
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetTimeFormat(const Logger: logger, const format[]);

/**
 * Retrieves the current Logger path format used to generate file paths for
 * logs created by the passed Logger relative to {@code amxx_logsdir}.
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate paths
 *                   for log files created by the passed Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetPathFormat(const Logger: logger, format[], const len);

/**
 * Changes the current path format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log paths
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetPathFormat(const Logger: logger, const format[]);

/**
 * Retrieves the current Logger stack trace format used when generating stack
 * traces for log messages created by the passed Logger.
 * 
 * @param logger Logger handle
 * @param format Destination array to copy the format used to generate the
 *                   stack traces of log messages of the Logger
 * @param len    Maximum number of bytes to copy
 * 
 * @return total number of bytes copied
 * 
 * @error If an invalid handle is provided
 */
native LoggerGetTraceFormat(const Logger: logger, format[], const len);

/**
 * Changes the current stack trace format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate()} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate stack traces
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetTraceFormat(const Logger: logger, const format[]);

/**
 * Logs a message to the specified Logger's log file if the severity is within
 * the current verbosity level set by the it.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLog2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal true} based on whether or not {@param severity} is greater
 *       than or equal to {@literal Severity_Warn}
 * 
 * @param logger   Logger handle
 * @param severity Severity of the message (i.e., value which represents how
 *                     important the message is, higher values imply greater
 *                     importance)
 * @param format   Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLog(const Logger: logger, const Severity: severity, const format[], any: ...);

/**
 * Logs an error message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Error}.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLogError2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal true}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogError(const Logger: logger, const format[], any: ...);

/**
 * Logs a warning message to the specified Logger's log file if the verbosity
 * of the log file is at least {@literal Severity_Warn}.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLogWarn2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal true}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarn(const Logger: logger, const format[], any: ...);

/**
 * Logs a warning message to the specified Logger's log file if the verbosity
 * of the log file is at least {@literal Severity_Warn}.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLogWarn2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal true}
 * 
 * @note this function is a duplicate of {@link #LoggerLogWarn()}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarning(const Logger: logger, const format[], any: ...);

/**
 * Logs an info message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Info}.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLogInfo2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal false}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogInfo(const Logger: logger, const format[], any: ...);

/**
 * Logs a debug message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Debug}.
 * 
 * @note this function behaves exactly the same as {@link #LoggerLogDebug2()},
 *       except that {@param printStackTrace} will be automatically set to
 *		 {@literal false}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogDebug(const Logger: logger, const format[], any: ...);

/**
 * Logs a message to the specified Logger's log file if the severity is within
 * the current verbosity level set by the it.
 * 
 * @param logger          Logger handle
 * @param severity        Severity of the message (i.e., value which represents
 *                            how important the message is, higher values imply
 *                            greater importance)
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLog2(const Logger: logger, const Severity: severity, const bool: printStackTrace, const format[], any: ...);

/**
 * Logs an error message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Error}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2()}
 *       were called with {@param severity} equal to {@literal Severity_Error}
 * 
 * @param logger          Logger handle
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogError2(const Logger: logger, const bool: printStackTrace = true, const format[], any: ...);

/**
 * Logs a warning message to the specified Logger's log file if the verbosity
 * of the log file is at least {@literal Severity_Warn}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2()}
 *       were called with {@param severity} equal to {@literal Severity_Warn}
 * 
 * @param logger          Logger handle
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarn2(const Logger: logger, const bool: printStackTrace = true, const format[], any: ...);

/**
 * Logs a warning message to the specified Logger's log file if the verbosity
 * of the log file is at least {@literal Severity_Warn}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2()}
 *       were called with {@param severity} equal to {@literal Severity_Warn}
 * 
 * @note this function is a duplicate of {@link #LoggerLogWarn2()}
 * 
 * @param logger          Logger handle
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarning2(const Logger: logger, const bool: printStackTrace = true, const format[], any: ...);

/**
 * Logs an info message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Info}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2()}
 *       were called with {@param severity} equal to {@literal Severity_Info}
 * 
 * @param logger          Logger handle
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogInfo2(const Logger: logger, const bool: printStackTrace = false, const format[], any: ...);

/**
 * Logs a debug message to the specified Logger's log file if the verbosity of
 * the log file is at least {@literal Severity_Debug}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2()}
 *       were called with {@param severity} equal to {@literal Severity_Debug}
 * 
 * @param logger          Logger handle
 * @param printStackTrace {@literal true} to print a stack trace following the
 *                            message, otherwise {@literal false}
 * @param format          Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogDebug2(const Logger: logger, const bool: printStackTrace = false, const format[], any: ...);