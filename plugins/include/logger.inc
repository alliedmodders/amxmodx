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
 * @note Uses the strftime C function for {@code dateFormat} and
 *       {@code timeFormat}. For a list of valid format parameters, see:
 *       http://cplusplus.com/reference/clibrary/ctime/strftime.html
 *       Common examples for these format strings would be:
 *           {@code "%Y-%m-%d"} and {@code "%H:%M:%S"} respectively
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param plugin      The plugin that created the logger
 * @param logger      Value used to uniquely identify the Logger
 * @param verbosity   Lowest severity of messages which will be logged
 * @param name        Name of the registering plugin (without {@code *.amxx} extension)
 * @param nameFormat  Logger format used to generate the name of log files of the Logger
 * @param msgFormat   Logger format used to generate log messages for the Logger
 * @param dateFormat  C time format used to generate the date of the Logger
 * @param timeFormat  C time format used to generate the time of the Logger
 * @param pathFormat  Logger format used to generate paths for log files created
 *                        by the Logger relative to {@code amxx_logsdir}
 * @param traceFormat Logger format used to generate stack traces for messages
 *                        logged by the Logger
 * 
 * @noreturn
 */
forward OnLoggerCreated(
        const plugin,
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
 *       {@code %d}  datestamp
 *       {@code %f}  calling function, or current function within a trace
 *       {@code %i}  counter for the number of map changes for the current day
 *       {@code %l}  line # within the calling script, or current line of trace
 *       {@code %m}  name of the map
 *       {@code %n}  script logging the message (with {@code *.sma} extension)
 *       {@code %p}  plugin logging the message (without {@code *.amxx} extension)
 *       {@code %s}  message being logged
 *       {@code %t}  timestamp
 *       {@code %v}  string representation of verbosity/severity of the message
 *       {@code %%}  percent literal (i.e., {@code %})
 * 
 * @note {@code %n} and {@code f} and stack trace printing is only available
 *       when the plugin is running in debug mode.
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
 * @return New Logger handle, which must be freed via {@link #LoggerDestroy}
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
 *       {@code Invalid_Logger} to aid in preventing accidental usage after
 *       destruction.
 * 
 * @return {@code 1} on success, {@code 0} if an invalid handle was passed
 */
native bool: LoggerDestroy(&Logger: logger);

/**
 * Retrieves the handle for the Logger assigned to the calling plugin. This
 * logger will be created automatically by this call if it does not already
 * exist.
 */
native Logger: LoggerGetThis();

/**
 * Sets the Logger assigned to the calling plugin to the specified Logger. This
 * logger will be used whenever {@code This_Logger} is referenced.
 * 
 * @return The handle of the previously assigned Logger, or
 *         {@code Invalid_Logger} if none was assigned. If a valid Logger handle
 *         is returned by this function, it is the resposibility of the caller
 *         for disposing it via {@link #LoggerDestroy}
 */
native Logger: LoggerSetThis(const Logger: logger);

/**
 * Retrieves the current verbosity of a Logger.
 * 
 * @param logger Logger handle or {@code All_Loggers} to retrieve the global
 *               verbosity level
 * 
 * @return current verbosity of the passed Logger
 * 
 * @error If an invalid handle is provided
 */
native Severity: LoggerGetVerbosity(const Logger: logger = This_Logger);

/**
 * Changes the current verbosity of a Logger to the specified value.
 * 
 * @note changing the verbosity of {@code All_Loggers} will not actually
 *       change their individual verbosities, only the global verbosity setting
 *       (i.e., loggable messages must be within both this verbosity and the
 *       Logger's assigned verbosity)
 * 
 * @param logger    {@code This_Logger} for the Logger assigned to the calling
 *                      plugin, a Logger handle, or {@code All_Loggers} for the
 *                      global verbosity
 * @param verbosity Verbosity to set the passed Logger to
 * 
 * @return previous verbosity of the passed Logger
 * 
 * @error If an invalid handle is provided
 */
native Severity: LoggerSetVerbosity(const Logger: logger = This_Logger, const Severity: verbosity);

/**
 * Retrieves the name format for the specified Logger, used to generate file
 * names for the logs it creates.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
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
native LoggerGetNameFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the name format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log file names
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetNameFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Retrieves the message format for the specified Logger, used to generate log
 * messages.
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
native LoggerGetMessageFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the message format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log messages
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetMessageFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Retrieves the datestamp format for the specified Logger, used to generate
 * date strings.
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
native LoggerGetDateFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the datestamp format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the C time format strings
 * 
 * @param logger Logger handle
 * @param format C time format which will be used to generate datestamps
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetDateFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Retrieves the timestamp format for the specified Logger, used to generate
 * time strings.
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
native LoggerGetTimeFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the current timestamp format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the C time format strings
 * 
 * @param logger Logger handle
 * @param format C time format which will be used to generate timestamps
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetTimeFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Retrieves the path format for the specified Logger, used to generate logs
 * it creates. The path should be relative to {@code amxx_logsdir}.
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
native LoggerGetPathFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the current path format of a Logger to a newly specified one. The
 * path should be relative to {@code amxx_logsdir}.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate log paths
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetPathFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Retrieves the stack trace format for the specified Logger, used when
 * outputting stack traces for applicable log messages it creates.
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
native LoggerGetTraceFormat(const Logger: logger = This_Logger, format[], const len);

/**
 * Changes the current stack trace format of a Logger to a newly specified one.
 * 
 * @note {@see #LoggerCreate} for a detailed list of the specifiers used in
 *       the Logger format strings
 * 
 * @param logger Logger handle
 * @param format Logger format which will be used to generate stack traces
 * 
 * @return {@code 1} on success, otherwise {@code 0}
 * 
 * @error If an invalid handle is provided
 */
native LoggerSetTraceFormat(const Logger: logger = This_Logger, const format[]);

/**
 * Logs a message using the Logger assigned to the calling plugin if its
 * verbosity accepts messages of the specified severity.
 * 
 * @param severity Severity of the message (i.e., value which represents how
 *                     important the message is, higher values imply greater
 *                     importance)
 * @param format   Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLog(const Severity: severity = Severity_Info, const format[], any: ...);

/**
 * Logs an error message using the Logger assigned to the calling plugin if its
 * verbosity is at least {@code Severity_Error}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2}
 *       were called with {@code severity} equal to {@code Severity_Error}
 * 
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogError(const format[], any: ...);

/**
 * Logs a warning message using the Logger assigned to the calling plugin if its
 * verbosity is at least {@code Severity_Warn}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2}
 *       were called with {@code severity} equal to {@code Severity_Warn}
 * 
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarn(const format[], any: ...);

/**
 * Logs a warning message using the Logger assigned to the calling plugin if its
 * verbosity is at least {@code Severity_Warn}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2}
 *       were called with {@code severity} equal to {@code Severity_Warn}
 * 
 * @note this function is a duplicate of {@link #LoggerLogWarn2}
 * 
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarning(const format[], any: ...);

/**
 * Logs an info message using the Logger assigned to the calling plugin if its
 * verbosity is at least {@code Severity_Info}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2}
 *       were called with {@code severity} equal to {@code Severity_Info}
 * 
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogInfo(const format[], any: ...);

/**
 * Logs a debug message using the Logger assigned to the calling plugin if its
 * verbosity is at least {@code Severity_Debug}.
 * 
 * @note this function behaves exactly the same as if {@link #LoggerLog2}
 *       were called with {@code severity} equal to {@code Severity_Debug}
 * 
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogDebug(const format[], any: ...);

/**
 * Logs a message using the specified Logger if the severity is within its
 * current verbosity level.
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
native LoggerLog2(const Logger: logger = This_Logger, const Severity: severity = Severity_Info, const format[], any: ...);

/**
 * Logs an error message using the specified Logger if its verbosity is at least
 * {@code Severity_Error}.
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogError2(const Logger: logger = This_Logger, const format[], any: ...);

/**
 * Logs a warning message using the specified Logger if its verbosity is at
 * least {@code Severity_Warn}.
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarn2(const Logger: logger = This_Logger, const format[], any: ...);

/**
 * Logs a warning message using the specified Logger if its verbosity is at
 * least {@code Severity_Warn}.
 * 
 * @note this function is a duplicate of {@link #LoggerLogWarn}
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogWarning2(const Logger: logger = This_Logger, const format[], any: ...);

/**
 * Logs an info message using the specified Logger if its verbosity is at
 * least {@code Severity_Info}.
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogInfo2(const Logger: logger = This_Logger, const format[], any: ...);

/**
 * Logs a debug message using the specified Logger if its verbosity is at
 * least {@code Severity_Debug}.
 * 
 * @param logger Logger handle
 * @param format Standard format string of the message
 * 
 * @noreturn
 * 
 * @error If an invalid handle is provided
 */
native LoggerLogDebug2(const Logger: logger = This_Logger, const format[], any: ...);
