#if defined _logger_included
    #endinput
#endif
#define _logger_included

#include <amxconst>
#include <logger_const>
#include <logger_stocks>

/*******************************************************************************
 * FORWARDS
 ******************************************************************************/

forward OnLoggerCreated(
        const plugin,
        const Logger: logger,
        const LoggerVerbosity: verbosity,
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

native Logger(const LoggerVerbosity: verbosity, const format[], any: ...);

native LoggerDebug(const format[], any: ...);
native LoggerInfo(const format[], any: ...);
native LoggerWarn(const format[], any: ...);
native LoggerError(const format[], any: ...);

native LoadLogger(const pluginId);

native LoggerVerbosity: GetLoggerVerbosity();
native LoggerVerbosity: SetLoggerVerbosity(const LoggerVerbosity: verbosity);

native LoggerVerbosity: SetGlobalLoggerVerbosity(const LoggerVerbosity: verbosity);
native LoggerVerbosity: GetGlobalLoggerVerbosity();

native GetLoggerFormat(const LoggerFormat: type, format[], const maxLength);
native SetLoggerFormat(const LoggerFormat: type, const format[]);
