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

native Logger:LoggerCreate(
        const Severity:verbosity = Severity_Warn,
        const nameFormat[] = DEFAULT_LOGGER_NAME_FORMAT,
        const msgFormat[] = DEFAULT_LOGGER_MSG_FORMAT,
        const dateFormat[] = DEFAULT_LOGGER_DATE_FORMAT,
        const timeFormat[] = DEFAULT_LOGGER_TIME_FORMAT,
        const pathFormat[] = DEFAULT_LOGGER_PATH);

native bool:LoggerDestroy(&Logger:logger);

native Severity:LoggerGetVerbosity(const Logger:logger);
native Severity:LoggerSetVerbosity(const Logger:logger, const Severity:verbosity);

native LoggerGetNameFormat(const Logger:logger, format[], const len);
native LoggerSetNameFormat(const Logger:logger, const format[]);

native LoggerGetMessageFormat(const Logger:logger, format[], const len);
native LoggerSetMessageFormat(const Logger:logger, const format[]);

native LoggerGetDateFormat(const Logger:logger, format[], const len);
native LoggerSetDateFormat(const Logger:logger, const format[]);

native LoggerGetTimeFormat(const Logger:logger, format[], const len);
native LoggerSetTimeFormat(const Logger:logger, const format[]);

native LoggerGetPathFormat(const Logger:logger, format[], const len);
native LoggerSetPathFormat(const Logger:logger, const format[]);

native LoggerLog(const Logger:logger, const Severity:severity, const bool:printStackTrace, const format[], any:...);

native LoggerLogError(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
native LoggerLogWarn(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
native LoggerLogInfo(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);
native LoggerLogDebug(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);