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
        const nameFormat[]  = "%p_%d",
        const msgFormat[]   = "[%5v] [%t] %n::%f - %s",
        const dateFormat[]  = "%Y-%m-%d",
        const timeFormat[]  = "%H:%M:%S",
        const pathFormat[]  = "",
        const traceFormat[] = "    at %n::%f : %l");

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

native LoggerGetTraceFormat(const Logger:logger, format[], const len);
native LoggerSetTraceFormat(const Logger:logger, const format[]);

native LoggerLog(const Logger:logger, const Severity:severity, const format[], any:...);
native LoggerLog2(const Logger:logger, const Severity:severity, const bool:printStackTrace, const format[], any:...);

native LoggerLogError(const Logger:logger, const format[], any:...);
native LoggerLogWarn(const Logger:logger, const format[], any:...);
native LoggerLogInfo(const Logger:logger, const format[], any:...);
native LoggerLogDebug(const Logger:logger, const format[], any:...);

native LoggerLogError2(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
native LoggerLogWarn2(const Logger:logger, const bool:printStackTrace = true, const format[], any:...);
native LoggerLogInfo2(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);
native LoggerLogDebug2(const Logger:logger, const bool:printStackTrace = false, const format[], any:...);