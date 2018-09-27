#if defined _logger_const_included
    #endinput
#endif
#define _logger_const_included

/*******************************************************************************
 * DATA / DEFINITIONS
 ******************************************************************************/

enum LoggerFormat {
    LogName,
    LogMessage,
    LogDate,
    LogTime,
    LogPath,
    LogTrace
};

enum LoggerVerbosity {
    UnsetLevel = 0,
    DebugLevel = 1,
    InfoLevel = 101,
    WarnLevel = 201,
    ErrorLevel = 301
};
