#if defined _logger_stocks_included
    #endinput
#endif
#define _logger_stocks_included

#include <amxconst>
#include <logger_const>

/*******************************************************************************
 * STOCKS
 ******************************************************************************/

stock LoggerVerbosity: toVerbosity(severity) {
    return LoggerVerbosity:(severity);
}

stock LoggerVerbosity: operator=(oper) {
    return toVerbosity(oper);
}

stock bool: operator==(LoggerVerbosity: oper1, oper2) {
    return oper1 == toVerbosity(oper2);
}

stock bool: operator!=(LoggerVerbosity: oper1, oper2) {
    return oper1 != toVerbosity(oper2);
}

stock bool: operator>(LoggerVerbosity: oper1, oper2) {
    return oper1 > toVerbosity(oper2);
}

stock bool: operator>(oper1, LoggerVerbosity: oper2) {
    return toVerbosity(oper1) > oper2;
}

stock bool: operator>=(LoggerVerbosity: oper1, oper2) {
    return oper1 >= toVerbosity(oper2);
}

stock bool: operator>=(oper1, LoggerVerbosity: oper2) {
    return toVerbosity(oper1) >= oper2;
}

stock bool: operator<(LoggerVerbosity: oper1, oper2) {
    return oper1 < toVerbosity(oper2);
}

stock bool: operator<(oper1, LoggerVerbosity: oper2) {
    return toVerbosity(oper1) < oper2;
}

stock bool: operator<=(LoggerVerbosity: oper1, oper2) {
    return oper1 <= toVerbosity(oper2);
}

stock bool: operator<=(oper1, LoggerVerbosity: oper2) {
    return toVerbosity(oper1) <= oper2;
}
