#if defined _logger_stocks_included
    #endinput
#endif
#define _logger_stocks_included

#include <amxconst>
#include <logger>
#include <logger_const>

/*******************************************************************************
 * STOCKS
 ******************************************************************************/

stock Severity: toSeverity(severity) {
    return Severity:(severity);
}

stock Severity: operator=(oper) {
    return toSeverity(oper);
}

stock bool: operator==(Severity: oper1, oper2) {
    return oper1 == toSeverity(oper2);
}

stock bool: operator!=(Severity: oper1, oper2) {
    return oper1 != toSeverity(oper2);
}

stock bool: operator>(Severity:oper1, oper2) {
    return oper1 > toSeverity(oper2);
}

stock bool: operator>(oper1, Severity:oper2) {
    return toSeverity(oper1) > oper2;
}

stock bool: operator>=(Severity:oper1, oper2) {
    return oper1 >= toSeverity(oper2);
}

stock bool: operator>=(oper1, Severity:oper2) {
    return toSeverity(oper1) >= oper2;
}

stock bool: operator<(Severity:oper1, oper2) {
    return oper1 < toSeverity(oper2);
}

stock bool: operator<(oper1, Severity:oper2) {
    return toSeverity(oper1) < oper2;
}

stock bool: operator<=(Severity:oper1, oper2) {
    return oper1 <= toSeverity(oper2);
}

stock bool: operator<=(oper1, Severity:oper2) {
    return toSeverity(oper1) <= oper2;
}