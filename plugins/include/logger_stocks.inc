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

stock Severity: severity(severity) {
	return Severity:severity;
}

stock Severity: operator=(oper) {
	return severity(oper);
}

stock bool: operator==(Severity: oper1, oper2) {
	return oper1 == severity(oper2);
}

stock bool: operator!=(Severity: oper1, oper2) {
	return oper1 != severity(oper2);
}

stock bool: operator>(Severity:oper1, oper2) {
	return oper1 > severity(oper2);
}

stock bool: operator>(oper1, Severity:oper2) {
	return severity(oper1) > oper2;
}

stock bool: operator>=(Severity:oper1, oper2) {
	return oper1 >= severity(oper2);
}

stock bool: operator>=(oper1, Severity:oper2) {
	return severity(oper1) >= oper2;
}

stock bool: operator<(Severity:oper1, oper2) {
	return oper1 < severity(oper2);
}

stock bool: operator<(oper1, Severity:oper2) {
	return severity(oper1) < oper2;
}

stock bool: operator<=(Severity:oper1, oper2) {
	return oper1 <= severity(oper2);
}

stock bool: operator<=(oper1, Severity:oper2) {
	return severity(oper1) <= oper2;
}