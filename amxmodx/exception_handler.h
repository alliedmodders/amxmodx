#ifndef _EXCEPTION_HANDLER_H_
#define _EXCEPTION_HANDLER_H_

#include "modules.h"

class ExceptionHandler {
private:
	static Logger *m_Logger;
	static ke::AString m_ExceptionString;
	static ke::AString m_ExceptionMessage;
public:
	static Logger* getLogger();
public:
	static void Throw(AMX *amx, const char *exception, const char* format = "", ...);
	static void TryBegin(AMX *amx, Logger *logger = nullptr);
	static bool TryCatch(const char *exception);
	static void TryHandle();
	static void TryEnd(AMX *amx);
};

extern AMX_NATIVE_INFO exception_handler_Natives[];

#endif // _EXCEPTION_HANDLER_H_