#if defined _exception_handler_included
    #endinput
#endif
#define _exception_handler_included

#include <amxconst>

stock const Exception[] = "Exception";
stock const IllegalArgumentException[] = "IllegalArgumentException";
stock const FileNotFoundException[] = "FileNotFoundException";

native Throw(const exception[] = Exception, const format[] = NULL_STRING, any: ...);
native TryBegin(const Logger: logger = Invalid_Logger);
native bool: TryCatch(const exception[] = NULL_STRING);
native TryHandle();
native TryEnd();