#ifndef UTILS_H
#define UTILS_H

int UTIL_CheckValidChar(char *c);
char *UTIL_ReplaceEx(char *subject, size_t maxLen, const char *search, size_t searchLen, const char *replace, size_t replaceLen, bool caseSensitive);
unsigned int strncopy(char *dest, const char *src, size_t count);

#endif // UTILS_H