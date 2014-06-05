#ifndef _INCLUDE_FORMATTING_H
#define _INCLUDE_FORMATTING_H

//Amx Templatized Cell Printf
template <typename D, typename S>
size_t atcprintf(D *buffer, size_t maxlen, const S *format, AMX *amx, cell *params, int *param);

const char *translate(AMX *amx, cell amxaddr, const char *key);

#endif //_INCLUDE_FORMATTING_H
