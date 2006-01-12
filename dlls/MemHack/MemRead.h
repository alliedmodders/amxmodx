#ifndef __MEMREAD_H__
#define __MEMREAD_H__

#include "MemMisc.h"

// Functions that read different data types in memory

// Base function
template <typename Type>
extern Type UTIL_ReadMemory(maddress BaseAddress, maddress StartAddress, char MemType, Type returnType);

// Inline stocks
inline char           UTIL_ReadMemory_Byte          (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline short          UTIL_ReadMemory_Word          (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline int32_t        UTIL_ReadMemory_Dword         (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline long long      UTIL_ReadMemory_Qword         (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline float          UTIL_ReadMemory_Float         (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline unsigned char  UTIL_ReadMemory_UnsignedByte  (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline unsigned short UTIL_ReadMemory_UnsignedWord  (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline uint32_t       UTIL_ReadMemory_UnsignedDword (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);
inline maddress       UTIL_ReadMemory_Pointer       (maddress baseaddress, maddress address, char memType = MEMTYPE_DATA);

#endif