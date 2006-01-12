#ifndef __MEMWRITE_H__
#define __MEMWRITE_H__

#include "MemMisc.h"

// Functions that patch different data types in memory

// Base function
template <typename Type>
inline int UTIL_PatchMemory(maddress baseaddress, maddress address, Type patch, char memType, size_t byteType, bool extraProtect);

// Inline stocks
extern inline int UTIL_PatchMemory_Byte          (maddress baseaddress, maddress address, char patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_Word          (maddress baseaddress, maddress address, short patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_Dword         (maddress baseaddress, maddress address, int32_t patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_Qword         (maddress baseaddress, maddress address, long long patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_Float         (maddress baseaddress, maddress address, float patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_UnsignedByte  (maddress baseaddress, maddress address, unsigned char patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_UnsignedWord  (maddress baseaddress, maddress address, unsigned short patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_UnsignedDword (maddress baseaddress, maddress address, uint32_t patch, char memType = MEMTYPE_DATA);
extern inline int UTIL_PatchMemory_Pointer       (maddress baseaddress, maddress address, maddress patch, char memType = MEMTYPE_DATA);

#endif