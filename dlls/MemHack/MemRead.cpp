#include "MemMisc.h"

/* Functions that read different data types in memory */

template <typename Type>
Type UTIL_ReadMemory(maddress BaseAddress, maddress StartAddress, char MemType, Type returnType)
{
	maddress EndAddress = GetRealMemoryAddress(BaseAddress, StartAddress, MemType);

	return *(Type*)EndAddress;
}

char UTIL_ReadMemory_Byte(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, char(NULL));
}

short UTIL_ReadMemory_Word(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, short(NULL));
}

int32_t UTIL_ReadMemory_Dword(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, int32_t(NULL));
}

long long UTIL_ReadMemory_Qword(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, (long long)(NULL));
}

float UTIL_ReadMemory_Float(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, float(NULL));
}

 unsigned char UTIL_ReadMemory_UnsignedByte(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, (unsigned char)(NULL));
}

 unsigned short UTIL_ReadMemory_UnsignedWord(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, (unsigned short)(NULL));
}

 uint32_t UTIL_ReadMemory_UnsignedDword(maddress BaseAddress, maddress address, char memType)
{
	return UTIL_ReadMemory( BaseAddress, address, memType, uint32_t(NULL));
}

 maddress UTIL_ReadMemory_Pointer(maddress BaseAddress, maddress address, char memType) 
{
	return UTIL_ReadMemory( BaseAddress, address, memType, maddress(NULL));
}
