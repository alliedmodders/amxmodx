#include "MemMisc.h"

/* Functions that patch different data types in memory */
template <typename Type>
int UTIL_PatchMemory(maddress baseaddress, maddress address, Type patch, char memType, size_t byteType, bool extraProtect) 
{
	unsigned long oldProtect = 0;
	maddress realAddress = GetRealMemoryAddress(baseaddress, address, memType);

	switch (memType) 
	{
		case MEMTYPE_CODE:
			if (MemoryProtect((void*)realAddress, byteType, MPROT_CODE_EDIT, &oldProtect, memType) == MP_FAIL) return MP_FAIL;
			break;

		case MEMTYPE_RODATA:
			if (MemoryProtect((void*)realAddress, byteType, MPROT_RODATA_EDIT, &oldProtect, memType) == MP_FAIL) return MP_FAIL;
			break;
	}

	*(Type*)realAddress = patch;

	if (memType == MEMTYPE_CODE)
	{
		MemoryProtect((void*)realAddress, byteType, oldProtect, &oldProtect);
	}
	else if(extraProtect == true)
	{
		if(memType == MEMTYPE_RODATA) MemoryProtect((void*)realAddress, byteType, oldProtect, &oldProtect);
	}

	return MP_OK;
}

 int UTIL_PatchMemory_Byte(maddress baseaddress, maddress address, char patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (char)patch, memType, BYTE_BYTES, true);
}

 int UTIL_PatchMemory_Word(maddress baseaddress, maddress address, short patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (short)patch, memType, WORD_BYTES, true);
}

 int UTIL_PatchMemory_Dword(maddress baseaddress, maddress address, int32_t patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (int32_t)patch, memType, DWORD_BYTES, true);
}

 int UTIL_PatchMemory_Qword(maddress baseaddress, maddress address, long long patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (long long)patch, memType, QWORD_BYTES, false);
}

 int UTIL_PatchMemory_Float(maddress baseaddress, maddress address, float patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (float)patch, memType, FLOAT_BYTES, false);
}

 int UTIL_PatchMemory_UnsignedByte(maddress baseaddress, maddress address, unsigned char patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (unsigned char)patch, memType, BYTE_BYTES, false);
}

 int UTIL_PatchMemory_UnsignedWord(maddress baseaddress, maddress address, unsigned short patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (unsigned short)patch, memType, WORD_BYTES, false);
}

 int UTIL_PatchMemory_UnsignedDword(maddress baseaddress, maddress address, uint32_t patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (uint32_t)patch, memType, DWORD_BYTES, false);
}

 int UTIL_PatchMemory_Pointer(maddress baseaddress, maddress address, maddress patch, char memType)
{
	return UTIL_PatchMemory(baseaddress, address, (maddress)patch, memType, DWORD_BYTES, true);
}
