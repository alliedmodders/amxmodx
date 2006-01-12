#include "MemConst.h"

// Game memory addresses
maddress gameDllAddress;
maddress gameEngAddress;

bool GetBaseAddress(void *pAddr, maddress &pBaseAddr)
{
#ifdef WIN32
	MEMORY_BASIC_INFORMATION mem;
	if (!VirtualQuery(pAddr, &mem, sizeof(mem)))
		return false;

	if (pBaseAddr)
		pBaseAddr = (maddress)mem.AllocationBase;

	IMAGE_DOS_HEADER *dos = (IMAGE_DOS_HEADER *)(mem.AllocationBase);
	IMAGE_NT_HEADERS *pe = reinterpret_cast<IMAGE_NT_HEADERS *>((unsigned long)dos + (unsigned long)dos->e_lfanew);
	if (pe->Signature != IMAGE_NT_SIGNATURE)
		return false;

	return true;
#else
	Dl_info info;
	struct stat buf;
	
	if (!dladdr(pAddr, &info))
		return false;
	
	if (!info.dli_fbase || !info.dli_fname)
		return false;
	
	if (stat(info.dli_fname, &buf) != 0)
		return false;
	
	if (pBaseAddr)
		pBaseAddr = (maddress)info.dli_fbase;
	if (memLength)
		*memLength = buf.st_size;
	
	return true;
#endif
}

/* Wrapper for mprotect and VirtualProtect */
int MemoryProtect(void *addr, size_t len, unsigned long newProt, unsigned long *oldProt, char memType) {
	int retVal;

	#ifdef __linux__
		maddress alignAddr = (maddress)addr - ((maddress)addr % pageSize);
		retVal = mprotect((void*)alignAddr, pageSize, newProt);

		// Linux's mprotect doesn't get the old protection flags, so we have to fake it
		switch (memType) {
			case MEMTYPE_CODE:
				*oldProt = MPROT_CODE;
				break;
			case MEMTYPE_RODATA:
				*oldProt = MPROT_RODATA;
				break;
			default:
				*oldProt = MPROT_CODE;
				break;
		}
	#else
		retVal = VirtualProtect(addr, len, newProt, oldProt);
		// This will match the Windows return value with the Linux ones, done for consistency
		if (retVal == 0) {
			retVal = -1;
		} else {
			retVal = 0;
		}
	#endif

	return retVal;
}

/* Gets real memory address */
maddress GetRealMemoryAddress(maddress baseaddress, maddress address, char memType) 
{
	if(baseaddress == NULL) return address;

	maddress realAddress = address;

	switch (memType) 
	{
		case MEMTYPE_CODE: case MEMTYPE_RODATA:
			realAddress = baseaddress + address;
			break;
		case MEMTYPE_DATA:
			// Linux's data segment is in a not so simple place in memory
			#ifdef __linux__
				realAddress = dataSectionStart + (address - dataSectionOffset);
			#else
				realAddress = baseaddress + address;
			#endif
			break;
	}

	return realAddress;
}