#ifndef __MEMCONST_H__
#define __MEMCONST_H__

#ifdef __linux__
	#include <sys/mman.h>
#endif

#include "amxxmodule.h"

// Define memory address type
#ifdef __amd64__
	typedef uint64_t maddress;
#else
	typedef uint32_t maddress;
#endif

// Number of bytes for different data types
#define BYTE_BYTES  1
#define WORD_BYTES  2
#define DWORD_BYTES 4
#define QWORD_BYTES 8
#define FLOAT_BYTES 4

// Return codes for MemoryProtect
#define MP_FAIL -1
#define MP_OK   0

// Memory protection constants
#ifdef __linux__
	#define MPROT_CODE        PROT_READ|PROT_EXEC
	#define MPROT_DATA        PROT_READ|PROT_WRITE
	#define MPROT_RODATA      PROT_READ
	#define MPROT_CODE_EDIT   PROT_READ|PROT_WRITE|PROT_EXEC
	#define MPROT_RODATA_EDIT PROT_READ|PROT_WRITE
#else
	#define MPROT_CODE        PAGE_EXECUTE_READ
	#define MPROT_DATA        PAGE_READWRITE
	#define MPROT_RODATA      PAGE_READONLY
	#define MPROT_CODE_EDIT   PAGE_EXECUTE_READWRITE
	#define MPROT_RODATA_EDIT PAGE_READWRITE
#endif

// Memory area types
#define MEMTYPE_CODE   0 // Code (usually .text segment, requires mprotect or VirtualProtect)
#define MEMTYPE_DATA   1 // Data (usually .data segment, writable by default)
#define MEMTYPE_RODATA 2 // Read-Only Data (usually .rodata on Linux, .rdata on Windows)

#endif // #ifndef __MEMHACK_H__
