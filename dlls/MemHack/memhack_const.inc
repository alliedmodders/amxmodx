#if defined _memhack_const_included
  #endinput
#endif
#define _memhack_const_included

// Different Address Bases
#define MEM_DLLBASE 0
#define MEM_ENGBASE 1
#define MEM_NULLBASE 2

// Signed or unsigned
#define MEM_SIGNED 0
#define MEM_UNSIGNED 1

// Memory area types
#define MEMTYPE_CODE   0 // Code (usually .text segment, requires mprotect or VirtualProtect)
#define MEMTYPE_DATA   1 // Data (usually .data segment, writable by default)
#define MEMTYPE_RODATA 2 // Read-Only Data (usually .rodata on Linux, .rdata on Windows)

// Return codes for patching (set natives)
#define MP_FAIL -1
#define MP_OK   0

