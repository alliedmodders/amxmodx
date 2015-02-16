// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if !defined __HASHING_H__
#define      __HASHING_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#if defined(__FreeBSD__) || defined(__OpenBSD__)
   #include <sys/endian.h>
   #define __BYTE_ORDER    BYTE_ORDER
   #define __LITTLE_ENDIAN LITTLE_ENDIAN
   #define __BIG_ENDIAN    BIG_ENDIAN
#elif defined(LINUX) || defined(EMSCRIPTEN)
   #include <endian.h>
#elif defined(__APPLE__)
   #include <sys/types.h>
   #define __BYTE_ORDER    BYTE_ORDER
#endif

#if !defined(BIG_ENDIAN)
   #define BIG_ENDIAN      4321
#endif

#if !defined(LITTLE_ENDIAN)
   #define LITTLE_ENDIAN   1234
#endif

/**
 * Gets hashers included.
 */
#include "hashers/crc32.h"
#include "hashers/md5.h"
#include "hashers/sha1.h"
#include "hashers/sha256.h"
#include "hashers/sha3.h"
#include "hashers/keccak.h"

/**
 * HashType constants.
 * To be used on hashFile() and hashString()
 */
enum HashType
{
	Hash_Crc32 = 0,   // Provides CRC32 hashing
	Hash_Md5,         // Provides MD5 hashing
	Hash_Sha1,        // Provides SHA1 hashing
	Hash_Sha256,      // Provides SHA256 hashing

	Hash_Sha3_224,    // Provides SHA3 224 bit hashing
	Hash_Sha3_256,    // Provides SHA3 256 bit hashing
	Hash_Sha3_384,    // Provides SHA3 384 bit hashing
	Hash_Sha3_512,    // Provides SHA3 512 bit hashing

	Hash_Keccak_224,  // Provides KECCAK 224 bit hashing
	Hash_Keccak_256,  // Provides KECCAK 256 bit hashing
	Hash_Keccak_384,  // Provides KECCAK 384 bit hashing
	Hash_Keccak_512,  // Provides KECCAK 512 bit hashing
};

/**
 * Hashes a file content (bytes)
 * @note       Returns NULL if "fileName" does not represent a file name
 * @note       Returns NULL if file could not be opened
 * @note       Returns NULL if invalid "Type" is specified
 */
const char* hashFile(const char* fileName, HashType Type);

/**
 * Hashes a string
 * @note       Returns NULL if "String" does not represent a string
 * @note       Returns NULL if the string has no bytes to hash
 * @note       Returns NULL if invalid "Type" is specified
 */
const char* hashString(const char* String, size_t stringLen, HashType Type);

#endif // __HASHING_H__
