// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "hashing.h"


/**
 * Hashes a file content (bytes)
 * @note       Returns NULL if "fileName" does not represent a file name
 * @note       Returns NULL if file could not be opened
 * @note       Returns NULL if invalid "Type" is specified
 */
const char* hashFile(const char* fileName, HashType Type)
{
	/**
	 * Sanity check of input parameters
	 */
	if (!fileName || fileName[0] == 0)
		return NULL;

	/**
	 * Opens and checks file
	 */
	FILE* pFile = fopen(fileName, "rb");
	if (!pFile)
		return NULL;

	/**
	 * Gets hashers ready.
	 */
	CRC32 Crc32;
	MD5 Md5;
	SHA1 Sha1;
	SHA256 Sha256;
	SHA3 Sha3;
	Keccak Kec;

	/**
	 * Changes bits if needed
	 */
	switch (Type)
	{
	case Hash_Sha3_224:   Sha3.changeBits(SHA3::Bits224);   break;
	case Hash_Sha3_384:   Sha3.changeBits(SHA3::Bits384);    break;
	case Hash_Sha3_512:   Sha3.changeBits(SHA3::Bits512);    break;
	case Hash_Keccak_224: Kec.changeBits(Keccak::Keccak224); break;
	case Hash_Keccak_384: Kec.changeBits(Keccak::Keccak384); break;
	case Hash_Keccak_512: Kec.changeBits(Keccak::Keccak512); break;
	};

	/**
	 * Retrieves file's content and fills hashers in.
	 */
	char Bytes[8192];
	size_t Count;
	while ((Count = fread(Bytes, sizeof(char), sizeof Bytes, pFile)))
	{
		switch (Type)
		{
		case Hash_Crc32:     Crc32.add(Bytes, Count);  break;
		case Hash_Md5:       Md5.add(Bytes, Count);    break;
		case Hash_Sha1:      Sha1.add(Bytes, Count);   break;
		case Hash_Sha256:    Sha256.add(Bytes, Count); break;
		case Hash_Sha3_224:
		case Hash_Sha3_256:
		case Hash_Sha3_384:
		case Hash_Sha3_512:   Sha3.add(Bytes, Count);  break;
		case Hash_Keccak_224:
		case Hash_Keccak_256:
		case Hash_Keccak_384:
		case Hash_Keccak_512: Kec.add(Bytes, Count);   break;
		};
	};

	/**
	 * Closes file
	 */
	fclose(pFile);

	/**
	 * Retrieves hash
	 */
	switch (Type)
	{
	case Hash_Crc32:      return Crc32.getHash();
	case Hash_Md5:        return Md5.getHash();
	case Hash_Sha1:       return Sha1.getHash();
	case Hash_Sha256:     return Sha256.getHash();
	case Hash_Sha3_224:
	case Hash_Sha3_256:
	case Hash_Sha3_384:
	case Hash_Sha3_512:   return Sha3.getHash();
	case Hash_Keccak_224:
	case Hash_Keccak_256:
	case Hash_Keccak_384:
	case Hash_Keccak_512: return Kec.getHash();
	};

	/**
	 * Something went wrong
	 */
	return NULL;
};

/**
 * Hashes a string
 * @note       Returns NULL if "String" does not represent a string
 * @note       Returns NULL if invalid "Type" is specified
 */
const char* hashString(const char* String, size_t stringLen, HashType Type)
{
	/**
	 * Sanity check of input parameters
	 */
	if (!String)
		return NULL;

	/**
	 * Gets hashers ready.
	 */
	CRC32 Crc32;
	MD5 Md5;
	SHA1 Sha1;
	SHA256 Sha256;
	SHA3 Sha3;
	Keccak Kec;

	/**
	 * Changes bits if needed
	 */
	switch (Type)
	{
	case Hash_Sha3_224:   Sha3.changeBits(SHA3::Bits224);    break;
	case Hash_Sha3_384:   Sha3.changeBits(SHA3::Bits384);    break;
	case Hash_Sha3_512:   Sha3.changeBits(SHA3::Bits512);    break;
	case Hash_Keccak_224: Kec.changeBits(Keccak::Keccak224); break;
	case Hash_Keccak_384: Kec.changeBits(Keccak::Keccak384); break;
	case Hash_Keccak_512: Kec.changeBits(Keccak::Keccak512); break;
	};

	/**
	 * Fills hashers in, computes string hash and returns the hash
	 */
	switch (Type)
	{
	case Hash_Crc32:      return Crc32(String, stringLen);
	case Hash_Md5:        return Md5(String, stringLen);
	case Hash_Sha1:       return Sha1(String, stringLen);
	case Hash_Sha256:     return Sha256(String, stringLen);
	case Hash_Sha3_224:
	case Hash_Sha3_256:
	case Hash_Sha3_384:
	case Hash_Sha3_512:   return Sha3(String, stringLen);
	case Hash_Keccak_224:
	case Hash_Keccak_256:
	case Hash_Keccak_384:
	case Hash_Keccak_512: return Kec(String, stringLen);
	};

	/**
	 * Something went wrong
	 */
	return NULL;
};
