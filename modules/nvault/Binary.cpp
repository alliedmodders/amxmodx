// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// NVault Module
//

#include "Binary.h"
#include "amxxmodule.h"

BinaryWriter::BinaryWriter(FILE *fp)
{
	m_Fp = fp;
}

bool BinaryWriter::WriteAddr(void *buffer, size_t size)
{
	if (fwrite(buffer, size, 1, m_Fp) != 1)
		return false;

	return true;
}

bool BinaryWriter::WriteUInt32(uint32_t num)
{
	if ( !WriteAddr(&num, sizeof(uint32_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteInt32(int32_t num)
{
	if ( !WriteAddr(&num, sizeof(int32_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteUInt16(uint16_t num)
{
	if ( !WriteAddr(&num, sizeof(uint16_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteInt16(int16_t num)
{
	if ( !WriteAddr(&num, sizeof(int16_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteUInt8(uint8_t num)
{
	if ( !WriteAddr(&num, sizeof(uint8_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteInt8(int8_t num)
{
	if ( !WriteAddr(&num, sizeof(int8_t)) )
		return false;
		
	return true;
}

bool BinaryWriter::WriteChars(const char buffer[], size_t chars)
{
	if (!chars)
		return true;

	if (fwrite(buffer, sizeof(char), chars, m_Fp) != chars)
		return false;
		
	return true;
}


BinaryReader::BinaryReader(FILE *fp)
{
	m_Fp = fp;
}

bool BinaryReader::ReadAddr(void *buffer, size_t size)
{
	if (fread(buffer, size, 1, m_Fp) != 1)
		return false;

	return true;
}
bool BinaryReader::ReadUInt32(uint32_t& num)
{
	if ( !ReadAddr(&num, sizeof(uint32_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadInt32(int32_t& num)
{
	if ( !ReadAddr(&num, sizeof(int32_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadUInt16(uint16_t& num)
{
	if ( !ReadAddr(&num, sizeof(uint16_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadInt16(int16_t& num)
{
	if ( !ReadAddr(&num, sizeof(int16_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadUInt8(uint8_t& num)
{
	if ( !ReadAddr(&num, sizeof(uint8_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadInt8(int8_t& num)
{
	if ( !ReadAddr(&num, sizeof(int8_t)) )
		return false;

	return true;
}

bool BinaryReader::ReadChars(char buffer[], size_t chars)
{
	if (!chars)
		return true;

	if (fread(buffer, sizeof(char), chars, m_Fp) != chars)
		return false;

	return true;
}

