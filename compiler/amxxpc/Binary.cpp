// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "Binary.h"

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

void BinaryWriter::WriteUInt32(uint32_t num)
{
	if ( !WriteAddr(&num, sizeof(uint32_t)) )
		throw -1;
}

void BinaryWriter::WriteInt32(int32_t num)
{
	if ( !WriteAddr(&num, sizeof(int32_t)) )
		throw -1;
}

void BinaryWriter::WriteUInt16(uint16_t num)
{
	if ( !WriteAddr(&num, sizeof(uint16_t)) )
		throw -1;
}

void BinaryWriter::WriteInt16(int16_t num)
{
	if ( !WriteAddr(&num, sizeof(int16_t)) )
		throw -1;
}

void BinaryWriter::WriteUInt8(uint8_t num)
{
	if ( !WriteAddr(&num, sizeof(uint8_t)) )
		throw -1;
}

void BinaryWriter::WriteInt8(int8_t num)
{
	if ( !WriteAddr(&num, sizeof(int8_t)) )
		throw -1;
}

void BinaryWriter::WriteChars(const char buffer[], size_t chars)
{
	if (!chars)
		return;

	if (fwrite(buffer, sizeof(char), chars, m_Fp) != chars)
		throw -1;
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

uint32_t BinaryReader::ReadUInt32()
{
	uint32_t num;

	if ( !ReadAddr(&num, sizeof(uint32_t)) )
		throw -1;

	return num;
}

int32_t BinaryReader::ReadInt32()
{
	int32_t num;

	if ( !ReadAddr(&num, sizeof(int32_t)) )
		throw -1;

	return num;
}

uint16_t BinaryReader::ReadUInt16()
{
	uint16_t num;

	if ( !ReadAddr(&num, sizeof(uint16_t)) )
		throw -1;

	return num;
}

int16_t BinaryReader::ReadInt16()
{
	int16_t num;

	if ( !ReadAddr(&num, sizeof(int16_t)) )
		throw -1;

	return num;
}

uint8_t BinaryReader::ReadUInt8()
{
	uint8_t num;

	if ( !ReadAddr(&num, sizeof(uint8_t)) )
		throw -1;

	return num;
}

int8_t BinaryReader::ReadInt8()
{
	int8_t num;

	if ( !ReadAddr(&num, sizeof(int8_t)) )
		throw -1;

	return num;
}

char *BinaryReader::ReadChars(char buffer[], size_t chars)
{
	if (!chars)
		return buffer;

	if (fread(buffer, sizeof(char), chars, m_Fp) != chars)
		throw -1;

	return buffer;
}
