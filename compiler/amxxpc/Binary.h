// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_BINARY_H
#define _INCLUDE_BINARY_H

#include <stdio.h>
#include "amx.h"

#ifdef WIN32
	typedef	__int8	int8_t;
	typedef	unsigned __int8 uint8_t;
	typedef	__int16	int16_t;
	typedef	unsigned __int16 uint16_t;
#else
#include <stdint.h>
#endif

class BinaryReader
{
public:
	BinaryReader(FILE *fp);
	//~BinaryReader();
public:
	uint32_t ReadUInt32();
	int32_t ReadInt32();
	uint16_t ReadUInt16();
	int16_t ReadInt16();
	uint8_t ReadUInt8();
	int8_t ReadInt8();
	char *ReadChars(char buffer[], size_t chars);
private:
	bool ReadAddr(void *buffer, size_t size);
private:
	FILE *m_Fp;
};

class BinaryWriter
{
public:
	BinaryWriter(FILE *fp);
public:
	void WriteUInt32(uint32_t num);
	void WriteInt32(int32_t num);
	void WriteUInt16(uint16_t num);
	void WriteInt16(int16_t num);
	void WriteUInt8(uint8_t num);
	void WriteInt8(int8_t num);
	void WriteChars(const char buffer[], size_t chars);
private:
	bool WriteAddr(void *buffer, size_t size);
private:
	FILE *m_Fp;
};

#endif //_INCLUDE_BINARY_H

