#ifndef _INCLUDE_BINARY_H
#define _INCLUDE_BINARY_H

#include <stdio.h>
#include "compat.h"

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
	BinaryWriter() { m_Fp = NULL; }
	BinaryWriter(FILE *fp);
public:
	void SetFilePtr(FILE *fp) { m_Fp = fp; }
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