#include <stdio.h>
#if defined __linux__
#include <unistd.h>
#endif
#include "Journal.h"

Journal::Journal(const char *file)
{
	m_File.assign(file);
}

bool Journal::Erase()
{
	return (unlink(m_File.c_str()) == 0);
}

int Journal::Replay(VaultMap *pMap)
{
	m_fp = fopen(m_File.c_str(), "rb");
	if (!m_fp)
		return -1;
	
	BinaryReader br(m_fp);

	int8_t len8;
	int16_t len16;
	char *key = NULL;
	char *val = NULL;
	String sKey;
	String sVal;
	time_t stamp;
	JOp op;
	int ops = 0;

	try
	{
		do
		{
			op = static_cast<JOp>(br.ReadUInt8());
			if (op == Journal_Clear)
			{
				pMap->Clear();
			} else if (op == Journal_Prune) {
				time_t start;
				time_t end;
				start = static_cast<time_t>(br.ReadUInt32());
				end = static_cast<time_t>(br.ReadUInt32());
				pMap->Prune(start, end);
			} else if (op == Journal_Insert) {
				stamp = static_cast<time_t>(br.ReadUInt32());
				len8 = br.ReadUInt8();
				key = new char[len8+1];
				br.ReadChars(key, len8);
				len16 = br.ReadUInt16();
				val = new char[len16+1];
				br.ReadChars(val, len16);
				key[len8] = '\0';
				val[len16] = '\0';
				sKey.assign(key);
				sVal.assign(val);
				pMap->Insert(sKey, sVal, stamp);
				//clean up
				delete [] key;
				key = NULL;
				delete [] val;
				val = NULL;
			} else if (op == Journal_Remove) {
				len8 = br.ReadUInt8();
				key = new char[len8+1];
				br.ReadChars(key, len8);
				key[len8] = '\0';
				sKey.assign(key);
				pMap->Remove(sKey);
			}
			ops++;
		} while (op < Journal_TotalOps && op);
	} catch (...) {
		//journal is done
		if (key)
		{
			delete [] key;
			key = NULL;
		}
		if (val)
		{
			delete [] val;
			val = NULL;
		}
	}

	fclose(m_fp);

	return ops;
}

bool Journal::Begin()
{
	m_fp = fopen(m_File.c_str(), "wb");
	m_Bw.SetFilePtr(m_fp);
	return (m_fp != NULL);
}

bool Journal::End()
{
	fclose(m_fp);
	m_Bw.SetFilePtr(NULL);
	return true;
}

bool Journal::Write_Clear()
{
	try
	{
		WriteOp(Journal_Clear);
		return true;
	} catch (...) {
		return false;
	}
}

bool Journal::Write_Insert(const char *key, const char *val, time_t stamp)
{
	try
	{
		WriteOp(Journal_Insert);
		WriteInt32(static_cast<int32_t>(stamp));
		WriteString(key, Encode_Small);
		WriteString(val, Encode_Medium);
		return true;
	} catch (...) {
		return false;
	}
}

bool Journal::Write_Prune(time_t start, time_t end)
{
	try
	{
		WriteOp(Journal_Prune);
		WriteInt32(static_cast<int32_t>(start));
		WriteInt32(static_cast<int32_t>(end));
		return true;
	} catch (...) {
		return false;
	}
}

bool Journal::Write_Remove(const char *key)
{
	try
	{
		WriteOp(Journal_Remove);
		WriteString(key, Encode_Small);
		return true;
	} catch (...) {
		return false;
	}
}

void Journal::WriteInt32(int num)
{
	m_Bw.WriteInt32(num);
}

void Journal::WriteOp(JOp op)
{
	m_Bw.WriteUInt8(static_cast<uint8_t>(op));
}

void Journal::WriteString(const char *str, Encode enc)
{
	size_t len = strlen(str);
	if (enc == Encode_Small)
	{
		m_Bw.WriteUInt8(static_cast<uint8_t>(len));
	} else if (enc == Encode_Medium) {
		m_Bw.WriteUInt16(static_cast<uint16_t>(len));
	}
	m_Bw.WriteChars(str, len); 
}

