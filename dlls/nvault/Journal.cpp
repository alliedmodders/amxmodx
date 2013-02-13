#include <stdio.h>
#include <stdarg.h>
#if defined(__linux__) || defined(__APPLE__)
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
	{
		return -1;
	}
	
	BinaryReader br(m_fp);

	uint8_t len8;
	uint16_t len16;
	char *key = NULL;
	char *val = NULL;
	String sKey;
	String sVal;
	time_t stamp;
	JOp op;
	int ops = 0;
	uint8_t temp8;
	
	uint32_t itemp;

//	try
//	{
	do
	{
		if (!br.ReadUInt8(temp8)) goto fail;
		op = static_cast<JOp>(temp8);
		if (op == Journal_Clear)
		{
			pMap->Clear();
		} else if (op == Journal_Prune) {
			time_t start;
			time_t end;
			
			if (!br.ReadUInt32(itemp)) goto fail;
			start = static_cast<time_t>(itemp);

			if (!br.ReadUInt32(itemp)) goto fail;
			end = static_cast<time_t>(itemp);
			
			pMap->Prune(start, end);
			
		} else if (op == Journal_Insert) {
		
			
			if (!br.ReadUInt32(itemp)) goto fail;
			stamp = static_cast<time_t>(itemp);
			
			if (!br.ReadUInt8(len8)) goto fail;
			
			key = new char[len8+1];
			if (!br.ReadChars(key, len8)) goto fail;
			
			if (!br.ReadUInt16(len16)) goto fail;
			val = new char[len16+1];

			if (!br.ReadChars(val, len16)) goto fail;
			
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
		
			if (!br.ReadUInt8(len8)) goto fail;

			key = new char[len8+1];
			if (!br.ReadChars(key, len8)) goto fail;
			key[len8] = '\0';
			sKey.assign(key);
			pMap->Remove(sKey);
		}
		ops++;
	} while (op < Journal_TotalOps && op);
	goto success;
//	} catch (...) { 

fail:
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
//	}

success:
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
//	try
//	{
		if (!WriteOp(Journal_Clear)) goto fail;
		return true;
//	} catch (...) {
fail:
		return false;
//	}
}

bool Journal::Write_Insert(const char *key, const char *val, time_t stamp)
{
//	try
//	{
		if (!WriteOp(Journal_Insert)) goto fail;
		if (!WriteInt32(static_cast<int32_t>(stamp))) goto fail;
		if (!WriteString(key, Encode_Small)) goto fail;
		if (!WriteString(val, Encode_Medium)) goto fail;
		return true;
//	} catch (...) {
fail:
		return false;
//	}
}

bool Journal::Write_Prune(time_t start, time_t end)
{
//	try
//	{
		if (!WriteOp(Journal_Prune)) goto fail;
		if (!WriteInt32(static_cast<int32_t>(start))) goto fail;
		if (!WriteInt32(static_cast<int32_t>(end))) goto fail;
		return true;
//	} catch (...) {

fail:
		return false;
//	}
}

bool Journal::Write_Remove(const char *key)
{
//	try
//	{
		if (!WriteOp(Journal_Remove)) goto fail;
		if (!WriteString(key, Encode_Small)) goto fail;
		return true;
//	} catch (...) {

fail:
		return false;
//	}
}

bool Journal::WriteInt32(int num)
{
	return m_Bw.WriteInt32(num);
}

bool Journal::WriteOp(JOp op)
{
	return m_Bw.WriteUInt8(static_cast<uint8_t>(op));
}

bool Journal::WriteString(const char *str, Encode enc)
{
	size_t len = strlen(str);
	if (enc == Encode_Small)
	{
		if (!m_Bw.WriteUInt8(static_cast<uint8_t>(len))) return false;
	} else if (enc == Encode_Medium) {
		if (!m_Bw.WriteUInt16(static_cast<uint16_t>(len))) return false;
	}
	return m_Bw.WriteChars(str, len); 
	
	
}

