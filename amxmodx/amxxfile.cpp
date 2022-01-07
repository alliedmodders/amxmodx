// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "amxxfile.h"
#include "zlib/zlib.h"

/**********************
 ****** AMXXFILE ******
 **********************/

#if defined __GNUC__
	#define PACKED		__attribute__((packed))
#else
	#define PACKED
#endif

#if defined(__linux__) || defined(__APPLE__)
	#pragma pack(1)				/* structures must be packed (byte-aligned) */
#else
	#pragma pack(1)				/* structures must be packed (byte-aligned) */
	#if defined __TURBOC__
		#pragma option -a-		/* "pack" pragma for older Borland compilers */
	#endif
#endif

struct TableEntry
{
	mint8_t cellSize;
	mint32_t origSize PACKED;			// contains AMX_HEADER->stp
	mint32_t offset PACKED;
};

#define DATAREAD(addr, itemsize, itemcount) \
	if (fread((addr), (itemsize), (itemcount), (m_pFile)) != (itemcount)) \
	{ \
		if (feof(m_pFile)) \
			m_Status = Err_FileInvalid; \
		else \
			m_Status = Err_FileRead; \
		fclose(m_pFile); \
		m_pFile = NULL; \
		return; \
	}

CAmxxReader::CAmxxReader(const char *filename, int cellsize)
{
	m_Bh.plugins = NULL;
	m_AmxxFile = false;
	
	if (!filename)
	{
		m_Status = Err_InvalidParam;
		return;
	}

	m_Status = Err_None;
	m_CellSize = cellsize;
	m_pFile = fopen(filename, "rb");
	
	if (!m_pFile)
	{
		m_Status = Err_FileOpen;
		return;
	}

	mint32_t magic;
	DATAREAD(&magic, sizeof(magic), 1);

	m_OldFile = false;
	
	if (magic == 0x524C4542)
	{
		//we have an invalid, old, RLEB file
		m_Status = Err_OldFile;
		fclose(m_pFile);
		m_pFile = NULL;
		
		return;
	}
	else if (magic == MAGIC_HEADER2)
	{
		DATAREAD(&m_Bh.version, sizeof(int16_t), 1);
		
		if (m_Bh.version > MAGIC_VERSION)
		{
			m_Status = Err_OldFile;
			fclose(m_pFile);
			m_pFile = NULL;
			
			return;
		}
		
		m_AmxxFile = true;
		DATAREAD(&m_Bh.numPlugins, sizeof(mint8_t), 1);
		m_Bh.plugins = new PluginEntry[m_Bh.numPlugins];
		PluginEntry *pe;
		m_SectionHdrOffset = 0;
		m_Entry = -1;
		
		for (mint8_t i = 0; i < m_Bh.numPlugins; i++)
		{
			pe = &(m_Bh.plugins[(unsigned)i]);
			DATAREAD(&pe->cellsize, sizeof(mint8_t), 1);
			DATAREAD(&pe->disksize, sizeof(int32_t), 1);
			DATAREAD(&pe->imagesize, sizeof(int32_t), 1);
			DATAREAD(&pe->memsize, sizeof(int32_t), 1);
			DATAREAD(&pe->offs, sizeof(int32_t), 1);
		}
		
		for (mint8_t i = 0; i < m_Bh.numPlugins; i++)
		{
			pe = &(m_Bh.plugins[(unsigned)i]);
			
			if (pe->cellsize == m_CellSize)
			{
				m_Entry = i;
				break;
			}
		}
		
		if (m_Entry == -1)
		{
			m_Status = Err_SectionNotFound;
			fclose(m_pFile);
			m_pFile = NULL;
			
			return;
		}
		
		pe = &(m_Bh.plugins[m_Entry]);
		m_SectionLength = pe->disksize;
	}
	else if (magic == MAGIC_HEADER)
	{
		// try to find the section
		mint8_t numOfPlugins;
		DATAREAD(&numOfPlugins, sizeof(numOfPlugins), 1);
	
		TableEntry entry;
	
		m_SectionHdrOffset = 0;
		int i = 0;
		
		for (i = 0; i < static_cast<int>(numOfPlugins); ++i)
		{
			DATAREAD(&entry, sizeof(entry), 1);
			if (entry.cellSize == m_CellSize)
			{
				m_SectionHdrOffset = ftell(m_pFile) - sizeof(entry);
				break;
			}
		}
		
		if (!m_SectionHdrOffset)
		{
			m_Status = Err_SectionNotFound;
			fclose(m_pFile);
			m_pFile = NULL;
			return;
		}

		// compute section length
		if ((i + 1) < static_cast<int>(numOfPlugins))
		{
			// there is a next section
			TableEntry nextEntry;
			DATAREAD(&nextEntry, sizeof(nextEntry), 1);
			m_SectionLength = nextEntry.offset - entry.offset;
		} else {
			fseek(m_pFile, 0, SEEK_END);
			m_SectionLength = ftell(m_pFile) - (long)entry.offset;
		}
	} else {
		// check for old file
		AMX_HEADER hdr;
		rewind(m_pFile);
		fread(&hdr, sizeof(hdr), 1, m_pFile);
		amx_Align16(&hdr.magic);
		
		if (hdr.magic == AMX_MAGIC)
		{
			if (cellsize != 4)
			{
				m_Status = Err_SectionNotFound;
				fclose(m_pFile);
				m_pFile = NULL;
				
				return;
			}

			m_OldFile = true;
			
			return;
		} else {
			// no known file format
			m_Status = Err_FileInvalid;
			fclose(m_pFile);
			m_pFile = NULL;
			
			return;
		}
	} 
}

CAmxxReader::~CAmxxReader()
{
	if (m_pFile)
	{
		fclose(m_pFile);
		m_pFile = NULL;
	}
	
	if (m_Bh.plugins)
	{
		delete [] m_Bh.plugins;
		m_Bh.plugins = NULL;
	}
}

CAmxxReader::Error CAmxxReader::GetStatus()
{
	return m_Status;
}

#undef DATAREAD
#define DATAREAD(addr, itemsize, itemcount) \
	if (fread(addr, itemsize, itemcount, m_pFile) != itemcount) \
	{ \
		if (feof(m_pFile)) \
			m_Status = Err_FileInvalid; \
		else \
			m_Status = Err_FileRead; \
		fclose(m_pFile); \
		m_pFile = NULL; \
		return 0; \
	}

size_t CAmxxReader::GetBufferSize()
{
	if (!m_pFile)
		return 0;

	long save = ftell(m_pFile);

	if (m_OldFile)
	{
		rewind(m_pFile);
		AMX_HEADER hdr;
		DATAREAD(&hdr, sizeof(hdr), 1);
		fseek(m_pFile, save, SEEK_SET);
		
		return hdr.stp;
	}
	else if (m_AmxxFile)
	{
		PluginEntry *pe = &(m_Bh.plugins[m_Entry]);
		
		if (pe->imagesize > pe->memsize)
			return pe->imagesize + 1;
		
		return pe->memsize + 1;
	}

	fseek(m_pFile, m_SectionHdrOffset, SEEK_SET);

	TableEntry entry;
	DATAREAD(&entry, sizeof(entry), 1);
	fseek(m_pFile, save, SEEK_SET);
	
	return entry.origSize + 1;			// +1 : safe
}

#undef DATAREAD
#define DATAREAD(addr, itemsize, itemcount) \
	if (fread(addr, itemsize, itemcount, m_pFile) != static_cast<size_t>(itemcount)) \
	{ \
		if (feof(m_pFile)) \
			m_Status = Err_FileInvalid; \
		else \
			m_Status = Err_FileRead; \
		fclose(m_pFile); \
		m_pFile = NULL; \
		return m_Status; \
	}
#define DATAREAD_RELEASE(addr, itemsize, itemcount) \
	if (fread(addr, itemsize, itemcount, m_pFile) != static_cast<size_t>(itemcount)) \
	{ \
		if (feof(m_pFile)) \
			m_Status = Err_FileInvalid; \
		else \
			m_Status = Err_FileRead; \
		fclose(m_pFile); \
		m_pFile = NULL; \
		delete[] tempBuffer;\
		return m_Status; \
	}
CAmxxReader::Error CAmxxReader::GetSection(void *buffer)
{
	if (!m_pFile)
		return m_Status;

	if (m_OldFile)
	{
		// get file size
		fseek(m_pFile, 0, SEEK_END);
		long filesize = ftell(m_pFile);
		rewind(m_pFile);
		DATAREAD(buffer, 1, filesize);
		m_Status = Err_None;
		
		return m_Status;
	}
	else if (m_AmxxFile)
	{
		PluginEntry *pe = &(m_Bh.plugins[m_Entry]);
		char *tempBuffer = new char[m_SectionLength + 1];
		fseek(m_pFile, pe->offs, SEEK_SET);
		DATAREAD_RELEASE((void *)tempBuffer, 1, m_SectionLength);
		uLongf destLen = GetBufferSize();
		int result = uncompress((Bytef *)buffer, &destLen, (Bytef *)tempBuffer, m_SectionLength);
		delete [] tempBuffer;
		
		if (result != Z_OK)
		{
			AMXXLOG_Log("[AMXX] Zlib error encountered: %d(%d)", result, m_SectionLength);
			m_Status = Err_Decompress;
			return Err_Decompress;
		}
		
		return Err_None;
	} else {
		// new file type: go to the section table entry
		fseek(m_pFile, m_SectionHdrOffset, SEEK_SET);
		// go to the offset
		TableEntry entry;
		DATAREAD(&entry, sizeof(entry), 1);
		fseek(m_pFile, entry.offset, SEEK_SET);
		uLongf destLen = GetBufferSize();
		// read the data to a temporary buffer
		char *tempBuffer = new char[m_SectionLength + 1];
		//fread(tempBuffer, sizeof(char), m_SectionLength, m_pFile);
		DATAREAD_RELEASE((void*)tempBuffer, 1, m_SectionLength);
		// decompress
		int result = uncompress((Bytef *)buffer, &destLen, (Bytef *)tempBuffer, m_SectionLength);
		delete [] tempBuffer;
		
		if (result != Z_OK)
		{
			AMXXLOG_Log("[AMXX] Zlib error encountered: %d(%d)", result, m_SectionLength);
			m_Status = Err_Decompress;
			
			return Err_Decompress;
		}
		
		return Err_None;
	}
}
