/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"
#include "amxxfile.h"
#include "zlib/zlib.h"

/**********************
 ****** AMXXFILE ******
 **********************/
#if defined __GNUC__
  #define PACKED        __attribute__((packed))
#else
  #define PACKED
#endif

#if defined __linux__
  #pragma pack(1)         /* structures must be packed (byte-aligned) */
#else
  #pragma pack(1)         /* structures must be packed (byte-aligned) */
  #if defined __TURBOC__
    #pragma option -a-    /* "pack" pragma for older Borland compilers */
  #endif
#endif

typedef char	mint8_t;
typedef int16_t		mint16_t;
typedef int32_t		mint32_t;

struct TableEntry
{
	mint8_t cellSize PACKED;
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
	if (magic != 0x414D5842)
	{
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
		}
		else
		{
			// no known file format
			m_Status = Err_FileInvalid;
			fclose(m_pFile);
			m_pFile = NULL;
			return;
		}
	} else if ( magic == 0x524C4542 ) {
		//we have an invalid, old, RLEB file
		m_Status = Err_OldFile;
		fclose(m_pFile);
		m_pFile = NULL;
		return;
	} else {

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
		if ((i+1) < static_cast<int>(numOfPlugins))
		{
			// there is a next section
			TableEntry nextEntry;
			DATAREAD(&nextEntry, sizeof(nextEntry), 1);
			m_SectionLength = nextEntry.offset - entry.offset;
		}
		else
		{
			fseek(m_pFile, 0, SEEK_END);
			m_SectionLength = ftell(m_pFile) - (long)entry.offset;
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

	fseek(m_pFile, m_SectionHdrOffset, SEEK_SET);

	TableEntry entry;
	DATAREAD(&entry, sizeof(entry), 1);
	fseek(m_pFile, save, SEEK_SET);
	return entry.origSize + 1;			// +1 : safe
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

	// new file type: go to the section table entry
	fseek(m_pFile, m_SectionHdrOffset, SEEK_SET);
	// go to the offset
	TableEntry entry;
	DATAREAD(&entry, sizeof(entry), 1);
	fseek(m_pFile, entry.offset, SEEK_SET);
//	AMXXLOG_Log("|||| Offset needed: %d At: %d", entry.offset, ftell(m_pFile));
	uLongf destLen = GetBufferSize();
	// read the data to a temporary buffer
	char *tempBuffer = new char[m_SectionLength + 1];
	//fread(tempBuffer, sizeof(char), m_SectionLength, m_pFile);
	DATAREAD((void*)tempBuffer, 1, m_SectionLength);
	// decompress
//	AMXXLOG_Log("|||| First Bytes: %d %d %d %d", tempBuffer[0], tempBuffer[1], tempBuffer[2], tempBuffer[3]);
	int result = uncompress((Bytef *)buffer, &destLen,
			(Bytef *)tempBuffer, m_SectionLength);
	delete [] tempBuffer;
//	AMXXLOG_Log("|||| Result: %d, m_SectionLength=%d, destLen=%d", result, m_SectionLength, destLen);
	if (result != Z_OK)
	{
		m_Status = Err_Decompress;
		return Err_Decompress;
	}
	return Err_None;
}
