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
/*****************
 ****** RLE ******
 *****************/

/*************************************************************************
* Name:        rle.c
* Author:      Marcus Geelnard
* Description: RLE coder/decoder implementation.
* Reentrant:   Yes
* $Id$
*
* RLE (Run Length Encoding) is the simplest possible lossless compression
* method. Nevertheless it serves a purpose, even in state of the art
* compression (it is used in JPEG compression, for instance). The basic
* principle is to identify sequences of equal bytes, and replace them with
* the byte in question and a repetition count (coded in some clever
* fashion).
*
* There are several different ways to do RLE. The particular method
* implemented here is a very efficient one. Instead of coding runs for
* both repeating and non-repeating sections, a special marker byte is
* used to indicate the start of a repeating section. Non-repeating
* sections can thus have any length without being interrupted by control
* bytes, except for the rare case when the special marker byte appears in
* the non-repeating section (which is coded with at most two bytes). For
* optimal efficiency, the marker byte is chosen as the least frequent
* (perhaps even non-existent) symbol in the input stream.
*
* Repeating runs can be as long as 32768 bytes. Runs shorter than 129
* bytes require three bytes for coding (marker + count + symbol), whereas
* runs longer than 128 bytes require four bytes for coding (marker +
* counthi|0x80 + countlo + symbol). This is normally a win in compression,
* and it's very seldom a loss of compression ratio compared to using a
* fixed coding of three bytes (which allows coding a run of 256 bytes in
* just three bytes).
*
* With this scheme, the worst case compression result is
* (257/256)*insize + 1.
*
*-------------------------------------------------------------------------
* Note: This code is based on the code found in "codrle2.c" and
* "dcodrle2.c" by David Bourgin, as described in "Introduction to the
* losslessy compression schemes", 1994. The main differences from Davids
* implementation are the addition of long (15-bit) run counts, the removal
* of file I/O (this implementation works solely with preallocated memory
* buffers), and that the code is now 100% reentrant.
*-------------------------------------------------------------------------
* Copyright (c) 2003-2004 Marcus Geelnard
*
* This software is provided 'as-is', without any express or implied
* warranty. In no event will the authors be held liable for any damages
* arising from the use of this software.
*
* Permission is granted to anyone to use this software for any purpose,
* including commercial applications, and to alter it and redistribute it
* freely, subject to the following restrictions:
*
* 1. The origin of this software must not be misrepresented; you must not
*    claim that you wrote the original software. If you use this software
*    in a product, an acknowledgment in the product documentation would
*    be appreciated but is not required.
*
* 2. Altered source versions must be plainly marked as such, and must not
*    be misrepresented as being the original software.
*
* 3. This notice may not be removed or altered from any source
*    distribution.
*
* Marcus Geelnard
* marcus.geelnard at home.se
*************************************************************************/

#include "amxxfile.h"

/*************************************************************************
*                           INTERNAL FUNCTIONS                           *
*************************************************************************/


/*************************************************************************
* _RLE_WriteRep() - Encode a repetition of 'symbol' repeated 'count'
* times.
*************************************************************************/

void CAmxxReader::_RLE_WriteRep( unsigned char *out, unsigned int *outpos,
    unsigned char marker, unsigned char symbol, unsigned int count )
{
    unsigned int i, idx;

    idx = *outpos;
    if( count < 4 )
    {
        if( symbol == marker )
        {
            out[ idx ++ ] = marker;
            out[ idx ++ ] = count-1;
            if( count == 3 )
            {
                out[ idx ++ ] = marker;
            }
        }
        else
        {
            for( i = 0; i < count; ++ i )
            {
                out[ idx ++ ] = symbol;
            }
        }
    }
    else
    {
        out[ idx ++ ] = marker;
        -- count;
        if( count >= 128 )
        {
            out[ idx ++ ] = (count >> 8) | 0x80;
        }
        out[ idx ++ ] = count & 0xff;
        out[ idx ++ ] = symbol;
    }
    *outpos = idx;
}


/*************************************************************************
* _RLE_WriteNonRep() - Encode a non-repeating symbol, 'symbol'. 'marker'
* is the marker symbol, and special care has to be taken for the case
* when 'symbol' == 'marker'.
*************************************************************************/

void CAmxxReader::_RLE_WriteNonRep( unsigned char *out, unsigned int *outpos,
    unsigned char marker, unsigned char symbol )
{
    unsigned int idx;

    idx = *outpos;
    if( symbol == marker )
    {
        out[ idx ++ ] = marker;
        out[ idx ++ ] = 0;
    }
    else
    {
        out[ idx ++ ] = symbol;
    }
    *outpos = idx;
}



/*************************************************************************
*                            PUBLIC FUNCTIONS                            *
*************************************************************************/


/*************************************************************************
* RLE_Compress() - Compress a block of data using an RLE coder.
*  in     - Input (uncompressed) buffer.
*  out    - Output (compressed) buffer. This buffer must be 0.4% larger
*           than the input buffer, plus one byte.
*  insize - Number of input bytes.
* The function returns the size of the compressed data.
*************************************************************************/

int CAmxxReader::RLE_Compress( unsigned char *in, unsigned char *out,
    unsigned int insize )
{
    unsigned char byte1, byte2, marker;
    unsigned int  inpos, outpos, count, i, histogram[ 256 ];

    /* Do we have anything to compress? */
    if( insize < 1 )
    {
        return 0;
    }

    /* Create histogram */
    for( i = 0; i < 256; ++ i )
    {
        histogram[ i ] = 0;
    }
    for( i = 0; i < insize; ++ i )
    {
        ++ histogram[ in[ i ] ];
    }

    /* Find the least common byte, and use it as the repetition marker */
    marker = 0;
    for( i = 1; i < 256; ++ i )
    {
        if( histogram[ i ] < histogram[ marker ] )
        {
            marker = i;
        }
    }

    /* Remember the repetition marker for the decoder */
    out[ 0 ] = marker;
    outpos = 1;

    /* Start of compression */
    byte1 = in[ 0 ];
    inpos = 1;
    count = 1;

    /* Are there at least two bytes? */
    if( insize >= 2 )
    {
        byte2 = in[ inpos ++ ];
        count = 2;

        /* Main compression loop */
        do
        {
            if( byte1 == byte2 )
            {
                /* Do we meet only a sequence of identical bytes? */
                while( (inpos < insize) && (byte1 == byte2) &&
                       (count < 32768) )
                {
                    byte2 = in[ inpos ++ ];
                    ++ count;
                }
                if( byte1 == byte2 )
                {
                    _RLE_WriteRep( out, &outpos, marker, byte1, count );
                    if( inpos < insize )
                    {
                        byte1 = in[ inpos ++ ];
                        count = 1;
                    }
                    else
                    {
                        count = 0;
                    }
                }
                else
                {
                    _RLE_WriteRep( out, &outpos, marker, byte1, count-1 );
                    byte1 = byte2;
                    count = 1;
                }
            }
            else
            {
                /* No, then don't handle the last byte */
                _RLE_WriteNonRep( out, &outpos, marker, byte1 );
                byte1 = byte2;
                count = 1;
            }
            if( inpos < insize )
            {
                byte2 = in[ inpos ++ ];
                count = 2;
            }
        }
        while( (inpos < insize) || (count >= 2) );
    }

    /* One byte left? */
    if( count == 1 )
    {
        _RLE_WriteNonRep( out, &outpos, marker, byte1 );
    }

    return outpos;
}


/*************************************************************************
* RLE_Uncompress() - Uncompress a block of data using an RLE decoder.
*  in      - Input (compressed) buffer.
*  out     - Output (uncompressed) buffer. This buffer must be large
*            enough to hold the uncompressed data.
*  insize  - Number of input bytes.
*************************************************************************/

void CAmxxReader::RLE_Uncompress( unsigned char *in, unsigned char *out,
    unsigned int insize )
{
    unsigned char marker, symbol;
    unsigned int  i, inpos, outpos, count;

    /* Do we have anything to compress? */
    if( insize < 1 )
    {
        return;
    }

    /* Get marker symbol from input stream */
    inpos = 0;
    marker = in[ inpos ++ ];

    /* Main decompression loop */
    outpos = 0;
    do
    {
        symbol = in[ inpos ++ ];
        if( symbol == marker )
        {
            /* We had a marker byte */
            count = in[ inpos ++ ];
            if( count < 3 )
            {
                for( i = 0; i <= count; ++ i )
                {
                    out[ outpos ++ ] = marker;
                }
            }
            else
            {
                if( count & 0x80 )
                {
                    count = ((count & 0x7f) << 8) + in[ inpos ++ ];
                }
                symbol = in[ inpos ++ ];
                for( i = 0; i <= count; ++ i )
                {
                    out[ outpos ++ ] = symbol;
                }
            }
        }
        else
        {
            /* No marker, plain copy */
            out[ outpos ++ ] = symbol;
        }
    }
    while( inpos < insize );
} 

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
	if (magic != 0x524C4542)
	{
		// check for old file
		AMX_HEADER hdr;
		rewind(m_pFile);
		fread(&hdr, sizeof(hdr), 1, m_pFile);
		amx_Align16(&hdr.magic);
		if (hdr.magic == AMX_MAGIC)
		{
			if (cellsize != 32)
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
	}

	// try to find the section
	mint8_t numOfPlugins;
	DATAREAD(&numOfPlugins, sizeof(numOfPlugins), 1);

	TableEntry entry;

	m_SectionHdrOffset = 0;
	for (int i = 0; i < static_cast<int>(numOfPlugins); ++i)
	{
		DATAREAD(&entry, sizeof(entry), 1);
		if (entry.cellSize == m_CellSize)
		{
			m_SectionHdrOffset = entry.offset;
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
	if (i < static_cast<int>(numOfPlugins))
	{
		// there is a next section
		TableEntry nextEntry;
		DATAREAD(&nextEntry, sizeof(nextEntry), 1);
		m_SectionLength = nextEntry.offset - entry.offset;
	}
	else
	{
		fseek(m_pFile, 0, SEEK_END);
		m_SectionLength = ftell(m_pFile) - entry.offset;
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

	if (m_OldFile)
	{
		rewind(m_pFile);
		AMX_HEADER hdr;
		DATAREAD(&hdr, sizeof(hdr), 1);
		return hdr.stp;
	}

	fseek(m_pFile, m_SectionHdrOffset, SEEK_SET);

	TableEntry entry;
	DATAREAD(&entry, sizeof(entry), 1);
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

	// read the data to a temporary buffer
	char *tempBuffer = new char[m_SectionLength + 1];
	DATAREAD(static_cast<void*>(tempBuffer), 1, m_SectionLength);
	// decompress
	RLE_Uncompress((unsigned char*)tempBuffer, (unsigned char*)(buffer), m_SectionLength);
	return Err_None;
}