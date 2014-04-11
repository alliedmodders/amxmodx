/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#ifndef _INCLUDE_SOURCEMOD_CDATAPACK_H_
#define _INCLUDE_SOURCEMOD_CDATAPACK_H_

#include "amxmodx.h"

/**
 * @brief Contains functions for packing data abstractly to/from plugins.
 */
class CDataPack
{
public:
	CDataPack();
	~CDataPack();

public:
	/**
	 * @brief Resets the position in the data stream to the beginning.
	 */
	void Reset() const;

	/**
	 * @brief Retrieves the current stream position.
	 *
	 * @return			Index into the stream.
	 */
	size_t GetPosition() const;

	/**
	 * @brief Sets the current stream position.
	 *
	 * @param pos		Index to set the stream at.
	 * @return			True if succeeded, false if out of bounds.
	 */
	bool SetPosition(size_t pos) const;

	/**
	 * @brief Reads one cell from the data stream.
	 *
	 * @return			A cell read from the current position.
	 */
	cell ReadCell() const;

	/**
	 * @brief Reads one float from the data stream.
	 *
	 * @return			A float read from the current position.
	 */
	float ReadFloat() const;

	/**
	 * @brief Returns whether or not a specified number of bytes from the current stream
	 *  position to the end can be read.
	 * 
	 * @param bytes		Number of bytes to simulate reading.
	 * @return			True if can be read, false otherwise.
	 */
	bool IsReadable(size_t bytes) const;

	/**
	 * @brief Reads a string from the data stream.
	 *
	 * @param len		Optional pointer to store the string length.
	 * @return			Pointer to the string, or NULL if out of bounds.
	 */
	const char *ReadString(size_t *len) const;

	/**
	 * @brief Reads the current position as a generic address.
	 *
	 * @return			Pointer to the memory.
	 */
	void *GetMemory() const;

	/**
	 * @brief Reads the current position as a generic data type.
	 *
	 * @param size		Optional pointer to store the size of the data type.
	 * @return			Pointer to the data, or NULL if out of bounds.
	 */
	void *ReadMemory(size_t *size) const;

public:
	/**
	 * @brief Resets the used size of the stream back to zero.
	 */
	void ResetSize();

	/**
	 * @brief Packs one cell into the data stream.
	 *
	 * @param cell		Cell value to write.
	 */
	void PackCell(cell cells);

	/**
	 * @brief Packs one float into the data stream.
	 *
	 * @param val		Float value to write.
	 */
	void PackFloat(float val);

	/**
	 * @brief Packs one string into the data stream.
	 * The length is recorded as well for buffer overrun protection.
	 *
	 * @param string	String to write.
	 */
	void PackString(const char *string);

	/**
	 * @brief Creates a generic block of memory in the stream.
	 *
	 * Note that the pointer it returns can be invalidated on further
	 * writing, since the stream size may grow.  You may need to double back
	 * and fetch the pointer again.
	 *
	 * @param size		Size of the memory to create in the stream.
	 * @param addr		Optional pointer to store the relocated memory address.
	 * @return			Current position of the stream beforehand.
	 */
	size_t CreateMemory(size_t size, void **addr);

public:
	void Initialize();

private:
	void CheckSize(size_t sizetype);

private:
	char *m_pBase;
	mutable char *m_curptr;
	size_t m_capacity;
	size_t m_size;
};

class CDataPackHandles
{
private:
	CVector<CDataPack *> m_packs;

public:
	CDataPackHandles() {}
	~CDataPackHandles()
	{
		this->clear();
	}

	void clear()
	{
		for (size_t i = 0; i < m_packs.size(); i++)
		{
			if (m_packs[i] != NULL)
			{
				delete m_packs[i];
			}
		}

		m_packs.clear();
	}

	CDataPack *lookup(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_packs.size()))
		{
			return NULL;
		}

		return m_packs[handle];
	}

	int create()
	{
		for (size_t i = 0; i < m_packs.size(); ++i)
		{
			if (m_packs[i] == NULL)
			{
				// reuse handle
				m_packs[i] = new CDataPack;

				return static_cast<int>(i) + 1;
			}
		}
		m_packs.push_back(new CDataPack);
		return m_packs.size();
	}

	bool destroy(int handle)
	{
		handle--;

		if (handle < 0 || handle >= static_cast<int>(m_packs.size()))
		{
			return false;
		}

		if (m_packs[handle] == NULL)
		{
			return false;
		}

		delete m_packs[handle];
		m_packs[handle] = NULL;

		return true;
	}
};

extern CDataPackHandles g_DataPackHandles;
extern AMX_NATIVE_INFO g_DatapackNatives[];

#endif //_INCLUDE_SOURCEMOD_CDATAPACK_H_
