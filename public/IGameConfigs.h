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

#ifndef _INCLUDE_IGAMECONFIG_H_
#define _INCLUDE_IGAMECONFIG_H_

#include <ITextParsers.h>

enum class FieldType
{
	FIELD_NONE,
	FIELD_FLOAT,        // Floating point value
	FIELD_STRINGINT,    // String ID (return from ALLOC_STRING)
	FIELD_STRINGPTR,    // String, pointer-to-char
	FIELD_STRING,       // String, fixed size
	FIELD_CLASSPTR,     // Classes pointer derived of CBaseEntity
	FIELD_CLASS,        // Arbitrary classes, direct
	FIELD_STRUCTURE,    // Arbitrary structures, direct
	FIELD_EHANDLE,      // Entity handle
	FIELD_ENTVARS,      // entvars_t*
	FIELD_EDICT,        // edict_t*
	FIELD_VECTOR,       // Vector
	FIELD_POINTER,      // Arbitrary data pointer
	FIELD_INTEGER,      // Integer or enum
	FIELD_FUNCTION,     // Class function pointer (Think, Use, etc)
	FIELD_BOOLEAN,      // Boolean
	FIELD_SHORT,        // 2 bytes integer
	FIELD_CHARACTER,    // 1 byte
};

struct TypeDescription
{
	TypeDescription()
	{
		reset();
	}

	void reset()
	{
		fieldType     = FieldType::FIELD_NONE;
		fieldOffset   = 0;
		fieldSize     = 0;
		fieldUnsigned = false;
	}

	FieldType  fieldType;
	int        fieldOffset;
	int        fieldSize;
	bool       fieldUnsigned;
};

/**
 * @brief Describes a game private data config file
 */
class IGameConfig
{
public:
	/**
	 * @brief Returns an offset value.
	 *
	 * @param key			Key to retrieve from the offset section.
	 * @param value			Pointer to store the TypeDescription reference in.
	 * @return				True if found, false otherwise.
	 */
	virtual bool GetOffset(const char *key, TypeDescription *value) = 0;

	/**
	 * @brief Returns an offset value from given class.
	 *
	 * @param classname		class name to match from the offset section.
	 * @param key			Key to retrieve from the offset section.
	 * @param value			Pointer to store the TypeDescription reference in.
	 * @return				True if found, false otherwise.
	 */
	virtual bool GetOffsetByClass(const char *classname, const char *key, TypeDescription *value) = 0;
 
	/**
	 * @brief Returns the value of a key from the "Keys" section.
	 *
	 * @param key			Key to retrieve from the Keys section.
	 * @return				String containing the value, or NULL if not found.
	 */
	virtual const char *GetKeyValue(const char *key) = 0;

	/**
	 * @brief Retrieves a cached memory signature.
	 *
	 * @param key			Name of the signature.
	 * @param addr			Pointer to store the memory address in.
	 *						(NULL is copied if signature is not found in binary).
	 * @return				True if the section exists and key for current
	 *						platform was found, false otherwise.
	 */
	virtual bool GetMemSig(const char *key, void **addr) = 0;

	/**
	 * @brief Retrieves the value of an address from the "Address" section.
	 *
	 * @param key			Key to retrieve from the Address section.
	 * @param addr          Pointer to store the memory address.
	 * @return				True on success, false on failure.
	 */
	virtual bool GetAddress(const char *key, void **addr) = 0;
};

/**
 * @brief Manages game config files
 */
class IGameConfigManager
{
public:
	/**
	 * @brief Loads or finds an already loaded game config file.  
	 *
	 * @param file		File to load.  The path must be relative to the 
	 *					'gamedata' folder and the extension should be 
	 *					omitted.
	 * @param pConfig	Pointer to store the game config pointer.  Pointer 
	 *					will be valid even on failure.
	 * @param error		Optional error message buffer.
	 * @param maxlength	Maximum length of the error buffer.
	 * @return			True on success, false if the file failed.
	 */
	virtual bool LoadGameConfigFile(const char *file, IGameConfig **pConfig, char *error, size_t maxlength) = 0;

	/**
	 * @brief Closes an IGameConfig pointer.  Since a file can be loaded 
	 * more than once, the file will not actually be removed from memory 
	 * until it is closed once for each call to LoadGameConfigfile().
	 *
	 * @param cfg		Pointer to the IGameConfig to close.
	 */
	virtual void CloseGameConfigFile(IGameConfig *cfg) = 0;

	/**
	 * @brief Adds a custom gamedata section hook.
	 *
	 * @param sectionname	Section name to hook.
	 * @param listener		Listener callback.
	 * @noreturn
	 */
	virtual void AddUserConfigHook(const char *sectionname, ITextListener_SMC *listener) = 0;

	/**
	 * @brief Removes a custom gamedata section hook.
	 *
	 * @param sectionname	Section name to unhook.
	 * @param listener		Listener callback.
	 * @noreturn
	 */
	virtual void RemoveUserConfigHook(const char *sectionname, ITextListener_SMC *listener) = 0;
};

#endif //_INCLUDE_IGAMECONFIG_H_
