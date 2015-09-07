// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "CGameConfigs.h"
#include <amxmodx.h>
#include <MemoryUtils.h>

CGameConfigManager ConfigManager;
static CGameMasterReader MasterReader;
IGameConfig *CommonConfig;

//
// GAME CONFIG
//

enum
{
	PSTATE_NONE,
	PSTATE_GAMES,
	PSTATE_GAMEDEFS,
	PSTATE_GAMEDEFS_CLASSES,
	PSTATE_GAMEDEFS_CLASSES_CLASS,
	PSTATE_GAMEDEFS_OFFSETS,
	PSTATE_GAMEDEFS_OFFSETS_OFFSET,
	PSTATE_GAMEDEFS_KEYS,
	PSTATE_GAMEDEFS_SUPPORTED,
	PSTATE_GAMEDEFS_SIGNATURES,
	PSTATE_GAMEDEFS_SIGNATURES_SIG,
	PSTATE_GAMEDEFS_CUSTOM,
	PSTATE_GAMEDEFS_ADDRESSES,
	PSTATE_GAMEDEFS_ADDRESSES_ADDRESS,
	PSTATE_GAMEDEFS_ADDRESSES_ADDRESS_READ,
};

struct TempSigInfo
{
	void Reset()
	{
		library[0] = '\0';
		signature[0] = '\0';
	}

	char signature[1024];
	char library[64];

} TempSig;

TypeDescription TempType;

static char ParseEngine[32];

static bool DoesGameMatch(const char *value)
{
	return g_mod_name.compare(value) == 0;
}

static bool DoesEngineMatch(const char* value)
{
	return strcmp(ParseEngine, value) == 0;
}

CGameConfig::CGameConfig(const char *path) : m_FoundOffset(false), m_CustomLevel(0), m_CustomHandler(nullptr)
{
	strncopy(m_File, path, sizeof(m_File));
	strncopy(ParseEngine, IS_DEDICATED_SERVER() ? "engine_ds" : "engine_ls", sizeof(ParseEngine));
}

CGameConfig::~CGameConfig()
{
	ConfigManager.RemoveCachedConfig(this);
}

SMCResult CGameConfig::ReadSMC_NewSection(const SMCStates *states, const char *name)
{
	if (m_IgnoreLevel)
	{
		m_IgnoreLevel++;
		return SMCResult_Continue;
	}

	switch (m_ParseState)
	{
		case PSTATE_NONE:
		{
			if (strcmp(name, "Games") == 0)
			{
				m_ParseState = PSTATE_GAMES;
			}
			else
			{
				m_IgnoreLevel++;
			}
			break;
		}
		case PSTATE_GAMES:
		{
			if (strcmp(name, "*") == 0 || strcmp(name, "#default") == 0 || DoesGameMatch(name))
			{
				m_ShouldBeReadingDefault = true;
				strncopy(m_Game, name, sizeof(m_Game));

				m_Class[0] = '\0';
				m_MatchedClasses = false;
				m_ParseState = PSTATE_GAMEDEFS;
			}
			else
			{
				m_IgnoreLevel++;
			}
			break;
		}
		case PSTATE_GAMEDEFS:
		case PSTATE_GAMEDEFS_CLASSES_CLASS:
		{
			if (strcmp(name, "Classes") == 0)
			{
				if (!m_Class[0])
				{
					m_ParseState = PSTATE_GAMEDEFS_CLASSES;
					m_MatchedClasses = true;
				}
				else
				{
					++m_IgnoreLevel;
				}
				break;
			}
			else if (strcmp(name, "Offsets") == 0)
			{
				m_ParseState = PSTATE_GAMEDEFS_OFFSETS;
			}
			else if (strcmp(name, "Keys") == 0)
			{
				m_ParseState = PSTATE_GAMEDEFS_KEYS;
			}
			else if (strcmp(name, "#supported") == 0 && strcmp(m_Game, "#default") == 0)
			{
				m_ParseState = PSTATE_GAMEDEFS_SUPPORTED;

				m_ShouldBeReadingDefault = false;
				m_HadGame                = false;
				m_MatchedGame            = false;
				m_HadEngine              = false;
				m_MatchedEngine          = false;
			}
			else if (strcmp(name, "Signatures") == 0)
			{
				m_ParseState = PSTATE_GAMEDEFS_SIGNATURES;
			}
			else if (strcmp(name, "Addresses") == 0)
			{
				m_ParseState = PSTATE_GAMEDEFS_ADDRESSES;
			}
			else
			{
				if (ConfigManager.m_customHandlers.retrieve(name, &m_CustomHandler))
				{
					m_CustomLevel = 0;
					m_ParseState = PSTATE_GAMEDEFS_CUSTOM;
					m_CustomHandler->ReadSMC_ParseStart();
					break;
				}

				++m_IgnoreLevel;
			}
			break;
		}
		case PSTATE_GAMEDEFS_CLASSES:
		{
			strncopy(m_Class, name, sizeof(m_Class));
			m_ParseState = PSTATE_GAMEDEFS_CLASSES_CLASS;
			break;
		}
		case PSTATE_GAMEDEFS_OFFSETS:
		{
			strncopy(m_Offset, name, sizeof(m_Offset));
			TempType.reset();

			m_ParseState = PSTATE_GAMEDEFS_OFFSETS_OFFSET;
			m_MatchedPlatform = false;
			m_FoundOffset = false;
			break;
		}
		case PSTATE_GAMEDEFS_SIGNATURES:
		{
			strncopy(m_Offset, name, sizeof(m_Offset));
			TempSig.Reset();

			m_ParseState = PSTATE_GAMEDEFS_SIGNATURES_SIG;
			m_MatchedPlatform = false;
			break;
		}
		case PSTATE_GAMEDEFS_CUSTOM:
		{
			++m_CustomLevel;
			return m_CustomHandler->ReadSMC_NewSection(states, name);
			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES:
		{
			m_Address[0]          = '\0';
			m_AddressSignature[0] = '\0';
			m_AddressReadCount    = 0;

			strncopy(m_Address, name, sizeof(m_Address));

			m_ParseState = PSTATE_GAMEDEFS_ADDRESSES_ADDRESS;
			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES_ADDRESS:
		{
			if (g_LibSys.DoesPlatformMatch(name))
			{
				m_ParseState = PSTATE_GAMEDEFS_ADDRESSES_ADDRESS_READ;
			}
			else
			{
				if (strcmp(name, PLATFORM_LINUX_NAME) != 0 && strcmp(name, PLATFORM_WINDOWNS_NAME) != 0 && strcmp(name, PLATFORM_MAC_NAME) != 0)
				{
					AMXXLOG_Error("Error while parsing Address section for \"%s\" (%s):", m_Address, m_CurrentPath);
					AMXXLOG_Error("Unrecognized platform \"%s\"", name);
				}

				m_IgnoreLevel = 1;
			}
			break;
		}
		default:
		{
			m_IgnoreLevel++;
			break;
		}
	}

	return SMCResult_Continue;
}

SMCResult CGameConfig::ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value)
{
	if (m_IgnoreLevel)
	{
		return SMCResult_Continue;
	}

	switch (m_ParseState)
	{
		case PSTATE_GAMEDEFS_OFFSETS_OFFSET:
		{
			if (!strcmp(key, "type"))
			{
				auto type = FieldType::FIELD_NONE;

				if (!strcmp(value, "stringint"))
				{
					type = FieldType::FIELD_STRINGINT;
				}
				else if (!strcmp(value, "stringptr"))
				{
					type = FieldType::FIELD_STRINGPTR;
				}
				else if (!strcmp(value, "string"))
				{
					type = FieldType::FIELD_STRING;
				}
				else if (!strcmp(value, "classptr"))
				{
					type = FieldType::FIELD_CLASSPTR;
				}
				else if (!strcmp(value, "class"))
				{
					type = FieldType::FIELD_CLASS;
				}
				else if (!strcmp(value, "ehandle"))
				{
					type = FieldType::FIELD_EHANDLE;
				}
				else if (!strcmp(value, "edict"))
				{
					type = FieldType::FIELD_EDICT;
				}
				else if (!strcmp(value, "entvars"))
				{
					type = FieldType::FIELD_ENTVARS;
				}
				else if (!strcmp(value, "vector"))
				{
					type = FieldType::FIELD_VECTOR;
				}
				else if (!strcmp(value, "pointer"))
				{
					type = FieldType::FIELD_POINTER;
				}
				else if (!strcmp(value, "integer"))
				{
					type = FieldType::FIELD_INTEGER;
				}
				else if (!strcmp(value, "function"))
				{
					type = FieldType::FIELD_FUNCTION;
				}
				else if (!strcmp(value, "boolean"))
				{
					type = FieldType::FIELD_BOOLEAN;
				}
				else if (!strcmp(value, "short"))
				{
					type = FieldType::FIELD_SHORT;
				}
				else if (!strcmp(value, "character"))
				{
					type = FieldType::FIELD_CHARACTER;
				}
				else if (!strcmp(value, "float"))
				{
					type = FieldType::FIELD_FLOAT;
				}

				TempType.fieldType = type;
			}
			else if (!strcmp(key, "size"))
			{
				TempType.fieldSize = ke::Max<int>(0, atoi(value));
			}
			else if (!strcmp(key, "unsigned"))
			{
				TempType.fieldUnsigned = !!atoi(value);
			}
			else if (g_LibSys.IsPlatformCompatible(key, &m_MatchedPlatform))
			{
				m_FoundOffset = true;
				TempType.fieldOffset = atoi(value);
			}
			break;
		}
		case PSTATE_GAMEDEFS_KEYS:
		{
			ke::AString vstr(value);
			m_Keys.replace(key, ke::Move(vstr));
			break;
		}
		case PSTATE_GAMEDEFS_SUPPORTED:
		{
			if (strcmp(key, "game") == 0)
			{
				m_HadGame = true;

				if (DoesGameMatch(value))
				{
					m_MatchedGame = true;
				}

				if ((!m_HadEngine && m_MatchedGame) || (m_MatchedEngine && m_MatchedGame))
				{
					m_ShouldBeReadingDefault = true;
				}
			}
			else if (strcmp(key, "engine") == 0)
			{
				m_HadEngine = true;

				if (DoesEngineMatch(value))
				{
					m_MatchedEngine = true;
				}

				if ((!m_HadGame && m_MatchedEngine) || (m_MatchedGame && m_MatchedEngine))
				{
					m_ShouldBeReadingDefault = true;
				}
			}
			break;
		}
		case PSTATE_GAMEDEFS_SIGNATURES_SIG:
		{
			if (g_LibSys.IsPlatformCompatible(key, &m_MatchedPlatform))
			{
				strncopy(TempSig.signature, value, sizeof(TempSig.signature));
			}
			else if (strcmp(key, "library") == 0)
			{
				strncopy(TempSig.library, value, sizeof(TempSig.library));
			}
			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES_ADDRESS:
		case PSTATE_GAMEDEFS_ADDRESSES_ADDRESS_READ:
		{
			if (strcmp(key, "read") == 0)
			{
				int limit = sizeof(m_AddressRead) / sizeof(m_AddressRead[0]);

				if (m_AddressReadCount < limit)
				{
					m_AddressRead[m_AddressReadCount] = atoi(value);
					m_AddressReadCount++;
				}
				else
				{
					AMXXLOG_Error("[SM] Error parsing Address \"%s\", does not support more than %d read offsets (gameconf \"%s\")",
								  m_Address, limit, m_CurrentPath);
				}
			}
			else if (strcmp(key, "signature") == 0)
			{
				strncopy(m_AddressSignature, value, sizeof(m_AddressSignature));
			}
			break;
		}
		case PSTATE_GAMEDEFS_CUSTOM:
		{
			return m_CustomHandler->ReadSMC_KeyValue(states, key, value);
		}
	}

	return SMCResult_Continue;
}

SMCResult CGameConfig::ReadSMC_LeavingSection(const SMCStates *states)
{
	if (m_IgnoreLevel)
	{
		m_IgnoreLevel--;
		return SMCResult_Continue;
	}

	if (m_CustomLevel)
	{
		m_CustomLevel--;
		m_CustomHandler->ReadSMC_LeavingSection(states);
		return SMCResult_Continue;
	}

	switch (m_ParseState)
	{
		case PSTATE_GAMES:
		{
			m_ParseState = PSTATE_NONE;
			break;
		}
		case PSTATE_GAMEDEFS:
		{
			m_ParseState = PSTATE_GAMES;
			break;
		}
		case PSTATE_GAMEDEFS_CLASSES:
		{
			m_MatchedClasses = false;
			m_ParseState = PSTATE_GAMEDEFS;
			break;
		}
		case PSTATE_GAMEDEFS_CLASSES_CLASS:
		{
			m_ParseState = PSTATE_GAMEDEFS_CLASSES;
			m_Class[0] = '\0';
			break;
		}
		case PSTATE_GAMEDEFS_CUSTOM:
		{
			m_ParseState = PSTATE_GAMEDEFS;
			m_CustomHandler->ReadSMC_ParseEnd(false, false);
			break;
		}
		case PSTATE_GAMEDEFS_KEYS:
		{
			m_ParseState = m_MatchedClasses ? PSTATE_GAMEDEFS_CLASSES_CLASS : PSTATE_GAMEDEFS;
			break;
		}
		case PSTATE_GAMEDEFS_OFFSETS:
		{
			m_ParseState = m_MatchedClasses ? PSTATE_GAMEDEFS_CLASSES_CLASS : PSTATE_GAMEDEFS;
			break;
		}
		case PSTATE_GAMEDEFS_OFFSETS_OFFSET:
		{
			if (m_FoundOffset)
			{
				if (m_Class[0])
				{
					auto ic = m_OffsetsByClass.findForAdd(m_Class);

					if (ic.found())
					{
						ic->value->list.replace(m_Offset, TempType);
					}
					else if (m_OffsetsByClass.add(ic, m_Class))
					{
						ic->value = new OffsetClass;
						ic->value->list.insert(m_Offset, TempType);
					}
				}
				else
				{
					m_Offsets.replace(m_Offset, TempType);
				}
			}

			m_ParseState = PSTATE_GAMEDEFS_OFFSETS;
			break;
		}
		case PSTATE_GAMEDEFS_SUPPORTED:
		{
			if (!m_ShouldBeReadingDefault)
			{
				m_IgnoreLevel = 1;
				m_ParseState = PSTATE_GAMES;
			}
			else
			{
				m_ParseState = PSTATE_GAMEDEFS;
			}
			break;
		}
		case PSTATE_GAMEDEFS_SIGNATURES:
		{
			m_ParseState = m_MatchedClasses ? PSTATE_GAMEDEFS_CLASSES_CLASS : PSTATE_GAMEDEFS;
			break;
		}
		case PSTATE_GAMEDEFS_SIGNATURES_SIG:
		{
			if (TempSig.library[0] == '\0')
			{
				strncopy(TempSig.library, "server", sizeof(TempSig.library));
			}

			void *addressInBase = nullptr;

			if (strcmp(TempSig.library, "server") == 0)
			{
				addressInBase = reinterpret_cast<void*>(MDLL_Spawn);
			}
			else if (strcmp(TempSig.library, "engine") == 0)
			{
				addressInBase = reinterpret_cast<void*>(gpGlobals);
			}

			void *finalAddress = nullptr;

			if (!addressInBase)
			{
				AMXXLOG_Error("Unrecognized library \"%s\" (gameconf \"%s\")", TempSig.library, m_CurrentPath);
			}
			else if (TempSig.signature[0])
			{
				if (TempSig.signature[0] == '@')
				{
#if defined PLATFORM_WINDOWS
					MEMORY_BASIC_INFORMATION mem;

					if (VirtualQuery(addressInBase, &mem, sizeof(mem)))
					{
						finalAddress = g_MemUtils.ResolveSymbol(mem.AllocationBase, &TempSig.signature[1]);
					}
					else
					{
						AMXXLOG_Error("Unable to find library \"%s\" in memory (gameconf \"%s\")", TempSig.library, m_File);
					}

#elif defined PLATFORM_POSIX
					Dl_info info;

					if (dladdr(addressInBase, &info) != 0)
					{
						void *handle = dlopen(info.dli_fname, RTLD_NOW);

						if (handle)
						{
							finalAddress = g_MemUtils.ResolveSymbol(handle, &TempSig.signature[1]);
							dlclose(handle);
						}
						else
						{
							AMXXLOG_Error("Unable to load library \"%s\" (gameconf \"%s\")", TempSig.library, m_File);
						}
					}
					else
					{
						AMXXLOG_Error("Unable to find library \"%s\" in memory (gameconf \"%s\")", TempSig.library, m_File);
					}
#endif
				}

				if (!finalAddress)
				{
					finalAddress = g_MemUtils.DecodeAndFindPattern(addressInBase, TempSig.signature);
				}

				m_Sigs.replace(m_Offset, finalAddress);
			}

			m_ParseState = PSTATE_GAMEDEFS_SIGNATURES;
			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES:
		{
			m_ParseState = m_MatchedClasses ? PSTATE_GAMEDEFS_CLASSES_CLASS : PSTATE_GAMEDEFS;
			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES_ADDRESS:
		{
			m_ParseState = PSTATE_GAMEDEFS_ADDRESSES;

			if (m_Address[0] != '\0' && m_AddressSignature[0] != '\0')
			{
				AddressConf addrConf(m_AddressSignature, sizeof(m_AddressSignature), m_AddressReadCount, m_AddressRead);
				m_Addresses.replace(m_Address, addrConf);
			}

			break;
		}
		case PSTATE_GAMEDEFS_ADDRESSES_ADDRESS_READ:
		{
			m_ParseState = PSTATE_GAMEDEFS_ADDRESSES_ADDRESS;
			break;
		}
	}

	return SMCResult_Continue;
}

bool CGameConfig::Reparse(char *error, size_t maxlength)
{
	m_Offsets.clear();
	m_OffsetsByClass.clear();
	m_Keys.clear();
	m_Addresses.clear();

	char path[PLATFORM_MAX_PATH];
	const char *dataDir = get_localinfo("amxx_datadir", "addons/amxmodx/data");

	build_pathname_r(path, sizeof(path), "%s/gamedata/%s/master.games.txt", dataDir, m_File);

	if (!g_LibSys.PathExists(path))
	{
#if 0
		// Single config file without master
		g_LibSys.PathFormat(path, sizeof(path), "%s.txt", m_File);

		if (!EnterFile(path, error, maxlength))
		{
			return false;
		}
#endif
		// Allow customizations of default gamedata files
		build_pathname_r(path, sizeof(path), "%s/gamedata/custom/%s.txt", dataDir, m_File);

		if (g_LibSys.PathExists(path))
		{
			g_LibSys.PathFormat(path, sizeof(path), "custom/%s.txt", m_File);
			return EnterFile(path, error, maxlength);
		}
		return true;
	}

	SMCError err;
	SMCStates state = { 0, 0 };

	ke::Vector<ke::AString> fileList;
	MasterReader.m_FileList = &fileList;

	err = textparsers->ParseSMCFile(path, &MasterReader, &state, error, maxlength);

	if (err != SMCError_Okay)
	{
		const char *msg = textparsers->GetSMCErrorString(err);

		AMXXLOG_Error("Error parsing master gameconf file \"%s\":", path);
		AMXXLOG_Error("Error %d on line %d, col %d: %s", err, state.line, state.col, msg ? msg : "Unknown error");

		return false;
	}

	for (size_t i = 0; i < fileList.length(); ++i)
	{
		g_LibSys.PathFormat(path, sizeof(path), "%s/%s", m_File, fileList[i].chars());

		if (!EnterFile(path, error, maxlength))
		{
			return false;
		}
	}

	build_pathname_r(path, sizeof(path), "%s/gamedata/%s/custom", dataDir, m_File);
	CDirectory *customDir = g_LibSys.OpenDirectory(path);

	if (!customDir)
	{
		return true;
	}

	while (customDir->MoreFiles())
	{
		if (!customDir->IsEntryFile())
		{
			customDir->NextEntry();
			continue;
		}

		const char *currentFile = customDir->GetEntryName();

		size_t length = strlen(currentFile);

		if (length > 4 && strcmp(&currentFile[length - 4], ".txt") != 0)
		{
			customDir->NextEntry();
			continue;
		}

		g_LibSys.PathFormat(path, sizeof(path), "%s/custom/%s", m_File, currentFile);

		if (!EnterFile(path, error, maxlength))
		{
			g_LibSys.CloseDirectory(customDir);
			return false;
		}

		customDir->NextEntry();
	}

	g_LibSys.CloseDirectory(customDir);

	return true;
}

bool CGameConfig::EnterFile(const char *file, char *error, size_t maxlength)
{
	build_pathname_r(m_CurrentPath, sizeof(m_CurrentPath), "%s/gamedata/%s", get_localinfo("amxx_datadir", "addons/amxmodx/data"), file);

	m_IgnoreLevel = 0;
	m_ShouldBeReadingDefault = true;
	m_ParseState = PSTATE_NONE;

	SMCError err;
	SMCStates state = { 0, 0 };

	if ((err = textparsers->ParseSMCFile(m_CurrentPath, this, &state, error, maxlength)) != SMCError_Okay)
	{
		const char *msg = textparsers->GetSMCErrorString(err);

		AMXXLOG_Error("Error parsing gameconfig file \"%s\":", m_CurrentPath);
		AMXXLOG_Error("Error %d on line %d, col %d: %s", err, state.line, state.col, msg ? msg : "Unknown error");

		if (m_ParseState == PSTATE_GAMEDEFS_CUSTOM)
		{
			m_CustomHandler->ReadSMC_ParseEnd(true, true);
			m_CustomHandler = nullptr;
			m_CustomLevel = 0;
		}

		return false;
	}

	return true;
}

bool CGameConfig::GetOffset(const char *key, TypeDescription *value)
{
	return m_Offsets.retrieve(key, value);
}

bool CGameConfig::GetOffsetByClass(const char *classname, const char *key, TypeDescription *value)
{
	auto r = m_OffsetsByClass.find(classname);

	if (!r.found())
	{
		return false;
	}

	return r->value->list.retrieve(key, value);
}

const char *CGameConfig::GetKeyValue(const char *key)
{
	auto r = m_Keys.find(key);

	if (!r.found())
	{
		return nullptr;
	}

	return r->value.chars();
}

//memory addresses below 0x10000 are automatically considered invalid for dereferencing
#define VALID_MINIMUM_MEMORY_ADDRESS 0x10000

bool CGameConfig::GetAddress(const char *key, void **retaddr)
{
	auto r = m_Addresses.find(key);

	if (!r.found())
	{
		*retaddr = nullptr;
		return false;
	}

	AddressConf &addrConf = r->value;

	void *address;

	if (!GetMemSig(addrConf.m_SignatureName, &address))
	{
		*retaddr = nullptr;
		return false;
	}

	for (size_t i = 0; i < addrConf.m_ReadCount; ++i)
	{
		int offset = addrConf.m_ReadBytes[i];

		if (!address || reinterpret_cast<uintptr_t>(address) < VALID_MINIMUM_MEMORY_ADDRESS)
		{
			*retaddr = nullptr;
			return false;
		}

		address = reinterpret_cast<void*>(reinterpret_cast<uint8_t*>(address) + offset);
	}

	*retaddr = address;

	return true;
}

CGameConfig::AddressConf::AddressConf(const char *sigName, size_t sigLength, size_t readCount, int *read)
{
	size_t limit = sizeof(m_ReadBytes) / sizeof(m_ReadBytes[0]);
	size_t readLimit = (readCount <= limit) ? readCount : limit;

	strncopy(m_SignatureName, sigName, sizeof(m_SignatureName));

	m_ReadCount = readLimit;
	memcpy(&this->m_ReadBytes[0], read, sizeof(this->m_ReadBytes[0]) * readLimit);
}

bool CGameConfig::GetMemSig(const char *key, void **addr)
{
	return m_Sigs.retrieve(key, addr);
}


//
// CONFIG MASTER READER
//

#define MSTATE_NONE		0
#define MSTATE_MAIN		1
#define MSTATE_FILE		2

void CGameMasterReader::ReadSMC_ParseStart()
{
	m_State = MSTATE_NONE;
	m_IgnoreLevel = 0;
}

SMCResult CGameMasterReader::ReadSMC_NewSection(const SMCStates *states, const char *name)
{
	if (m_IgnoreLevel)
	{
		return SMCResult_Continue;
	}

	if (m_State == MSTATE_NONE)
	{
		if (strcmp(name, "Game Master") == 0)
		{
			m_State = MSTATE_MAIN;
		}
		else
		{
			m_IgnoreLevel++;
		}
	}
	else if (m_State == MSTATE_MAIN)
	{
		strncopy(m_CurrentPath, name, sizeof(m_CurrentPath));

		m_HadEngine     = false;
		m_MatchedEngine = false;
		m_HadGame       = false;
		m_MatchedGame   = false;

		m_State = MSTATE_FILE;
	}
	else if (m_State == MSTATE_FILE)
	{
		m_IgnoreLevel++;
	}

	return SMCResult_Continue;
}

SMCResult CGameMasterReader::ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value)
{
	if (m_IgnoreLevel || m_State != MSTATE_FILE)
	{
		return SMCResult_Continue;
	}

	if (strcmp(key, "engine") == 0)
	{
		m_HadEngine = true;

		if (DoesEngineMatch(value))
		{
			m_MatchedEngine = true;
		}
	}
	else if (strcmp(key, "game") == 0)
	{
		m_HadGame = true;

		if (DoesGameMatch(value))
		{
			m_MatchedGame = true;
		}
	}

	return SMCResult_Continue;
}

SMCResult CGameMasterReader::ReadSMC_LeavingSection(const SMCStates *states)
{
	if (m_IgnoreLevel)
	{
		m_IgnoreLevel--;
		return SMCResult_Continue;
	}

	if (m_State == MSTATE_FILE)
	{
		// The four success conditions:
		//   1. Needed nothing.
		//   2. Needed game only.
		//   3. Needed engine only.
		//   4. Needed both engine and game.
		// Final result is minimized via k-map.

		if ((!m_HadEngine    && !m_HadGame) ||
			(!m_HadEngine    && m_MatchedGame) ||
			(!m_HadGame      && m_MatchedEngine) ||
			(m_MatchedEngine && m_MatchedGame))
		{
			m_FileList->append(m_CurrentPath);
		}

		m_State = MSTATE_MAIN;
	}
	else if (m_State == MSTATE_MAIN)
	{
		m_State = MSTATE_NONE;
	}

	return SMCResult_Continue;
}


//
// CONFIG MANAGER
//

CGameConfigManager::CGameConfigManager()
{
}

CGameConfigManager::~CGameConfigManager()
{
}

void CGameConfigManager::OnAmxxStartup()
{
	char error[256] = "";

	if (!LoadGameConfigFile("common.games", &CommonConfig, error, sizeof(error)))
	{
		AMXXLOG_Log("Could not read common.games gamedata: %s", error);
		return;
	}
}

bool CGameConfigManager::LoadGameConfigFile(const char *file, IGameConfig **config, char *error, size_t maxlength)
{
	CGameConfig *configFromCache;

	if (m_Lookup.retrieve(file, &configFromCache))
	{
		configFromCache->AddRef();
		*config = configFromCache;

		return true;
	}

	configFromCache = new CGameConfig(file);
	configFromCache->AddRef();

	bool returnValue = configFromCache->Reparse(error, maxlength);

	m_Lookup.insert(file, configFromCache);
	*config = configFromCache;

	return returnValue;
}

void CGameConfigManager::CloseGameConfigFile(IGameConfig *config)
{
	CGameConfig *currentConfig = static_cast<CGameConfig*>(config);
	currentConfig->Release();
}

void CGameConfigManager::AddUserConfigHook(const char *sectionName, ITextListener_SMC *listener)
{
	m_customHandlers.insert(sectionName, listener);
}

void CGameConfigManager::RemoveUserConfigHook(const char *sectionName, ITextListener_SMC *listener)
{
	ITextListener_SMC *listenerFromCache;

	if (!m_customHandlers.retrieve(sectionName, &listenerFromCache))
	{
		return;
	}

	if (listenerFromCache != listener)
	{
		return;
	}

	m_customHandlers.remove(sectionName);
}

void CGameConfigManager::RemoveCachedConfig(CGameConfig *config)
{
	m_Lookup.remove(config->m_File);
}
