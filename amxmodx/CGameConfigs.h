// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_GAMECONFIG_H_
#define _INCLUDE_GAMECONFIG_H_

#include <IGameConfigs.h>
#include "CLibrarySys.h"
#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include <amtl/am-refcounting.h>
#include <sm_stringhashmap.h>
#include <sm_namehashset.h>

class CGameConfig 
	:
	public ITextListener_SMC,
	public IGameConfig,
	public ke::Refcounted <CGameConfig>
{
	friend class CGameConfigManager;

	public:

		CGameConfig(const char *file);
		~CGameConfig();

	public:

		bool Reparse(char *error, size_t maxlength);
		bool EnterFile(const char *file, char *error, size_t maxlength);

	public: // ITextListener_SMC

		SMCResult ReadSMC_NewSection(const SMCStates *states, const char *name);
		SMCResult ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value);
		SMCResult ReadSMC_LeavingSection(const SMCStates *states);

	public: // IGameConfig

		const char* GetKeyValue(const char *key);
		bool        GetOffset(const char *key, TypeDescription *value);
		bool        GetOffsetByClass(const char *classname, const char *key, TypeDescription *value);
		bool        GetMemSig(const char *key, void **addr);
		bool        GetAddress(const char *key, void **addr);

	public: // NameHashSet

		static inline bool matches(const char *key, const CGameConfig *value)
		{
			return strcmp(key, value->m_File) == 0;
		}

	private:

		struct OffsetClass
		{
			StringHashMap<TypeDescription> list;
		};

		typedef StringHashMap<ke::AutoPtr<OffsetClass>> OffsetClassMap;
		typedef StringHashMap<TypeDescription> OffsetMap;

		char                       m_File[PLATFORM_MAX_PATH];
		char                       m_CurrentPath[PLATFORM_MAX_PATH];

		OffsetMap                  m_Offsets;
		OffsetClassMap             m_OffsetsByClass;
		StringHashMap<ke::AString> m_Keys;
		StringHashMap<void*>       m_Sigs;

		int                        m_ParseState;
		unsigned int               m_IgnoreLevel;

		char                       m_Class[64];
		char                       m_Offset[64];
		char                       m_Game[256];

		bool                       m_FoundOffset;
		bool                       m_MatchedClasses;
		bool                       m_ShouldBeReadingDefault;
		bool                       m_HadGame;
		bool                       m_MatchedGame;
		bool                       m_HadEngine;
		bool                       m_MatchedEngine;
		bool                       m_MatchedPlatform;

		unsigned int               m_CustomLevel;
		ITextListener_SMC*         m_CustomHandler;

		struct AddressConf
		{
			char    m_SignatureName[64];
			size_t  m_ReadCount;
			int     m_ReadBytes[8];

			AddressConf(const char *sigName, size_t sigLength, size_t readCount, int *read);
			AddressConf() {}
		};

		char m_Address[64];
		char m_AddressSignature[64];
		int  m_AddressReadCount;
		int  m_AddressRead[8];
		StringHashMap<AddressConf> m_Addresses;

		char m_pEngine[64];
};

class CGameMasterReader : public ITextListener_SMC
{
	public:

		void ReadSMC_ParseStart();

		SMCResult ReadSMC_NewSection(const SMCStates *states, const char *name);
		SMCResult ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value);
		SMCResult ReadSMC_LeavingSection(const SMCStates *states);

	public:

		ke::Vector<ke::AString>* m_FileList;

		unsigned int m_State;
		unsigned int m_IgnoreLevel;

		char         m_CurrentPath[PLATFORM_MAX_PATH];

		bool         m_HadEngine;
		bool         m_MatchedEngine;
		bool         m_HadGame;
		bool         m_MatchedGame;
};

class CGameConfigManager : public IGameConfigManager
{
	public:

		CGameConfigManager();
		~CGameConfigManager();

	public: // IGameConfigManager

		bool LoadGameConfigFile(const char *file, IGameConfig **pConfig, char *error, size_t maxlength);
		void CloseGameConfigFile(IGameConfig *cfg);
		void AddUserConfigHook(const char *sectionname, ITextListener_SMC *listener);
		void RemoveUserConfigHook(const char *sectionname, ITextListener_SMC *listener);

	public:

		void OnAmxxStartup();
		void RemoveCachedConfig(CGameConfig *config);

	private:

		NameHashSet<CGameConfig*> m_Lookup;

	public:

		StringHashMap<ITextListener_SMC*> m_customHandlers;
};

extern CGameConfigManager ConfigManager;
extern IGameConfig *CommonConfig;

#endif // _INCLUDE_GAMECONFIG_H_
