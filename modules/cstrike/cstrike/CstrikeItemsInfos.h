// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#ifndef _CSTRIKE_WEAPONS_INFOS_H_
#define _CSTRIKE_WEAPONS_INFOS_H_

#include <amxxmodule.h>
#include "CstrikeDatas.h"
#include <ITextParsers.h>
#include <amtl/am-string.h>
#include <sm_stringhashmap.h>

struct AliasInfo
{
	AliasInfo()
	{
		clear();
	}

	void clear()
	{
		itemid  = CSI_NONE;
		classid = CS_WEAPONCLASS_NONE;
	}

	int itemid;
	int classid;
	ke::AString classname;
};

enum class Equipments
{
	None,
	Vest,
	Vesthelm,
	Flashbang,
	HEGrenade,
	SmokeGrenade,
	Nvg,
	Defuser,
	Count
};

class CsItemInfo : public ITextListener_SMC
{
	public:

		CsItemInfo();
		virtual ~CsItemInfo();

	public:

		void Clear();
		bool HasConfigError();

	public:

		SMCResult ReadSMC_NewSection(const SMCStates *states, const char *name) override;
		SMCResult ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value) override;
		SMCResult ReadSMC_LeavingSection(const SMCStates *states) override;
		void      ReadSMC_ParseEnd(bool halted, bool failed) override;

	public:

		bool GetAliasInfos(const char *alias, AliasInfo *info);
		bool GetAliasInfosFromBuy(const char *alias, AliasInfo *info);
		bool GetAliasInfosFromName(const char *classname, AliasInfo *info);

		CsWeaponClassType WeaponIdToClass(int id);

		int GetItemPrice(int id);

	private: // Retrieved datas

		typedef StringHashMap<AliasInfo> AliasMap;

		AliasMap     m_CommonAliasesList;
		AliasMap     m_WeaponAliasesList;
		AliasMap     m_BuyAliasesList;

		CsWeaponClassType m_WeaponIdToClass[CSI_MAX_COUNT];

	private: // Config parsing

		int          m_ParseState;
		AliasMap*    m_List;
		ke::AString  m_Alias;
		AliasInfo    m_AliasInfo;
		bool         m_ListsRetrievedFromConfig;
		int          m_EquipmentsPrice[static_cast<size_t>(Equipments::Count)];
};

extern char WeaponNameList[MAX_WEAPONS][64];
extern CsItemInfo ItemsManager;

#endif // _CSTRIKE_WEAPONS_INFOS_H_
