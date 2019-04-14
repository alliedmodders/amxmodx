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

struct ItemInfos
{
	ItemInfos() : name("Empty"), ammoIndex1(-1), maxAmmo1(0), ammoIndex2(-1), maxAmmo2(0), slot(0), position(0), id(0), flags(0)
	{}

	ItemInfos &operator = (ItemInfos &other)
	{
		name       = other.name;
		ammoIndex1 = other.ammoIndex1;
		maxAmmo1   = other.maxAmmo1;
		ammoIndex2 = other.ammoIndex2;
		maxAmmo2   = other.maxAmmo2;
		slot       = other.slot;
		position   = other.position;
		id         = other.id;
		flags      = other.flags;

		return *this;
	}

	ke::AString name;
	int         ammoIndex1;
	int         maxAmmo1;
	int         ammoIndex2;
	int         maxAmmo2;
	int         slot;
	int         position;
	int         id;
	int         flags;
};

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
		classname = nullptr;
		alt_alias = nullptr;
	}

	int itemid;
	int classid;
	ke::AString classname;
	ke::AString alt_alias;
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
		bool GetAliasInfosFromName(const char *classname, AliasInfo *info);
		bool GetAliasFromId(size_t id, ke::AString &name, ke::AString &altname);

		CsWeaponClassType WeaponIdToClass(int id);

		int GetItemPrice(int id);

	private: // Retrieved datas

		typedef StringHashMap<AliasInfo> AliasMap;

		AliasMap     m_BuyAliasesList;
		AliasMap     m_BuyAliasesAltList;

		CsWeaponClassType m_WeaponIdToClass[CSI_MAX_COUNT];

	private: // Config parsing

		int          m_ParseState;
		ke::AString  m_Alias;
		ke::AString  m_AliasAlt;
		AliasInfo    m_AliasInfo;
		bool         m_ListsRetrievedFromConfig;
		int          m_EquipmentsPrice[static_cast<size_t>(Equipments::Count)];
};

extern ItemInfos WeaponsList[MAX_WEAPONS];
extern CsItemInfo ItemsManager;

#endif // _CSTRIKE_WEAPONS_INFOS_H_
