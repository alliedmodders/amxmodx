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

#include "CstrikeItemsInfos.h"
#include "CstrikeHacks.h"
#include <amtl/am-utility.h>

CsItemInfo ItemsManager;
ItemInfo WeaponsList[MAX_WEAPONS];

#define PSTATE_ALIASES_TYPE       0
#define PSTATE_ALIASES_ALIAS      1
#define PSTATE_ALIASES_ALIAS_DEFS 2

CsItemInfo::CsItemInfo()
	:
	m_ParseState(PSTATE_ALIASES_TYPE),
	m_ListsRetrievedFromConfig(false)
{}

CsItemInfo::~CsItemInfo()
{
	Clear();
}

void CsItemInfo::Clear()
{
	m_BuyAliasesList.clear();
	m_BuyAliasesAltList.clear();
}

bool CsItemInfo::HasConfigError()
{
	return !m_ListsRetrievedFromConfig;
}

SMCResult CsItemInfo::ReadSMC_NewSection(const SMCStates *states, const char *name)
{
	switch (m_ParseState)
	{
		case PSTATE_ALIASES_TYPE:
		{
			if (!strcmp(name, "BuyAliases"))
			{
				m_ParseState = PSTATE_ALIASES_ALIAS;
			}
			break;
		}
		case PSTATE_ALIASES_ALIAS:
		{
			m_Alias = name;
			m_ParseState = PSTATE_ALIASES_ALIAS_DEFS;
			break;
		}
	}

	return SMCResult_Continue;
}

SMCResult CsItemInfo::ReadSMC_KeyValue(const SMCStates *states, const char *key, const char *value)
{
	switch (m_ParseState)
	{
		case PSTATE_ALIASES_ALIAS_DEFS:
		{
			if (!strcmp(key, "itemid"))
			{
				m_AliasInfo.itemid = atoi(value);

				if (m_AliasInfo.itemid < CSI_NONE || m_AliasInfo.itemid >= CSI_MAX_COUNT)
				{
					m_AliasInfo.itemid = CSI_NONE;
				}
			}
			else if (!strcmp(key, "classid"))
			{
				m_AliasInfo.classid = atoi(value);

				if (m_AliasInfo.classid < CS_WEAPONCLASS_NONE || m_AliasInfo.classid >= CS_WEAPONCLASS_MAX_COUNT)
				{
					m_AliasInfo.classid = CS_WEAPONCLASS_NONE;
				}
			}
			else if (!strcmp(key, "classname"))
			{
				m_AliasInfo.classname = value;
			}
			else if (!strcmp(key, "price"))
			{
				static int equipmentsList[static_cast<size_t>(Equipments::Count)] =
				{
					CSI_NONE, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, CSI_DEFUSER
				};

				for (int i = 0; i < ARRAY_LENGTH(equipmentsList); ++i)
				{
					if (m_AliasInfo.itemid == equipmentsList[i])
					{
						m_EquipmentsPrice[i] = atoi(value);
					}
				}
			}
			else if (!strcmp(key, "altname"))
			{
				m_AliasAlt = value;
			}
			break;
		}
	}

	return SMCResult_Continue;
}

SMCResult CsItemInfo::ReadSMC_LeavingSection(const SMCStates *states)
{
	switch (m_ParseState)
	{
		case PSTATE_ALIASES_ALIAS:
		{
			m_ParseState = PSTATE_ALIASES_TYPE;
			break;
		}
		case PSTATE_ALIASES_ALIAS_DEFS:
		{
			if (m_AliasAlt.length())
			{
				m_BuyAliasesAltList.replace(m_AliasAlt.chars(), m_AliasInfo);
				m_AliasInfo.alt_alias = Move(m_AliasAlt);
			}

			m_BuyAliasesList.replace(m_Alias.chars(), m_AliasInfo);
			m_WeaponIdToClass[m_AliasInfo.itemid] = static_cast<CsWeaponClassType>(m_AliasInfo.classid);

			m_AliasInfo.clear();
			m_ParseState = PSTATE_ALIASES_ALIAS;
			break;
		}
	}

	return SMCResult_Continue;
}

void CsItemInfo::ReadSMC_ParseEnd(bool halted, bool failed)
{
	if (!halted && !failed)
	{
		m_ListsRetrievedFromConfig = true;
	}
}

bool CsItemInfo::GetAliasInfos(const char *alias, AliasInfo *info)
{
	if (m_BuyAliasesList.retrieve(alias, info) || m_BuyAliasesAltList.retrieve(alias, info))
	{
		return true;
	}

	return false;
}

bool CsItemInfo::GetAliasFromId(size_t id, ke::AString &name, ke::AString &altname)
{
	for (auto iter = m_BuyAliasesList.iter(); !iter.empty(); iter.next())
	{
		if (iter->value.itemid == id)
		{
			name = iter->key;
			altname = iter->value.alt_alias;

			return true;
		}
	}

	return false;
}

bool CsItemInfo::GetAliasInfosFromName(const char *name, AliasInfo *info)
{
	static const char prefix_weapon[] = "weapon";
	static const char prefix_item[] = "item";

	static const size_t prefix_weapon_length = sizeof(prefix_weapon) - 1;
	static const size_t prefix_item_length = sizeof(prefix_item) - 1;

	if (name[prefix_weapon_length] == '_' && !strncmp(name, prefix_weapon, prefix_weapon_length))
	{
		name += prefix_weapon_length + 1; // including '_'
	}
	else if (name[prefix_item_length] == '_' && !strncmp(name, prefix_item, prefix_item_length))
	{
		name += prefix_item_length + 1;
	}

	for (size_t id = 0; id < ARRAYSIZE(WeaponsList); ++id)
	{
		const char *weapon = WeaponsList[id].name.chars();

		if (weapon[prefix_weapon_length] == '_' && !strncmp(weapon, prefix_weapon, prefix_weapon_length))
		{
			weapon += prefix_weapon_length + 1;
		}

		if (!strcmp(weapon, name))
		{
			info->classname = name;
			info->itemid = id;
			info->classid = WeaponIdToClass(id);

			return true;
		}
	}

	for (auto iter = m_BuyAliasesList.iter(); !iter.empty(); iter.next())
	{
		if (iter->value.classname.length() && !strcmp(iter->value.classname.chars() + prefix_item_length + 1, name))
		{
			*info = iter->value;
			return true;
		}
	}

	return GetAliasInfos(name, info);
}

CsWeaponClassType CsItemInfo::WeaponIdToClass(int id)
{
	if ((id > CSI_NONE && id <= CSI_P90) || id == CSI_SHIELD || id == CSI_SHIELDGUN)
	{
		if (id == CSI_SHIELDGUN)
		{
			id = CSI_SHIELD;
		}

		return m_WeaponIdToClass[id];
	}

	return CS_WEAPONCLASS_NONE;
}

int CsItemInfo::GetItemPrice(int itemId)
{
	if (itemId <= CSI_NONE || itemId > CSI_SHIELD)
	{
		return 0;
	}

	Equipments id = Equipments::None;

	switch (itemId)
	{
		case CSI_VEST:         id = Equipments::Vest;         break;
		case CSI_VESTHELM:     id = Equipments::Vesthelm;     break;
		case CSI_HEGRENADE:    id = Equipments::HEGrenade;    break;
		case CSI_SMOKEGRENADE: id = Equipments::SmokeGrenade; break;
		case CSI_FLASHBANG:    id = Equipments::Flashbang;    break;
		case CSI_NVGS:         id = Equipments::Nvg;          break;
		case CSI_DEFUSER:      id = Equipments::Defuser;      break;
	}

	if (id != Equipments::None)
	{
		return m_EquipmentsPrice[static_cast<size_t>(id)];
	}

	return GetWeaponInfo(itemId == CSI_SHIELD ? CSI_SHIELDGUN : itemId)->cost;
}
