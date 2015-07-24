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

#include "CstrikeitemsInfos.h"

CsItemInfo ItemsManager;

#define PSTATE_NONE  
#define PSTATE_ALIASES_TYPE       0
#define PSTATE_ALIASES_ALIAS      1
#define PSTATE_ALIASES_ALIAS_DEFS 2

CsItemInfo::CsItemInfo() 
	: 
	m_ParseState(0),
	m_List(nullptr),
	m_ListsRetrievedFromConfig(false)
{
}

CsItemInfo::~CsItemInfo()
{
	Clear();
}

void CsItemInfo::Clear()
{
	m_CommonAliasesList.clear();
	m_WeaponAliasesList.clear();
	m_BuyAliasesList.clear();
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
			m_List = nullptr;

			if (!strcmp(name, "Common"))
			{
				m_List = &m_CommonAliasesList;
			}
			else if (!strcmp(name, "Weapon"))
			{
				m_List = &m_WeaponAliasesList;
			}
			else if (strstr(name, "Buy"))
			{
				m_List = &m_BuyAliasesList;
			}
			else
			{
				return SMCResult_HaltFail;
			}

			m_ParseState = PSTATE_ALIASES_ALIAS;
			break;
		}
		case PSTATE_ALIASES_ALIAS:
		{
			m_AliasInfo.clear();
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
			else
			{
				return SMCResult_HaltFail;
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
			m_List->replace(m_Alias.chars(), m_AliasInfo);
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
	if (halted)
	{
		MF_Log("Invalid or missing key in \"%s\" section. Please check your gamedata files.", m_Alias.chars());
		return;
	}

	m_ListsRetrievedFromConfig = true;
}

bool CsItemInfo::GetAliasInfos(const char *alias, AliasInfo *info)
{
	if (GetAliasInfosFromBuy(alias, info) || m_WeaponAliasesList.retrieve(alias, info))
	{
		return true;
	}

	return false;
}

bool CsItemInfo::GetAliasInfosFromBuy(const char *alias, AliasInfo *info)
{
	if (m_CommonAliasesList.retrieve(alias, info) || m_BuyAliasesList.retrieve(alias, info))
	{
		return true;
	}

	return false;
}

bool CsItemInfo::GetAliasInfosFromName(const char *name, AliasInfo *info)
{
	static const char prefix_weapon[] = "weapon_";
	static const char prefix_item[] = "item_";

	const char *alias = name;

	if (strstr(name, prefix_weapon) && strcmp(name + sizeof(prefix_weapon) - 1, "shield"))
	{
		for (size_t id = 0; id < ARRAYSIZE(WeaponNameList); ++id)
		{
			if (!strcmp(name, WeaponNameList[id]))
			{
				info->classname = name;
				info->itemid    = id;
				info->classid   = WeaponIdToClass(id);

				return true;
			}
		}

		alias = name + sizeof(prefix_weapon) - 1;
	}
	else if (strstr(name, prefix_item))
	{
		for (auto iter = m_BuyAliasesList.iter(); !iter.empty(); iter.next())
		{
			if (iter->value.classname.length() && !iter->value.classname.compare(name))
			{
				*info = iter->value;
				return true;
			}
		}

		alias = name + sizeof(prefix_item) - 1;
	}

	if (GetAliasInfos(alias, info))
	{
		return true;
	}

	return false;
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