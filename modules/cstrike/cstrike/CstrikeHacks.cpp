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

#include "CstrikeDatas.h"
#include "CstrikeUtils.h"
#include "CstrikeHacks.h"
#include "CstrikeHLTypeConversion.h"
#include <sm_stringhashmap.h>

void CtrlDetours_ClientCommand(bool set);
void CtrlDetours_BuyCommands(bool set);
void CtrlDetours_Natives(bool set);

int ForwardInternalCommand = -1;
int ForwardOnBuy           = -1;
int ForwardOnBuyAttempt    = -1;

int *UseBotArgs;
const char **BotArgs;

CDetour *ClientCommandDetour;
CDetour *GiveShieldDetour;
CDetour *GiveNamedItemDetour;
CDetour *AddAccountDetour;
CDetour *GiveDefaultItemsDetour;

CreateNamedEntityFunc       CS_CreateNamedEntity;
UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;

int CurrentItemId;
StringHashMap<int> ItemAliasList;
int TeamOffset;
int MenuOffset;

void InitializeHacks()
{
	CtrlDetours_ClientCommand(true);
	CtrlDetours_BuyCommands(true);
	CtrlDetours_Natives(true);
}

void ShutdownHacks()
{
	CtrlDetours_ClientCommand(false);
	CtrlDetours_BuyCommands(false);
	CtrlDetours_Natives(false);
}

#undef CMD_ARGV

const char *CMD_ARGV(int i)
{
	if (*UseBotArgs)
	{
		if (i < 4)
		{
			return BotArgs[i];
		}

		return nullptr;
	}

	return g_engfuncs.pfnCmd_Argv(i);
}

void OnEmitSound(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch)
{
	// If shield is blocked with CS_OnBuy, we need to block the pickup sound ("items/gunpickup2.wav") 
	// as well played right after. Why this sound is not contained in GiveShield()?

	g_pengfuncsTable->pfnEmitSound = nullptr;

	RETURN_META(MRES_SUPERCEDE);
}

DETOUR_DECL_STATIC1(C_ClientCommand, void, edict_t*, pEdict) // void ClientCommand(edict_t *pEntity)
{
	const char *command = CMD_ARGV(0);
	
	// A new command is triggered, reset variable, always.
	CurrentItemId = 0;

	// Purpose is to retrieve an item id based on alias name or selected item from menu,
	// to be used in OnBuy* forwards.
	if ((ForwardOnBuyAttempt != -1 || ForwardOnBuy != -1) && command && *command)
	{
		int itemId = 0;
		
		// Handling buy via menu.
		if (!strcmp(command, "menuselect")) 
		{
			int slot = atoi(CMD_ARGV(1));

			if (slot > 0 && slot < 9)
			{
			    static const int menuItemsTe[][9] = 
				{
					/* Menu_Buy              */ { 0, 0, 0, 0, 0, 0, CSI_PRIMAMMO, CSI_SECAMMO, 0 },
					/* Menu_BuyPistol        */ { 0, CSI_GLOCK18, CSI_USP, CSI_P228, CSI_DEAGLE, CSI_ELITE, 0, 0, 0 },
					/* Menu_BuyRifle         */ { 0, CSI_GALI, CSI_AK47, CSI_SCOUT, CSI_SG552, CSI_AWP, CSI_G3SG1, 0, 0 },
					/* Menu_BuyMachineGun    */ { 0, CSI_M249, 0, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuyShotgun       */ { 0, CSI_M3, CSI_XM1014, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuySubMachineGun */ { 0, CSI_MAC10, CSI_MP5NAVY, CSI_UMP45, CSI_P90, 0, 0, 0, 0 },
					/* Menu_BuyItem          */ { 0, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, 0, 0 }
				};

				static const int menuItemsCt[][9] = 
				{
					/* Menu_Buy              */ { 0, 0, 0, 0, 0, 0, CSI_PRIMAMMO, CSI_SECAMMO, 0 },
					/* Menu_BuyPistol        */ { 0, CSI_GLOCK18, CSI_USP, CSI_P228, CSI_DEAGLE, CSI_FIVESEVEN, 0, 0, 0 },
					/* Menu_BuyRifle         */ { 0, CSI_FAMAS, CSI_SCOUT, CSI_M4A1, CSI_AUG, CSI_SG550, CSI_AWP, 0, 0 },
					/* Menu_BuyMachineGun    */ { 0, CSI_M249, 0, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuyShotgun       */ { 0, CSI_M3, CSI_XM1014, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuySubMachineGun */ { 0, CSI_TMP, CSI_MP5NAVY, CSI_UMP45, CSI_P90, 0, 0, 0, 0 },
					/* Menu_BuyItem          */ { 0, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, CSI_DEFUSER, CSI_SHIELDGUN }
				};

				int menuId = get_pdata<int>(pEdict, MenuOffset);

				if (menuId >= Menu_Buy && menuId <= Menu_BuyItem)
				{
					switch (get_pdata<int>(pEdict, TeamOffset))
					{
						case TEAM_T: itemId = menuItemsTe[menuId - 4][slot]; break; // -4 because array is zero-based and Menu_Buy* constants starts from 4.
						case TEAM_CT:itemId = menuItemsCt[menuId - 4][slot]; break;
					}

					if (itemId)
					{
						CurrentItemId = itemId;
					}
				}
			}
		}
		else // Handling buy via alias
		{
			if (ItemAliasList.retrieve(command, &itemId))
			{
				CurrentItemId = itemId;
			}
		}
	}

	int client = ENTINDEX(pEdict);

	if (ForwardInternalCommand != -1 && *UseBotArgs)
	{
		const char *args = *BotArgs;

		if (MF_ExecuteForward(ForwardInternalCommand, static_cast<cell>(client), args) > 0)
		{
			return;
		}
	}

	if (ForwardOnBuyAttempt != -1 && 
		CurrentItemId             && 
		MF_IsPlayerAlive(client)  && 
		MF_ExecuteForward(ForwardOnBuyAttempt, static_cast<cell>(client), static_cast<cell>(CurrentItemId)) > 0)
	{
		return;
	}

	DETOUR_STATIC_CALL(C_ClientCommand)(pEdict);
}

edict_s* OnCreateNamedEntity(int classname)
{
	if (NoKifesMode)
	{
		if (!strcmp(STRING(classname), "weapon_knife"))
		{
			RETURN_META_VALUE(MRES_SUPERCEDE, nullptr);
		}
	}
	else
	{
		g_pengfuncsTable->pfnCreateNamedEntity = nullptr;
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

DETOUR_DECL_MEMBER0(GiveDefaultItems, void)  // void CBasePlayer::GiveDefaultItems(void)
{
	if (NoKifesMode)
	{
		g_pengfuncsTable->pfnCreateNamedEntity = OnCreateNamedEntity;
	}

	DETOUR_MEMBER_CALL(GiveDefaultItems)();

	g_pengfuncsTable->pfnCreateNamedEntity = nullptr;
}

DETOUR_DECL_MEMBER1(GiveNamedItem, void, const char*, pszName) // void CBasePlayer::GiveNamedItem(const char *pszName)
{
	// If the current item id is not null, this means player has triggers a buy command.
	if (CurrentItemId)
	{
		int client = G_HL_TypeConversion.cbase_to_id(this);

		if (MF_IsPlayerAlive(client) && MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), static_cast<cell>(CurrentItemId)) > 0)
		{
			return;
		}
	}

	// From here, forward is not blocked, resetting this
	// to ignore code in AddAccount which is called right after.
	CurrentItemId = 0;

	// Give me my item!
	DETOUR_MEMBER_CALL(GiveNamedItem)(pszName);
}

DETOUR_DECL_MEMBER1(GiveShield, void, bool, bRetire) // void CBasePlayer::GiveShield(bool bRetire)
{
	// Special case for shield. Game doesn't use GiveNamedItem() to give a shield.
	if (CurrentItemId == CSI_SHIELDGUN)
	{
		int client = G_HL_TypeConversion.cbase_to_id(this);

		if (MF_IsPlayerAlive(client) && MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), CSI_SHIELDGUN) > 0)
		{
			return;
		}
	}

	// From here, forward is not blocked, resetting this
	// to ignore code in AddAccount which is called right after.
	CurrentItemId = 0;

	// Give me my shield!
	DETOUR_MEMBER_CALL(GiveShield)(bRetire);
}

DETOUR_DECL_MEMBER2(AddAccount, void, int, amount, bool, bTrackChange) // void CBasePlayer::AddAccount(int amount, bool bTrackChange)
{
	// No buy command or forward not blocked.
	// Resuming game flow.
	if (!CurrentItemId)
	{
		DETOUR_MEMBER_CALL(AddAccount)(amount, bTrackChange);
	}
	// Shield is blocked.
	// We need to hook EmitSound to block pickup sound played right after.
	else if (CurrentItemId == CSI_SHIELDGUN)
	{
		g_pengfuncsTable->pfnEmitSound = OnEmitSound;
	}

	// Let's reset this right away to avoid issues.
	CurrentItemId = 0;
}


void CtrlDetours_ClientCommand(bool set)
{
	if (set)
	{
		void *base = reinterpret_cast<void *>(MDLL_ClientCommand);

#if defined(WIN32)

		int offset = 0;

		if (MainConfig->GetOffset("UseBotArgs", &offset))
		{
			UseBotArgs = get_pdata<int*>(base, offset);
		}

		if (MainConfig->GetOffset("BotArgs", &offset))
		{
			BotArgs = get_pdata<const char**>(base, offset);
		}

#elif defined(__linux__) || defined(__APPLE__)

		void *address = nullptr;

		if (MainConfig->GetMemSig("UseBotArgs", &address))
		{
			UseBotArgs = reinterpret_cast<int *>(address);
		}

		if (MainConfig->GetMemSig("BotArgs", &address))
		{
			BotArgs = reinterpret_cast<const char **>(address);
		}
#endif
		ClientCommandDetour = DETOUR_CREATE_STATIC_FIXED(C_ClientCommand, base);

		OffsetConfig->GetOffsetByClass("CBasePlayer", "m_iTeam", &TeamOffset);
		OffsetConfig->GetOffsetByClass("CBasePlayer", "m_iMenu", &MenuOffset);

		if (!ClientCommandDetour || !UseBotArgs || !BotArgs || !TeamOffset || !MenuOffset)
		{
			MF_Log("ClientCommand is not available - forward client_command has been disabled");
		}
	}
	else
	{
		if (ClientCommandDetour)
		{
			ClientCommandDetour->Destroy();
		}

		ItemAliasList.clear();
	}
}

void ToggleDetour_ClientCommands(bool enable)
{
	if (ClientCommandDetour)
	{
		(enable) ? ClientCommandDetour->EnableDetour() : ClientCommandDetour->DisableDetour();
	}

	if (enable)
	{
		// Build the item alias list.
		// Used in ClientCommand to check and get fastly item id from alias name.
		typedef struct
		{
			const char *alias;
			int id;

		} itemBuyAliasInfo;
		
		itemBuyAliasInfo aliasToId[] =
		{
			{ "p228"       , CSI_P228       }, { "228compact" , CSI_P228         },
			{ "scout"      , CSI_SCOUT      }, { "hegren"     , CSI_HEGRENADE    },           
			{ "xm1014"     , CSI_XM1014     }, { "autoshotgun", CSI_XM1014       },
			{ "mac10"      , CSI_MAC10      }, { "aug"        , CSI_AUG          },
			{ "bullpup"    , CSI_AUG        }, { "sgren"      , CSI_SMOKEGRENADE },
			{ "elites"     , CSI_ELITE      }, { "fn57"       , CSI_FIVESEVEN    },
			{ "fiveseven"  , CSI_FIVESEVEN  }, { "ump45"      , CSI_UMP45        },
			{ "sg550"      , CSI_SG550      }, { "krieg550"   , CSI_SG550        },
			{ "galil"      , CSI_GALI       }, { "defender"   , CSI_GALI         },
			{ "famas"      , CSI_FAMAS      }, { "clarion"    , CSI_FAMAS        },
			{ "usp"        , CSI_USP        }, { "km45"       , CSI_USP          },
			{ "glock"      , CSI_GLOCK18    }, { "9x19mm"     , CSI_GLOCK18      },
			{ "awp"        , CSI_AWP        }, { "magnum"     , CSI_AWP          },
			{ "mp5"        , CSI_MP5NAVY    }, { "smg"        , CSI_MP5NAVY      },
			{ "m249"       , CSI_M249       }, { "m3"         , CSI_M3           },
			{ "12gauge"    , CSI_M3         }, { "m4a1"       , CSI_M4A1         },
			{ "tmp"        , CSI_TMP        }, { "mp"         , CSI_TMP          },
			{ "g3sg1"      , CSI_G3SG1      }, { "d3au1"      , CSI_G3SG1        },
			{ "flash"      , CSI_FLASHBANG  }, { "deagle"     , CSI_DEAGLE       },
			{ "nighthawk"  , CSI_DEAGLE     }, { "sg552"      , CSI_SG552        },
			{ "krieg552"   , CSI_SG552      }, { "ak47"       , CSI_AK47         },
			{ "cv47"       , CSI_AK47       }, { "p90"        , CSI_P90          },
			{ "c90"        , CSI_P90        }, { "vest"       , CSI_VEST         },
			{ "vesthelm"   , CSI_VESTHELM   }, { "defuser"    , CSI_DEFUSER      },
			{ "nvgs"       , CSI_NVGS       }, { "shield"     , CSI_SHIELDGUN    },
			{ "buyammo1"   , CSI_PRIMAMMO   }, { "primammo"   , CSI_PRIMAMMO     },
			{ "buyammo2"   , CSI_SECAMMO    }, { "secammo"    , CSI_SECAMMO      },
			{ nullptr         , 0 }
		};

		for (size_t i = 0; aliasToId[i].alias != nullptr; ++i)
		{
			ItemAliasList.insert(aliasToId[i].alias, aliasToId[i].id);
		}
	}
	else
	{
		ItemAliasList.clear();
	}
}


void CtrlDetours_BuyCommands(bool set)
{
	if (set)
	{
		void *address = nullptr;

		if (MainConfig->GetMemSig("GiveShield", &address))
		{
			GiveShieldDetour = DETOUR_CREATE_MEMBER_FIXED(GiveShield, address);
		}

		if (MainConfig->GetMemSig("GiveNamedItem", &address))
		{
			GiveNamedItemDetour = DETOUR_CREATE_MEMBER_FIXED(GiveNamedItem, address);
		}

		if (MainConfig->GetMemSig("AddAccount", &address))
		{
			AddAccountDetour = DETOUR_CREATE_MEMBER_FIXED(AddAccount, address);
		}
		
		if (!GiveShieldDetour || !GiveNamedItemDetour || !AddAccountDetour)
		{
			if (!GiveShieldDetour)
			{
				MF_Log("GiveShield is not available");
			}

			if (!GiveNamedItemDetour)
			{
				MF_Log("GiveNamedItem is not available");
			}

			if (!AddAccountDetour)
			{
				MF_Log("AddAccount is not available");
			}

			MF_Log("Some functions are not available - forward CS_OnBuyAttempt and CS_OnBuy have been disabled");
		}
	}
	else
	{
		if (GiveShieldDetour)
		{
			GiveShieldDetour->Destroy();
		}

		if (GiveNamedItemDetour)
		{
			GiveNamedItemDetour->Destroy();
		}

		if (AddAccountDetour)
		{
			AddAccountDetour->Destroy();
		}

		ItemAliasList.clear();
	}
}

void ToggleDetour_BuyCommands(bool enable)
{
	if (GiveShieldDetour)
	{
		(enable) ? GiveShieldDetour->EnableDetour() : GiveShieldDetour->DisableDetour();
	}

	if (GiveNamedItemDetour)
	{
		(enable) ? GiveNamedItemDetour->EnableDetour() : GiveNamedItemDetour->DisableDetour();
	}

	if (AddAccountDetour)
	{
		(enable) ? AddAccountDetour->EnableDetour() : AddAccountDetour->DisableDetour();
	}
}

void CtrlDetours_Natives(bool set)
{
	if (set)
	{
		void *address = nullptr;

		if (MainConfig->GetMemSig("CreateNamedEntity", &address)) // cs_create_entity()
		{
			CS_CreateNamedEntity = reinterpret_cast<CreateNamedEntityFunc>(address);
		}

		if (MainConfig->GetMemSig("FindEntityByString", &address)) // cs_find_ent_by_class()
		{
			CS_UTIL_FindEntityByString = reinterpret_cast<UTIL_FindEntityByStringFunc>(address);
		}

		if (!CS_CreateNamedEntity)
		{
			MF_Log("CREATE_NAMED_ENITTY is not available - native cs_create_entity() has been disabled");
		}

		if (!CS_UTIL_FindEntityByString)
		{
			MF_Log("UTIL_FindEntByString is not available - native cs_find_ent_by_class() has been disabled");
		}

		if (MainConfig->GetMemSig("GiveDefaultItems", &address))
		{
			GiveDefaultItemsDetour = DETOUR_CREATE_MEMBER_FIXED(GiveDefaultItems, address);
		}

		if (!GiveDefaultItemsDetour)
		{
			MF_Log("GiveDefaultItems is not available - native cs_set_no_knives has been disabled");
		}
	}
	else
	{
		if (GiveDefaultItemsDetour)
		{
			GiveDefaultItemsDetour->Destroy();
		}
	}
}
