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
GetWeaponInfoFunc           GetWeaponInfo;

int CurrentItemId;
bool TriggeredFromCommand;

// m_iTeam and m_iMenu from CBasePlayer.
TypeDescription TeamDesc;
TypeDescription MenuDesc;

// Engine global variables.
server_static_t *ServerStatic;
server_t *Server;

void InitializeHacks()
{
	CtrlDetours_ClientCommand(true);
	CtrlDetours_BuyCommands(true);
	CtrlDetours_Natives(true);

	InitGlobalVars();
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

DETOUR_DECL_STATIC1(C_ClientCommand, void, edict_t*, pEdict) // void ClientCommand(edict_t *pEntity)
{
	const char *command = CMD_ARGV(0);

	CurrentItemId = CSI_NONE;

	// Purpose is to retrieve an item id based on alias name or selected item from menu,
	// to be used in CS_OnBuy* forwards.
	if ((ForwardOnBuyAttempt != -1 || ForwardOnBuy != -1) && command && *command)
	{
		int itemId = CSI_NONE;

		// Handling buy via menu.
		if (!strcmp(command, "menuselect"))
		{
			int slot = atoi(CMD_ARGV(1));

			if (slot > 0 && slot < 9)
			{
				static const int menuItemsTe[][9] =
				{
					/* Menu_Buy              */ { 0, 0, 0, 0, 0, 0, CSI_PRIAMMO, CSI_SECAMMO, 0 },
					/* Menu_BuyPistol        */ { 0, CSI_GLOCK18, CSI_USP, CSI_P228, CSI_DEAGLE, CSI_ELITE, 0, 0, 0 },
					/* Menu_BuyRifle         */ { 0, CSI_GALIL, CSI_AK47, CSI_SCOUT, CSI_SG552, CSI_AWP, CSI_G3SG1, 0, 0 },
					/* Menu_BuyMachineGun    */ { 0, CSI_M249, 0, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuyShotgun       */ { 0, CSI_M3, CSI_XM1014, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuySubMachineGun */ { 0, CSI_MAC10, CSI_MP5NAVY, CSI_UMP45, CSI_P90, 0, 0, 0, 0 },
					/* Menu_BuyItem          */ { 0, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, 0, 0 }
				};

				static const int menuItemsCt[][9] =
				{
					/* Menu_Buy              */ { 0, 0, 0, 0, 0, 0, CSI_PRIAMMO, CSI_SECAMMO, 0 },
					/* Menu_BuyPistol        */ { 0, CSI_GLOCK18, CSI_USP, CSI_P228, CSI_DEAGLE, CSI_FIVESEVEN, 0, 0, 0 },
					/* Menu_BuyRifle         */ { 0, CSI_FAMAS, CSI_SCOUT, CSI_M4A1, CSI_AUG, CSI_SG550, CSI_AWP, 0, 0 },
					/* Menu_BuyMachineGun    */ { 0, CSI_M249, 0, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuyShotgun       */ { 0, CSI_M3, CSI_XM1014, 0, 0, 0, 0, 0, 0 },
					/* Menu_BuySubMachineGun */ { 0, CSI_TMP, CSI_MP5NAVY, CSI_UMP45, CSI_P90, 0, 0, 0, 0 },
					/* Menu_BuyItem          */ { 0, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, CSI_DEFUSER, CSI_SHIELD }
				};

				int menuId = get_pdata<int>(pEdict, MenuDesc.fieldOffset);

				if (menuId >= Menu_Buy && menuId <= Menu_BuyItem)
				{
					switch (get_pdata<int>(pEdict, TeamDesc.fieldOffset))
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
			AliasInfo info;

			if (ItemsManager.GetAliasInfosFromBuy(command, &info))
			{
				CurrentItemId = info.itemid;
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
		MF_IsPlayerAlive(client) &&
		MF_ExecuteForward(ForwardOnBuyAttempt, static_cast<cell>(client), static_cast<cell>(CurrentItemId)) > 0)
	{
		return;
	}

	// CS_OnBuy()
	// -
	// This forward should be called right before game gives the item.
	// All items except shield, defuser and nvgs, games executes in the following order:
	//   GiveNamedItem -> AddAccount
	//   Forward is fired on GiveNamedItem.
	// Shield only:
	//   GiveShield -> AddAccount -> EmitSound
	//   Forward is fired on GiveShield.
	// Defusal kit only:
	//   m_bHasDefuser(true) -> StatusIcon -> pev_body(1) -> AddAccount -> EmitSound -> ItemStatus
	//   Forward is fired on StatusIcon.
	// Nightvision only:
	//   EmitSound -> m_bHasNightVision(true) -> AddAccount -> ItemStatus
	//   Forward is fired on EmitSound.

	TriggeredFromCommand = CurrentItemId != CSI_NONE;

	if (TriggeredFromCommand)
	{
		if (CurrentItemId == CSI_NVGS)
		{
			g_pengfuncsTable->pfnEmitSound = OnEmitSound;
		}
		else if (CurrentItemId == CSI_DEFUSER)
		{
			GET_OFFSET_NO_ERROR("CBasePlayer", m_bHasDefuser);

			if (!get_pdata<bool>(pEdict, m_bHasDefuser))
			{
				EnableMessageHooks();
			}
		}
	}

	DETOUR_STATIC_CALL(C_ClientCommand)(pEdict);

	TriggeredFromCommand = false;
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

void OnEmitSound(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch)
{
	if (TriggeredFromCommand)
	{
		switch (CurrentItemId)
		{
			case CSI_NVGS:
			{
				auto client = ENTINDEX(entity);

				if (!MF_IsPlayerAlive(client) || MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), CSI_NVGS) <= 0)
				{
					g_pengfuncsTable->pfnEmitSound = nullptr;
					RETURN_META(MRES_IGNORED);
				}
			}
			case CSI_DEFUSER:
			case CSI_SHIELD:
			{
				g_pengfuncsTable->pfnEmitSound = nullptr;
				RETURN_META(MRES_SUPERCEDE);
			}
		}
	}

	RETURN_META(MRES_IGNORED);
}

bool OnMessageItemStatus(edict_t *pPlayer)
{
	if (TriggeredFromCommand && (CurrentItemId == CSI_DEFUSER || CurrentItemId == CSI_NVGS))
	{
		if (!g_pengfuncsTable->pfnEmitSound)
		{
			return true; // Block message
		}

		DisableMessageHooks();
	}

	return false;
}

bool OnMessageStatusIcon(edict_t *pPlayer)
{
	if (TriggeredFromCommand && CurrentItemId == CSI_DEFUSER)
	{
		auto client = ENTINDEX(pPlayer);

		if (MF_IsPlayerAlive(client) && MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), CSI_DEFUSER) > 0)
		{
			GET_OFFSET_NO_ERROR_RET("CBasePlayer", m_bHasDefuser);
			set_pdata<bool>(pPlayer, m_bHasDefuser, false);

			return true; // Block message
		}

		DisableMessageHooks();
	}

	return false;
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
	if (TriggeredFromCommand)
	{
		int client = TypeConversion.cbase_to_id(this);

		if (MF_IsPlayerAlive(client) && MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), static_cast<cell>(CurrentItemId)) > 0)
		{
			// Reset this to not call AddAccount() called right after.
			CurrentItemId = CSI_NONE;
			return;
		}
	}

	DETOUR_MEMBER_CALL(GiveNamedItem)(pszName);
}

DETOUR_DECL_MEMBER1(GiveShield, void, bool, bRetire) // void CBasePlayer::GiveShield(bool bRetire)
{
	if (TriggeredFromCommand && CurrentItemId == CSI_SHIELD)
	{
		int client = TypeConversion.cbase_to_id(this);

		if (MF_IsPlayerAlive(client) && MF_ExecuteForward(ForwardOnBuy, static_cast<cell>(client), CSI_SHIELD) > 0)
		{
			// If shield blocked, we need to hook EmitSound to block pickup sound played right after.
			g_pengfuncsTable->pfnEmitSound = OnEmitSound;

			// Reset this to not call AddAccount() called right after.
			CurrentItemId = CSI_NONE;
			return;
		}
	}

	DETOUR_MEMBER_CALL(GiveShield)(bRetire);
}

DETOUR_DECL_MEMBER2(AddAccount, void, int, amount, bool, bTrackChange) // void CBasePlayer::AddAccount(int amount, bool bTrackChange)
{
	if (TriggeredFromCommand)
	{
		switch (CurrentItemId)
		{
			case CSI_DEFUSER:
			{
				G_HL_TypeConversion.cbase_to_entvar(this)->body = 0;
				g_pengfuncsTable->pfnEmitSound = OnEmitSound; // To block pickup sound.
				EnableMessageHooks(); // To block ItemStatus
				break;
			}
			case CSI_NVGS:
			{
				GET_OFFSET_NO_ERROR("CBasePlayer", m_bHasNightVision);
				set_pdata<bool>(G_HL_TypeConversion.cbase_to_edict(this), m_bHasNightVision, false);
				EnableMessageHooks(); // To block ItemStatus
				break;
			}
			case CSI_NONE:
			{
				return;
			}
		}
	}

	DETOUR_MEMBER_CALL(AddAccount)(amount, bTrackChange);
}


void CtrlDetours_ClientCommand(bool set)
{
	if (set)
	{
		void *base = reinterpret_cast<void *>(MDLL_ClientCommand);

#if defined(WIN32)

		TypeDescription type;

		if (MainConfig->GetOffset("UseBotArgs", &type))
		{
			UseBotArgs = get_pdata<int*>(base, type.fieldOffset);
		}

		if (MainConfig->GetOffset("BotArgs", &type))
		{
			BotArgs = get_pdata<const char**>(base, type.fieldOffset);
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

		CommonConfig->GetOffsetByClass("CBasePlayer", "m_iTeam", &TeamDesc);
		CommonConfig->GetOffsetByClass("CBasePlayer", "m_iMenu", &MenuDesc);

		if (!ClientCommandDetour || !UseBotArgs || !BotArgs || !TeamDesc.fieldOffset || !MenuDesc.fieldOffset)
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
	}
}

void ToggleDetour_ClientCommands(bool enable)
{
	if (ClientCommandDetour)
	{
		(enable) ? ClientCommandDetour->EnableDetour() : ClientCommandDetour->DisableDetour();
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

		if (MainConfig->GetMemSig("GetWeaponInfo", &address)) // cs_get_weapon_info()
		{
			GetWeaponInfo = reinterpret_cast<GetWeaponInfoFunc>(address);
		}


		if (!CS_CreateNamedEntity)
		{
			MF_Log("CREATE_NAMED_ENITTY is not available - native cs_create_entity() has been disabled");
		}

		if (!CS_UTIL_FindEntityByString)
		{
			MF_Log("UTIL_FindEntByString is not available - native cs_find_ent_by_class() has been disabled");
		}

		if (!GetWeaponInfo)
		{
			MF_Log("GetWeaponInfo is not available - native cs_get_weapon_info() has been disabled");
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

void InitGlobalVars()
{
	void *address = nullptr;

#if defined(WIN32)

	TypeDescription typeDesc;

	if (CommonConfig->GetOffset("svs", &typeDesc))
	{
		uintptr_t base = *reinterpret_cast<uintptr_t*>(reinterpret_cast<byte*>(g_engfuncs.pfnGetCurrentPlayer) + typeDesc.fieldOffset);
		ServerStatic = reinterpret_cast<server_static_t*>(base - 4);
	}

	if (CommonConfig->GetAddress("sv", &address))
	{
		Server = *reinterpret_cast<server_t**>(address);
	}
#else
	if (CommonConfig->GetMemSig("svs", &address))
	{
		ServerStatic = reinterpret_cast<server_static_t*>(address);
	}

	if (CommonConfig->GetMemSig("sv", &address))
	{
		Server = reinterpret_cast<server_t*>(address);
	}
#endif

	if (!ServerStatic)
	{
		MF_Log("svs global variable is not available");
	}

	if (!Server)
	{
		MF_Log("sv global variable is not available");
	}
}
