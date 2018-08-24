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
#include "CstrikeItemsInfos.h"
#include <resdk/mod_rehlds_api.h>
#include <resdk/mod_regamedll_api.h>

int ForwardInternalCommand = -1;
int ForwardOnBuy           = -1;
int ForwardOnBuyAttempt    = -1;

bool HasInternalCommandForward;
bool HasOnBuyAttemptForward;
bool HasOnBuyForward;

int *UseBotArgs;
const char **BotArgs;

CDetour *ClientCommandDetour;
CDetour *GiveShieldDetour;
CDetour *GiveNamedItemDetour;
CDetour *AddAccountDetour;
CDetour *CanPlayerBuyDetour;
CDetour *CanBuyThisDetour;
CDetour *GiveDefaultItemsDetour;
CDetour *BuyGunAmmoDetour;

CreateNamedEntityFunc       CS_CreateNamedEntity;
UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;
GetWeaponInfoFunc           GetWeaponInfo;
AddEntityHashValueFunc      AddEntityHashValue;
RemoveEntityHashValueFunc   RemoveEntityHashValue;

int CurrentItemId;
bool TriggeredFromCommand;
bool BlockMoneyUpdate;
bool BlockAmmosUpdate;

// CBasePlayer members.
TypeDescription TeamDesc;
TypeDescription MenuDesc;
TypeDescription NvgsDesc;
TypeDescription DefuserDesc;
TypeDescription SignalsDesc;
TypeDescription MoneyDesc;

// GameRules members.
TypeDescription BombTargetDesc;

// Engine global variables.
server_static_t *ServerStatic;
server_t *Server;

// Mod global variable
void **GameRules;
void *GameRulesRH;

bool HasReHlds;
bool HasReGameDll;

bool HasRestricteditem_Enabled;
bool InternalCommand_Enabled;
bool GiveDefaultItems_Enabled;

void InitializeHacks()
{
	HasReHlds    = RehldsApi_Init();
	HasReGameDll = RegamedllApi_Init();

	CtrlDetours_ClientCommand(true);
	CtrlDetours_BuyCommands(true);
	CtrlDetours_Natives(true);

	InitFuncsAddresses();
	InitClassMembers();
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
	if (HasReGameDll)
	{
		return ReGameFuncs->Cmd_Argv(i);
	}
	else if (*UseBotArgs)
	{
		if (i < 4)
		{
			return BotArgs[i];
		}

		return nullptr;
	}

	return g_engfuncs.pfnCmd_Argv(i);
}

void (*C_ClientCommand_Actual)(edict_t *) = nullptr;

void ClientCommand_Custom(edict_t *pEdict, const char *command, const char *arg1, IReGameHook_InternalCommand *chain = nullptr)
{
	auto client = TypeConversion.edict_to_id(pEdict);

	CurrentItemId = CSI_NONE;

	if (MF_IsPlayerAlive(client))
	{
		// Purpose is to retrieve an item id based on alias name or selected item from menu,
		// to be used in CS_OnBuy* forwards.
		if ((HasOnBuyAttemptForward || HasOnBuyForward) && command && *command)
		{
			// Handling buy via menu.
			if (!strcmp(command, "menuselect"))
			{
				auto slot = atoi(arg1);

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

					auto menuId = get_pdata<int>(pEdict, MenuDesc.fieldOffset);

					if (menuId >= Menu_Buy && menuId <= Menu_BuyItem)
					{
						switch (get_pdata<int>(pEdict, TeamDesc.fieldOffset))
						{
							case TEAM_T: CurrentItemId = menuItemsTe[menuId - 4][slot]; break; // -4 because array is zero-based and Menu_Buy* constants starts from 4.
							case TEAM_CT:CurrentItemId = menuItemsCt[menuId - 4][slot]; break;
						}
					}
				}
			}
			else // Handling buy via alias
			{
				if (get_pdata<CUnifiedSignals>(pEdict, SignalsDesc.fieldOffset).GetState() & SIGNAL_BUY) // Are we inside the buy zone?
				{
					AliasInfo info;
					char commandLowered[32];

					UTIL_StringToLower(command, commandLowered, sizeof(commandLowered));

					if (ItemsManager.GetAliasInfos(commandLowered, &info))
					{
						CurrentItemId = info.itemid;
					}
				}
			}
		}

		if (HasInternalCommandForward && (HasReGameDll || *UseBotArgs) && MF_ExecuteForward(ForwardInternalCommand, client, CMD_ARGV(0)) > 0)
		{
			return;
		}

		if (HasOnBuyAttemptForward && CurrentItemId && MF_ExecuteForward(ForwardOnBuyAttempt, client, CurrentItemId) > 0)
		{
			return;
		}
	}

	TriggeredFromCommand = CurrentItemId != CSI_NONE;

	chain ? chain->callNext(pEdict, command, arg1) : C_ClientCommand_Actual(pEdict);

	TriggeredFromCommand = BlockMoneyUpdate = BlockAmmosUpdate = false;
}

void C_ClientCommand(edict_t* pEdict) // void ClientCommand(edict_t *pEntity)
{
	ClientCommand_Custom(pEdict, CMD_ARGV(0), CMD_ARGV(1));
}

void InternalCommand_RH(IReGameHook_InternalCommand* chain, edict_t *pEdict, const char *command, const char *arg1)
{
	ClientCommand_Custom(pEdict, CMD_ARGV(0), CMD_ARGV(1), chain);
}

edict_s* OnCreateNamedEntity(int classname)
{
	if (NoKnivesMode)
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

	RETURN_META_VALUE(MRES_IGNORED, nullptr);
}

DETOUR_DECL_MEMBER0(GiveDefaultItems, void)  // void CBasePlayer::GiveDefaultItems(void)
{
	if (NoKnivesMode)
	{
		g_pengfuncsTable->pfnCreateNamedEntity = OnCreateNamedEntity;
	}

	DETOUR_MEMBER_CALL(GiveDefaultItems)();

	g_pengfuncsTable->pfnCreateNamedEntity = nullptr;
}

void GiveDefaultItems_RH(IReGameHook_CBasePlayer_GiveDefaultItems *chain, class CBasePlayer *pPlayer)
{
	if (NoKnivesMode)
	{
		g_pengfuncsTable->pfnCreateNamedEntity = OnCreateNamedEntity;
	}

	chain->callNext(pPlayer);

	g_pengfuncsTable->pfnCreateNamedEntity = nullptr;
}

DETOUR_DECL_MEMBER1(CanPlayerBuy, bool, bool, display)  // bool CBasePlayer::CanPlayerBuy(bool display)
{
	auto canBuy = DETOUR_MEMBER_CALL(CanPlayerBuy)(display);

	if (!canBuy || !TriggeredFromCommand || !(CurrentItemId == CSI_NVGS || CurrentItemId == CSI_DEFUSER))
	{
		return canBuy;
	}

	auto pPlayer  = TypeConversion.cbase_to_edict(this);
	auto playerId = TypeConversion.edict_to_id(pPlayer);

	if (!MF_IsPlayerAlive(playerId))
	{
		return canBuy;
	}

	auto allowedToBuy = false;
	auto itemPrice = ItemsManager.GetItemPrice(CurrentItemId);

	switch (CurrentItemId)
	{
		case CSI_NVGS:
		{
			allowedToBuy = !get_pdata<bool>(pPlayer, NvgsDesc.fieldOffset) &&
							get_pdata<int>(pPlayer, MoneyDesc.fieldOffset) >= itemPrice;
			break;
		}
		case CSI_DEFUSER:
		{
			allowedToBuy = !get_pdata<bool>(pPlayer, DefuserDesc.fieldOffset)        &&
							get_pdata<int>(pPlayer, TeamDesc.fieldOffset) == TEAM_CT &&
							get_pdata<bool>(*GameRules, BombTargetDesc.fieldOffset)  &&
							get_pdata<int>(pPlayer, MoneyDesc.fieldOffset) >= itemPrice;
			break;
		}
	}

	if (allowedToBuy && MF_ExecuteForward(ForwardOnBuy, playerId, CurrentItemId) > 0)
	{
		canBuy = false;
	}

	return canBuy;
}

DETOUR_DECL_STATIC2(CanBuyThis, bool, void*, pvPlayer, int, weaponId) // bool CanBuyThis(CBasePlayer *pPlayer, int weaponId)
{
	auto canBuy = DETOUR_STATIC_CALL(CanBuyThis)(pvPlayer, weaponId);

	if (!canBuy || !TriggeredFromCommand || !((1 << CurrentItemId & CSI_ALL_GUNS) || CurrentItemId == CSI_SHIELD))
	{
		return canBuy;
	}

	auto playerId = TypeConversion.cbase_to_id(pvPlayer);

	if (MF_IsPlayerAlive(playerId) && get_pdata<int>(pvPlayer, MoneyDesc.fieldOffset) >= ItemsManager.GetItemPrice(CurrentItemId))
	{
		if (MF_ExecuteForward(ForwardOnBuy, playerId, CurrentItemId) > 0)
		{
			canBuy = false;
		}
	}

	return canBuy;
}

DETOUR_DECL_STATIC3(BuyGunAmmo, bool, void*, player, int, nSlot, bool, bBlinkMoney) // bool BuyGunAmmo(CBasePlayer *player, int nSlot, bool bBlinkMoney)
{
	auto result = DETOUR_STATIC_CALL(BuyGunAmmo)(player, nSlot, bBlinkMoney);

	if (result && BlockAmmosUpdate)
	{
		BlockAmmosUpdate = false;
		return false;
	}

	return result;
}

DETOUR_DECL_MEMBER1(GiveNamedItem, void, const char*, pszName) // void CBasePlayer::GiveNamedItem(const char *pszName)
{
	if (TriggeredFromCommand)
	{
		switch (CurrentItemId)
		{
			case CSI_VEST:
			case CSI_VESTHELM:
			case CSI_FLASHBANG:
			case CSI_HEGRENADE:
			case CSI_SMOKEGRENADE:
			case CSI_PRIAMMO:
			case CSI_SECAMMO:
			{
				auto playerId = TypeConversion.cbase_to_id(this);

				if (MF_IsPlayerAlive(playerId) && MF_ExecuteForward(ForwardOnBuy, playerId, CurrentItemId) > 0)
				{
					BlockAmmosUpdate = CurrentItemId == CSI_PRIAMMO || CurrentItemId == CSI_SECAMMO;
					BlockMoneyUpdate = true;
					return;
				}
			}
		}
	}

	DETOUR_MEMBER_CALL(GiveNamedItem)(pszName);
}

DETOUR_DECL_MEMBER2(AddAccount, void, int, amount, bool, bTrackChange) // void CBasePlayer::AddAccount(int amount, bool bTrackChange)
{
	if (BlockMoneyUpdate)
	{
		BlockMoneyUpdate = false;
		return;
	}

	DETOUR_MEMBER_CALL(AddAccount)(amount, bTrackChange);
}

bool CBasePlayer_HasRestrictItem_RH(IReGameHook_CBasePlayer_HasRestrictItem *chain, class CBasePlayer *pPlayer, ItemID item, ItemRestType type)
{
	if (type == ITEM_TYPE_BUYING && CurrentItemId != CSI_NONE)
	{
		auto player = TypeConversion.cbase_to_id(pPlayer);

		if (MF_IsPlayerAlive(player) && MF_ExecuteForward(ForwardOnBuy, player, CurrentItemId) > 0)
		{
			return true;
		}
	}

	return chain->callNext(pPlayer, item, type);
}

bool BuyGunAmmo_RH(IReGameHook_BuyGunAmmo *chain, class CBasePlayer *pPlayer, class CBasePlayerItem *pWeapon, bool blinkMoney)
{
	if (CurrentItemId == CSI_PRIAMMO || CurrentItemId == CSI_SECAMMO)
	{
		auto player = TypeConversion.cbase_to_id(pPlayer);

		if (MF_IsPlayerAlive(player) && MF_ExecuteForward(ForwardOnBuy, player, CurrentItemId) > 0)
		{
			return false;
		}
	}

	return chain->callNext(pPlayer, pWeapon, blinkMoney);
}

void ToggleDetour(CDetour *detour, bool enable)
{
	if (detour)
	{
		(enable) ? detour->EnableDetour() : detour->DisableDetour();
	}
}

void DestroyDetour(CDetour *&detour)
{
	if (detour)
	{
		detour->Destroy();
		detour = nullptr;
	}
}


void CtrlDetours_ClientCommand(bool set)
{
	if (set)
	{
		if (HasReGameDll)
		{
			if (!InternalCommand_Enabled)
			{
				ReGameHookchains->InternalCommand()->registerHook(InternalCommand_RH);
				InternalCommand_Enabled = true;
			}
		}
		else
		{
			auto base = reinterpret_cast<void *>(MDLL_ClientCommand);

#if defined(KE_WINDOWS)

			TypeDescription type;

			if (MainConfig->GetOffset("UseBotArgs", &type))
			{
				UseBotArgs = get_pdata<decltype(UseBotArgs)>(base, type.fieldOffset);
			}

			if (MainConfig->GetOffset("BotArgs", &type))
			{
				BotArgs = get_pdata<decltype(BotArgs)>(base, type.fieldOffset);
			}
#else
			void *address = nullptr;

			if (MainConfig->GetMemSig("UseBotArgs", &address))
			{
				UseBotArgs = reinterpret_cast<decltype(UseBotArgs)>(address);
			}

			if (MainConfig->GetMemSig("BotArgs", &address))
			{
				BotArgs = reinterpret_cast<decltype(BotArgs)>(address);
			}
#endif
			ClientCommandDetour = DETOUR_CREATE_STATIC_FIXED(C_ClientCommand, base);

			if (!ClientCommandDetour)
			{
				MF_Log("ClientCommand is not available - forwards CS_InternalCommand and CS_OnBuy[Attempt] have been disabled");
				ToggleHook_ClientCommands(false);
			}
			else if (!UseBotArgs || !BotArgs)
			{
				MF_Log("UseBotArgs or BotArgs is not available - forward CS_InternalCommand has been disabled");
			}
		}	
	}
	else
	{
		if (HasReGameDll)
		{
			ReGameHookchains->InternalCommand()->unregisterHook(InternalCommand_RH);
			InternalCommand_Enabled = false;
		}
		else
		{
			DestroyDetour(ClientCommandDetour);
		}
	}
}

void ToggleHook_ClientCommands(bool enable)
{
	if (HasReGameDll)
	{
		CtrlDetours_ClientCommand(enable);
	}
	else
	{
		ToggleDetour(ClientCommandDetour, enable);
	}
}

void CtrlDetours_BuyCommands(bool set)
{
	if (set)
	{
		if (HasReGameDll)
		{
			if (!HasRestricteditem_Enabled)
			{
				ReGameHookchains->CBasePlayer_HasRestrictItem()->registerHook(CBasePlayer_HasRestrictItem_RH);
				ReGameHookchains->BuyGunAmmo()->registerHook(BuyGunAmmo_RH);
				HasRestricteditem_Enabled = true;
			}
		}
		else
		{
			void *address = nullptr;

			if (MainConfig->GetMemSig("BuyGunAmmo", &address))
			{
				BuyGunAmmoDetour = DETOUR_CREATE_STATIC_FIXED(BuyGunAmmo, address);
			}

			if (MainConfig->GetMemSig("GiveNamedItem", &address))
			{
				GiveNamedItemDetour = DETOUR_CREATE_MEMBER_FIXED(GiveNamedItem, address);
			}

			if (MainConfig->GetMemSig("AddAccount", &address))
			{
				AddAccountDetour = DETOUR_CREATE_MEMBER_FIXED(AddAccount, address);
			}

			if (MainConfig->GetMemSig("CanPlayerBuy", &address))
			{
				CanPlayerBuyDetour = DETOUR_CREATE_MEMBER_FIXED(CanPlayerBuy, address);
			}

			if (MainConfig->GetMemSig("CanBuyThis", &address))
			{
				CanBuyThisDetour = DETOUR_CREATE_STATIC_FIXED(CanBuyThis, address);
			}

			if (!BuyGunAmmoDetour || !GiveNamedItemDetour || !AddAccountDetour || !CanPlayerBuyDetour || !CanBuyThisDetour)
			{
				if (!BuyGunAmmoDetour)
				{
					MF_Log("BuyGunAmmo is not available");
				}

				if (!GiveNamedItemDetour)
				{
					MF_Log("GiveNamedItem is not available");
				}

				if (!AddAccountDetour)
				{
					MF_Log("AddAccount is not available");
				}

				if (!CanPlayerBuyDetour)
				{
					MF_Log("CanPlayerBuy is not available");
				}

				if (!CanBuyThisDetour)
				{
					MF_Log("CanBuyThis is not available");
				}

				MF_Log("Some functions are not available - forwards CS_OnBuy[Attempt] have been disabled");
				ToggleHook_BuyCommands(false);
			}
		}
	}
	else
	{
		if (HasReGameDll)
		{
			ReGameHookchains->CBasePlayer_HasRestrictItem()->unregisterHook(CBasePlayer_HasRestrictItem_RH);
			ReGameHookchains->BuyGunAmmo()->unregisterHook(BuyGunAmmo_RH);
			HasRestricteditem_Enabled = false;
		}
		else
		{
			DestroyDetour(BuyGunAmmoDetour);
			DestroyDetour(GiveNamedItemDetour);
			DestroyDetour(AddAccountDetour);
			DestroyDetour(CanPlayerBuyDetour);
			DestroyDetour(CanBuyThisDetour);
		}
	}
}

void ToggleHook_BuyCommands(bool enable)
{
	if (HasReGameDll)
	{
		CtrlDetours_BuyCommands(enable);
	}
	else
	{
		ToggleDetour(BuyGunAmmoDetour, enable);
		ToggleDetour(GiveNamedItemDetour, enable);
		ToggleDetour(AddAccountDetour, enable);
		ToggleDetour(CanPlayerBuyDetour, enable);
		ToggleDetour(CanBuyThisDetour, enable);
	}
}


void CtrlDetours_Natives(bool set)
{
	if (set)
	{
		if (HasReGameDll)
		{
			if (!GiveDefaultItems_Enabled)
			{
				ReGameHookchains->CBasePlayer_GiveDefaultItems()->registerHook(GiveDefaultItems_RH);
				GiveDefaultItems_Enabled = true;
			}
		}
		else
		{
			void *address = nullptr;

			if (MainConfig->GetMemSig("GiveDefaultItems", &address))
			{
				GiveDefaultItemsDetour = DETOUR_CREATE_MEMBER_FIXED(GiveDefaultItems, address);
			}

			if (!GiveDefaultItemsDetour)
			{
				MF_Log("GiveDefaultItems is not available - native cs_set_no_knives has been disabled");
			}
		}
	}
	else
	{
		if (HasReGameDll)
		{
			ReGameHookchains->CBasePlayer_GiveDefaultItems()->unregisterHook(GiveDefaultItems_RH);
			GiveDefaultItems_Enabled = false;
		}
		else
		{
			DestroyDetour(GiveDefaultItemsDetour);
		}
	}
}

void ToggleHook_GiveDefaultItems(bool enable)
{
	if (HasReGameDll)
	{
		CtrlDetours_Natives(enable);
	}
	else
	{
		ToggleDetour(GiveDefaultItemsDetour, enable);
	}
}


void InitFuncsAddresses()
{
	if (HasReGameDll)
	{
		RemoveEntityHashValue      = ReGameFuncs->RemoveEntityHashValue;
		CS_CreateNamedEntity       = ReGameFuncs->CREATE_NAMED_ENTITY2;
		CS_UTIL_FindEntityByString = ReGameFuncs->UTIL_FindEntityByString;
		AddEntityHashValue         = ReGameFuncs->AddEntityHashValue;
	}
	else
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

		if (MainConfig->GetMemSig("AddEntityHashValue", &address)) // cs_set_ent_class()
		{
			AddEntityHashValue = reinterpret_cast<AddEntityHashValueFunc>(address);
		}

		if (MainConfig->GetMemSig("RemoveEntityHashValue", &address)) // cs_set_ent_class()
		{
			RemoveEntityHashValue = reinterpret_cast<RemoveEntityHashValueFunc>(address);
		}
	}

	if (!CS_CreateNamedEntity)
	{
		MF_Log("CREATE_NAMED_ENITTY is not available - native cs_create_entity() has been disabled");
	}

	if (!CS_UTIL_FindEntityByString)
	{
		MF_Log("UTIL_FindEntByString is not available - native cs_find_ent_by_class() has been disabled");
	}

	if (!AddEntityHashValue || !RemoveEntityHashValue)
	{
		MF_Log("AddEntityHashValue or RemoveEntityHashValue is not available - native cs_set_ent_class() has been disabled");
	}

	if (!HasReGameDll && !GetWeaponInfo)
	{
		MF_Log("GetWeaponInfo is not available - native cs_get_weapon_info() and forward CS_OnBuy have been disabled");
		CtrlDetours_BuyCommands(false);
	}
}

void InitClassMembers()
{
	// CBasePlayer members.
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_iTeam"          , &TeamDesc   );
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_iMenu"          , &MenuDesc   );
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_bHasNightVision", &NvgsDesc   );
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_bHasDefuser"    , &DefuserDesc);
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_signals"        , &SignalsDesc);
	CommonConfig->GetOffsetByClass("CBasePlayer", "m_iAccount"       , &MoneyDesc  );

	// GameRules members.
	CommonConfig->GetOffsetByClass("CHalfLifeMultiplay", "m_bMapHasBombTarget", &BombTargetDesc);

	if (!TeamDesc.fieldOffset    ||
		!MenuDesc.fieldOffset    ||
		!NvgsDesc.fieldOffset    ||
		!DefuserDesc.fieldOffset ||
		!SignalsDesc.fieldOffset ||
		!MoneyDesc.fieldOffset   ||
		!BombTargetDesc.fieldOffset)
	{
		MF_Log("Invalid or missing entity gamedata files - forwards CS_OnBuy[Attempt] have been disabled");
		ToggleHook_BuyCommands(false);
	}
}

CGameRules* InstallGameRules(IReGameHook_InstallGameRules *chain)
{
	GameRulesRH = chain->callNext();
	return static_cast<CGameRules*>(GameRulesRH);
}

void InitGlobalVars()
{
	void *address = nullptr;

	if (!HasReHlds)
	{
#if defined(KE_WINDOWS)
		TypeDescription typeDesc;

		if (CommonConfig->GetOffset("svs", &typeDesc))
		{
			uintptr_t base = *reinterpret_cast<uintptr_t*>(reinterpret_cast<byte*>(g_engfuncs.pfnGetCurrentPlayer) + typeDesc.fieldOffset);
			ServerStatic = reinterpret_cast<decltype(ServerStatic)>(base - 4);
		}

		if (CommonConfig->GetAddress("sv", &address))
		{
			Server = *reinterpret_cast<decltype(Server)*>(address);
		}
#else
		if (CommonConfig->GetMemSig("svs", &address))
		{
			ServerStatic = reinterpret_cast<decltype(ServerStatic)>(address);
		}

		if (CommonConfig->GetMemSig("sv", &address))
		{
			Server = reinterpret_cast<decltype(Server)>(address);
		}
#endif
	}

	if (HasReGameDll)
	{
		ReGameHookchains->InstallGameRules()->registerHook(InstallGameRules);
	}
	else
	{
#if defined(KE_WINDOWS)
		if (CommonConfig->GetAddress("g_pGameRules", &address))
		{
			GameRules = *reinterpret_cast<decltype(GameRules)*>(address);
		}
#else
		if (CommonConfig->GetMemSig("g_pGameRules", &address))
		{
			GameRules = reinterpret_cast<decltype(GameRules)>(address);
		}
#endif
	}

	if (!HasReHlds)
	{
		if (!ServerStatic)
		{
			MF_Log("svs global variable is not available");
		}

		if (!Server)
		{
			MF_Log("sv global variable is not available");
		}
	}
	
	if (!HasReGameDll)
	{
		if (!GameRules)
		{
			MF_Log("g_pGameRules is not available - Forward CS_OnBuy has been disabled");
			CtrlDetours_BuyCommands(false);
		}
	}
}
