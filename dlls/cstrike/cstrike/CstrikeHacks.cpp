/* AMX Mod X
 *   Counter-Strike Module
 *
 * by the AMX Mod X Development Team
 *
 * This file is part of AMX Mod X.
 *
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#include "CstrikeDatas.h"
#include "CstrikeUtils.h"
#include "CDetour/detours.h"

void CtrlDetours_ClientCommand(bool set);
void CtrlDetours_BuyCommands(bool set);

int g_CSCliCmdFwd = -1;
int g_CSBuyCmdFwd = -1;

int *g_UseBotArgs = NULL;
const char **g_BotArgs = NULL;

CDetour *g_ClientCommandDetour = NULL;
CDetour *g_CanBuyThisDetour = NULL;
CDetour *g_BuyItemDetour = NULL;
CDetour *g_BuyGunAmmoDetour = NULL;


void InitializeHacks()
{
#if defined AMD64
	#error UNSUPPORTED
#endif

	CtrlDetours_ClientCommand(true);
	CtrlDetours_BuyCommands(true);
}

void ShutdownHacks()
{
	CtrlDetours_ClientCommand(false);
	CtrlDetours_BuyCommands(false);
}


DETOUR_DECL_STATIC1(C_ClientCommand, void, edict_t*, pEdict) // void ClientCommand(edict_t *pEntity)
{
	if (*g_UseBotArgs)
	{
		int client = ENTINDEX(pEdict);
		const char *args = *g_BotArgs;

		if (MF_ExecuteForward(g_CSCliCmdFwd, static_cast<cell>(client), args) > 0)
		{
			return;
		}
	}

	DETOUR_STATIC_CALL(C_ClientCommand)(pEdict);
}

DETOUR_DECL_STATIC2(CanBuyThis, bool, void*, pvPlayer, int, weaponId) // bool CanBuyThis(CBasePlayer *pPlayer, int weaponId)
{
	if (weaponId != CSI_SHIELDGUN) // This will be handled before with BuyItem. Avoiding duplicated call.
	{
		int player = PrivateToIndex(pvPlayer);

		if (MF_IsPlayerAlive(player) && MF_ExecuteForward(g_CSBuyCmdFwd, static_cast<cell>(player), static_cast<cell>(weaponId)) > 0)
		{
			return false;
		}
	}
	
	return DETOUR_STATIC_CALL(CanBuyThis)(pvPlayer, weaponId);
}

DETOUR_DECL_STATIC2(BuyItem, void, void*, pvPlayer, int, iSlot) // void BuyItem(CBasePlayer *pPlayer, int iSlot)
{
	int player = PrivateToIndex(pvPlayer);

	if (MF_IsPlayerAlive(player))
	{
		static const int itemSlotToWeaponId[] = {-1, CSI_VEST, CSI_VESTHELM, CSI_FLASHBANG, CSI_HEGRENADE, CSI_SMOKEGRENADE, CSI_NVGS, CSI_DEFUSER, CSI_SHIELDGUN};

		if (iSlot >= 1 && iSlot <= 8 && MF_ExecuteForward(g_CSBuyCmdFwd, static_cast<cell>(player), static_cast<cell>(itemSlotToWeaponId[iSlot])) > 0)
		{
			return;
		}
	}

	DETOUR_STATIC_CALL(BuyItem)(pvPlayer, iSlot);
}

DETOUR_DECL_STATIC3(BuyGunAmmo, bool, void*, pvPlayer, void*, pvWeapon, bool, bBlinkMoney) // bool BuyGunAmmo(CBasePlayer *player, CBasePlayerItem *weapon, bool bBlinkMoney)
{
	int player = PrivateToIndex(pvPlayer);

	if (MF_IsPlayerAlive(player))
	{
		edict_t *pWeapon = PrivateToEdict(pvWeapon);

		if (pWeapon)
		{
			int weaponId = *((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
			int ammoId = (1<<weaponId & BITS_PISTOLS) ? CSI_SECAMMO : CSI_PRIMAMMO;

			if (MF_ExecuteForward(g_CSBuyCmdFwd, static_cast<cell>(player), static_cast<cell>(ammoId)) > 0)
			{
				return false;
			}
		}
	}

	return DETOUR_STATIC_CALL(BuyGunAmmo)(pvPlayer, pvWeapon, bBlinkMoney);
}


void CtrlDetours_ClientCommand(bool set)
{
	if (set)
	{
		void *target = (void *)MDLL_ClientCommand;

#if defined(WIN32)

		g_UseBotArgs = *(int **)((unsigned char *)target + CS_CLICMD_OFFS_USEBOTARGS);
		g_BotArgs = (const char **)*(const char **)((unsigned char *)target + CS_CLICMD_OFFS_BOTARGS);

#elif defined(__linux__) || defined(__APPLE__)

		g_UseBotArgs = (int *)UTIL_FindAddressFromEntry(CS_IDENT_USEBOTARGS, CS_IDENT_HIDDEN_STATE);
		g_BotArgs = (const char **)UTIL_FindAddressFromEntry(CS_IDENT_BOTARGS, CS_IDENT_HIDDEN_STATE);

#endif
		g_ClientCommandDetour = DETOUR_CREATE_STATIC_FIXED(C_ClientCommand, target);

		if (g_ClientCommandDetour == NULL)
		{
			MF_Log("No Client Commands detour could be initialized - Disabled Client Command forward.");
		}
	}
	else
	{
		if (g_ClientCommandDetour) 
			g_ClientCommandDetour->Destroy();
	}
}

void ToggleDetour_ClientCommands(bool enable)
{
	if (g_ClientCommandDetour)
		(enable) ? g_ClientCommandDetour->EnableDetour() : g_ClientCommandDetour->DisableDetour();
}


void CtrlDetours_BuyCommands(bool set)
{
	if (set)
	{
		void *canBuyThisAddress = UTIL_FindAddressFromEntry(CS_IDENT_CANBUYTHIS, CS_IDENT_HIDDEN_STATE);
		void *buyItemAddress	= UTIL_FindAddressFromEntry(CS_IDENT_BUYITEM, CS_IDENT_HIDDEN_STATE);
		void *buyGunAmmoAddress = UTIL_FindAddressFromEntry(CS_IDENT_BUYGUNAMMO, CS_IDENT_HIDDEN_STATE);

		g_CanBuyThisDetour	= DETOUR_CREATE_STATIC_FIXED(CanBuyThis, canBuyThisAddress);
		g_BuyItemDetour		= DETOUR_CREATE_STATIC_FIXED(BuyItem, buyItemAddress);
		g_BuyGunAmmoDetour	= DETOUR_CREATE_STATIC_FIXED(BuyGunAmmo, buyGunAmmoAddress);

		if (g_CanBuyThisDetour == NULL || g_BuyItemDetour == NULL || g_BuyGunAmmoDetour == NULL)
		{
			MF_Log("No Buy Commands detours could be initialized - Disabled Buy forward.");
		}
	}
	else
	{
		if (g_CanBuyThisDetour)
			g_CanBuyThisDetour->Destroy();

		if (g_BuyItemDetour)
			g_BuyItemDetour->Destroy();

		if (g_BuyGunAmmoDetour)
			g_BuyGunAmmoDetour->Destroy();
	}
}

void ToggleDetour_BuyCommands(bool enable)
{
	if (g_CanBuyThisDetour)
		(enable) ? g_CanBuyThisDetour->EnableDetour() : g_CanBuyThisDetour->DisableDetour();

	if (g_BuyItemDetour)
		(enable) ? g_BuyItemDetour->EnableDetour() : g_BuyItemDetour->DisableDetour();

	if (g_BuyGunAmmoDetour)
		(enable) ? g_BuyGunAmmoDetour->EnableDetour() : g_BuyGunAmmoDetour->DisableDetour();
}