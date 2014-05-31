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
#include <MemoryUtils.h>
#include "CDetour/detours.h"

#if defined(__APPLE__)
	#include <mach-o/nlist.h>
#endif

void CtrlDetours(bool set);

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
	CtrlDetours(true);
}

void ShutdownHacks()
{
	CtrlDetours(false);
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


void CtrlDetours(bool set)
{
#if defined AMD64
	#error UNSUPPORTED
#endif

	if (set)
	{
		char libName[256];
		uintptr_t base;

		void *target = (void *)MDLL_ClientCommand;
		
		if (!g_MemUtils.GetLibraryOfAddress(target, libName, sizeof(libName), &base))
		{
			return;
		}

		void *canBuyThisAddress = NULL;
		void *buyItemAddress	= NULL;
		void *buyGunAmmoAddress = NULL;

#if defined(WIN32)

		canBuyThisAddress	= g_MemUtils.DecodeAndFindPattern(target, CS_SIG_CANBUYTHIS);
		buyItemAddress		= g_MemUtils.DecodeAndFindPattern(target, CS_SIG_BUYITEM);
		buyGunAmmoAddress	= g_MemUtils.DecodeAndFindPattern(target, CS_SIG_BUYGUNAMMO);

		g_UseBotArgs = *(int **)((unsigned char *)target + CS_CLICMD_OFFS_USEBOTARGS);
		g_BotArgs = (const char **)*(const char **)((unsigned char *)target + CS_CLICMD_OFFS_BOTARGS);

#elif defined(__linux__) || defined(__APPLE__)

		Dl_info info; 
		void *handle = NULL;

		if (dladdr(target, &info) == 0) || (handle = dlopen(info.dli_fname, RTLD_NOW)) == NULL)
		{
			return;
		}

		canBuyThisAddress	= g_MemUtils.ResolveSymbol(handle, CS_SYM_CANBUYTHIS);
		buyItemAddress		= g_MemUtils.ResolveSymbol(handle, CS_SYM_BUYITEM);
		buyGunAmmoAddress	= g_MemUtils.ResolveSymbol(handle, CS_SYM_BUYGUNAMMO);

		g_UseBotArgs = (int *)g_MemUtils.ResolveSymbol(handle, CS_SYM_USEBOTARGS);
		g_BotArgs = (const char **)g_MemUtils.ResolveSymbol(handle, CS_SYM_BOTARGS);

		dlclose(handle);

#endif
		g_ClientCommandDetour	= DETOUR_CREATE_STATIC_FIXED(C_ClientCommand, target);
		g_CanBuyThisDetour		= DETOUR_CREATE_STATIC_FIXED(CanBuyThis, canBuyThisAddress);
		g_BuyItemDetour			= DETOUR_CREATE_STATIC_FIXED(BuyItem, buyItemAddress);
		g_BuyGunAmmoDetour		= DETOUR_CREATE_STATIC_FIXED(BuyGunAmmo, buyGunAmmoAddress);
		
		if (g_ClientCommandDetour != NULL)
			g_ClientCommandDetour->EnableDetour();
		else
		{
			MF_Log("No Client Commands detours could be initialized - Disabled Client Command forward.");
		}

		if (g_CanBuyThisDetour != NULL && g_BuyItemDetour != NULL && g_BuyGunAmmoDetour != NULL)
		{
			g_CanBuyThisDetour->EnableDetour();
			g_BuyItemDetour->EnableDetour();
			g_BuyGunAmmoDetour->EnableDetour();
		}
		else
		{
			MF_Log("No Buy Commands detours could be initialized - Disabled Buy forward.");
		}
	}
	else
	{
		g_CanBuyThisDetour->Destroy();
		g_BuyItemDetour->Destroy();
		g_BuyGunAmmoDetour->Destroy();
		g_ClientCommandDetour->Destroy();
	}
}
