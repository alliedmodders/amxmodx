// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017 SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxxmodule.h"
#include "weapinfo.h"

Weapon WeaponsList[] = {
	{ "Kung Fu", "kung_fu", {}, 1 },
	{ "Glock-18", "glock-18", { 50, 80, (1 << 8) }},
	{ "Beretta 92F", "beretta", { 50, 94, (1 << 8) }},
	{ "Mini-Uzi", "mini-uzi", { 50, 108, (1 << 8) }},
	{ "BENELLI-M3", "benelli_m3", { 52, 122, (1 << 8) + (1 << 9) }},
	{ "M4A1", "m4a1", { 53, 136, (1 << 10) }},
	{ "MP5SD", "mp5sd", { 50, 150, (1 << 8) }},
	{ "MP5K", "mp5k", { 50, 164, (1 << 8) }},
	{ "Akimbo Berettas", "akimbo_berettas", { 50, 178, (1 << 8) }},
	{ "SOCOM-MK23", "socom-mk23", { 51, 192, (1 << 9) }},
	{ "Akimbo MK23", "akimbo_mk23", { 51, 192, (1 << 9) }},
	{ "USAS-12", "usas-12", { 52, 220, (1 << 9) + (1 << 8) }},
	{ "Desert Eagle", "desert_eagle", { 54, 234, (1 << 10) + (1 << 8) }},
	{ "AK47", "ak47", { 55, 248, (1 << 10) + (1 << 9) }},
	{ "Five-seveN", "five-seven", { 56, 262, (1 << 10) + (1 << 9) + (1 << 8) }},
	{ "STEYR-AUG", "steyr-aug", { 53, 276, (1 << 10) }},
	{ "Akimbo Mini-Uzi", "akimbo_mini-uzi", { 50, 290, (1 << 8) }},
	{ "Skorpion", "skorpion", { 50, 304, (1 << 10) + (1 << 11) }},
	{ "Barrett M82A1", "barrett_m82a1", { 57, 318, (1 << 1) }},
	{ "MP7-PDW", "mp7-pdw", { 56, 332, (1 << 10) + (1 << 9) + (1 << 8) }},
	{ "SPAS-12", "spas-12", { 52, 346, (1 << 9) + (1 << 8) }},
	{ "Golden Colts", "golden_colts", { 51, 360, (1 << 9) }},
	{ "Glock-20C", "glock-20c", { 58, 374, (1 << 11) + (1 << 8) }},
	{ "UMP", "ump", { 51, 388, (1 << 9) }},
	{ "M61 Grenade", "m61_grenade", { 402, 402 }, 1 },
	{ "Combat Knife", "combat_knife", { 416, 416 }, 1 },
	{ "Mossberg 500", "mossberg_500", { 52, 430, (1 << 9) + (1 << 8) }},
	{ "M16A4", "m16a4", { 53, 444, (1 << 10) }},
	{ "Ruger-MK1", "ruger-mk1", { 59, 458, (1 << 11) + (1 << 9) }},
	{ "C4", "c4" },
	{ "Akimbo Five-seveN", "akimbo_five-seven", { 56, 486, (1 << 10) + (1 << 9) + (1 << 8) }},
	{ "Raging Bull", "raging_bull", { 56, 486, (1 << 11) + (1 << 10) }},
	{ "M60E3", "m60e3", { 61, 500, (1 << 10) }},
	{ "Sawed-off", "sawed-off", { 53, 514, (1 << 9) + (1 << 8) }},
	{ "Katana", "katana", {}, 1 },
	{ "Seal Knife", "seal_knife", { 556, 556 }, 1 },
	{ "Contender G2", "contender", { 62, 570, (1 << 11) + (1 << 10) + (1 << 8) }},
	{ "Akimbo Skorpions", "throwing_knife", { 61, 584, (1 << 10) + (1 << 11) }}
};

uint32_t FireModes[6] = {
	TSE_FM_FULLAUTO,
	TSE_FM_SEMIAUTO,
	TSE_FM_BURST,
	TSE_FM_PUMP,
	TSE_FM_FREESEMI,
	TSE_FM_FREEFULL
};

uint32_t GetFiremodeByMask(uint32_t mask)
{
	for (int i = 0; i < 6; i++)
		if (mask == FireModes[i])
			return i;
	return -1;
}