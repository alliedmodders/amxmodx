#include "engine.h"
#include <cbase.h>

static cvar_t *sv_knockback1 = NULL;
static cvar_t *sv_knockback2 = NULL;
static cvar_t *sv_friendlyfire = NULL;
static bool g_ff_check = false;
static bool g_kb1_check = false;
static bool g_kb2_check = false;
static int gmsgDamage = 0;
static int gmsgDeathMsg = 0;
static int gmsgScoreInfo = 0;

//From VexdUM (AMX Mod 2006.2)
//This is not exposed, and is only provided as a compatibility helper.
BOOL is_breakable(edict_t* pBreak)
{
	if (FStrEq("func_breakable", STRING(pBreak->v.classname))
		|| (FStrEq("func_pushable", STRING(pBreak->v.classname))
		&& pBreak->v.spawnflags & SF_PUSH_BREAKABLE))
	{
		return true;
	}

	return false;
}

//From VexdUM (AMX Mod 2006.2)
//This is not exposed, and is only provided as a compatibility helper.
BOOL is_monster(edict_t* pMonster)
{
	if(pMonster->v.flags & FL_MONSTER)
	{
		return true;
	}

	return false;
}

//From VexdUM (AMX Mod 2006.2)
// Damage Monsters
void hurt_monster(edict_t* pMonster, float dmg, int bit, const float *origin)
{
	int mdmg = (int)pMonster->v.health;
	pMonster->v.health -= dmg;

	// No need to create a trigger_hurt unless we are going to kill the monster ;)
	if((int)pMonster->v.health < 1)
	{
		int hurt = MAKE_STRING("trigger_hurt");
		char hbit[16];
		char horigin[16];
		snprintf(hbit, 15, "%i", bit);
		snprintf(horigin, 15, "%f %f %f", origin[0], origin[1], origin[2]);

		edict_t* pEntity = CREATE_NAMED_ENTITY(hurt);
		KeyValueData pkvd1;
		pkvd1.szClassName = "trigger_hurt";
		pkvd1.szKeyName = "dmg";
		pkvd1.szValue = "1.0";
		pkvd1.fHandled = FALSE;
		MDLL_KeyValue(pEntity, &pkvd1);

		KeyValueData pkvd2;
		pkvd2.szClassName = "trigger_hurt";
		pkvd2.szKeyName = "damagetype";
		pkvd2.szValue = hbit;
		pkvd2.fHandled = FALSE;
		MDLL_KeyValue(pEntity, &pkvd2);

		KeyValueData pkvd3;
		pkvd3.szClassName = "trigger_hurt";
		pkvd3.szKeyName = "origin";
		pkvd3.szValue = horigin;
		pkvd3.fHandled = FALSE;
		MDLL_KeyValue(pEntity, &pkvd3);

		MDLL_Spawn(pEntity);
		MDLL_Touch(pEntity, pMonster);
		REMOVE_ENTITY(pEntity);
	}
	mdmg -= (int)pMonster->v.health;
	//~dvander - Note, not porting the forward until this function is known to be truly wrapped
	//g_forwards.executeForward(FF_MonsterHurt, ENTINDEX(pMonster), ENTINDEX(pMonster->v.dmg_inflictor), mdmg);
}

//From VexdUM (AMX Mod 2006.2)
//This appears to be from the HLSDK CBasePlayer::TakeDamage() function.
//This is not exposed, and is only provided as a compatibility helper.
float ArmorDamage(edict_t* pVictim, float dmg, int bit)
{
	float flRatio = 0.2;
	float flBonus = 0.5;
	if(bit & DMG_BLAST)
	{
		// blasts damage armor more.
		flBonus *= 2;
	}
	if(pVictim->v.armorvalue && !(bit & (DMG_FALL | DMG_DROWN)))
	{
		// armor doesn't protect against fall or drown damage!
		float flNew = dmg * flRatio;
		float flArmor = (dmg - flNew) * flBonus;

		// Does this use more armor than we have?
		if(flArmor > pVictim->v.armorvalue)
		{
			flArmor = pVictim->v.armorvalue;
			flArmor *= (1/flBonus);
			flNew = dmg - flArmor;
			pVictim->v.armorvalue = 0;
		} else {
			pVictim->v.armorvalue -= flArmor;
		}
		dmg = flNew;
	}
	// Lets knock the view about abit
	pVictim->v.punchangle.x = -4;
	return dmg;
}

// Death emulation
//This is not exposed, and is only provided as a compatibility helper.
void Death(edict_t* pVictim, edict_t* pKiller, const char* weapon, int hs)
{

	if (!gmsgDeathMsg)
	{
		gmsgDeathMsg = GET_USER_MSG_ID(PLID, "DeathMsg", NULL);
	}

	if (!gmsgScoreInfo)
	{
		gmsgScoreInfo = GET_USER_MSG_ID(PLID, "ScoreInfo", NULL);
	}

	// Make sure an entity is allowed to take damage
	if(pVictim->v.takedamage > DAMAGE_NO)
	{
		// Breakable Check
		if(is_breakable(pVictim))
		{
			MDLL_Use(pVictim, pKiller);
		}
		// Monster Check
		if (is_monster(pVictim))
		{
			pVictim->v.dmg_inflictor = pKiller;
			float dmg = pVictim->v.health;
			int bit = DMG_BULLET;
			const float *origin = pVictim->v.origin;
			hurt_monster(pVictim, dmg, bit, origin);
		}
		// Player Check
		if (pVictim->v.flags & (FL_CLIENT | FL_FAKECLIENT))
		{
			pVictim->v.dmg_inflictor = pKiller;
			edict_t* inflictor = pKiller->v.owner;
			int inflictorId = FNullEnt(inflictor) ? ENTINDEX(inflictor) : 0;

			// See if it is a player attacking with a default weapon
			if (pKiller->v.flags & (FL_CLIENT | FL_FAKECLIENT))
			{
				// We only modify the weapon if it = 0, otherwise its been specified
				if(strcmp(weapon, "") == 0)
				{
					char view_weapon[64];
					// Get the name from the view model
					weapon = STRING(pKiller->v.viewmodel);

					// Strip out the beginning of the viewmodel (models/v_)
					if(strncmp(weapon, "models/v_", 9) == 0)
					{
						strcpy(view_weapon, weapon + 9);
					}

					// Strip out the end of viewmodel (.mdl)
					view_weapon[strlen(view_weapon) - 4] = '\0';
					weapon = view_weapon;
				}
				// See if its an entity attacking, if so lets find its owner
			} else if (inflictorId >= 1 && inflictorId <= gpGlobals->maxClients) {
				// We only modify the weapon if it = 0, otherwise its been specified
				if(strcmp(weapon, "") == 0)
				{
					weapon = STRING(pKiller->v.classname);
					// Strip out the default part of weapon name (HLSDK)
					if(strncmp(weapon, "weapon_", 7) == 0)
					{
						weapon += 7;
					} else if(strncmp(weapon, "monster_", 8) == 0) {
						weapon += 8;
					} else if(strncmp(weapon, "func_", 5) == 0) {
						weapon += 5;
					}
				}
				// Check to see if the victim is the owner
				if(inflictor == pVictim)
				{
					pKiller = pVictim;
				} else {
					pKiller = inflictor;
				}
			}
			// Send the Death Event
			int killerId = ENTINDEX(pKiller);
			int victimId = ENTINDEX(pVictim);
			MESSAGE_BEGIN( MSG_ALL, gmsgDeathMsg );
			WRITE_BYTE( killerId > gpGlobals->maxClients ? 0 : killerId );
			WRITE_BYTE( victimId );
			WRITE_BYTE( hs );
			WRITE_STRING( weapon );
			MESSAGE_END();
			// Log this kill
			if(pVictim == pKiller)
			{
				// killed self
				UTIL_LogPrintf("\"%s<%i><%s><%s>\" killed self with \"%s\"\n",
					STRING( pVictim->v.netname ),
					GETPLAYERUSERID( pVictim ),
					GETPLAYERAUTHID( pVictim ),
					MF_GetPlayerTeam(victimId),
					weapon );
				// Killed by another player
			} else if(pKiller->v.flags & (FL_CLIENT | FL_FAKECLIENT)) {
				UTIL_LogPrintf("\"%s<%i><%s><%s>\" killed \"%s<%i><%s><%s>\" with \"%s\"\n",
					STRING( pKiller->v.netname ),
					GETPLAYERUSERID( pKiller ),
					GETPLAYERAUTHID( pKiller ),
					MF_GetPlayerTeam(killerId),
					STRING( pVictim->v.netname ),
					GETPLAYERUSERID( pVictim ),
					GETPLAYERAUTHID( pVictim ),
					MF_GetPlayerTeam(victimId),
					weapon);

				int killerTeam = MF_GetPlayerTeamID(killerId);
				int victimTeam = MF_GetPlayerTeamID(victimId);
				if (killerTeam != victimTeam)
				{
					// Give Killer credit for this kill
					pKiller->v.frags += 1;
				} else {
					pKiller->v.frags -= 1;
				}
				// Update the scoreboard for the killer
				if (gmsgScoreInfo)
				{
					MESSAGE_BEGIN(MSG_ALL, gmsgScoreInfo);
					WRITE_BYTE( killerId );
					WRITE_SHORT( (int)pKiller->v.frags );
					WRITE_SHORT( MF_GetPlayerDeaths(killerId) );
					WRITE_SHORT( 0 );
					WRITE_SHORT( MF_GetPlayerTeamID(killerId) );
					MESSAGE_END();
				}
				// Give Victim back 1 point since they didn't kill themselves
				pVictim->v.frags += 1;
			}
			// Killed by something else?
			else {
				UTIL_LogPrintf( "\"%s<%i><%s><%s>\" killed by \"%s\"\n",
					STRING( pVictim->v.netname ),
					GETPLAYERUSERID( pVictim ),
					GETPLAYERAUTHID( pVictim ),
					MF_GetPlayerTeam(victimId),
					weapon );
				// Give Victim back 1 point since they didn't commit suicide
				pVictim->v.frags += 1;
			}
#if 0
			//still a todo on this one
			gInfo.logBlock = true;
#endif
			int opt = BLOCK_ONCE;
			MF_MessageBlock(MSGBLOCK_SET, gmsgDeathMsg, &opt);
			// Kill the client, since the relevent logging blocks are in place
			MDLL_ClientKill(pVictim);
			// Restore the old message type
			MF_MessageBlock(MSGBLOCK_SET, gmsgDeathMsg, &opt);
			// Show the Victim the killing location
			pVictim->v.iuser3 = (killerId > gpGlobals->maxClients) ? 0 : killerId;
		}
	}
}

// Damage emulation
// From VexdUM (AMX Mod 2006.2)
//This is not exposed, and is only provided as a compatibility helper.
void Damage(edict_t *pVictim, 
			edict_t *pAttacker, 
			const float *origin, 
			float dmg, 
			int bit, 
			const char *weapon, 
			int hs)
{
	if (!g_ff_check && !sv_friendlyfire)
	{
		sv_friendlyfire = CVAR_GET_POINTER("sv_friendlyfire");
		g_ff_check = true;
	}

	if (!gmsgDamage)
	{
		gmsgDamage = GET_USER_MSG_ID(PLID, "Damage", NULL);
	}

	// Make sure an entity is allowed to take damage
	if(pVictim->v.takedamage > DAMAGE_NO)
	{
		// Breakable Check
		if(is_breakable(pVictim) && (int)dmg > 0)
		{
			MDLL_Use(pVictim, pAttacker);
		}
		// Monster Check
		if(is_monster(pVictim) && (int)dmg > 0)
		{
			pVictim->v.dmg_inflictor = pAttacker;
			hurt_monster(pVictim, dmg, bit, origin);
		}
		// Player Check
		if(pVictim->v.flags & (FL_CLIENT | FL_FAKECLIENT))
		{
			int AttackerId = ENTINDEX(pAttacker);
			int AttackerOwnerId = ENTINDEX(pAttacker->v.owner);
			int VictimId = ENTINDEX(pVictim);
			int vTeam = MF_GetPlayerTeamID(VictimId);
			int aTeam = 0;
			if (AttackerId >= 1 && AttackerId <= gpGlobals->maxClients)
			{
				aTeam = MF_GetPlayerTeamID(AttackerId);
			} else if (AttackerOwnerId >= 1 && AttackerOwnerId <= gpGlobals->maxClients) {
				aTeam = MF_GetPlayerTeamID(AttackerOwnerId);
			}
			if((sv_friendlyfire && (int)sv_friendlyfire->value) || (vTeam != aTeam))
			{
				// Recalculate the damage since we might have armor
				dmg = ArmorDamage(pVictim, dmg, bit);
				// Only allow damage to process if more than 0.5
				if((int)dmg > 0)
				{
					// Setting to externally flag who last attacked the Victim, pretty neat huh?
					pVictim->v.dmg_inflictor = pAttacker;
					pVictim->v.dmg_take += dmg;
					// Register the Damage Event
					MESSAGE_BEGIN( MSG_ONE, gmsgDamage, NULL, pVictim );
					WRITE_BYTE( (int)pVictim->v.dmg_save );
					WRITE_BYTE( (int)pVictim->v.dmg_take );
					WRITE_LONG( bit );
					WRITE_COORD( origin[0] );
					WRITE_COORD( origin[1] );
					WRITE_COORD( origin[2] );
					MESSAGE_END();

					if((int)dmg >= (int)pVictim->v.health)
					{
						// Kill the victim
						pVictim->v.health = 0.0;
						// Send info to Death system
						Death(pVictim, pAttacker, weapon, hs);
					}else {
						// Take health away from victim
						pVictim->v.health -= dmg;
					}
				}
			}
		}
	}
}

// Radius Damage emulation - 
// From VexdUM (AMX Mod 2006.2)
//This is not exposed, and is only provided as a compatibility helper.
void RadiusDamage_AMXMod_Base(edict_t *pAttacker, 
							  float dmg, 
							  Vector vecSrc, 
							  float rad, 
							  int bit, 
							  const char *weapon, 
							  int hs)
{
	edict_t *pTarget = NULL;
	TraceResult tr;
	float falloff;
	Vector vecSpot;
	Vector vecSee;

	if (!g_kb1_check && !sv_knockback1)
	{
		sv_knockback1 = CVAR_GET_POINTER("sv_knockback1");
		g_kb1_check = true;
	}
	if (!g_kb2_check && !sv_knockback2)
	{
		sv_knockback2 = CVAR_GET_POINTER("sv_knockback2");
		g_kb2_check = true;
	}

	if(rad > 0.0)
	{
		falloff = dmg / rad;
	} else {
		falloff = 1.0;
	}
	vecSrc.z += 1; // In case grenade is lying on the ground

	int hitId;
	int targetId;

	while ((pTarget = UTIL_FindEntityInSphere(pTarget, vecSrc, rad)) != NULL)
	{
		// Make sure an entity is allowed to take damage
		if (pTarget->v.takedamage > DAMAGE_NO)
		{
			//none of this code was working so I simplified it
			//damage doesn't check for visibility now (it probably shouldn't anyway)
			//for this to work it seems like an exception needs to be made for world OR
			// the spot/see things aren't being calculated right.
#if 0
			vecSpot = (pTarget->v.absmin + pTarget->v.absmax) * 0.5;
			vecSee = (pAttacker->v.absmin + pAttacker->v.absmax) * 0.5;
			TRACE_LINE(vecSee, vecSpot, FALSE, pAttacker, &tr);
			// Explosion can 'see' this entity, so hurt them!
#endif
			TRACE_LINE(vecSrc, pTarget->v.origin, FALSE, pAttacker, &tr);
			hitId = ENTINDEX(tr.pHit);
			targetId = ENTINDEX(pTarget);
			if(tr.flFraction < 1.0 || (hitId == targetId))
			{
				// Work out the distance between impact and entity
				float dist = (tr.vecEndPos - vecSrc).Length() * falloff;
				// Damage algorithm, its just that easy :)
				dmg -= dist;
				// Knockback Effect
				if(pTarget->v.flags & (FL_CLIENT | FL_FAKECLIENT) && (bit & (DMG_BLAST | DMG_CLUB | DMG_SHOCK | DMG_SONIC | DMG_ENERGYBEAM | DMG_MORTAR)))
				{
					Vector vecPush = (pTarget->v.origin - (pAttacker->v.absmin + pAttacker->v.absmax) * 0.5).Normalize();
					if(dmg < 60.0)
					{
						pTarget->v.velocity = pTarget->v.velocity + vecPush * dmg * (sv_knockback1 ? sv_knockback1->value : 1.0f);
					} else {
						pTarget->v.velocity = pTarget->v.velocity + vecPush * dmg * (sv_knockback2 ? sv_knockback2->value : 1.0f);
					}
				}
				// Send info to Damage system
				Damage(pTarget, pAttacker, vecSrc, dmg, bit, weapon, hs);
			}
		}
	}
	pTarget = NULL;
}
