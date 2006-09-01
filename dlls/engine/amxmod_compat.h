#ifndef _INCLUDE_ENGINE_AMXMOD_BCOMPAT_H_
#define _INCLUDE_ENGINE_AMXMOD_BCOMPAT_H_

BOOL is_breakable(edict_t* pBreak);
BOOL is_monster(edict_t* pMonster);
void hurt_monster(edict_t* pMonster, float dmg, int bit, const float *origin);
float ArmorDamage(edict_t* pVictim, float dmg, int bit);
void Death(edict_t* pVictim, edict_t* pKiller, const char* weapon, int hs);
void Damage(edict_t *pVictim, 
			edict_t *pAttacker, 
			const float *origin, 
			float dmg, 
			int bit, 
			const char *weapon, 
			int hs);
void RadiusDamage_AMXMod_Base(edict_t *pAttacker, 
							  float dmg, 
							  Vector vecSrc, 
							  float rad, 
							  int bit, 
							  const char *weapon, 
							  int hs);

#endif //_INCLUDE_ENGINE_AMXMOD_BCOMPAT_H_

