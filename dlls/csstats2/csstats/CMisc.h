

#ifndef CMISC_H
#define CMISC_H

#include "amxxmodule.h"
#include "CRank.h"

#define MAX_CWEAPONS		6

#define CSW_HEGRENADE      4
#define CSW_SMOKEGRENADE   9
#define CSW_FLASHBANG     25

// *****************************************************
// class CPlayer
// *****************************************************

struct CPlayer {
	edict_t* pEdict;
	char ip[32];
	int index;
	int aiming;
	int current;
	bool bot;
	float clearStats;
	RankSystem::RankStats*	rank;

	struct PlayerWeapon : Stats {
		const char* name;
		int	ammo;
		int	clip;
	};

	PlayerWeapon	weapons[MAX_WEAPONS+MAX_CWEAPONS];
	PlayerWeapon	attackers[33];
	PlayerWeapon	victims[33];
	Stats			weaponsRnd[MAX_WEAPONS+MAX_CWEAPONS]; // DEC-Weapon (Round) stats
	Stats			life;

	int teamId;

	void Init(  int pi, edict_t* pe );
	void Connect(const char* ip );
	void PutInServer();
	void Disconnect();
	void saveKill(CPlayer* pVictim, int weapon, int hs, int tk);
	void saveHit(CPlayer* pVictim, int weapon, int damage, int aiming);
	void saveShot(int weapon);

	void saveBPlant();
	void saveBExplode();
	void saveBDefusing();
	void saveBDefused();

	void restartStats(bool all = true);
	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( auth && !strcmp( auth , "BOT" ) );
	}
	inline bool IsAlive(){
		return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
	}
};

// *****************************************************
// class Grenades
// *****************************************************

class Grenades
{
  struct Obj 
  {
    CPlayer* player;
    edict_t* grenade;
    float time;
    int type;
    Obj* next;
    Obj* prev;
  } *head;

public:
  Grenades() { head = 0; }
  ~Grenades() { clear(); }
  void put( edict_t* grenade, float time, int type, CPlayer* player  );
  bool find( edict_t* enemy, CPlayer** p, int* type );
  void clear();
};

// *****************************************************
// class Forward
// *****************************************************

class Forward
{
	struct AmxCall {
		AMX *amx;
		int iFunctionIdx;
		AmxCall* next;
		AmxCall( AMX *a , int i, AmxCall* n ): amx(a), iFunctionIdx(i), next(n) {}
	} *head;
public:
	Forward() { head = 0; }
	~Forward() { clear(); }
	void clear();
	void put( AMX *a , int i );
	void exec(int p1,int p2,int p3,int p4,int p5,int p6);
	void exec(int p1,int p2,int p3,int p4,int p5);
	void exec(int p1,int p2);
};

#endif // CMISC_H



