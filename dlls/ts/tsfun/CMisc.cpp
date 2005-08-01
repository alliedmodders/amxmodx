#include "tsfun.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
int mPlayerIndex;
int mState;
CPlayer players[33];

int gKnifeOffset;

int gmsgResetHUD;
int gmsgWeaponInfo;
int gmsgClipInfo;
int gmsgScoreInfo;
int gmsgTSHealth;

int gmsgWStatus;
int gmsgTSCash;
int gmsgTSSpace;
int gmsgPwUp;

struct sUserMsg {
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{ "ResetHUD",&gmsgResetHUD,Client_ResetHUD_End,true },
	{ "WeaponInfo",&gmsgWeaponInfo,Client_WeaponInfo,false },
	{ "ClipInfo",&gmsgClipInfo,Client_ClipInfo,false },
	{ "ScoreInfo",&gmsgScoreInfo,Client_ScoreInfo,false },
	{ "TSHealth",&gmsgTSHealth,Client_TSHealth_End,true },

	{ "WStatus",&gmsgWStatus,Client_WStatus,false },
	{ "TSSpace",&gmsgTSSpace,Client_TSSpace,false },
	{ "PwUp",&gmsgPwUp,Client_PwUp,false},

	{ 0,0,0,false }
};

const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}

weapon_t weaponData[] = { 
	{ 1,"Kung Fu","kung_fu",3 }, // id 0 like in WeaponInfo , DeathMsg 
	{ 0,"Glock-18","glock-18",1 },
	{ 0,"Unk1","Unk1",0 }, // bomb ?
	{ 0,"Mini-Uzi","mini-uzi",1 }, 
	{ 0,"BENELLI-M3","benelli_m3",1 }, 
	{ 0,"M4A1","m4a1",1 },
	{ 0,"MP5SD","mp5sd",1 },
	{ 0,"MP5K","mp5k",1 },
	{ 0,"Akimbo Berettas","akimbo_berettas",1 },
	{ 0,"SOCOM-MK23","socom-mk23",1 },
	{ 0,"Akimbo MK23","akimbo_mk23",1 },
	{ 0,"USAS-12","usas-12",1 },
	{ 0,"Desert Eagle","desert_eagle",1 },
	{ 0,"AK47","ak47",1 },
	{ 0,"Five-seveN","five-seven",1 },
	{ 0,"STEYR-AUG","steyr-aug",1 },
	{ 0,"Akimbo Mini-Uzi","akimbo_mini-uzi",1 },
	{ 0,"STEYR-TMP","steyr-tmp",1 },
	{ 0,"Barrett M82A1","barrett_m82a1",1 },
	{ 0,"MP7-PDW","mp7-pdw",1 }, 
	{ 0,"SPAS-12","spas-12",1 },
	{ 0,"Golden Colts","golden_colts",1 },
	{ 0,"Glock-20C","glock-20c",1 },
	{ 0,"UMP", "ump",1 }, 
	{ 0,"M61 Grenade","m61_grenade",1 },
	{ 1,"Combat Knife","combat_knife",1 },
	{ 0,"MOSSBERG 500","mossberg_500",1 },
	{ 0,"M16A4","m16a4",1 },
	{ 0,"Ruger-MK1","ruger-mk1",1 },
	{ 0,"C4","c4",0 },
	{ 0,"Akimbo Five-seveN","akimbo_five-seven",1 },
	{ 0,"Raging Bull","raging_bull",1 },
	{ 0,"M60E3","m60e3",1 }, 
	{ 0,"Sawed-off","sawed-off",1 }, // 33 
	{ 1,"Katana", "katana",2 }, 
	{ 1,"Seal Knife","seal_knife",1 }, // 35 
	{ 1,"Kung Fu","kung_fu",3 }, // Again new id 36 
	{ 1,"Throwing Knife","throwing_knife",2 }, // new id 37 
	{ 0,"breakable", "breakable", 1 },
};
