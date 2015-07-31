// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Fun Module
//

#ifndef CMISC_H
#define CMISC_H

#if defined(_WIN32)
#define LINUXOFFSET		0
#else
#define LINUXOFFSET		5
#endif

#define DODFUN_VERSION "1.0.2"

// DoD Player
#define STEAM_PDOFFSET_WDEPLOY		229	 + LINUXOFFSET // weapon deploy

#define STEAM_PDOFFSET_TEAMNAME		1396 + (LINUXOFFSET * sizeof(char)) // team name 349 char[16]
#define STEAM_PDOFFSET_CLASS		366  + LINUXOFFSET // player class
#define STEAM_PDOFFSET_RCLASS		367  + LINUXOFFSET // random class  

#define STEAM_PDOFFSET_SCORE		475  + LINUXOFFSET // score
#define STEAM_PDOFFSET_DEATHS		476  + LINUXOFFSET // deaths

// DoD Control Point
struct pd_dcp {
	int iunk_0;
#if defined(_WIN32)
	int iunk_1; // windows only
#endif
	int iunk_2;	// pointer edict_t*
	int iunk_3;

	float origin_x;
	float origin_y;
	float origin_z; // 6

	float mins_x;
	float mins_y;
	float mins_z;

	float maxs_x;
	float maxs_y;
	float maxs_z;
	
	float angles_x;
	float angles_y;
	float angles_z; // 15

	// 19 - spawnflags ?
	// 20-int , always 1
	int unknown_block1[19];
	int iunk_35; // pointer entvars_t*
	int iunk_36; // pointer entvars_t*
	int unknown_block2[52];
	int iunk_89; // pointer entvars_t*
#if defined (__linux__) || defined (__APPLE__)
	int iunk_extra1;
	int iunk_extra2;
	int iunk_extra3;
	int iunk_extra4;
#endif
	int owner; // 90
	int iunk_91;
	int iunk_92;
	int default_owner; // 93
	int flag_id;
	int pointvalue;
	int points_for_player;
	int points_for_team;
	float funk_98; // always 1.0
	float cap_time;
	char cap_message[256]; // 100 MAP_PLAYER_CAP , %p player , %n pointname , %t teamname
	int iunk_164;
	int iunk_165;
	char target_allies[256]; //  166
	char target_axis[256]; // 230
	char target_reset[256];
	char model_allies[256]; // 358
	char model_axis[256]; // 422
	char model_neutral[256]; // 486
	int model_body_allies; // 550
	int model_body_axis;
	int model_body_neutral;
	int icon_allies;
	int icon_axis;
	int icon_neutral;
	int can_touch; // flags : 1-allies can't, 256-axis can't , default 0 (all can)
	int iunk_557;
	int iunk_558; //  ? -2 , 4
	char pointgroup[256];
	int iunk_623;
	int iunk_624;
	int iunk_625;
};

#define GET_CP_PD( x ) (*(pd_dcp*)x->pvPrivateData)

// DoD Capture Area
struct pd_dca {
	int iunk_0;
	int iunk_1;
	int iunk_2;
#if defined(_WIN32)
	int iunk_3; // if def windows
#endif

	float origin_x;
	float origin_y;
	float origin_z; // 6

	float mins_x;
	float mins_y;
	float mins_z;

	float maxs_x;
	float maxs_y;
	float maxs_z;
	
	float angles_x;
	float angles_y;
	float angles_z; // 15
	
	// 16-135
#if defined(_WIN32)
	int unknown_block_16[111];
#else
	int unknown_block_16[116]; // linux +5 more
#endif

	int time_to_cap; // 127
	int iunk_128;
	int allies_numcap; // 129
	int axis_numcap; // 130

	int iunk_131;
	int iunk_132;

	int can_cap; // 133 flags : 1-allies can , 256-axis can, default 257 (all can)

	int iunk_134;
	int iunk_135;

	char allies_endcap[256]; // 136
	char axis_endcap[256]; // 200
	char allies_startcap[256]; // 264
	char axis_startcap[256]; // 328
	char allies_breakcap[256]; // 392
	char axis_breakcap[256]; // 456
	int iunk_520; // -1 allies area, blowable (charlie) ??
	char hud_sprite[256]; // 521 

	// 585 - 649
	int unknown_block_585[65];

	char object_group[256]; // 650
	int iunk_714;
	int iunk_715;
	int iunk_716;
	// 717 size
};

#define GET_CA_PD( x ) (*(pd_dca*)x->pvPrivateData)

/* DoD weapons */
enum {
	DODW_AMERKNIFE = 1,
	DODW_GERKNIFE,
	DODW_COLT,
	DODW_LUGER,
	DODW_GARAND,
	DODW_SCOPED_KAR,
	DODW_THOMPSON,
	DODW_STG44,
	DODW_SPRINGFIELD,
	DODW_KAR,
	DODW_BAR,
	DODW_MP40,
	DODW_HANDGRENADE,
	DODW_STICKGRENADE,
	DODW_STICKGRENADE_EX,
	DODW_HANDGRENADE_EX,
	DODW_MG42,
	DODW_30_CAL,
	DODW_SPADE,
	DODW_M1_CARBINE,
	DODW_MG34,
	DODW_GREASEGUN,
	DODW_FG42,
	DODW_K43,
	DODW_ENFIELD,
	DODW_STEN,
	DODW_BREN,
	DODW_WEBLEY,
	DODW_BAZOOKA,
	DODW_PANZERSCHRECK,
	DODW_PIAT,
	DODW_SCOPED_FG42,
	DODW_FOLDING_CARBINE,
	DODW_KAR_BAYONET,
	DODW_SCOPED_ENFIELD,
	DODW_MILLS_BOMB,
	DODW_BRITKNIFE,
	DODW_GARAND_BUTT,
	DODW_ENFIELD_BAYONET,
	DODW_MORTAR,
	DODW_K43_BUTT,
};


// *****************************************************
// class CPlayer
// *****************************************************

class CPlayer {

public:
	edict_t* pEdict;
	int index;
	int current;

	int staminaMin;
	int staminaMax;
	bool staminaSet;

	bool fuseSet;
	int fuseType; // 1<<0 - for new , 1<<1 - for cought
	float nadeFuse;

	bool ingame;
	bool bot;

	void Init(  int pi, edict_t* pe );
	void Connect();
	void PutInServer();
	void Disconnect();
	void killPlayer();
	void setTeamName( const char *szName );
	void getTeamName( char *szName );

	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( auth && !strcmp( auth , "BOT" ) );
	}
	inline bool IsAlive(){
		return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
	}
};

typedef struct objinfo_s {
	// initobj
	edict_t* pEdict;
	int index;
	int default_owner;
	int visible;
	int icon_neutral;
	int icon_allies;
	int icon_axis;
	float origin_x;
	float origin_y;
	// setobj
	int owner;
	// control area
	int areaflags; // 0-need check , 1-no area , 2-found area 
	edict_t* pAreaEdict;
} objinfo_t;

class CObjective {
public:
	int count;
	objinfo_t obj[12];
	inline void Clear() { count = 0; memset(obj,0,sizeof(obj)); }
	void SetKeyValue( int index, char *keyname, char *value );

	void InitObj(int dest = MSG_ALL , edict_t* ed = NULL);
	void SetObj(int index);
	
	void UpdateOwner( int index, int team );
	void Sort();
};

enum CP_VALUE {
	CP_edict = 1,
	CP_area,
	CP_index,
	CP_owner,
	CP_default_owner,
	CP_visible,
	CP_icon_neutral,
	CP_icon_allies,
	CP_icon_axis,
	CP_origin_x,
	CP_origin_y,
	
	CP_can_touch,
	CP_pointvalue,

	CP_points_for_cap,
	CP_team_points,

	CP_model_body_neutral,
	CP_model_body_allies,
	CP_model_body_axis,

	// strings
	CP_name,
	CP_cap_message,
	CP_reset_capsound,
	CP_allies_capsound,
	CP_axis_capsound,
	CP_targetname,

	CP_model_neutral,
	CP_model_allies,
	CP_model_axis,
};

enum CA_VALUE {
	CA_edict = 1,
	CA_allies_numcap,
	CA_axis_numcap,
	CA_timetocap,
	CA_can_cap,

	// strings
	CA_target,
	CA_sprite,
};

#endif // CMISC_H

