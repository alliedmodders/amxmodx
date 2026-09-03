// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// CSX Module
//

#include "amxxmodule.h"
#include "rank.h"
#include <IGameConfigs.h>

#if !defined(WIN32) && !defined(_WINDOWS)
#include <sys/mman.h> /// mprotect()
#endif

size_t** g_ppvtbl_CBasePlayer = NULL;
size_t** g_ppvtbl_CBasePlayer_Bots = NULL;

TraceAttack_Type g_origTraceAttack = NULL;
TakeDamage_Type g_origTakeDamage = NULL;

TraceAttack_Type g_origTraceAttack_Bots = NULL;
TakeDamage_Type g_origTakeDamage_Bots = NULL;

size_t g_ofsBaseclass; /// "base"
size_t g_vfidxTraceAttack; /// "traceattack"
size_t g_vfidxTakeDamage; /// "takedamage"

bool g_virtualCfg = false;

edict_t* g_pEdictList; /// from ServerActivate_Post()

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];

void (*function)(void*);
void (*endfunction)(void*);

IGameConfigManager* ConfigManager;
IGameConfig* CommonConfig = NULL;
size_t m_LastHitGroup = 0;

CPlayer players[33];

CPlayer* mPlayer;

int mPlayerIndex;
int mState;

RankSystem g_rank;

Grenades g_grenades;

int iFGrenade;
int iFDeath;
int iFDamage;

int iFBPlanted;
int iFBDefused;
int iFBPlanting;
int iFBDefusing;
int iFBExplode;

int g_bombAnnounce;
int g_Planter;
int g_Defuser;

bool rankBots;

int gmsgCurWeapon;
int gmsgDeathMsg;
int gmsgDamage;
int gmsgDamageEnd;
int gmsgWeaponList;
int gmsgResetHUD;
int gmsgAmmoX;
int gmsgScoreInfo;
int gmsgAmmoPickup;
int gmsgSendAudio;
int gmsgTextMsg;
int gmsgBarTime;

int g_CurrentMsg;

cvar_t init_csstats_maxsize ={"csstats_maxsize","3500", 0 , 3500.0 };
cvar_t init_csstats_reset ={"csstats_reset","0"};
cvar_t init_csstats_rank ={"csstats_rank","1"};
cvar_t *csstats_maxsize;
cvar_t *csstats_reset;
cvar_t *csstats_rank;

cvar_t* csstats_rankbots;
cvar_t* csstats_pause;
cvar_t init_csstats_rankbots ={"csstats_rankbots","0"};
cvar_t init_csstats_pause = {"csstats_pause","0"};

struct sUserMsg
{
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{"CurWeapon",	&gmsgCurWeapon,		Client_CurWeapon,	false},
	{"Damage",		&gmsgDamage,		Client_Damage,		false},	
	{"Damage",		&gmsgDamageEnd,		Client_Damage_End,	true},
	{"WeaponList",	&gmsgWeaponList,	Client_WeaponList,	false},
	{"ResetHUD",	&gmsgResetHUD,		Client_ResetHUD,	true},
	{"AmmoX",		&gmsgAmmoX,			Client_AmmoX,		false},
	{"ScoreInfo",	&gmsgScoreInfo,		Client_ScoreInfo,	false},
	{"AmmoPickup",	&gmsgAmmoPickup,	Client_AmmoPickup,	false},
	{"SendAudio",	&gmsgSendAudio,		Client_SendAudio,	false},
	{"TextMsg",		&gmsgTextMsg,		Client_TextMsg,		false},
	{"BarTime",		&gmsgBarTime,		Client_BarTime,		false},
	{"DeathMsg",	&gmsgDeathMsg,		Client_DeathMsg,	false},

	{0, 0, 0, false}
};

int RegUserMsg_Post(const char *pszName, int iSize)
{
	for (int i = 0; g_user_msg[ i ].name; ++i )
	{
		if ( !*g_user_msg[i].id && strcmp( g_user_msg[ i ].name , pszName  ) == 0 )
		{
			int id = META_RESULT_ORIG_RET( int );

			*g_user_msg[ i ].id = id;
		
			if ( g_user_msg[ i ].endmsg )
				modMsgsEnd[ id  ] = g_user_msg[ i ].func;
			else
				modMsgs[ id  ] = g_user_msg[ i ].func;
			//break;
		}
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}

void allowFullMemAccess(void* pAddr, size_t Size)
{
#if defined(WIN32) || defined(_WINDOWS) /// Windows
    unsigned long oldAccess;
    VirtualProtect(pAddr, Size, PAGE_EXECUTE_READWRITE, &oldAccess);
#else /// Linux/ Mac
    size_t Addr = (size_t)pAddr;
    long pageMask = sysconf(_SC_PAGESIZE) - 1;
    size_t Begin = Addr & ~pageMask; /// Would turn '0xABC777AB' into '0xABC77000'.
    size_t End = (Addr + Size + pageMask) & ~pageMask; /// Would turn '0xABC777AB' into '0xABC78000', '0xABC79000', ...
    mprotect((void*)Begin, End - Begin /** 0x1000(4096), 0x2000(8192), ... */, PROT_READ | PROT_WRITE | PROT_EXEC);
#endif
}

static bool ClientKill_wasAlive;

void ClientKill(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	ClientKill_wasAlive = pPlayer->IsAlive();

	RETURN_META(MRES_IGNORED);
}

void ClientKill_Post(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (ClientKill_wasAlive && !pPlayer->IsAlive())
	{
		MF_ExecuteForward(iFDamage, static_cast<cell>(pPlayer->index), static_cast<cell>(pPlayer->index),
			static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0));		// he would
		pPlayer->saveKill(pPlayer, 0, 0, 0);
		MF_ExecuteForward(iFDeath, static_cast<cell>(pPlayer->index), static_cast<cell>(pPlayer->index),
			static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0));
	}

	RETURN_META(MRES_IGNORED);
}

void SetClientKeyValue(int playerIdx, char* pInfoBuffer, const char* pcszKey, const char* pcszValue)
{ /// For bot CBasePlayer vtbl. hooking (i.e. CZ Bots).
    if (false == g_virtualCfg || g_ppvtbl_CBasePlayer_Bots)
    {
        RETURN_META(MRES_IGNORED);
    }
	edict_t* pPlayer = MF_GetPlayerEdict(playerIdx);
    if (!(pPlayer->v.flags & FL_FAKECLIENT))
    {
        RETURN_META(MRES_IGNORED);
    }
    const char* pcszAuth = GETPLAYERAUTHID(pPlayer); 	 
    if (!pcszAuth || '\0' == *pcszAuth || strcmp(pcszAuth, "BOT") ||
        strcmp(pcszKey, "*bot") || strcmp(pcszValue, "1"))
    {
        RETURN_META(MRES_IGNORED);
    }
    const unsigned char* pBase = (unsigned char*)pPlayer->pvPrivateData;
    if (!pBase)
    {
        RETURN_META(MRES_IGNORED);
    }
	g_ppvtbl_CBasePlayer_Bots = *((size_t***) (pBase + g_ofsBaseclass));
    if (!g_ppvtbl_CBasePlayer_Bots)
    {
        RETURN_META(MRES_IGNORED);
    }
    /// Assign original vf. addr. only once.
    if (!g_origTraceAttack_Bots)
        g_origTraceAttack_Bots = (TraceAttack_Type)g_ppvtbl_CBasePlayer_Bots[g_vfidxTraceAttack];
    if (!g_origTakeDamage_Bots)
        g_origTakeDamage_Bots = (TakeDamage_Type)g_ppvtbl_CBasePlayer_Bots[g_vfidxTakeDamage];
    allowFullMemAccess(&g_ppvtbl_CBasePlayer_Bots[g_vfidxTraceAttack], sizeof(size_t*));
    allowFullMemAccess(&g_ppvtbl_CBasePlayer_Bots[g_vfidxTakeDamage], sizeof(size_t*));
    g_ppvtbl_CBasePlayer_Bots[g_vfidxTraceAttack] = (size_t*)Hook_TraceAttack_Bots;
    g_ppvtbl_CBasePlayer_Bots[g_vfidxTakeDamage] = (size_t*)Hook_TakeDamage_Bots;
	RETURN_META(MRES_IGNORED);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){

	rankBots = (int)csstats_rankbots->value ? true:false;

	for( int i = 1; i <= gpGlobals->maxClients; ++i)
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );

    g_pEdictList = pEdictList;

    /// For human CBasePlayer vtbl. hooking.
    if (false == g_virtualCfg || g_ppvtbl_CBasePlayer)
    {
        RETURN_META(MRES_IGNORED);
    }
    edict_t* pPlayer = CREATE_ENTITY();
    if (!pPlayer)
    {
        RETURN_META(MRES_IGNORED);
    }
    CALL_GAME_ENTITY(PLID, "player", &pPlayer->v);
    const unsigned char* pBase = (unsigned char*)pPlayer->pvPrivateData;
    if (!pBase)
    {
        REMOVE_ENTITY(pPlayer);
        RETURN_META(MRES_IGNORED);
    }
	g_ppvtbl_CBasePlayer = *((size_t***) (pBase + g_ofsBaseclass));
    if (!g_ppvtbl_CBasePlayer)
    {
        REMOVE_ENTITY(pPlayer); /// Also frees pvPrivateData.
        RETURN_META(MRES_IGNORED);
    }
    /// Assign original vf. addr. only once.
    if (!g_origTraceAttack)
        g_origTraceAttack = (TraceAttack_Type)g_ppvtbl_CBasePlayer[g_vfidxTraceAttack];
    if (!g_origTakeDamage)
        g_origTakeDamage = (TakeDamage_Type)g_ppvtbl_CBasePlayer[g_vfidxTakeDamage];
    allowFullMemAccess(&g_ppvtbl_CBasePlayer[g_vfidxTraceAttack], sizeof(size_t*));
    allowFullMemAccess(&g_ppvtbl_CBasePlayer[g_vfidxTakeDamage], sizeof(size_t*));
    g_ppvtbl_CBasePlayer[g_vfidxTraceAttack] = (size_t*)Hook_TraceAttack;
    g_ppvtbl_CBasePlayer[g_vfidxTakeDamage] = (size_t*)Hook_TakeDamage;
    REMOVE_ENTITY(pPlayer); /// Also frees pvPrivateData.

	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post( edict_t *pEntity ) {
    if ( !isModuleActive() )
	{
		RETURN_META(MRES_IGNORED);
	}

	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (pPlayer->clearStats && pPlayer->clearStats < gpGlobals->time ){

		if ( !ignoreBots(pEntity) ){
			pPlayer->clearStats = 0.0f;
			if (pPlayer->rank)
				pPlayer->rank->updatePosition( &pPlayer->life );
			pPlayer->restartStats(false);
		}
	}
	RETURN_META(MRES_IGNORED);
}

#if defined(WIN32) || defined(_WINDOWS)
void __fastcall Hook_TraceAttack(void* pThis, void* /** ignored */, entvars_s* pAtk, float Dmg, Vector Dir, TraceResult* pRes, int dmgType)
#else
void Hook_TraceAttack(void* pThis, entvars_s* pAtk, float Dmg, Vector Dir, TraceResult* pRes, int dmgType)
#endif
{
    g_origTraceAttack(pThis, pAtk, Dmg, Dir, pRes, dmgType);
    if (!pAtk)
        return;
    edict_t* pAtkEntity = pAtk->pContainingEntity;
    if (!pAtkEntity || pAtkEntity->v.deadflag || pAtkEntity->v.health <= 0.f)
        return;
    int atkEntity = F_EToI(pAtkEntity);
    if (atkEntity < 1 || atkEntity > gpGlobals->maxClients)
        return;
    CPlayer* pAtkPlayer = GET_PLAYER_POINTER(pAtkEntity);
    if (!pAtkPlayer)
        return;
    pAtkPlayer->current_atk = pAtkPlayer->current; // Attacker attacked with this weapon last time.
}

#if defined(WIN32) || defined(_WINDOWS)
int __fastcall Hook_TakeDamage(void* pThis, void* /** ignored */, entvars_s* pInflictor, entvars_s* pAtk, float Dmg, int dmgType)
#else
int Hook_TakeDamage(void* pThis, entvars_s* pInflictor, entvars_s* pAtk, float Dmg, int dmgType)
#endif
{
    int Res = g_origTakeDamage(pThis, pInflictor, pAtk, Dmg, dmgType);
    if (!pAtk)
        return Res;
    edict_t* pAtkEntity = pAtk->pContainingEntity;
    if (!pAtkEntity || pAtkEntity->v.deadflag || pAtkEntity->v.health <= 0.f)
        return Res;
    int atkEntity = F_EToI(pAtkEntity);
    if (atkEntity < 1 || atkEntity > gpGlobals->maxClients)
        return Res;
    CPlayer* pAtkPlayer = GET_PLAYER_POINTER(pAtkEntity);
    if (!pAtkPlayer)
        return Res;
    pAtkPlayer->current_atk = pAtkPlayer->current; // Attacker attacked with this weapon last time.
    return Res;
}

#if defined(WIN32) || defined(_WINDOWS)
void __fastcall Hook_TraceAttack_Bots(void* pThis, void* /** ignored */, entvars_s* pAtk, float Dmg, Vector Dir, TraceResult* pRes, int dmgType)
#else
void Hook_TraceAttack_Bots(void* pThis, entvars_s* pAtk, float Dmg, Vector Dir, TraceResult* pRes, int dmgType)
#endif
{
    g_origTraceAttack_Bots(pThis, pAtk, Dmg, Dir, pRes, dmgType);
    if (!pAtk)
        return;
    edict_t* pAtkEntity = pAtk->pContainingEntity;
    if (!pAtkEntity || pAtkEntity->v.deadflag || pAtkEntity->v.health <= 0.f)
        return;
    int atkEntity = F_EToI(pAtkEntity);
    if (atkEntity < 1 || atkEntity > gpGlobals->maxClients)
        return;
    CPlayer* pAtkPlayer = GET_PLAYER_POINTER(pAtkEntity);
    if (!pAtkPlayer)
        return;
    pAtkPlayer->current_atk = pAtkPlayer->current; // Attacker attacked with this weapon last time.
}

#if defined(WIN32) || defined(_WINDOWS)
int __fastcall Hook_TakeDamage_Bots(void* pThis, void* /** ignored */, entvars_s* pInflictor, entvars_s* pAtk, float Dmg, int dmgType)
#else
int Hook_TakeDamage_Bots(void* pThis, entvars_s* pInflictor, entvars_s* pAtk, float Dmg, int dmgType)
#endif
{
    int Res = g_origTakeDamage_Bots(pThis, pInflictor, pAtk, Dmg, dmgType);
    if (!pAtk)
        return Res;
    edict_t* pAtkEntity = pAtk->pContainingEntity;
    if (!pAtkEntity || pAtkEntity->v.deadflag || pAtkEntity->v.health <= 0.f)
        return Res;
    int atkEntity = F_EToI(pAtkEntity);
    if (atkEntity < 1 || atkEntity > gpGlobals->maxClients)
        return Res;
    CPlayer* pAtkPlayer = GET_PLAYER_POINTER(pAtkEntity);
    if (!pAtkPlayer)
        return Res;
    pAtkPlayer->current_atk = pAtkPlayer->current; // Attacker attacked with this weapon last time.
    return Res;
}

void ServerDeactivate() 
{
	int i;

	for( i = 1; i <= gpGlobals->maxClients; ++i)
	{
		GET_PLAYER_POINTER_I(i)->Disconnect();
	}

	if (static_cast<int>(csstats_maxsize->value) <= 0 || g_rank.getRankNum() >= static_cast<int>(csstats_maxsize->value) || static_cast<int>(csstats_reset->value) != 0)
	{
		CVAR_SET_FLOAT("csstats_reset", 0.0f);
		g_rank.clear(); // clear before save to file
	}
	g_rank.saveRank( MF_BuildPathname("%s",get_localinfo("csstats")) );	

	// clear custom weapons info
	for ( i=MAX_WEAPONS;i<MAX_WEAPONS+MAX_CWEAPONS;i++)
		weaponData[i].used = false;

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	
	pPlayer->Connect(pszAddress);

	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void ClientDisconnect( edict_t *pEntity ) 
{
	GET_PLAYER_POINTER(pEntity)->Disconnect();

	RETURN_META(MRES_IGNORED);
}

void ClientPutInServer_Post( edict_t *pEntity ) 
{
	GET_PLAYER_POINTER(pEntity)->PutInServer();

	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged_Post( edict_t *pEntity, char *infobuffer ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	if (pPlayer->pEdict == NULL)
	{
		pPlayer->Init(ENTINDEX(pEntity), pEntity);
	}

	const char* name = INFOKEY_VALUE(infobuffer,"name");
	const char* oldname = STRING(pEntity->v.netname);

	if ( pPlayer->rank ){
		if ( strcmp(oldname,name) != 0 ) {
			if ((int)csstats_rank->value == 0)
				pPlayer->rank = g_rank.findEntryInRank( name, name );
			else
				pPlayer->rank->setName( name );
		}
	}
	else if ( pPlayer->IsBot() ) {
		pPlayer->Connect( "127.0.0.1" );
		pPlayer->PutInServer();
	}
	RETURN_META(MRES_IGNORED);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	if (ed){
		mPlayerIndex = ENTINDEX(ed);
		mPlayer = GET_PLAYER_POINTER_I(mPlayerIndex);
	} else {
		mPlayerIndex = 0;
		mPlayer = 0;
	}
	mState = 0;
	g_CurrentMsg = msg_type;
	if ( g_CurrentMsg < 0 || g_CurrentMsg >= MAX_REG_MSGS )
		g_CurrentMsg = 0;
	function=modMsgs[g_CurrentMsg];
	endfunction=modMsgsEnd[g_CurrentMsg];
	RETURN_META(MRES_IGNORED);
}

void MessageEnd_Post(void) {
	if (endfunction) (*endfunction)(NULL);
	RETURN_META(MRES_IGNORED);
}

void WriteByte_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteChar_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteShort_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteLong_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteAngle_Post(float flValue) {
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteCoord_Post(float flValue) {
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteString_Post(const char *sz) {
	if (function) (*function)((void *)sz);
	RETURN_META(MRES_IGNORED);
}

void WriteEntity_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void StartFrame_Post(){
	if (g_bombAnnounce){
		switch (g_bombAnnounce){
		case BOMB_PLANTING:
			MF_ExecuteForward( iFBPlanting, static_cast<cell>(g_Planter) );
			break;
		case BOMB_PLANTED:
			MF_ExecuteForward( iFBPlanted, static_cast<cell>(g_Planter) );
			break;
		case BOMB_EXPLODE:
			MF_ExecuteForward( iFBExplode, static_cast<cell>(g_Planter), static_cast<cell>(g_Defuser) );
			break;
		case BOMB_DEFUSING:
			MF_ExecuteForward( iFBDefusing, static_cast<cell>(g_Defuser) );
			break;
		case BOMB_DEFUSED:
			MF_ExecuteForward( iFBDefused, static_cast<cell>(g_Defuser) );
			break;
		}
		g_bombAnnounce = 0;
	}
	RETURN_META(MRES_IGNORED);
}

void SetModel_Post(edict_t *e, const char *m){

	if ( !isModuleActive() )
	{
		RETURN_META(MRES_IGNORED);
	}

	if ( e->v.owner && m[7]=='w' && m[8]=='_' ){
		int w_id = 0;
		CPlayer *pPlayer = GET_PLAYER_POINTER(e->v.owner);
		switch(m[9]){
		case 'h':
			w_id = CSW_HEGRENADE;
			g_grenades.put(e, 2.0, 4, pPlayer);
			pPlayer->saveShot(CSW_HEGRENADE);
			break;
		case 'f':
			if (m[10]=='l') w_id = CSW_FLASHBANG;
			break;
		case 's':
			if (m[10]=='m') w_id = CSW_SMOKEGRENADE;
			break;
		}
		if ( w_id )	
			MF_ExecuteForward( iFGrenade, static_cast<cell>(pPlayer->index), 
			static_cast<cell>(ENTINDEX(e)), static_cast<cell>(w_id));
	}

	RETURN_META(MRES_IGNORED);
}

void EmitSound_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch) {
	if (sample[0]=='w'&&sample[1]=='e'&&sample[8]=='k'&&sample[9]=='n'&&sample[14]!='d'){
		CPlayer*pPlayer = GET_PLAYER_POINTER(entity);
		pPlayer->saveShot(CSW_KNIFE); /// Player attacks using their knife.
	}
	RETURN_META(MRES_IGNORED);
}

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr)
{
	if (ptr->pHit && (ptr->pHit->v.flags & (FL_CLIENT|FL_FAKECLIENT))
		&& e 
		&& (e->v.flags & (FL_CLIENT|FL_FAKECLIENT)))
	{
		CPlayer *pPlayer = GET_PLAYER_POINTER(e);
		pPlayer->aiming = ptr->iHitgroup;
	}

	RETURN_META(MRES_IGNORED);
}

void OnMetaAttach() {
	CVAR_REGISTER (&init_csstats_maxsize);
	CVAR_REGISTER (&init_csstats_reset);
	CVAR_REGISTER (&init_csstats_rank);
	csstats_maxsize=CVAR_GET_POINTER(init_csstats_maxsize.name);
	csstats_reset=CVAR_GET_POINTER(init_csstats_reset.name);
	csstats_rank=CVAR_GET_POINTER(init_csstats_rank.name);

	CVAR_REGISTER (&init_csstats_rankbots);
	CVAR_REGISTER (&init_csstats_pause);
	csstats_rankbots = CVAR_GET_POINTER(init_csstats_rankbots.name);
	csstats_pause = CVAR_GET_POINTER(init_csstats_pause.name);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "cstrike") == 0 ||
		strcasecmp(game, "czero") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}
void OnAmxxAttach(){
	MF_AddNatives(stats_Natives);

    char error[256];
	ConfigManager = MF_GetConfigManager();
	if (!ConfigManager->LoadGameConfigFile("common.games", &CommonConfig, error, sizeof(error)))
		MF_Log("Could not read common.games gamedata: %s", error);
	else
	{
		TypeDescription ofs;
		if (CommonConfig->GetOffsetByClass("CBaseMonster", "m_LastHitGroup", &ofs))
			m_LastHitGroup = ofs.fieldOffset;
        if (m_LastHitGroup < 1)
            MF_Log("Could not read CBaseMonster::m_LastHitGroup ofs.");

        if (CommonConfig->GetOffset("base", &ofs))
        {
            g_ofsBaseclass = ofs.fieldOffset;

            if (CommonConfig->GetOffset("traceattack", &ofs))
            {
                g_vfidxTraceAttack = ofs.fieldOffset;

                if (CommonConfig->GetOffset("takedamage", &ofs))
                {
                    g_vfidxTakeDamage = ofs.fieldOffset;
                    g_virtualCfg = true; /// Virtual cfg. successfully loaded.
                }
            }
        }

        if (false == g_virtualCfg)
            MF_Log("Could not read virtual data from common.games gamedata.");
	}

	const char* path =  get_localinfo("csstats_score");
	if ( path && *path ) 
		g_rank.loadCalc( MF_BuildPathname("%s",path) , error, sizeof(error));
	
	if ( !g_rank.begin() )
	{		
		g_rank.loadRank( MF_BuildPathname("%s",
			get_localinfo("csstats") ) );
	}
}

void OnAmxxDetach() {
	g_grenades.clear();
	g_rank.clear();
	g_rank.unloadCalc();

    /**
     * Restore orig. vfunc. addr. on module detach.
     */
    if (g_origTraceAttack)
    {
        allowFullMemAccess(&g_ppvtbl_CBasePlayer[g_vfidxTraceAttack], sizeof(size_t*));
        g_ppvtbl_CBasePlayer[g_vfidxTraceAttack] = (size_t*)g_origTraceAttack;
    }
    if (g_origTakeDamage)
    {
        allowFullMemAccess(&g_ppvtbl_CBasePlayer[g_vfidxTakeDamage], sizeof(size_t*));
        g_ppvtbl_CBasePlayer[g_vfidxTakeDamage] = (size_t*)g_origTakeDamage;
    }

    if (g_origTraceAttack_Bots)
    {
        allowFullMemAccess(&g_ppvtbl_CBasePlayer_Bots[g_vfidxTraceAttack], sizeof(size_t*));
        g_ppvtbl_CBasePlayer_Bots[g_vfidxTraceAttack] = (size_t*)g_origTraceAttack_Bots;
    }
    if (g_origTakeDamage_Bots)
    {
        allowFullMemAccess(&g_ppvtbl_CBasePlayer_Bots[g_vfidxTakeDamage], sizeof(size_t*));
        g_ppvtbl_CBasePlayer_Bots[g_vfidxTakeDamage] = (size_t*)g_origTakeDamage_Bots;
    }

    if (CommonConfig)
        ConfigManager->CloseGameConfigFile(CommonConfig);
}

void OnPluginsLoaded(){
	iFDeath = MF_RegisterForward("client_death",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFDamage = MF_RegisterForward("client_damage",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFBPlanted = MF_RegisterForward("bomb_planted",ET_IGNORE,FP_CELL,FP_DONE);
	iFBDefused = MF_RegisterForward("bomb_defused",ET_IGNORE,FP_CELL,FP_DONE);
	iFBPlanting = MF_RegisterForward("bomb_planting",ET_IGNORE,FP_CELL,FP_DONE);
	iFBDefusing = MF_RegisterForward("bomb_defusing",ET_IGNORE,FP_CELL,FP_DONE);
	iFBExplode = MF_RegisterForward("bomb_explode",ET_IGNORE,FP_CELL,FP_CELL,FP_DONE);
	iFGrenade = MF_RegisterForward("grenade_throw",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
}
