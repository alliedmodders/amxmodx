/* AMX Mod X
*   Engine Module
*
* by the AMX Mod X Development Team
*  thanks to Vexd
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

#define VERSION "0.1"

using namespace std;

plugin_info_t Plugin_info = {

	META_INTERFACE_VERSION, // ifvers
	"Engine", // name
	VERSION,  // version
	__DATE__, // date
	"AMX Mod X Dev Team",  // author
	"http://www.amxmodx.org",  // url
	"ENGINE", // logtag
	PT_ANYTIME,// (when) loadable
	PT_ANYTIME,// (when) unloadable

};

module_info_s module_info = {
	"Engine", // name
	"AMX Mod X Dev Team", // author
	VERSION, // version
	AMX_INTERFACE_VERSION,
	STATIC_MODULE,
};

class AmxCallList {
  public:
struct AmxCall {
  AMX *amx;
  int iFunctionIdx;
   AmxCall* next;
   AmxCall( AMX *a , int i, AmxCall* n  ) : amx(a), iFunctionIdx(i), next(n) {}
} *head;

  AmxCallList() { head = 0; }

  ~AmxCallList() { clear(); }

  void clear() {
      while ( head )  {
          AmxCall* a = head->next;
          delete head;
          head = a;
      }
  }

  void put( AMX *a , int i )
  {
    head = new AmxCall( a, i , head  );
  }

};

AmxCallList pfnTouch;
AmxCallList serverFrame;
AmxCallList preThink;
AmxCallList postThink;
AmxCallList clientKill;
AmxCallList Msgs;

meta_globals_t *gpMetaGlobals;
gamedll_funcs_t *gpGamedllFuncs;
mutil_funcs_t *gpMetaUtilFuncs;
enginefuncs_t g_engfuncs;
globalvars_t  *gpGlobals;
pfnamx_engine_g* g_engAmxFunc;
pfnmodule_engine_g* g_engModuleFunc;

extern AMX_NATIVE_INFO Engine_Natives[];

void (*function)(void*);

void (*endfunction)(void*);

#ifdef __linux__
int thread_fork(void *arg);
#endif

#define AMS_OFFSET 0.01

#define SPEAK_NORMAL 0
#define SPEAK_MUTED 1
#define SPEAK_ALL 2
#define SPEAK_LISTENALL 4

#define CAMERA_NONE 0
#define CAMERA_3RDPERSON 1
#define CAMERA_UPLEFT 2
#define CAMERA_TOPDOWN 3

#define MAX_MESSAGES 255

#define BLOCK_NOT 0
#define BLOCK_ONCE 1
#define BLOCK_SET 2

enum {
	gamestate,
	oldbuttons,
	groupinfo,
	iuser1,
	iuser2,
	iuser3,
	iuser4,
	weaponanim,
	pushmsec,
	bInDuck,
	flTimeStepSound,
	flSwimTime,
	flDuckTime,
	iStepLeft,
	movetype,
	solid,
	skin,			
	body,
	effects,
	light_level,
	sequence,
	gaitsequence,
	modelindex,
	playerclass,
	waterlevel,
	watertype,
	spawnflags,
	flags,
	colormap,
	team,
	fixangle,
	weapons,
	rendermode,
	renderfx,
	button,
	impulse,
	deadflag,
};

enum {
	impacttime,
	starttime,
	idealpitch,
	pitch_speed,
	ideal_yaw,
	yaw_speed,
	ltime,
	nextthink,
	gravity,
	friction,
	frame,
	animtime,
	framerate,
	health,
	frags,
	takedamage,
	max_health,
	teleport_time,
	armortype,
	armorvalue,
	dmg_take,
	dmg_save,
	dmg,
	dmgtime,
	speed,
	air_finished,
	pain_finished,
	radsuit_finished,
	scale,
	renderamt,
	maxspeed,
	fov,
	flFallVelocity,
	fuser1,
	fuser2,
	fuser3,
	fuser4,
};

enum {
	origin,
	oldorigin,
	velocity,
	basevelocity,
	clbasevelocity,
	movedir,
	angles,
	avelocity,
	punchangle,
	v_angle,
	endpos,
	startpos,
	absmin,
	absmax,
	mins,
	maxs,
	size,
	rendercolor,
	view_ofs,
	vuser1,
	vuser2,
	vuser3,
	vuser4,
};

enum {
	chain,
	dmg_inflictor,
	enemy,
	aiment,
	owner,
	groundentity,
	pContainingEntity,
	euser1,
	euser2,
	euser3,
	euser4,
};

enum {
	classname,
	globalname,
	model,
	target,
	targetname,
	netname,
	message,
	noise,
	noise1,
	noise2,
	noise3,
	viewmodel,
	weaponmodel,
};

enum {
	controller1,
	controller2,
	controller3,
	controller4,
	blending1,
	blending2,
};

struct PlayerInfo {
	bool bModeled;
	char szModel[32];
	float fModelSet;

	int iSpeakFlags;

	edict_t *pViewEnt;
	int iViewType;
	int iRenderMode;
	float fRenderAmt;
};

PlayerInfo PlInfo[33];

struct GlobalInfo {
	bool bLights;
	float fNextLights;
	char *szLastLights[128];
	char *szRealLights[128];

	int iMessageBlock[MAX_MESSAGES];
	bool bBlocking;
};

enum {
	arg_byte = 1,
	arg_char,
	arg_short,
	arg_long,
	arg_angle,
	arg_coord,
	arg_string,
	arg_entity,
};

class argStack {
public:
	argStack(argStack *p=NULL)	//initialize with link to previous
	{
		next = p;
		init();
	}

	void init()
	{
		writebyte = 0;
		writechar = '\0';
		writeshort = 0;
		writelong = (long)0;
		writeangle = 0.0;
		writecoord = 0.0;
		writeentity = 0;
	}

	argStack* arg()
	{
		argStack *p;
		p = new argStack();	//get the new pointer
		next = p;			//link this to the next pointer
		return p;
	}

	argStack* link()		//return link
	{
		return next;
	}

	void put(int arg_type, int i)
	{
		argtype = arg_type;
		switch (argtype)
		{
		case arg_byte:
			writebyte = i;
			break;
		case arg_char:
			writechar = i;
			break;
		case arg_entity:
			writeentity = i;
			break;
		case arg_short:
			writeshort = i;
			break;
		case arg_long:
			writelong = i;
			break;
		}
	}

	void put(int arg_type, float f)
	{
		argtype = arg_type;
		switch (argtype)
		{
		case arg_angle:
			writeangle = f;
			break;
		case arg_coord:
			writecoord = f;
			break;
		}
	}

	void put_string(int arg_type, const char *sz)
	{
		argtype = arg_type;
		switch (argtype)
		{
		case arg_string:
			writestring.append((char *)sz);
			break;
		}
	}

	void write_arg()
	{
		switch (argtype)
		{
		case arg_byte:
			WRITE_BYTE(writebyte);
			break;
		case arg_char:
			WRITE_CHAR(writechar);
			break;
		case arg_short:
			WRITE_SHORT(writeshort);
			break;
		case arg_long:
			WRITE_LONG(writelong);
			break;
		case arg_angle:
			WRITE_ANGLE(writeangle);
			break;
		case arg_coord:
			WRITE_COORD(writecoord);
			break;
		case arg_string:
			WRITE_STRING(writestring.c_str());
			break;
		case arg_entity:
			WRITE_ENTITY(writeentity);
			break;
		}
	}

	int getarg_int(int arg_type)
	{
		switch (argtype)
		{
		case arg_byte:
			return writebyte;
			break;
		case arg_char:
			return writechar;
			break;
		case arg_short:
			return writeshort;
			break;
		case arg_long:
			return writelong;
			break;
		case arg_entity:
			return writeentity;
			break;
		}

		return 0;
	}

	float getarg_float(int arg_type)
	{
		switch (argtype)
		{
		case arg_angle:
			return writeangle;
			break;
		case arg_coord:
			return writecoord;
			break;
		}
		return 0.0;
	}

	int getarg_strlen(int arg_type)
	{
		switch (argtype)
		{
		case arg_string:
			return (writestring.length());
			break;
		}
		return 0;
	}

	const char *getarg_string(int arg_type)
	{
		switch (argtype)
		{
		case arg_string:
			return writestring.c_str();
			break;
		}

		return '\0';
	}

	int get_argtype()
	{
		return argtype;
	}

	bool is_arg(int arg_type)
	{
		switch (argtype)
		{
		case arg_byte:
			{
				switch (arg_type)
				{
				case arg_byte:
					return true;
					break;
				case arg_char:
					return true;
					break;
				case arg_short:
					return true;
					break;
				case arg_long:
					return true;
					break;
				case arg_entity:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_char:
			{
				switch (arg_type)
				{
				case arg_byte:
					return true;
					break;
				case arg_char:
					return true;
					break;
				case arg_short:
					return true;
					break;
				case arg_long:
					return true;
					break;
				case arg_entity:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_short:
			{
				switch (arg_type)
				{
				case arg_byte:
					return true;
					break;
				case arg_char:
					return true;
					break;
				case arg_short:
					return true;
					break;
				case arg_long:
					return true;
					break;
				case arg_entity:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_long:
			{
				switch (arg_type)
				{
				case arg_byte:
					return true;
					break;
				case arg_char:
					return true;
					break;
				case arg_short:
					return true;
					break;
				case arg_long:
					return true;
					break;
				case arg_entity:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_entity:
			{
				switch (arg_type)
				{
				case arg_byte:
					return true;
					break;
				case arg_char:
					return true;
					break;
				case arg_short:
					return true;
					break;
				case arg_long:
					return true;
					break;
				case arg_entity:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_angle:
			{
				switch (arg_type)
				{
				case arg_angle:
					return true;
					break;
				case arg_coord:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_coord:
			{
				switch (arg_type)
				{
				case arg_angle:
					return true;
					break;
				case arg_coord:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		case arg_string:
			{
				switch (arg_type)
				{
				case arg_string:
					return true;
					break;
				default:
					return false;
					break;
				}
				break;
			}
		default:
			{
				return false;
				break;
			}
		}
	}

private:
	int argtype;
	int writebyte;
	int writechar;
	int writeshort;
	int writelong;
	float writeangle;
	float writecoord;
	string writestring;
	int writeentity;
	argStack *next;
};

class MessageInfo
{
public:
	MessageInfo(int msgdest, int msg_id, const float *Origin, edict_t* ed)
	{
		msgID = msg_id;
		msg_dest = msgdest;
		pOrigin = Origin;
		v = ed;
		CHeadArg = new argStack();
		CTailArg = NULL;
		argcount = 0;
	}

	~MessageInfo()
	{
		Destroy();
	}

	void SendMsg()
	{
		//we are going to build a message =D yay! gather 'round and watch.
		argStack *p;
		MESSAGE_BEGIN(msg_dest, msgID, pOrigin, v);
		for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
			p->write_arg();
		}
		MESSAGE_END();
		Destroy();	//clean up old arguments
	}

	void AddArg(int i, int j)
	{
		argStack *p;
		if (CTailArg == NULL) {
			p = CHeadArg->arg();
			CTailArg = p;
		} else {
			p = CTailArg->arg();
			CTailArg = p;
		}
		CTailArg->put(i,j);
		argcount++;
	}

	void AddArg(int i, float f)
	{
		argStack *p;
		if (CTailArg == NULL) {
			p = CHeadArg->arg();
			CTailArg = p;
		} else {
			p = CTailArg->arg();
			CTailArg = p;
		}
		CTailArg->put(i,f);
		argcount++;
	}

	void AddArgString(int i, const char *sz)
	{
		argStack *p;
		if (CTailArg == NULL) {
			p = CHeadArg->arg();
			CTailArg = p;
		} else {
			p = CTailArg->arg();
			CTailArg = p;
		}
		CTailArg->put_string(i,sz);
		argcount++;
	}

	void Destroy()
	{
		argStack *p;

		p = CHeadArg->link();

		while (p)  {
			argStack *n = p->link();
			delete p;
			p = n;
		}
		argcount = 0;
	}

	bool Set(int n, int arg_type, int data)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return false;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					p->put(arg_type, data);
					return true;
				}
			}
		}

		return false;
	}

	bool Set(int n, int arg_type, float data)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return false;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					p->put(arg_type, data);
					return true;
				}
			}
		}

		return false;
	}

	bool Set(int n, int arg_type, char* data)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return false;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					p->put_string(arg_type, (const char*)data);
					return true;
				}
			}
		}

		return false;
	}

	int RetArg_Int(int n)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return -1;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					return p->getarg_int(arg_short);
				}
			}
		}

		return 0;
	}

	float RetArg_Float(int n)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return -1;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					return p->getarg_float(arg_coord);
				}
			}
		}

		return 0.0;
	}

	int RetArg_Strlen(int n)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return 0;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					return p->getarg_strlen(arg_string);
				}
			}
		}

		return 0;
	}

	const char* RetArg_String(int n)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return '\0';
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					return p->getarg_string(arg_string);
				}
			}
		}

		return '\0';
	}

	int ArgType(int n)
	{
		argStack *p;
		int i=0;
		if (n>argcount) {
			return -1;
		} else {
			for (p=CHeadArg->link(); p!=NULL; p=p->link()) {
				i++;
				if (i==n) {
					return p->get_argtype();
				}
			}
		}

		return 0;
	}

	int args()
	{
		return argcount;
	}

private:
	int argcount;
	int msgID;
	int msg_dest;
	const float *pOrigin;
	edict_t *v;
	argStack *CHeadArg;
	argStack *CTailArg;
};
