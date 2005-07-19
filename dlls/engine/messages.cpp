#include "engine.h"

CVector<argMsg> Msg;
CVector<int> msgHooks[256];
int msgBlocks[256] = {0};
int msgDest;
int msgType;
float *msgOrigin;
edict_t *msgpEntity;
bool inhook = false;
bool inblock = false;
unsigned int msgCount = 0;

argMsg::argMsg()
{
	Reset();
}

void argMsg::Reset()
{
	memset(&v, 0, sizeof(v));
	cData.clear();
	type = 0;
}

void argMsg::Send()
{
	switch (type)
	{
	case arg_byte:
		WRITE_BYTE(v.iData);
		break;
	case arg_char:
		WRITE_CHAR(v.iData);
		break;
	case arg_short:
		WRITE_SHORT(v.iData);
		break;
	case arg_long:
		WRITE_LONG(v.iData);
		break;
	case arg_angle:
		WRITE_ANGLE(v.fData);
		break;
	case arg_coord:
		WRITE_COORD(v.fData);
		break;
	case arg_string:
		WRITE_STRING(cData.c_str());
		break;
	case arg_entity:
		WRITE_ENTITY(v.iData);
		break;
	}
	Reset();
}

int argMsg::Type()
{
	switch (type)
	{
	case arg_byte:
		return type_int;
		break;
	case arg_char:
		return type_int;
		break;
	case arg_short:
		return type_int;
		break;
	case arg_long:
		return type_int;
		break;
	case arg_angle:
		return type_float;
		break;
	case arg_coord:
		return type_float;
		break;
	case arg_string:
		return type_string;
		break;
	case arg_entity:
		return type_int;
		break;
	}

	return 0;
}

void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (msgBlocks[msg_type]) {
		inblock = true;
		msgType = msg_type;
		RETURN_META(MRES_SUPERCEDE);
	} else if (msgHooks[msg_type].size()) {
		inhook = true;
		msgCount = 0;
		msgDest = msg_dest;
		msgType = msg_type;
		msgOrigin = (float *)pOrigin;
		msgpEntity = ed;
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteByte(int iValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.iData = iValue;
			p.type = arg_byte;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.iData = iValue;
			Msg[msgCount-1].type = arg_byte;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteChar(int iValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.iData = iValue;
			p.type = arg_char;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.iData = iValue;
			Msg[msgCount-1].type = arg_char;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteShort(int iValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.iData = iValue;
			p.type = arg_short;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.iData = iValue;
			Msg[msgCount-1].type = arg_short;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteLong(int iValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.iData = iValue;
			p.type = arg_long;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.iData = iValue;
			Msg[msgCount-1].type = arg_long;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteAngle(float flValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.fData = flValue;
			p.type = arg_angle;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.fData = flValue;
			Msg[msgCount-1].type = arg_angle;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteCoord(float flValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.fData = flValue;
			p.type = arg_coord;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.fData = flValue;
			Msg[msgCount-1].type = arg_coord;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteString(const char *sz)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.cData.assign(sz);
			p.type = arg_string;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].cData.assign(sz);
			Msg[msgCount-1].type = arg_string;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void WriteEntity(int iValue)
{
	if (inblock) {
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		if (++msgCount > Msg.size()) {
			argMsg p;
			p.v.iData = iValue;
			p.type = arg_entity;
			Msg.push_back(p);
		} else {
			Msg[msgCount-1].v.iData = iValue;
			Msg[msgCount-1].type = arg_entity;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void MessageEnd(void)
{
	int mres = 0, mresB = 0;
	unsigned int i = 0;
	if (inblock) {
		inblock = false;
		if (msgBlocks[msgType] == BLOCK_ONCE)
			msgBlocks[msgType] = BLOCK_NOT;
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		for (i=0; i<msgHooks[msgType].size(); i++)
		{
				mresB = MF_ExecuteForward(msgHooks[msgType].at(i), msgType, msgDest, ENTINDEX(msgpEntity));
				if (mresB > mres)
						mres = mresB;
		}
		inhook = false;
		if (mres & 1)
		{
			msgCount = 0;
			RETURN_META(MRES_SUPERCEDE);
		}
		MESSAGE_BEGIN(msgDest, msgType, msgOrigin, msgpEntity);
		for (i=0; i<msgCount; i++) {
			Msg[i].Send();
			Msg[i].Reset();
		}
		MESSAGE_END();
		msgCount = 0;
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

static cell AMX_NATIVE_CALL register_message(AMX *amx, cell *params)
{
	int len;
	char *name = MF_GetAmxString(amx, params[2], 0, &len);
	if (params[1]>0 && params[1] < 256) {
		int id = MF_RegisterSPForwardByName(amx, name, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		if (id != -1)
		{
			msgHooks[params[1]].push_back(id);
			return id;
		} else {
			MF_LogError(amx, AMX_ERR_NOTFOUND, "Could not find function \"%s\"", name);
			return -1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL set_msg_block(AMX *amx, cell *params)
{
	int msgid = params[1];
	int block = params[2];

	if (msgid < 1 || msgid > 255) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	msgBlocks[msgid] = block;

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_block(AMX *amx, cell *params)
{
	int msgid = params[1];

	if (msgid < 1 || msgid > 255) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return msgBlocks[msgid];
}

static cell AMX_NATIVE_CALL get_msg_args(AMX *amx, cell *params)
{
	return msgCount;
}

static cell AMX_NATIVE_CALL get_msg_argtype(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return Msg[argn].type;
}

static cell AMX_NATIVE_CALL get_msg_arg_int(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int iVal = Msg[argn].v.iData;

	return iVal;
}

static cell AMX_NATIVE_CALL set_msg_arg_int(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	Msg[argn].type = params[2];
	Msg[argn].v.iData = params[3];

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_float(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return amx_ftoc(Msg[argn].v.fData);
}

static cell AMX_NATIVE_CALL set_msg_arg_float(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	REAL fVal = amx_ctof(params[2]);

	Msg[argn].v.fData = fVal;

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_string(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	const char *szVal = Msg[argn].cData.c_str();

	return MF_SetAmxString(amx, params[2], szVal?szVal:"", params[3]);
}

static cell AMX_NATIVE_CALL set_msg_arg_string(AMX *amx, cell *params)
{
	unsigned int argn = params[1]-1;
	int iLen;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	char *szVal = MF_GetAmxString(amx, params[2], 0, &iLen);

	Msg[argn].cData.assign(szVal);

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_origin(AMX *amx, cell *params)
{
	if (!inhook) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	cell *cAddr = MF_GetAmxAddr(amx, params[1]);

	if (msgDest >= MSG_PVS && msgDest <= MSG_PAS_R)
	{
		vec3_t vRet = (Vector)msgOrigin;
		cAddr[0] = amx_ftoc(vRet.x);
		cAddr[1] = amx_ftoc(vRet.y);
		cAddr[2] = amx_ftoc(vRet.z);
	} else {
		cAddr[0] = 0;
		cAddr[1] = 0;
		cAddr[2] = 0;
	}

	return 1;
}

AMX_NATIVE_INFO msg_Natives[] = {
	{"register_message",	register_message},

	{"set_msg_block",		set_msg_block},
	{"get_msg_block",		get_msg_block},

	{"get_msg_args",		get_msg_args},
	{"get_msg_argtype",		get_msg_argtype},
	{"get_msg_arg_int",		get_msg_arg_int},
	{"set_msg_arg_int",		set_msg_arg_int},
	{"get_msg_arg_float",	get_msg_arg_float},
	{"set_msg_arg_float",	set_msg_arg_float},
	{"get_msg_arg_string",	get_msg_arg_string},
	{"set_msg_arg_string",	set_msg_arg_string},
	{"get_msg_origin",		get_msg_origin},

	{NULL,					NULL},
};
