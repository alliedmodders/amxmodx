#include "messages.h"

using namespace std;

std::vector<argMsg*> Msg;
int msgDest;
int msgType;
float *msgOrigin;
edict_t *msgpEntity;
int msgHooks[256] = {0};
int msgBlocks[256] = {0};
bool inhook = false;
bool inblock = false;
unsigned int msgCount = 0;

argMsg::argMsg()
{
	Reset();
}

void argMsg::Reset()
{
	iData = 0;
	fData = 0.0;
	cData.clear();
	type = 0;
}

void argMsg::Send()
{
	switch (type)
	{
	case arg_byte:
		WRITE_BYTE(iData);
		break;
	case arg_char:
		WRITE_CHAR(iData);
		break;
	case arg_short:
		WRITE_SHORT(iData);
		break;
	case arg_long:
		WRITE_LONG(iData);
		break;
	case arg_angle:
		WRITE_ANGLE(fData);
		break;
	case arg_coord:
		WRITE_COORD(fData);
		break;
	case arg_string:
		WRITE_STRING(cData.c_str());
		break;
	case arg_entity:
		WRITE_ENTITY(iData);
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
		RETURN_META(MRES_SUPERCEDE);
	} else if (msgHooks[msg_type]) {
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
			argMsg *p = new argMsg();
			p->iData = iValue;
			p->type = arg_byte;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->iData = iValue;
			Msg.at(msgCount-1)->type = arg_byte;
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
			argMsg *p = new argMsg();
			p->iData = iValue;
			p->type = arg_char;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->iData = iValue;
			Msg.at(msgCount-1)->type = arg_char;
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
			argMsg *p = new argMsg();
			p->iData = iValue;
			p->type = arg_short;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->iData = iValue;
			Msg.at(msgCount-1)->type = arg_short;
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
			argMsg *p = new argMsg();
			p->iData = iValue;
			p->type = arg_long;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->iData = iValue;
			Msg.at(msgCount-1)->type = arg_long;
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
			argMsg *p = new argMsg();
			p->fData = flValue;
			p->type = arg_angle;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->fData = flValue;
			Msg.at(msgCount-1)->type = arg_angle;
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
			argMsg *p = new argMsg();
			p->fData = flValue;
			p->type = arg_coord;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->fData = flValue;
			Msg.at(msgCount-1)->type = arg_coord;
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
			argMsg *p = new argMsg();
			p->cData.assign(sz);
			p->type = arg_string;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->cData.assign(sz);
			Msg.at(msgCount-1)->type = arg_string;
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
			argMsg *p = new argMsg();
			p->iData = iValue;
			p->type = arg_entity;
			Msg.push_back(p);
		} else {
			Msg.at(msgCount-1)->iData = iValue;
			Msg.at(msgCount-1)->type = arg_entity;
		}
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void MessageEnd(void)
{
	int mres = 0;
	int mct = msgCount;
	unsigned int i = 0;
	if (inblock) {
		if (msgBlocks[msgType] == BLOCK_ONCE)
			msgBlocks[msgType] = BLOCK_NOT;
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		inhook = false;
		mres = MF_ExecuteForward(msgHooks[msgType], msgType, msgDest, ENTINDEX(msgpEntity));
		MF_Log("Executing forward: %d with retval %d", msgHooks[msgType], mres);
		if (mres & 1)
			RETURN_META(MRES_SUPERCEDE);
		MESSAGE_BEGIN(msgDest, msgType, msgOrigin, msgpEntity);
		for (i=0; i<msgCount; i++) {
			Msg.at(i)->Send();
			Msg.at(i)->Reset();
		}
		MESSAGE_END();
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

static cell AMX_NATIVE_CALL register_message(AMX *amx, cell *params)
{
	int len;
	if (params[1]>0 && params[1] < 256) {
		int id = MF_RegisterSPForwardByName(amx, MF_GetAmxString(amx, params[2], 0, &len), FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		MF_Log("Registering message %d with result %d", params[1], id);
		msgHooks[params[1]] = id;
		return id;
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
	int block = params[2];

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
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return Msg.at(argn-1)->Type();
}

static cell AMX_NATIVE_CALL get_msg_arg_int(AMX *amx, cell *params)
{
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int iVal = Msg.at(argn-1)->iData;

	return iVal;
}

static cell AMX_NATIVE_CALL set_msg_arg_int(AMX *amx, cell *params)
{
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	Msg.at(argn-1)->iData = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_float(AMX *amx, cell *params)
{
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return amx_ftoc(Msg.at(argn-1)->fData);
}

static cell AMX_NATIVE_CALL set_msg_arg_float(AMX *amx, cell *params)
{
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	REAL fVal = amx_ctof(params[2]);

	Msg.at(argn-1)->fData = fVal;

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_string(AMX *amx, cell *params)
{
	unsigned int argn = params[1];

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	const char *szVal = Msg.at(argn-1)->cData.c_str();

	return MF_SetAmxString(amx, params[2], szVal, params[3]);
}

static cell AMX_NATIVE_CALL set_msg_arg_string(AMX *amx, cell *params)
{
	unsigned int argn = params[1];
	int iLen;

	if (!inhook || argn >= Msg.size()) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	char *szVal = MF_GetAmxString(amx, params[2], 0, &iLen);

	Msg.at(argn-1)->cData.assign(szVal);

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_origin(AMX *amx, cell *params)
{
	if (!inhook) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}
	vec3_t vRet = (Vector)msgOrigin;
	cell *cAddr = MF_GetAmxAddr(amx, params[1]);

	cAddr[0] = amx_ftoc(vRet.x);
	cAddr[1] = amx_ftoc(vRet.y);
	cAddr[2] = amx_ftoc(vRet.z);

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