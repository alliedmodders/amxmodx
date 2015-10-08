// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "messages.h"

Message Msg;
RegisteredMessage msgHooks[256];
int msgBlocks[256] = {BLOCK_NOT};
int msgDest;
int msgType;
float *msgOrigin;
edict_t *msgpEntity;
bool inhook = false;
bool inblock = false;
enginefuncs_t *g_pEngTable = NULL;

void ClearMessages()
{
	for (size_t i=0; i<MAX_MESSAGES; i++)
	{
		msgHooks[i].Clear();
		msgBlocks[i] = BLOCK_NOT;
	}
}

Message::Message()
{
	m_CurParam = 0;
}

bool Message::Ready()
{
	if (!m_Params.length())
		return false;
	return true;
}

void Message::Init()
{
	if (!Ready())
	{
		msgparam *p = new msgparam;
		m_Params.append(p);
	}
	m_CurParam = 0;
}

Message::~Message()
{
	for (size_t i=0; i<m_Params.length(); i++)
		delete m_Params[i];
	
	m_Params.clear();
}

msgparam *Message::AdvPtr()
{
	msgparam *pParam = NULL;

	if (++m_CurParam >= m_Params.length())
	{
		pParam = new msgparam;
		m_Params.append(pParam);
	} else {
		pParam = m_Params[m_CurParam];
	}

	return pParam;
}

void Message::AddParam(const char *data, msgtype type)
{
	msgparam *pParam = AdvPtr();

	pParam->szData = data;
	pParam->type = type;
}

void Message::AddParam(int data, msgtype type)
{
	msgparam *pParam = AdvPtr();
	
	pParam->v.iData = data;
	pParam->type = type;
}

void Message::AddParam(float data, msgtype type)
{
	msgparam *pParam = AdvPtr();

	pParam->v.fData = data;
	pParam->type = type;
}

msgtype Message::GetParamType(size_t index)
{
	if (index < 1 || index > m_CurParam)
		return static_cast<msgtype>(0);

	return m_Params[index]->type;
}

float Message::GetParamFloat(size_t index)
{
	if (index < 1 || index > m_CurParam)
		return 0;

	return m_Params[index]->v.fData;
}

const char *Message::GetParamString(size_t index)
{
	if (index < 1 || index > m_CurParam)
		return 0;

	return m_Params[index]->szData.chars();
}

int Message::GetParamInt(size_t index)
{
	if (index < 1 || index > m_CurParam)
		return 0;

	return m_Params[index]->v.iData;
}

void Message::SetParam(size_t index, float data)
{
	if (index < 1 || index > m_CurParam)
		return;

	m_Params[index]->v.fData = data;
}

void Message::SetParam(size_t index, int data)
{
	if (index < 1 || index > m_CurParam)
		return;

	m_Params[index]->v.iData = data;
}

void Message::SetParam(size_t index, const char *data)
{
	if (index < 1 || index > m_CurParam)
		return;

	m_Params[index]->szData = data;
}

void Message::Reset()
{
	m_CurParam = 0;
}

size_t Message::Params()
{
	return m_CurParam;
}

void Message::Send()
{
	msgparam *pParam = NULL;

	for (size_t i=1; i<=m_CurParam; i++)
	{
		pParam = m_Params[i];
		switch (pParam->type)
		{
		case arg_byte:
			WRITE_BYTE(pParam->v.iData);
			break;
		case arg_char:
			WRITE_CHAR(pParam->v.iData);
			break;
		case arg_short:
			WRITE_SHORT(pParam->v.iData);
			break;
		case arg_long:
			WRITE_LONG(pParam->v.iData);
			break;
		case arg_angle:
			WRITE_ANGLE(pParam->v.fData);
			break;
		case arg_coord:
			WRITE_COORD(pParam->v.fData);
			break;
		case arg_string:
			WRITE_STRING(pParam->szData.chars());
			break;
		case arg_entity:
			WRITE_ENTITY(pParam->v.iData);
			break;
		}
	}
}

void C_MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (msgBlocks[msg_type])
	{
		inblock = true;
		msgType = msg_type;
		RETURN_META(MRES_SUPERCEDE);
	} else if (msgHooks[msg_type].Hooked()) {
		inhook = true;
		msgDest = msg_dest;
		msgType = msg_type;
		msgOrigin = (float *)pOrigin;
		msgpEntity = ed;
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteByte(int iValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(iValue, arg_byte);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteChar(int iValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(iValue, arg_char);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteShort(int iValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(iValue, arg_short);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteLong(int iValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(iValue, arg_long);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteAngle(float flValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(flValue, arg_angle);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteCoord(float flValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(flValue, arg_coord);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteString(const char *sz)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(sz, arg_string);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_WriteEntity(int iValue)
{
	if (inblock)
	{
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {
		Msg.AddParam(iValue, arg_entity);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void C_MessageEnd(void)
{
	int mres = 0;
	
	if (inblock)
	{
		inblock = false;
		if (msgBlocks[msgType] == BLOCK_ONCE)
		{
			msgBlocks[msgType] = BLOCK_NOT;
		}
		RETURN_META(MRES_SUPERCEDE);
	} else if (inhook) {

		mres = msgHooks[msgType].Execute((cell)msgType, (cell)msgDest, (cell)ENTINDEX(msgpEntity));

		/*
		for (i=0; i<msgHooks[msgType].size(); i++)
		{
				mresB = executeForwards(msgHooks[msgType].at(i), (cell)msgType, (cell)msgDest, (cell)ENTINDEX(msgpEntity));
				if (mresB > mres)
						mres = mresB;
		}
		*/
		inhook = false;
		if (mres & 1)
		{
			Msg.Reset();
			RETURN_META(MRES_SUPERCEDE);
		}

		/* send the real message */
		MESSAGE_BEGIN(msgDest, msgType, msgOrigin, msgpEntity);
		Msg.Send();
		MESSAGE_END();

		/* reset */
		Msg.Reset();

		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

static cell _message_begin(AMX *amx, cell *params, bool useFloat) /* 4 param */
{
	int numparam = *params / sizeof(cell);
	float vecOrigin[3];
	cell *cpOrigin;

	if (params[2] < 1 || ((params[2] > 63)		// maximal number of engine messages
		&& !GET_USER_MSG_NAME(PLID, params[2], NULL)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Plugin called message_begin with an invalid message id (%d).", params[2]);
		return 0;
	}

	switch (params[1])
	{
	case MSG_BROADCAST:
	case MSG_ALL:
	case MSG_SPEC:
	case MSG_INIT:
		MESSAGE_BEGIN(params[1], params[2], NULL);
		break;
	case MSG_PVS: case MSG_PAS:
	case MSG_PVS_R: case MSG_PAS_R:
		if (numparam < 3)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
			return 0;
		}

		cpOrigin = get_amxaddr(amx, params[3]);

		if (!useFloat)
		{
			vecOrigin[0] = static_cast<float>(*cpOrigin);
			vecOrigin[1] = static_cast<float>(*(cpOrigin + 1));
			vecOrigin[2] = static_cast<float>(*(cpOrigin + 2));
		} else {
			vecOrigin[0] = amx_ctof(*cpOrigin);
			vecOrigin[1] = amx_ctof(*(cpOrigin + 1));
			vecOrigin[2] = amx_ctof(*(cpOrigin + 2));
		}

		MESSAGE_BEGIN(params[1], params[2], vecOrigin);

		break;
	case MSG_ONE_UNRELIABLE:
	case MSG_ONE:
		if (numparam < 4)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
			return 0;
		}

		MESSAGE_BEGIN(params[1], params[2], NULL, TypeConversion.id_to_edict(params[4]));
		break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL message_begin(AMX *amx, cell *params) /* 4 param */
{
	return _message_begin(amx, params, false);
}

static cell AMX_NATIVE_CALL message_begin_f(AMX *amx, cell *params) /* 4 param */
{
	return _message_begin(amx, params, true);
}

static cell AMX_NATIVE_CALL message_end(AMX *amx, cell *params)
{
	MESSAGE_END();
	return 1;
}

static cell AMX_NATIVE_CALL write_byte(AMX *amx, cell *params) /* 1 param */
{
	WRITE_BYTE(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL write_char(AMX *amx, cell *params) /* 1 param */
{
	WRITE_CHAR(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL write_short(AMX *amx, cell *params) /* 1 param */
{
	WRITE_SHORT(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL write_long(AMX *amx, cell *params) /* 1 param */
{
	WRITE_LONG(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL write_entity(AMX *amx, cell *params) /* 1 param */
{
	WRITE_ENTITY(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL write_angle(AMX *amx, cell *params) /* 1 param */
{
	WRITE_ANGLE(static_cast<float>(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL write_angle_f(AMX *amx, cell *params) /* 1 param */
{
	WRITE_ANGLE(amx_ctof(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL write_coord(AMX *amx, cell *params) /* 1 param */
{
	WRITE_COORD(static_cast<float>(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL write_coord_f(AMX *amx, cell *params) /* 1 param */
{
	WRITE_COORD(amx_ctof(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL write_string(AMX *amx, cell *params) /* 1 param */
{
	int a;
	WRITE_STRING(get_amxstring(amx, params[1], 3, a));

	return 1;
}

static cell AMX_NATIVE_CALL register_message(AMX *amx, cell *params)
{
	int len;
	char *name = get_amxstring(amx, params[2], 0, len);

	if (!Msg.Ready())
		Msg.Init();

	if (params[1]>0 && params[1] < 256)
	{
		int id = registerSPForwardByName(amx, name, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		if (id != -1)
		{
			msgHooks[params[1]].AddHook(id);
			return id;
		} else {
			LogError(amx, AMX_ERR_NOTFOUND, "Could not find function \"%s\"", name);
			return -1;
		}
	}

	return 0;
}

// unregister_message(msgid, msghandle)
static cell AMX_NATIVE_CALL unregister_message(AMX *amx, cell *params)
{
	if (!Msg.Ready())
		Msg.Init();

	if (params[1]>0 && params[1] < 256)
	{
		int id = params[2];
		if (id != -1)
		{
			msgHooks[params[1]].RemoveHook(id);
			return id;
		} else {
			LogError(amx, AMX_ERR_NOTFOUND, "Invalid registered message handle");
			return -1;
		}
	}

	return 0;
}


static cell AMX_NATIVE_CALL set_msg_block(AMX *amx, cell *params)
{
	int msgid = params[1];
	int block = params[2];

	if (msgid < 1 || msgid > 255)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message id");
		return 0;
	}

	msgBlocks[msgid] = block;

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_block(AMX *amx, cell *params)
{
	int msgid = params[1];

	if (msgid < 1 || msgid > 255)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message id");
		return 0;
	}

	return msgBlocks[msgid];
}

static cell AMX_NATIVE_CALL get_msg_args(AMX *amx, cell *params)
{
	return Msg.Params();
}

static cell AMX_NATIVE_CALL get_msg_argtype(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	return Msg.GetParamType(argn);
}

static cell AMX_NATIVE_CALL get_msg_arg_int(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	return Msg.GetParamInt(argn);
}

static cell AMX_NATIVE_CALL set_msg_arg_int(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	Msg.SetParam(argn, (int)params[3]);

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_float(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	REAL f = (REAL)Msg.GetParamFloat(argn);
	return amx_ftoc(f);
}

static cell AMX_NATIVE_CALL set_msg_arg_float(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	REAL fVal = amx_ctof(params[3]);

	Msg.SetParam(argn, (float)fVal);

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_arg_string(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	const char *szVal = Msg.GetParamString(argn);

	return set_amxstring(amx, params[2], szVal, params[3]);
}

static cell AMX_NATIVE_CALL set_msg_arg_string(AMX *amx, cell *params)
{
	size_t argn = static_cast<size_t>(params[1]);
	int iLen;

	if (!inhook || argn > Msg.Params())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid message argument %d", argn);
		return 0;
	}

	char *szVal = get_amxstring(amx, params[2], 0, iLen);

	Msg.SetParam(argn, szVal);

	return 1;
}

static cell AMX_NATIVE_CALL get_msg_origin(AMX *amx, cell *params)
{
	if (!inhook)
	{
		LogError(amx, AMX_ERR_NATIVE, "Not in a message hook");
		return 0;
	}

	cell *cAddr = get_amxaddr(amx, params[1]);

	if (msgDest >= MSG_PVS && msgDest <= MSG_PAS_R)
	{
		vec3_t vRet = (Vector)msgOrigin;
		cAddr[0] = FloatToCell(vRet.x);
		cAddr[1] = FloatToCell(vRet.y);
		cAddr[2] = FloatToCell(vRet.z);
	} else {
		cAddr[0] = 0;
		cAddr[1] = 0;
		cAddr[2] = 0;
	}

	return 1;
}

static cell _emessage_begin(AMX *amx, cell *params, bool useFloat)
{
	int numparam = *params / sizeof(cell);
	float vecOrigin[3];
	cell *cpOrigin;

	if (params[2] < 1 || ((params[2] > 63)		// maximal number of engine messages
		&& !GET_USER_MSG_NAME(PLID, params[2], NULL)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Plugin called message_begin with an invalid message id (%d).", params[2]);
		return 0;
	}

	switch (params[1])
	{
	case MSG_BROADCAST:
	case MSG_ALL:
	case MSG_SPEC:
		g_pEngTable->pfnMessageBegin(params[1], params[2], NULL, NULL);
		break;
	case MSG_PVS: case MSG_PAS:
	case MSG_PVS_R: case MSG_PAS_R:
		if (numparam < 3)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
			return 0;
		}

		cpOrigin = get_amxaddr(amx, params[3]);

		if (!useFloat)
		{
			vecOrigin[0] = static_cast<float>(*cpOrigin);
			vecOrigin[1] = static_cast<float>(*(cpOrigin + 1));
			vecOrigin[2] = static_cast<float>(*(cpOrigin + 2));
		} else {
			vecOrigin[0] = amx_ctof(*cpOrigin);
			vecOrigin[1] = amx_ctof(*(cpOrigin + 1));
			vecOrigin[2] = amx_ctof(*(cpOrigin + 2));
		}

		g_pEngTable->pfnMessageBegin(params[1], params[2], vecOrigin, NULL);

		break;
	case MSG_ONE_UNRELIABLE:
	case MSG_ONE:
		if (numparam < 4)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
			return 0;
		}

		g_pEngTable->pfnMessageBegin(params[1], params[2], NULL, TypeConversion.id_to_edict(params[4]));
		break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL emessage_begin(AMX *amx, cell *params) /* 4 param */
{
	return _emessage_begin(amx, params, false);
}

static cell AMX_NATIVE_CALL emessage_begin_f(AMX *amx, cell *params) /* 4 param */
{
	return _emessage_begin(amx, params, true);
}

static cell AMX_NATIVE_CALL emessage_end(AMX *amx, cell *params)
{
	g_pEngTable->pfnMessageEnd();
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_byte(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteByte(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_char(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteChar(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_short(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteShort(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_long(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteLong(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_entity(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteEntity(params[1]);
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_angle(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteAngle(static_cast<float>(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_angle_f(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteAngle(amx_ctof(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_coord(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteCoord(static_cast<float>(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_coord_f(AMX *amx, cell *params) /* 1 param */
{
	g_pEngTable->pfnWriteCoord(amx_ctof(params[1]));
	return 1;
}

static cell AMX_NATIVE_CALL ewrite_string(AMX *amx, cell *params) /* 1 param */
{
	int a;
	g_pEngTable->pfnWriteString(get_amxstring(amx, params[1], 3, a));

	return 1;
}

AMX_NATIVE_INFO msg_Natives[] =
{
	{"message_begin",		message_begin},
	{"message_begin_f",		message_begin_f},
	{"message_end",			message_end},
	
	{"write_angle",			write_angle},
	{"write_angle_f",		write_angle_f},
	{"write_byte",			write_byte},
	{"write_char",			write_char},
	{"write_coord",			write_coord},
	{"write_coord_f",		write_coord_f},
	{"write_entity",		write_entity},
	{"write_long",			write_long},
	{"write_short",			write_short},
	{"write_string",		write_string},

	{"register_message",	register_message},
	{"unregister_message",	unregister_message},

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
	
	{"emessage_begin",		emessage_begin},
	{"emessage_begin_f",	emessage_begin_f},
	{"emessage_end",		emessage_end},
	
	{"ewrite_angle",		ewrite_angle},
	{"ewrite_angle_f",		ewrite_angle_f},
	{"ewrite_byte",			ewrite_byte},
	{"ewrite_char",			ewrite_char},
	{"ewrite_coord",		ewrite_coord},
	{"ewrite_coord_f",		ewrite_coord_f},
	{"ewrite_entity",		ewrite_entity},
	{"ewrite_long",			ewrite_long},
	{"ewrite_short",		ewrite_short},
	{"ewrite_string",		ewrite_string},

	{NULL,					NULL},
};
