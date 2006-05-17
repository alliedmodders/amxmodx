#ifndef _MSGS_INCLUDE_H
#define _MSGS_INCLUDE_H

#include <extdll.h>
#include <meta_api.h>
#include "amx.h"
#include "CVector.h"
#include "CString.h"

#define MAX_MESSAGES 255

#define BLOCK_NOT 0
#define BLOCK_ONCE 1
#define BLOCK_SET 2

enum msgtype
{
	arg_byte = 1,
	arg_char,
	arg_short,
	arg_long,
	arg_angle,
	arg_coord,
	arg_string,
	arg_entity,
};

struct msgparam
{
	msgtype type;
	union
	{
		REAL fData;
		int iData;
	} v;
	String szData;
};

class Message
{
public:
	Message();
	~Message();
	void AddParam(float data, msgtype type);
	void AddParam(int data, msgtype type);
	void AddParam(const char *data, msgtype type);
	void SetParam(size_t index, float data);
	void SetParam(size_t index, int data);
	void SetParam(size_t index, const char *data);
	const char *GetParamString(size_t index);
	float GetParamFloat(size_t index);
	bool Ready();
	void Init();
	int GetParamInt(size_t index);
	msgtype GetParamType(size_t index);
	void Reset();
	void Send();
	size_t Params();
private:
	msgparam *AdvPtr();
private:
	CVector<msgparam *> m_Params;
	size_t m_CurParam;
};

void C_MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
void C_WriteByte(int iValue);
void C_WriteChar(int iValue);
void C_WriteShort(int iValue);
void C_WriteLong(int iValue);
void C_WriteAngle(float flValue);
void C_WriteCoord(float flValue);
void C_WriteString(const char *sz);
void C_WriteEntity(int iValue);
void C_MessageEnd(void);

extern CVector<int> msgHooks[256];
extern int msgBlocks[256];

void ClearMessages();

#endif //_MSGS_INCLUDE_H

