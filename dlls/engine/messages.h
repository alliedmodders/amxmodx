#ifndef _MSGS_INCLUDE_H
#define _MSGS_INCLUDE_H

#include "engine.h"

#define MAX_MESSAGES 255

#define BLOCK_NOT 0
#define BLOCK_ONCE 1
#define BLOCK_SET 2

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

enum {
	type_int = 1,
	type_float,
	type_string,
};

class argMsg
{
public:
	argMsg();
	void Reset();
	void Send();
	int Type();

	int type;
	REAL fData;
	String cData;
	int iData;
};

extern AMX_NATIVE_INFO msg_Natives[];
extern CVector<argMsg*> Msg;
extern CVector<int> msgHooks[256];
extern int msgBlocks[256];

#endif //_MSGS_INCLUDE_H

