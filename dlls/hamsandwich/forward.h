#include "sdk/amxxmodule.h"

#ifndef FORWARD_H
#define FORWARD_H

enum fwdstate
{
	FSTATE_INVALID = 0,
	FSTATE_OK,
	FSTATE_PAUSE,
	FSTATE_STOP,
	FSTATE_DESTROY
};

class Forward
{
public:
	int      id;    // id of the forward
	fwdstate state;
	Forward(int id_) : id(id_), state(FSTATE_OK)
	{
		/* do nothing */
	};
	Forward() : id(-1), state(FSTATE_INVALID)
	{
		/* do nothing */
	}
	~Forward()
	{
		MF_UnregisterSPForward(id);
	}
	inline void Set(int i)
	{
		state=FSTATE_OK;
		id=i;
	};

};

#endif
