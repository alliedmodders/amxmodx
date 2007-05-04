#ifndef HOOKLIST_T_H
#define HOOKLIST_T_H

typedef struct hook_s
{
	int isset;								// whether or not this hook is registered with hamdata
	int vtid;								// vtable index of this function
	const char *name;						// name used in the keys
	bool isvoid;							// whether or not the target trampoline uses voids
	int  paramcount;						// how many parameters are in the func
	void *targetfunc;						// the target hook
	int (*makefunc)(AMX *, const char*);	// function that creates forwards
	cell (*call)(AMX *, cell*);				// function to call the vcall
} hook_t;

extern hook_t hooklist[];

#endif
