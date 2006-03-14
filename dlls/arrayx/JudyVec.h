#ifndef _JUDYVEC_INCLUDED
#define _JUDYVEC_INCLUDED

#include "JudyIncludes.h"

class JudyVec
{
public:
	REAL first;
	REAL second;
	REAL third;

	JudyVec(REAL one, REAL two, REAL three) { Set(one, two, three); }
	~JudyVec() {}

	void Get(REAL& one, REAL& two, REAL& three) { one = first; two = second; three = third; }
	void Set(REAL one, REAL two, REAL three) { first = one; second = two; third = three; }
};

#endif