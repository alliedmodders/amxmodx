#ifndef _JUDYCAP_INCLUDED
#define _JUDYCAP_INCLUDED

#include "JudyIncludes.h"

enum 
{
	capsule_type_none,
	capsule_type_bool,
	capsule_type_int,
	capsule_type_flo,
	capsule_type_vec,
	capsule_type_str
};

extern const char* capsule_types[];

class Capsule
{
private:
	Pvoid_t data;
	char type;

protected:
	void Clear( void );
	void ThrowTypeError(cell get_type);

public:
	Capsule()				{ data = NULL; type = capsule_type_none;}
	~Capsule()				{ Clear(); }

	Capsule(bool set)		{ SetBool(set); }
	Capsule(cell set)		{ SetInt(set); }
	Capsule(REAL set)		{ SetFlo(set); }
	Capsule(JudyVec* set)	{ SetVec(set); }
	Capsule(char* set)		{ SetStr(set); }

	bool GetBool( void );
	void SetBool(bool set);

	cell GetInt( void );
	void SetInt(cell set);

	REAL GetFlo( void );
	void SetFlo(REAL set);

	const JudyVec* GetVec( void );
	void SetVec(JudyVec* set);

	const char* GetStr( void );
	void SetStr(char* set);

	void GetAsStr(char* value);

	void Load(FILE* db);
	void Save(FILE* db);

	bool CheckEmpty(bool clear);

	Pvoid_t GetData( void )		{ return data; }
	char GetType( void )			{ return type; }
};

#endif