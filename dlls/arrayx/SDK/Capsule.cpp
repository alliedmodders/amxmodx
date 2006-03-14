#include "Capsule.h"

const char* capsule_types[] =
{
	"-NO VALUE-",
	"BOOLEAN",
	"INTEGER",
	"FLOAT",
	"VECTOR",
	"STRING"
};

void Capsule::ThrowTypeError(cell get_type)
{ 
	char ValStr[15];
	GetAsStr(ValStr);

	char value[100];
	sprintf(value,"Function attempted to read NON-%s value, actual type is: %s, actual value is: %s", capsule_types[get_type], capsule_types[type], ValStr );

	throw JudyEx(value, true);
}

bool Capsule::CheckEmpty(bool clear)
{
	bool empty = ( data == NULL );

	if(empty != true && clear == true) Clear();
	return empty;
}

void Capsule::Clear()
{ 
	//This function intelligently creates a pointer x,
	//which will be of correct type and then deletes it.

	switch (type)
	{
		case capsule_type_flo:
		{
			REAL *real_val = reinterpret_cast<REAL*>(data);
			delete real_val;

			break;
		}
		case capsule_type_vec:
		{
			JudyVec *vector_val = reinterpret_cast<JudyVec*>(data);
			delete vector_val;

			break;
		}
		case capsule_type_str:
		{
			char *char_val = reinterpret_cast<char*>(data);
			delete char_val;

			break;
		}
	}

	data = NULL; //Null the address as well. (Used for ints too.)
}

bool Capsule::GetBool( void )
{
	if (type != capsule_type_bool) ThrowTypeError(capsule_type_bool);

	return reinterpret_cast<bool>(data);
}

void Capsule::SetBool(bool Value)
{
	CheckEmpty(true);
	type = capsule_type_bool;
	data = reinterpret_cast<void*>(Value);
};

cell Capsule::GetInt( void )
{
	if (type != capsule_type_int) ThrowTypeError(capsule_type_int);

	return reinterpret_cast<cell>(data);
}

void Capsule::SetInt(cell Value)
{
	CheckEmpty(true);
	type = capsule_type_int;
	data = reinterpret_cast<void*>(Value);
};

REAL Capsule::GetFlo( void )
{
	if (type != capsule_type_flo) ThrowTypeError(capsule_type_flo);

	return *reinterpret_cast<REAL*>(data);
}

void Capsule::SetFlo(REAL Value)
{
	CheckEmpty(true);
	type = capsule_type_flo;
	data = new REAL(Value);
};

const JudyVec* Capsule::GetVec( void )
{
	if (type != capsule_type_vec) ThrowTypeError(capsule_type_vec);

	return reinterpret_cast<const JudyVec*>(data);
}

void Capsule::SetVec(JudyVec* Value)
{
	CheckEmpty(true);
	type = capsule_type_vec;
	data = reinterpret_cast<void*>(Value);
}

const char* Capsule::GetStr( void )
{
	if (type != capsule_type_str) ThrowTypeError(capsule_type_str);

	return reinterpret_cast<const char*>(data);
}

void Capsule::SetStr(char* Value)
{
	CheckEmpty(true);
	type = capsule_type_str;

	char *string_val = new char[strlen(Value)+1];
	strcpy(string_val,Value);

	data = reinterpret_cast<void*>(string_val);
}

void Capsule::GetAsStr(char* value)
{
	switch (type)
	{
		case capsule_type_bool:
			sprintf(value, "%i",(cell)GetBool());
			break;
		case capsule_type_int:
			sprintf(value, "%d", GetInt() );
			break;
		case capsule_type_flo:
			sprintf(value, "%f", GetFlo() );
			break;
		case capsule_type_vec:
			sprintf(value, "{%f,%f,%f}", GetVec()->first, GetVec()->second, GetVec()->third);
			break;
		case capsule_type_str:
			sprintf(value, "\"%s\"", GetStr() );
			break;
		default:
			sprintf(value, "-NO VALUE-");
	}
}

void Capsule::Save(FILE* capsuleDB)
{
	fwrite(&type,sizeof(char),1,capsuleDB);

	switch(type)
	{
		case capsule_type_none: { break; }
		case capsule_type_bool: { bool var = GetBool(); fwrite(&var, sizeof(bool), 1, capsuleDB); break; }
		case capsule_type_int: { cell var = GetInt(); fwrite(&var, sizeof(cell), 1, capsuleDB); break; }
		case capsule_type_flo: { fwrite(reinterpret_cast<REAL*>(GetData()), sizeof(REAL), 1, capsuleDB); break; }
		case capsule_type_str:
		{
			const char* str = GetStr();
			size_t len = strlen(str);
			fwrite(&len,sizeof(size_t), 1, capsuleDB);
			fwrite(&str, sizeof(char), len, capsuleDB);

			break;
		}
		case capsule_type_vec:
		{
			const JudyVec* buffer = GetVec();
			fwrite(buffer, sizeof(JudyVec), 1, capsuleDB);

			break;
		}
		default:
		{
			char value[20];
			sprintf(value,"Invalid type found!");

			throw JudyEx(value, true);
			break;
		}
	};
}

void Capsule::Load(FILE* capsuleDB)
{
	fread(&type, sizeof(char), 1, capsuleDB);

	switch(type)
	{
		case capsule_type_none: { CheckEmpty(true); break; }
		case capsule_type_bool:
		{
			bool value = false;
			fread(&value, sizeof(bool), 1, capsuleDB);
			SetBool(value);

			break;
		}
		case capsule_type_int:
		{
			cell value = NULL;
			fread(&value, sizeof(cell), 1, capsuleDB);
			SetInt(value);

			break;
		}
		case capsule_type_flo:
		{
			REAL value = NULL;
			fread(&value, sizeof(REAL), 1, capsuleDB);
			SetFlo(value);

			break;
		}
		case capsule_type_str:
		{
			size_t length; 
			fread(&length, sizeof(size_t), 1, capsuleDB);

			char* value = new char[length+1]; 
			fgets(value, length+1, capsuleDB);

			SetStr(value);
			delete(value);

			break;
		}
		case capsule_type_vec:
		{
			JudyVec* value = new JudyVec(NULL,NULL,NULL);
			fread(value, sizeof(JudyVec), 1, capsuleDB);

			SetVec(value);

			break;
		}
		default:
		{
			char value[20];
			sprintf(value,"Invalid type found: %i",(int)type);

			throw JudyEx(value, true);
		}
	};
}