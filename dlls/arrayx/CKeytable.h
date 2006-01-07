#if !defined _JUDYSL_ENABLED_
#define _JUDYSL_ENABLED_

#define MAXLINELEN 1024

Pvoid_t MasterKeytable = (Pvoid_t) NULL;		//Create the control array

//Create an array that stores whether or not indices are used.
Pvoid_t MasterKeytable_Binary = (Pvoid_t) NULL;

void Delete_MasterKeytable(void);

Word_t New_Keytable(Word_t Index, Word_t reserve = 0);
PPvoid_t Find_Keytable(Word_t Index, Word_t disable_check = 1, AMX *amx = 0);
void Delete_Keytable(Word_t Index);
void Clear_Keytable(Word_t Index);

template <class Type>
void Keytable_Set(PPvoid_t Keytable, char *Index, Type value);

PPvoid_t Keytable_Get(AMX* amx, Pvoid_t Keytable, char *Index, int ignore_error = 0);


void Delete_MasterKeytable(void)
{
	Word_t 
		Index = 0,
		success;
	J1F(success, MasterKeytable_Binary, Index);
	while( success )
	{
		Delete_Keytable(Index);
		J1F(success, MasterKeytable_Binary, Index);
	}
}

Word_t New_Keytable(Word_t Index, Word_t reserve)
{
	Word_t success; //Dummy for macros.
	J1T(success, MasterKeytable_Binary, Index);

	if (success && reserve)
		return Index; //If the bit is set but it's 'reserved', return the index.

	//Only do this if the bit is not set or not reserved.
	J1FE(success, MasterKeytable_Binary, Index);
	J1S(success, MasterKeytable_Binary, Index);

	PPvoid_t Keytable = JudyLIns(&MasterKeytable, Index, PJE0);
	*Keytable = (PWord_t) NULL;

	return Index;
}

PPvoid_t Find_Keytable(Word_t Index, Word_t disable_check, AMX* amx)
{
	Word_t success;
	J1T(success, MasterKeytable_Binary, Index);
	if (success || disable_check)
	{ //Bit is valid
		if(!success) 
			New_Keytable(Index);

		return JudyLGet(MasterKeytable, Index, PJE0);
	}
	MF_LogError(amx, AMX_ERR_NATIVE, "Keytable \"%s\" is invalid", Index);
	return NULL;
}

void Delete_Keytable(Word_t Index)
{
	int success;
	J1T(success, MasterKeytable_Binary, Index);
	if (success)
	{ //If the bit was set, clear and delete keytable.
		Clear_Keytable(Index);
		J1U(success, MasterKeytable_Binary, Index);
		JudyLDel(&MasterKeytable, Index, PJE0);
	}
}

void Clear_Keytable(Word_t Index)
{
	int success;
	J1T(success, MasterKeytable_Binary, Index);
	if (success) //dont bother with unset Keytables.
	{
		PPvoid_t Keytable = Find_Keytable(Index);
		char *Key = "";
		PPvoid_t PValue = JudySLFirst(*Keytable, Key, PJE0);
		while (PValue != NULL)
		{
			element elem_value = *reinterpret_cast<element*>(*PValue);
			elem_value.delete_element();
			PValue = JudySLNext(*Keytable, Key, PJE0);
		}
		JudySLFreeArray(Keytable, PJE0);
	}
}


static cell AMX_NATIVE_CALL Keytable_Save(AMX *amx, cell *params)
{
	PPvoid_t Keytable = Find_Keytable(params[1], params[3], amx);
	if (Keytable == NULL) return 0;

	int filename_length;
	char *file = MF_GetAmxString(amx, params[2], 0, &filename_length);
	file = MF_BuildPathname("%s", file);
	unlink(file);
	FILE *KeytableDB = fopen(file,"w");
	if (!KeytableDB)
		return 0;
	char* Key = new char[1024]; Key[0] = '\0';
	PPvoid_t PValue = JudySLFirst(*Keytable, reinterpret_cast<uint8_t*>(Key), PJE0);
	element elem = NULL;
	char elem_type = 0;

	int error;
	
	REAL vector_data[3] = { 0.0, 0.0, 0.0 };
	while (PValue)
	{
		elem = *reinterpret_cast<element*>(*PValue);
		elem_type = elem.get_type();

		if (elem_type < elem_type_int || elem_type > elem_type_vector)
			continue;

		short key_len = strlen(Key);
		fwrite(&key_len, sizeof(short), 1, KeytableDB);
		fwrite(Key, sizeof(char), key_len, KeytableDB);
		fwrite(&elem_type, sizeof(char), 1, KeytableDB);
		if (elem_type == elem_type_int)
		{
			int int_buffer = elem.get_int(error);
			fwrite(&int_buffer, sizeof(int), 1, KeytableDB);
		}
		else if (elem_type == elem_type_real)
		{
			REAL flo_buffer = elem.get_flo(error);
			fwrite(&flo_buffer, sizeof(REAL), 1, KeytableDB);
		}
		else if (elem_type == elem_type_char)
		{
			const char* str_buffer = elem.get_str(error);
			short buf_len = strlen(str_buffer);
			fwrite(&buf_len, sizeof(short), 1, KeytableDB);
			fwrite(str_buffer, sizeof(char), buf_len, KeytableDB);
		}
		else if (elem_type == elem_type_vector)
		{
			const Vector* vec_buffer = elem.get_vec(error);
			fwrite(vec_buffer, sizeof(Vector), 1, KeytableDB);
		}
		PValue = JudySLNext(*Keytable, Key, PJE0);
	}
	fclose(KeytableDB);
	return 1;
}

static cell AMX_NATIVE_CALL Keytable_Load(AMX *amx, cell *params)
{
	//params[1]: file
	int filename_length;
	char *file = MF_GetAmxString(amx, params[1], 0, &filename_length);
	file = MF_BuildPathname("%s", file);
	FILE *KeytableDB = fopen(file, "a+");
	if (!KeytableDB)
		return 0;

	//params[2]: keytable to create (optional index supplied)
	int KeytableIndex = New_Keytable(params[2], params[3]);
	Clear_Keytable(KeytableIndex); //make sure the keytable is empty.
	PPvoid_t Keytable = Find_Keytable(KeytableIndex);
	while(!feof(KeytableDB))
	{
		char* index = ""; char type = 0; short index_len;
		element *elem_value = NULL;
		fread(&index_len, sizeof(short), 1, KeytableDB);
		index = new char[index_len+1];
		fgets(index, index_len+1, KeytableDB);
		if (feof(KeytableDB) || ferror(KeytableDB))
			break;
		fread(&type, sizeof(char), 1, KeytableDB);
		if (type < elem_type_int || type > elem_type_vector)
		{
			MF_LogError(amx, AMX_ERR_FORMAT, "Error loading keytable database \"%s\" into keytable %d. Bad file.", file, KeytableIndex);
			return KeytableIndex;
		}
		else if (type == elem_type_int)
		{
			int value = 0; fread(&value, sizeof(int), 1, KeytableDB);
			elem_value = new element(value);
		}
		else if (type == elem_type_real)
		{
			REAL value = 0; fread(&value, sizeof(REAL), 1, KeytableDB);
			elem_value = new element(value);
		}
		else if (type == elem_type_char)
		{
			short length; fread(&length, sizeof(short), 1, KeytableDB);
			char* value = new char[length+1]; fgets(value, length+1, KeytableDB);
			elem_value = new element(value);
			delete(value);
		}
		else if (type == elem_type_vector)
		{
			Vector *value = new Vector(); fread(value, sizeof(Vector), 1, KeytableDB);
			elem_value = new element(value);
		}
		Keytable_Set(Keytable,index,elem_value);
		delete (index);
	}
	fclose(KeytableDB);
	return KeytableIndex;
}

static cell AMX_NATIVE_CALL Keytable_Save_ASCII(AMX *amx, cell *params)
{
	//params[1]: file
	int filename_length;
	char *inputfile = MF_GetAmxString(amx, params[1], 0, &filename_length);
	inputfile = MF_BuildPathname("%s", inputfile);
	FILE *KeytableDB = fopen(inputfile, "a+");
	if (!KeytableDB)
		return 0;

	char *outputfile = MF_GetAmxString(amx, params[2], 0, &filename_length);
	outputfile = MF_BuildPathname("%s", outputfile);
	FILE *ReadableDB = fopen(outputfile, "w");

	char *buffer = "\0";

	while(!feof(KeytableDB))
	{
		char* key = NULL; char type = 0; short key_len;
		fread(&key_len, sizeof(short), 1, KeytableDB);
		key = new char[key_len+1];
		fgets(key, key_len+1, KeytableDB);
		if (feof(KeytableDB) || ferror(KeytableDB))
			break;
		fread(&type, sizeof(char), 1, KeytableDB);

		sprintf(buffer, "Key %-32s Length %3d, Type %7s", key, key_len, elem_types[type]);
		if (type < elem_type_int || type > elem_type_vector)
		{
			MF_LogError(amx, AMX_ERR_FORMAT, "Error loading array database \"%s\" into readable format. Bad file.", inputfile);
			return 0;
		}
		else if (type == elem_type_int)
		{
			int value = 0; fread(&value, sizeof(int), 1, KeytableDB);
			fprintf(ReadableDB, "%s\t\t\t\tValue: %d\n", buffer, value);
		}
		else if (type == elem_type_real)
		{
			REAL value = 0; fread(&value, sizeof(REAL), 1, KeytableDB);
			fprintf(ReadableDB, "%s\t\t\t\tValue: %f\n", buffer, value);
		}
		else if (type == elem_type_char)
		{
			short length; fread(&length, sizeof(short), 1, KeytableDB);
			char* value = new char[length+1]; fgets(value, length+1, KeytableDB);
			fprintf(ReadableDB, "%s Length %3d\tValue: \"%s\"\n", buffer, length, value);
			delete value;
		}
		else if (type == elem_type_vector)
		{
			Vector *value = new Vector(); fread(value, sizeof(Vector), 1, KeytableDB);
			fprintf(ReadableDB, "%s\t\t\t\tValue: {%f,%f,%f}\n", buffer, (*value).x, (*value).y, (*value).z);
			delete value;
		}
	}
	fclose(KeytableDB);
	fclose(ReadableDB);
	return 1;
}

template <class Type> //This will support input char*, Vector*, int, and cell_real*.
void Keytable_Set(PPvoid_t Keytable, char* Index, Type value)
{
	PPvoid_t PValue;						// pointer to keytable element value
	PValue = JudySLIns(Keytable, Index, PJE0);
	*PValue = reinterpret_cast<void*>(value);
}

PPvoid_t Keytable_Get(AMX* amx, PPvoid_t Keytable, char *Index, int ignore_error = 0)
{
	PPvoid_t PValue = JudySLGet( *Keytable, Index, PJE0 );
	if (PValue == NULL && !ignore_error)
		MF_LogError(amx, AMX_ERR_NATIVE, "Keytable get on key \"%s\" is invalid", Index);
	return PValue;
}

static cell AMX_NATIVE_CALL Keytable_Create(AMX *amx, cell *params)
{
	return New_Keytable(params[1],params[2]);
}

static cell AMX_NATIVE_CALL Keytable_Delete(AMX *amx, cell *params)
{
	Delete_Keytable( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL Keytable_Clear(AMX *amx, cell *params)
{
	Clear_Keytable( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL Keytable_SetVector(AMX *amx,cell *params)
{
	PPvoid_t Keytable = Find_Keytable(params[1], params[4], amx);
	if (Keytable == NULL) return 0;

	cell *input_vec = MF_GetAmxAddr(amx, params[3]); 
	Vector *value = new Vector(
		amx_ctof(input_vec[0]), 
		amx_ctof(input_vec[1]), 
		amx_ctof(input_vec[2])
	);
	int strlen;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlen);

	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_vec(value);
	}
	Keytable_Set(Keytable,Index,elem_value);
	return 1;
}

static cell AMX_NATIVE_CALL Keytable_GetVector(AMX *amx, cell *params)
{
	PPvoid_t Keytable = Find_Keytable(params[1], params[4], amx);
	if (Keytable == NULL) return 0;

	int strlen;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlen);
	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, params[4]);

	cell *vAmx = MF_GetAmxAddr(amx, params[3]);

	if( PValue == NULL ) {
		vAmx[0] = amx_ftoc(0);
		vAmx[1] = amx_ftoc(0);
		vAmx[2] = amx_ftoc(0);
		return 0;
	}
	element elem_value = *reinterpret_cast<element*>(*PValue);
	int error = 0;
	const Vector retr_vec = *elem_value.get_vec(error);
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	vAmx[0] = amx_ftoc(retr_vec.x);
	vAmx[1] = amx_ftoc(retr_vec.y);
	vAmx[2] = amx_ftoc(retr_vec.z);
	return 1;
}

static cell AMX_NATIVE_CALL Keytable_SetString(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[4], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	//params[3]: value
	int iLen = 0;
	char *value = MF_GetAmxString(amx,params[3],1,&iLen);

	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_str(value);
	}
	Keytable_Set(Keytable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Keytable_GetString(AMX *amx,cell *params) 
{ 
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[5], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	
	Pvoid_t * PValue = Keytable_Get(amx, Keytable, Index, params[5]);

	//params[3] and params[4] are the return string and length respectively.
	

	if( PValue == NULL ) 
		return MF_SetAmxString(amx, params[3] , "dne", params[4] );

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	const char* str_out = elem_value.get_str(error);
	return MF_SetAmxString( amx , params[3] , str_out, params[4] );
}

static cell AMX_NATIVE_CALL Keytable_SetFloat(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[4], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3]: value
	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(amx_ctof(params[3]));
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_flo(amx_ctof(params[3]));
	}
	Keytable_Set(Keytable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Keytable_GetFloat(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[3], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, params[3]);

	if( PValue == NULL ) return amx_ftoc(0.0);

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_float = amx_ftoc(elem_value.get_flo(error));
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_float;
}


static cell AMX_NATIVE_CALL Keytable_SetInt(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[4], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t PValue = Keytable_Get(amx, Keytable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(params[3]);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_int(params[3]);
	}
	Keytable_Set(Keytable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Keytable_GetInt(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[3], amx);
	if (Keytable == NULL) return 0;
	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	Pvoid_t * PValue = Keytable_Get(amx, Keytable, Index, params[3]);

	if( PValue == NULL ) return 0;

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_int = elem_value.get_int(error);
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_int;
}

static cell AMX_NATIVE_CALL Keytable_Memory(AMX *amx,cell *params)
{
	Pvoid_t * Keytable = Find_Keytable(params[1],params[2],amx);
	if (Keytable == NULL) return 0;
	
	return JudyLMemUsed(*Keytable);
}

static cell AMX_NATIVE_CALL Keytable_Remove(AMX *amx,cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], 0, amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//Have to delete the element
	PPvoid_t PValue = JudySLGet(*Keytable, Index, PJE0);
	if (PValue == NULL) return 1;
	element elem_value = *reinterpret_cast<element*>(*PValue);
	elem_value.delete_element();

	JudySLDel(Keytable, Index, PJE0 );
	return 1;
}

static cell AMX_NATIVE_CALL Keytable_Next(AMX *amx,cell *params)  
{  
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[5], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3], params[4]: return key and length
	
	PPvoid_t pointer;  
	pointer = JudySLNext(*Keytable, Index, PJE0);  

	if (pointer == NULL) {
		MF_SetAmxString(amx, params[3], "dne", 0);
		return 0;
	}
	MF_SetAmxString(amx, params[3], Index, params[4]);
	return 1;
} 

static cell AMX_NATIVE_CALL Keytable_Prev(AMX *amx,cell *params)  
{  
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[5], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3], params[4]: return key and length
	
	PPvoid_t pointer;
	pointer = JudySLPrev(*Keytable, Index, PJE0);  

	if (pointer == NULL) {
		MF_SetAmxString(amx, params[3], "dne", 0);
		return 0;
	}
	MF_SetAmxString(amx, params[3], Index, params[4]);
	return 1;
} 


static cell AMX_NATIVE_CALL Keytable_First(AMX *amx,cell *params)  
{  
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[5], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3], params[4]: return key and length
	
	PPvoid_t pointer;
	pointer = JudySLFirst(*Keytable, Index, PJE0);  

	if (pointer == NULL) {
		MF_SetAmxString(amx, params[3], "dne", 0);
		return 0;
	}
	MF_SetAmxString(amx, params[3], Index, params[4]);
	return 1;
} 

static cell AMX_NATIVE_CALL Keytable_Last(AMX *amx,cell *params)  
{  
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[5], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3], params[4]: return key and length
	
	PPvoid_t pointer;
	pointer = JudySLLast(*Keytable, Index, PJE0);  

	if (pointer == NULL) {
		MF_SetAmxString(amx, params[3], "dne", 0);
		return 0;
	}
	MF_SetAmxString(amx, params[3], Index, params[4]);
	return 1;
}

static cell AMX_NATIVE_CALL Key_IsEmpty(AMX *amx, cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[3], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t pointer = JudySLGet(*Keytable, Index, PJE0);
	
	return (pointer == NULL) ? 1 : 0;
}

static cell AMX_NATIVE_CALL Key_IsFilled(AMX *amx, cell *params)
{
	//params[1]: keytable
	PPvoid_t Keytable = Find_Keytable(params[1], params[3], amx);
	if (Keytable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t pointer = JudySLGet(*Keytable, Index, PJE0);

	return (pointer != NULL) ? 1 : 0;
}

AMX_NATIVE_INFO keytable_exports[] = {
  { "keytable_set_string",	Keytable_SetString	},
  { "keytable_get_string",	Keytable_GetString	},

  { "keytable_set_vector",	Keytable_SetVector	},
  { "keytable_get_vector",	Keytable_GetVector	},

  { "keytable_set_int",		Keytable_SetInt		},
  { "keytable_get_int",		Keytable_GetInt		},

  { "keytable_set_float",	Keytable_SetFloat	},
  { "keytable_get_float",	Keytable_GetFloat	},

  { "keytable_isempty",		Key_IsEmpty			},
  { "keytable_isfilled",	Key_IsFilled		},

  { "keytable_memory",		Keytable_Memory		},

  { "keytable_remove",		Keytable_Remove		},

  { "keytable_create",		Keytable_Create		},
  { "keytable_delete",		Keytable_Delete		},
  { "keytable_clear",		Keytable_Clear		},

  { "keytable_next",		Keytable_Next		}, 
  { "keytable_prev",		Keytable_Prev		}, 
  { "keytable_first",		Keytable_First		}, 
  { "keytable_last",		Keytable_Last		},

  { "keytable_save",		Keytable_Save		},
  { "keytable_load",		Keytable_Load		},
  
  { "keytable_save_ascii",	Keytable_Save_ASCII	},

  { NULL, NULL }
};

#endif