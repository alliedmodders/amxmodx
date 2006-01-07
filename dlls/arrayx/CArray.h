Pvoid_t MasterArray = (Pvoid_t) NULL; //Create the control array

//Create an array that stores whether or not indices are used.
Pvoid_t MasterArray_Binary = (Pvoid_t) NULL; 

void DeleteMasterArray(void);

Word_t NewArray(Word_t Index, Word_t reserve = 0);
PPvoid_t Find_Array(Word_t Index, Word_t disable_check = 1, AMX *amx = 0);
void DeleteArray(Word_t Index);
void ClearArray(Word_t Index);

template <class Type>
void Array_Set(PPvoid_t Array, char* Index, Type value);

PPvoid_t Array_Get(AMX* amx, PPvoid_t Array, Word_t Index, int ignore_error = 0);
void DeleteCell(Pvoid_t* Array, Word_t Index);

void Delete_MasterArray(void)
{
	Word_t
		Index = 0,
		success = 0;
	J1F(success, MasterArray_Binary, Index);
	while( success )
	{
		DeleteArray( Index );						//Delete array.
		J1F(success, MasterArray_Binary, Index);	//Get next array
	}
}

Word_t NewArray(Word_t Index, Word_t reserve)
{
	Word_t success; //Dummy for macros.
	J1T(success, MasterArray_Binary, Index); //Check if bit is already set.

	if (success && reserve) 
		return Index; //If the bit is set but it's 'reserved', return the index.

	//Only do this if the bit is not set.
	J1FE(success, MasterArray_Binary, Index);
	J1S(success, MasterArray_Binary, Index);

	PPvoid_t Array = JudyLIns(&MasterArray, Index, PJE0);
	*Array = (PWord_t) NULL;

	return Index;
}

PPvoid_t Find_Array(Word_t Index, Word_t disable_check, AMX *amx)
{
	Word_t success;
	J1T(success, MasterArray_Binary, Index);
	if (success || disable_check)
	{ //Bit is valid
		if(!success)
			NewArray(Index);

		return JudyLGet( MasterArray, Index, PJE0);
	}
	MF_LogError(amx,AMX_ERR_NATIVE,"Array %d is invalid", Index);
	return NULL;
}

void DeleteArray(Word_t Index)
{
	int success;
	J1T(success, MasterArray_Binary, Index);
	if (success)
	{ //If the bit is set, clear and delete array.
		ClearArray(Index);
		J1U(success, MasterArray_Binary, Index);
		JudyLDel(&MasterArray, Index, PJE0);
	}
}

void ClearArray(Word_t Index)
{
	int success;
	J1T(success, MasterArray_Binary, Index);
	if (success) //dont bother with unset arrays.
	{
		PPvoid_t Array = Find_Array(Index);	
		Word_t index = 0;
		PPvoid_t PValue = JudyLFirst(*Array, &index, PJE0);
		while (PValue != NULL)
		{
			element elem_value = *reinterpret_cast<element*>(*PValue);
			elem_value.delete_element();
			PValue = JudyLNext(*Array, &index, PJE0);
		}
		JudyLFreeArray(Array, PJE0);
	}
}

static cell AMX_NATIVE_CALL new_array(AMX *amx,cell *params)
{
	return NewArray(params[1], params[2]);
}

template <class Type> //This will support input char*, Vector*, int, and cell_real*.
void Array_Set(PPvoid_t Array, int Index, Type value)
{
	PPvoid_t PValue;						// pointer to array element value
	PValue = JudyLIns(Array, Index, PJE0);
	*PValue = reinterpret_cast<void*>(value);
}

PPvoid_t Array_Get(AMX* amx, PPvoid_t Array, int Index, int ignore_error = 0)
{
	PPvoid_t PValue = JudyLGet( *Array, Index, PJE0 );
	if (PValue == NULL && !ignore_error)
		MF_LogError(amx, AMX_ERR_NATIVE, "Array index %d is invalid", Index);
	return PValue;
}

void DeleteCell(PPvoid_t Array, Word_t Index)
{
	JudyLDel(Array, Index, PJE0);
}


static cell AMX_NATIVE_CALL delete_array(AMX *amx,cell *params)
{
	DeleteArray( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL clear_array(AMX *amx,cell *params)
{
	ClearArray( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL Array_Save(AMX *amx, cell *params)
{
	PPvoid_t Array = Find_Array(params[1], params[3], amx);
	if (Array == NULL) return 0;

	int filename_length;
	char *file = MF_GetAmxString(amx, params[2], 0, &filename_length);
	file = MF_BuildPathname("%s", file);
	unlink(file);
	FILE *ArrayDB = fopen(file,"w");
	if (!ArrayDB)
		return 0;
	Word_t Index = 0;
	PPvoid_t PValue = JudyLFirst(*Array, &Index, PJE0);
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

		fwrite(&Index, sizeof(int), 1, ArrayDB);
		fwrite(&elem_type, sizeof(char), 1, ArrayDB);
		if (elem_type == elem_type_int)
		{
			int int_buffer = elem.get_int(error);
			fwrite(&int_buffer, sizeof(int), 1, ArrayDB);
		}
		else if (elem_type == elem_type_real)
		{
			REAL flo_buffer = elem.get_flo(error);
			fwrite(&flo_buffer, sizeof(REAL), 1, ArrayDB);
		}
		else if (elem_type == elem_type_char)
		{
			const char* str_buffer = elem.get_str(error);
			short buf_len = strlen(str_buffer);
			fwrite(&buf_len, sizeof(short), 1, ArrayDB);
			fwrite(str_buffer, sizeof(char), buf_len, ArrayDB);
		}
		else if (elem_type == elem_type_vector)
		{
			const Vector* vec_buffer = elem.get_vec(error);
			fwrite(vec_buffer, sizeof(Vector), 1, ArrayDB);
		}
		PValue = JudyLNext(*Array, &Index, PJE0);
	}
	fclose(ArrayDB);
	return 1;
}

static cell AMX_NATIVE_CALL Array_Load(AMX *amx, cell *params)
{
	//params[1]: file
	int filename_length;
	char *file = MF_GetAmxString(amx, params[1], 0, &filename_length);
	file = MF_BuildPathname("%s", file);
	FILE *ArrayDB = fopen(file, "a+");
	if (!ArrayDB)
		return 0;

	//params[2]: array to create (optional index supplied)
	int ArrayIndex = NewArray(params[2], params[3]);
	ClearArray(ArrayIndex); //make sure the array is empty.
	PPvoid_t Array = Find_Array(ArrayIndex);
	while(!feof(ArrayDB))
	{
		int index = 0; char type = 0;
		element *elem_value = NULL;
		fread(&index, sizeof(int), 1, ArrayDB);
		if (feof(ArrayDB) || ferror(ArrayDB))
			break;

		fread(&type, sizeof(char), 1, ArrayDB);

		if (type < elem_type_int || type > elem_type_vector)
		{
			MF_LogError(amx, AMX_ERR_FORMAT, "Error loading array database \"%s\" into array %d. Bad file.", file, ArrayIndex);
			return ArrayIndex;
		}
		else if (type == elem_type_int)
		{
			int value = 0; fread(&value, sizeof(int), 1, ArrayDB);
			elem_value = new element(value);
		}
		else if (type == elem_type_real)
		{
			REAL value = 0; fread(&value, sizeof(REAL), 1, ArrayDB);
			elem_value = new element(value);
		}
		else if (type == elem_type_char)
		{
			short length; fread(&length, sizeof(short), 1, ArrayDB);
			char* value = new char[length+1]; fgets(value, length+1, ArrayDB);
			elem_value = new element(value);
			delete(value);
		}
		else if (type == elem_type_vector)
		{
			Vector *value = new Vector(); fread(value, sizeof(Vector), 1, ArrayDB);
			elem_value = new element(value);
		}
		Array_Set(Array,index,elem_value);
	}
	fclose(ArrayDB);
	return ArrayIndex;
}

static cell AMX_NATIVE_CALL Array_Save_ASCII(AMX *amx, cell *params)
{
	//params[1]: file
	int filename_length;
	char *inputfile = MF_GetAmxString(amx, params[1], 0, &filename_length);
	inputfile = MF_BuildPathname("%s", inputfile);
	FILE *ArrayDB = fopen(inputfile, "a+");
	if (!ArrayDB)
		return 0;

	char *outputfile = MF_GetAmxString(amx, params[2], 0, &filename_length);
	outputfile = MF_BuildPathname("%s", outputfile);
	FILE *ReadableDB = fopen(outputfile, "w");

	char *buffer = "\0";
	char *buffer_two = "\0";

	while(!feof(ArrayDB))
	{
		Word_t index = 0; char type = 0;
		fread(&index, sizeof(int), 1, ArrayDB);
		if (feof(ArrayDB) || ferror(ArrayDB))
			break;

		fread(&type, sizeof(char), 1, ArrayDB);

		sprintf(buffer, "Index % 11d\tType %7s,", index, elem_types[type]);
		if (type < elem_type_int || type > elem_type_vector)
		{
			MF_LogError(amx, AMX_ERR_FORMAT, "Error loading array database \"%s\" into readable format. Bad file.", inputfile);
			return 0;
		}
		else if (type == elem_type_int)
		{
			int value = 0; fread(&value, sizeof(int), 1, ArrayDB);
			sprintf(buffer, "%s\t\t\tValue: %d\n", buffer, value);
		}
		else if (type == elem_type_real)
		{
			REAL value = 0; fread(&value, sizeof(REAL), 1, ArrayDB);
			sprintf(buffer, "%s\t\t\tValue: %f\n", buffer, value);
		}
		else if (type == elem_type_char)
		{
			short length; fread(&length, sizeof(short), 1, ArrayDB);
			char* value = new char[length+1]; fgets(value, length+1, ArrayDB);
			sprintf(buffer, "%s Length: %d\tValue: \"%s\"\n", buffer, length, value);
			delete value;
		}
		else if (type == elem_type_vector)
		{
			Vector *value = new Vector(); fread(value, sizeof(Vector), 1, ArrayDB);
			sprintf(buffer, "%s\t\t\tValue: {%f,%f,%f}\n", buffer, (*value).x, (*value).y, (*value).z);
			delete value;
		}
		fwrite(buffer, sizeof(char), strlen(buffer), ReadableDB);
	}
	fclose(ArrayDB);
	fclose(ReadableDB);
	return 1;
}

static cell AMX_NATIVE_CALL Array_SetVector(AMX *amx,cell *params)
{
	PPvoid_t Array = Find_Array(params[1], params[4], amx);
	if (Array == NULL) return 0;

	cell *input_vec = MF_GetAmxAddr(amx, params[3]); 
	Vector *value = new Vector(
		amx_ctof(input_vec[0]), 
		amx_ctof(input_vec[1]), 
		amx_ctof(input_vec[2])
	);
	int Index = params[2];

	PPvoid_t PValue = Array_Get(amx, Array, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_vec(value);
	}
	Array_Set(Array,Index,elem_value);
	return 1;
}

static cell AMX_NATIVE_CALL Array_GetVector(AMX *amx, cell *params)
{
	PPvoid_t Array = Find_Array(params[1], params[4], amx);
	if (Array == NULL) return 0;

	int Index = params[2];
	PPvoid_t PValue = Array_Get(amx, Array, Index, params[4]);

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

static cell AMX_NATIVE_CALL Array_SetString(AMX *amx,cell *params)
{
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[4], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];

	//params[3]: value
	int iLen = 0;
	char *value = MF_GetAmxString(amx,params[3],1,&iLen);

	//element that is stored at index
	PPvoid_t PValue = Array_Get(amx, Array, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_str(value);
	}
	Array_Set(Array,Index,elem_value);
	return 1;
}

static cell AMX_NATIVE_CALL Array_GetString(AMX *amx,cell *params) 
{ 
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[5], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];
	
	Pvoid_t * PValue = Array_Get(amx, Array, Index, params[5]);

	//params[3] and params[4] are the return string and length respectively.

	if( PValue == NULL ) return MF_SetAmxString( amx , params[3] , "dne", params[4] );

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	const char* str_out = elem_value.get_str(error);
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return MF_SetAmxString( amx , params[3] , str_out, params[4] );
}

static cell AMX_NATIVE_CALL Array_SetFloat(AMX *amx,cell *params)
{
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[4], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];

	//params[3]: value
	PPvoid_t PValue = Array_Get(amx, Array, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(amx_ctof(params[3]));
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_flo(amx_ctof(params[3]));
	}
	Array_Set(Array,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Array_GetFloat(AMX *amx,cell *params)
{
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[3], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];

	PPvoid_t PValue = Array_Get(amx, Array, Index, params[3]);

	if( PValue == NULL ) return amx_ftoc(0.0);

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_float = amx_ftoc(elem_value.get_flo(error));
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_float;
}

static cell AMX_NATIVE_CALL Array_SetInt(AMX *amx,cell *params)
{
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[3], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];

	PPvoid_t PValue = Array_Get(amx, Array, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(params[3]);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_int(params[3]);
	}
	Array_Set(Array,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Array_GetInt(AMX *amx,cell *params)
{
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1], params[3], amx);
	if (Array == NULL) return 0;

	//params[2]: index
	int Index = params[2];

	Pvoid_t * PValue = Array_Get(amx, Array, Index, params[3]);

	if( PValue == NULL ) return 0;

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_int = elem_value.get_int(error);
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_int;
}

static cell AMX_NATIVE_CALL array_size(AMX *amx,cell *params)
{
	Pvoid_t * Array = Find_Array(params[1],params[4],amx);
	if (Array == NULL) return 0;
	
	return JudyLCount( *Array, params[2], params[3],PJE0);
}

static cell AMX_NATIVE_CALL array_count(AMX *amx,cell *params)
{
	return JudyLCount( MasterArray, params[1], params[2],PJE0);
}

static cell AMX_NATIVE_CALL array_memory(AMX *amx,cell *params)
{
	Pvoid_t * Array = Find_Array(params[1],params[2],amx);
	if (Array == NULL) return 0;
	
	return JudyLMemUsed(*Array);
}

static cell AMX_NATIVE_CALL delete_cell(AMX *amx,cell *params)
{
	Pvoid_t * Array = Find_Array(params[1]);
	if (Array == NULL) return 0;
	
	DeleteCell( Array, params[2] );

	return 1;
}

static cell AMX_NATIVE_CALL array_next(AMX *amx,cell *params)  
{  
     PPvoid_t Array = Find_Array(params[1],params[4],amx); 
	if (Array == NULL) return 0;
	  
     Word_t Index = Word_t(params[2]); 
     cell *success = MF_GetAmxAddr(amx, params[3]);   
      
     PPvoid_t pointer;  
     pointer = JudyLNext(*Array, &Index, PJE0);  
      
     *success = (pointer == NULL) ? 0 : 1; 
     return cell(Index); 
} 
  
static cell AMX_NATIVE_CALL array_prev(AMX *amx,cell *params)  
{  
     PPvoid_t Array = Find_Array(params[1],params[4],amx);
	if (Array == NULL) return 0;
	   
     Word_t Index = Word_t(params[2]); 
     cell *success = MF_GetAmxAddr(amx, params[3]);   
      
     PPvoid_t pointer;  
     pointer = JudyLPrev(*Array, &Index, PJE0);  
      
     *success = (pointer == NULL) ? 0 : 1; 
     return cell(Index); 
}  
  
static cell AMX_NATIVE_CALL array_first(AMX *amx,cell *params)   
{   
     PPvoid_t Array = Find_Array(params[1],params[4],amx);  
	if (Array == NULL) return 0;
	 
     Word_t Index = Word_t(params[2]); 
     cell *success = MF_GetAmxAddr(amx, params[3]);   
      
     PPvoid_t pointer;  
     pointer = JudyLFirst(*Array, &Index, PJE0);  
      
     *success = (pointer == NULL) ? 0 : 1; 
     return cell(Index); 
} 
  
static cell AMX_NATIVE_CALL array_last(AMX *amx,cell *params)   
{   
     PPvoid_t Array = Find_Array(params[1],params[4],amx);
	if (Array == NULL) return 0;
	   
     Word_t Index = Word_t(params[2]); 
     cell *success = MF_GetAmxAddr(amx, params[3]);   
      
     PPvoid_t pointer;  
     pointer = JudyLLast(*Array, &Index, PJE0); 
      
     *success = (pointer == NULL) ? 0 : 1; 
     return cell(Index); 
} 

static cell AMX_NATIVE_CALL array_nextempty(AMX *amx,cell *params) 
{  
     PPvoid_t Array = Find_Array(params[1],params[4],amx); 
	if (Array == NULL) return 0;
	
     Word_t Index = (Word_t)params[2]; 
      
     cell *success = MF_GetAmxAddr(amx, params[3]); 
     *success = JudyLNextEmpty(*Array, &Index, PJE0); 
      
     return (cell)Index; 
} 
  
static cell AMX_NATIVE_CALL array_prevempty(AMX *amx,cell *params)  
{ 
     PPvoid_t Array = Find_Array(params[1],params[4],amx);
	if (Array == NULL) return 0;
	 
     Word_t Index = (Word_t)params[2]; 
      
     cell *success = MF_GetAmxAddr(amx, params[3]); 
     *success = JudyLPrevEmpty(*Array, &Index, PJE0); 
      
     return (cell)Index; 
}  
  
static cell AMX_NATIVE_CALL array_firstempty(AMX *amx,cell *params)   
{   
     PPvoid_t Array = Find_Array(params[1],params[4],amx);
	if (Array == NULL) return 0;
	 
     Word_t Index = (Word_t)params[2]; 
      
     cell *success = MF_GetAmxAddr(amx, params[3]); 
     *success = JudyLFirstEmpty(*Array, &Index, PJE0); 
      
     return (cell)Index; 
}  
  
static cell AMX_NATIVE_CALL array_lastempty(AMX *amx,cell *params)   
{   
     PPvoid_t Array = Find_Array(params[1],params[4],amx); 
	if (Array == NULL) return 0;
	
     Word_t Index = (Word_t)params[2]; 
      
     cell *success = MF_GetAmxAddr(amx, params[3]); 
     *success = JudyLLastEmpty(*Array, &Index, PJE0); 
      
     return (cell)Index; 
}

static cell AMX_NATIVE_CALL array_isempty(AMX *amx,cell *params)   
{   
	PPvoid_t Array = Find_Array(params[1],params[3],amx); 
	if (Array == NULL) return 0;
		
	PPvoid_t pointer;
	pointer = JudyLGet(*Array, params[2], PJE0); 
	
	return (pointer == NULL) ? 1 : 0;
}

static cell AMX_NATIVE_CALL array_isfilled(AMX *amx,cell *params)   
{   
	//params[1]: array
	PPvoid_t Array = Find_Array(params[1],params[3],amx);
	if (Array == NULL) return 0;
	 
	//params[2]: index
	PPvoid_t pointer;
	pointer = JudyLGet(*Array, params[2], PJE0); 

	return (pointer != NULL) ? 1 : 0;
}

static cell AMX_NATIVE_CALL Array_ByCount(AMX *amx,cell *params)   
{   
	PPvoid_t Array = Find_Array(params[1],params[4],amx);  
	if (Array == NULL) return 0;

	Word_t Index = Word_t(params[3]); 
	cell *success = MF_GetAmxAddr(amx, params[4]);   

	PPvoid_t pointer;  
	pointer = JudyLByCount(*Array, params[2], &Index, PJE0);  

	*success = (pointer == NULL) ? 0 : 1; 
	return cell(Index);
} 

AMX_NATIVE_INFO array_exports[] = {
  { "array_set_string",	Array_SetString		},
  { "array_get_string",	Array_GetString		},

  { "array_set_int",		Array_SetInt		},
  { "array_get_int",		Array_GetInt		},

  { "array_set_float",	Array_SetFloat		},
  { "array_get_float",	Array_GetFloat		},

  { "array_set_vector",	Array_SetVector		},
  { "array_get_vector",	Array_GetVector		},
  
  { "array_isempty",	array_isempty		},
  { "array_isfilled",	array_isfilled		},

  { "array_remove",		delete_cell			},

  { "array_create",		new_array			},
  { "array_delete",		delete_array		},
  { "array_clear",		clear_array			},

  { "array_size",		array_size			},
  { "array_count",		array_count			},
  { "array_memory",		array_memory		},

  { "array_nextempty",	array_nextempty		}, 
  { "array_prevempty",	array_prevempty		}, 
  { "array_firstempty",	array_firstempty	}, 
  { "array_lastempty",	array_lastempty		}, 
  { "array_next",		array_next			}, 
  { "array_prev",		array_prev			}, 
  { "array_first",		array_first			}, 
  { "array_last",		array_last			},

  { "array_save",		Array_Save			},
  { "array_load",		Array_Load			},

  { "array_get_nth",	Array_ByCount		},

  { "array_save_ascii",	Array_Save_ASCII	},

  { NULL, NULL }
};