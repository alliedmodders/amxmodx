#if !defined _JUDYHS_ENABLED_
#define _JUDYHS_ENABLED_

Pvoid_t MasterHashtable = (Pvoid_t) NULL;     //Create the new array 

//Create an array that stores whether or not indices are used.
Pvoid_t MasterHashtable_Binary = (Pvoid_t) NULL; 

void Delete_MasterHashtable(void);

Word_t		New_Hashtable(Word_t Index, Word_t reserve = 0);
Pvoid_t*	Find_Hashtable(Word_t Index, Word_t disable_check = 1, AMX *amx = 0);
void		Delete_Hashtable(Word_t Index);
void		Clear_Hashtable(Word_t Index);

template <class Type>
void Hashtable_Set(PPvoid_t Hashtable, char *Index, Word_t Length, Type value);

PPvoid_t Hashtable_Get(AMX* amx, Pvoid_t Hashtable, char *Index, int ignore_error = 0);

void Delete_MasterHashtable(void)
{
	Word_t 
		Index = 0,
		success;
	J1F(success, MasterHashtable_Binary, Index);
	while( success )
	{
		Delete_Hashtable(Index);
		J1F(success, MasterHashtable_Binary, Index);
	}
}

Word_t New_Hashtable(Word_t Index, Word_t reserve)
{
	Word_t success; //Dummy for macros.
	J1T(success, MasterHashtable_Binary, Index);

	if (success && reserve)
		return Index; //If the bit is set but it's 'reserved', return the index.

	//Only do this if the bit is not set or not reserved.
	J1FE(success, MasterHashtable_Binary, Index);
	J1S(success, MasterHashtable_Binary, Index);

	PPvoid_t Hashtable = JudyLIns(&MasterHashtable, Index, PJE0);
	*Hashtable = (PWord_t) NULL;

	return Index;
}

PPvoid_t Find_Hashtable(Word_t Index, Word_t disable_check, AMX* amx)
{
	Word_t success;
	J1T(success, MasterHashtable_Binary, Index);
	if (success || disable_check)
	{ //Bit is valid
		if(!success) 
			New_Hashtable(Index);

		return JudyLGet(MasterHashtable, Index, PJE0);
	}
	MF_LogError(amx,AMX_ERR_NATIVE,"Hashtable %d is invalid.", Index);
	return NULL;
}

void Delete_Hashtable(Word_t Index)
{
	int success;
	J1T(success, MasterHashtable_Binary, Index);
	if (success)
	{ //If the bit was set, clear, unset and delist hashtable.
		Clear_Hashtable(Index);
		J1U(success, MasterHashtable_Binary, Index);
		JudyLDel(&MasterHashtable, Index, PJE0);
	}
}

void Clear_Hashtable(Word_t Index)
{
	int success;
	J1T(success, MasterHashtable_Binary, Index);
	if (success) //dont bother with unset hashtables.
	{
		PPvoid_t Hashtable = Find_Hashtable(Index);
		JHSFA(success, *Hashtable);
	}
}

template <class Type> //This will support input char*, Vector*, int, and cell_real*.
void Hashtable_Set(PPvoid_t Hashtable, char* Index, Type value)
{
	int Len = strlen(Index)+1;
	PPvoid_t PValue = JudyHSIns(Hashtable, Index, Len, PJE0);
	*PValue = reinterpret_cast<void*>(value);
}

PPvoid_t Hashtable_Get(AMX* amx,PPvoid_t Hashtable, char *Index, int ignore_error = 0)
{
	PPvoid_t PValue = JudyHSGet(*Hashtable, Index, strlen(Index)+1);
	if (PValue == NULL && !ignore_error)
		MF_LogError(amx, AMX_ERR_NATIVE, "Hashtable get on index \"%s\" is invalid", Index);
	return PValue;
}

static cell AMX_NATIVE_CALL Hashtable_Create(AMX *amx, cell *params)
{
	return New_Hashtable(params[1],params[2]);
}

static cell AMX_NATIVE_CALL Hashtable_Delete(AMX *amx, cell *params)
{
	Delete_Hashtable( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL Hashtable_Clear(AMX *amx, cell *params)
{
	Clear_Hashtable( params[1] );

	return 1;
}

static cell AMX_NATIVE_CALL Hashtable_SetVector(AMX *amx,cell *params)
{
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[4], amx);
	if (Hashtable == NULL) return 0;

	cell *input_vec = MF_GetAmxAddr(amx, params[3]); 
	Vector *value = new Vector(
		amx_ctof(input_vec[0]), 
		amx_ctof(input_vec[1]), 
		amx_ctof(input_vec[2])
	);
	int strlen;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlen);

	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_vec(value);
	}
	Hashtable_Set(Hashtable,Index,elem_value);
	return 1;
}


static cell AMX_NATIVE_CALL Hashtable_GetVector(AMX *amx, cell *params)
{
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[4], amx);
	if (Hashtable == NULL) return 0;

	int strlen;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlen);
	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, params[4]);

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

static cell AMX_NATIVE_CALL Hashtable_SetString(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[4], amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	//params[3]: value
	int iLen = 0;
	char *value = MF_GetAmxString(amx,params[3],1,&iLen);

	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(value);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_str(value);
	}
	Hashtable_Set(Hashtable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Hashtable_GetString(AMX *amx,cell *params) 
{ 
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[5], amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	
	Pvoid_t * PValue = Hashtable_Get(amx, Hashtable, Index, params[5]);

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

static cell AMX_NATIVE_CALL Hashtable_SetFloat(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[4], amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	//params[3]: value
	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(amx_ctof(params[3]));
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_flo(amx_ctof(params[3]));
	}
	Hashtable_Set(Hashtable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Hashtable_GetFloat(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[3], amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, params[3]);

	if( PValue == NULL ) return amx_ftoc(0.0);

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_float = amx_ftoc(elem_value.get_flo(error));
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_float;
}

static cell AMX_NATIVE_CALL Hashtable_SetInt(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[4], amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	PPvoid_t PValue = Hashtable_Get(amx, Hashtable, Index, 1);
	element *elem_value = NULL;
	if ( PValue == NULL )
		elem_value = new element(params[3]);
	else
	{
		elem_value = reinterpret_cast<element*>(*PValue);
		(*elem_value).set_int(params[3]);
	}
	Hashtable_Set(Hashtable,Index,elem_value);

	return 1;
}

static cell AMX_NATIVE_CALL Hashtable_GetInt(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], params[3], amx);
	if (Hashtable == NULL) return 0;
	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);

	Pvoid_t * PValue = Hashtable_Get(amx, Hashtable, Index, params[3]);

	if( PValue == NULL ) return 0;

	element elem_value = *reinterpret_cast<element*>(*PValue);

	int error = 0;
	cell retr_int = elem_value.get_int(error);
	if (error)
		elem_value.issue_type_error(amx, params[1], Index);
	return retr_int;
}

static cell AMX_NATIVE_CALL Hashtable_Memory(AMX *amx,cell *params)
{
	Pvoid_t * Array = Find_Hashtable(params[1],params[2],amx);
	if (Array == NULL) return 0;
	
	return JudyLMemUsed(*Array);
}


static cell AMX_NATIVE_CALL Hashtable_Remove(AMX *amx,cell *params)
{
	//params[1]: hashtable
	PPvoid_t Hashtable = Find_Hashtable(params[1], 0, amx);
	if (Hashtable == NULL) return 0;

	//params[2]: key
	int strlength;
	char *Index = MF_GetAmxString(amx, params[2], 0, &strlength);
	JudyHSDel(Hashtable, Index, strlength+1, PJE0 );

	return 1;
}

AMX_NATIVE_INFO hashtable_exports[] = {
  { "hashtable_set_str",	Hashtable_SetString },
  { "hashtable_get_str",	Hashtable_GetString },

  { "hashtable_set_vec",	Hashtable_SetVector },
  { "hashtable_get_vec",	Hashtable_GetVector },

  { "hashtable_set_int",	Hashtable_SetInt },
  { "hashtable_get_int",	Hashtable_GetInt },

  { "hashtable_set_float",	Hashtable_SetFloat },
  { "hashtable_get_float",	Hashtable_GetFloat },

  { "hashtable_memory",		Hashtable_Memory },

  { "hashtable_remove",		Hashtable_Remove },

  { "hashtable_create",		Hashtable_Create },
  { "hashtable_delete",		Hashtable_Delete },
  { "hashtable_clear",		Hashtable_Clear },
  { NULL, NULL }
};

#endif