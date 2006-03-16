#ifndef _GENERIC_INC_H
#define _GENERIC_INC_H

// Master table
ComboArray MNAME;

///*		MASTER FUNCTIONS		*///

///*		Start Master Edit Funcs		*///
#ifdef JUDY_MASTER_EDIT_FUNCTIONS

	#ifdef JUDY_MASTER_DELETE_FUNC

		// generic_delete(id)
		static cell AMX_NATIVE_CALL JUDY_MASTER_DELETE_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1] );

			Unit->Remove();

			try { return MNAME.Delete( params[1] ); }
			JUDY_ERROR_CATCH("Judy Error: (No error possible) - Delete function ");
		}

	#else

		#error Must Have Delete func: JUDY_MASTER_DELETE_FUNC not defined!

	#endif

	#ifdef JUDY_MASTER_CLEAR_FUNC

		// generic_clear(id)
		static cell AMX_NATIVE_CALL JUDY_MASTER_CLEAR_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1] );

			try { return Unit->Clear(); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - Clear function ");
		}

	#else

		#error Must Have Clear func: JUDY_MASTER_CLEAR_FUNC not defined!

	#endif

///*		End Master Edit Funcs		*///
#endif

///*		Start Master IO Funcs		*///
#ifdef JUDY_MASTER_IO_FUNCTIONS

	#ifdef JUDY_MASTER_SAVE_FUNC

		// generic_save(id,file[])
		static cell AMX_NATIVE_CALL JUDY_MASTER_SAVE_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			return JUDY_SAVE_FUNC(Unit, JUDY_BUILD_PATH(amx,params[2]) );
		}

	#else

		#error Must Have Save func: JUDY_MASTER_SAVE_FUNC not defined properly!

	#endif

	#ifdef JUDY_MASTER_LOAD_FUNC

		// generic_load(file[],id)
		static cell AMX_NATIVE_CALL JUDY_MASTER_LOAD_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[2]);

			return JUDY_LOAD_FUNC(Unit, JUDY_BUILD_PATH(amx,params[1]) );
		}

	#else

		#error Must Have Load func: JUDY_MASTER_LOAD_FUNC not defined!

	#endif

///*		End Master IO Funcs		*///
#endif

///*		Start Master Amount Funcs		*///
#ifdef JUDY_MASTER_AMOUNT_FUNCTIONS

	#ifdef JUDY_MASTER_COUNT_FUNC

		// generic_count(start = 0, stop = -1)
		static cell AMX_NATIVE_CALL JUDY_MASTER_COUNT_FUNC(AMX *amx,cell *params)
		{
			try { return MNAME.Count(params[1],params[2] ); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - Count Function ");
		}

	#else

		#error Must Have Count func: JUDY_MASTER_COUNT_FUNC not defined!

	#endif

	#ifdef JUDY_MASTER_BYCOUNT_FUNC

		// generic_bycount(nth, start = -1)
		static cell AMX_NATIVE_CALL JUDY_MASTER_BYCOUNT_FUNC(AMX *amx,cell *params)
		{
			try { return MNAME.ByCount(params[1],params[2] ); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - ByCount Function ");
		}

	#else

		#error Must Have ByCount func: JUDY_MASTER_BYCOUNT_FUNC not defined!

	#endif

///*		End Master Amount Funcs		*///
#endif

///*			SLAVE FUNCTIONS		*///

///*		Start Slave Amount Funcs		*///
#ifdef JUDY_SLAVE_AMOUNT_FUNCTIONS

	#ifdef JUDY_SLAVE_COUNT_FUNC

		// generic_size(id, start = 0, stop = -1)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_COUNT_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			try { return Unit->Count(JUDY_GET_KEY(params,2),JUDY_GET_KEY(params, 3) ); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - Slave Count Function ");
		}

	#else

		#error Must Have Count func: JUDY_SLAVE_COUNT_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_BYCOUNT_FUNC

		// generic_get_nth(id, nth, start = -1)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_BYCOUNT_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			try { return Unit->ByCount(JUDY_GET_KEY(params,2),JUDY_GET_KEY(params, 3) ); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - Slave ByCount Function ");
		}

	#else

		#error Must Have ByCount func: JUDY_SLAVE_BYCOUNT_FUNC not defined!

	#endif

///*		End Slave Amount Funcs		*///
#endif

///*		Start Slave Edit Funcs		*///
#ifdef JUDY_SLAVE_EDIT_FUNCTIONS

		#ifdef JUDY_SLAVE_MEMORY_FUNC

		// generic_memory(id)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_MEMORY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			try { return Unit->MemoryUsed(); }
			JUDY_ERROR_CATCH("Judy Error: (Search error likely) - Slave ByCount Function ");
		}

	#else

		#error Must Have Memory func: JUDY_SLAVE_MEMORY_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_ISFILLED_FUNC

		// generic_isfilled(id, index)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_ISFILLED_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			try { return Unit->IsFilled(JUDY_GET_KEY(params,2) ); }
			JUDY_ERROR_CATCH("Judy Error: (No error possible) - Slave IsFilled Function ");
		}

	#else

		#error Must Have IsFilled func: JUDY_SLAVE_ISFILLED_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_ISEMPTY_FUNC

		// generic_isempty(id, index)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_ISEMPTY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			try { return Unit->IsEmpty(JUDY_GET_KEY(params,2) ); }
			JUDY_ERROR_CATCH("Judy Error: (No error possible) - Slave IsEmpty Function ");
		}

	#else

		#error Must Have IsEmpty func: JUDY_SLAVE_ISEMPTY_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_REMOVE_FUNC

		// generic_remove(id, index)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_REMOVE_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );
			Storage->Remove();

			try { return Unit->Delete(Indice); }
			JUDY_ERROR_CATCH("Judy Error: (No Error Possible) - Delete function ");
		}
	#else

		#ifdef NO_JUDY_SLAVE_REMOVE_FUNC
		#else

			#error Must Have Delete func: JUDY_SLAVE_REMOVE_FUNC not defined!

		#endif

	#endif
///*		End Required Slave Edit Funcs		*///

///*		Start Slave Bool Funcs		*///
#ifdef JUDY_SLAVE_EDIT_BOOL

	#ifdef JUDY_SLAVE_SET_BOOL_FUNC
		// generic_set_bool(id, index, Bool:val)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_SET_BOOL_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			bool Value = (params[3] != NULL);
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );

			if(Storage == NULL) Storage = new STYPE(Value);
			else Storage->SetBool(Value);

			JUDY_SET_INDEX_P(Unit,Storage,Indice);
		}

	#else

		#error Must Have Set func: JUDY_SLAVE_SET_BOOL_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_GET_BOOL_FUNC

		// Bool:generic_get_bool(id, index, disable_check = 0)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_GET_BOOL_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			bool disable_check = (params[3] != NULL);
		
			try { Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, disable_check ) ); }
			JUDY_ERROR_CATCH("Judy Error: (Retrieve unset value) - Slave Get Function ");

			if(Storage == NULL) return 0;
			
			return Storage->GetBool();
		}

	#else

		#error Must Have Get func: JUDY_SLAVE_GET_BOOL_FUNC not defined!

	#endif

///*		End Slave Bool Funcs		*///
#endif

///*		Start Slave Int Funcs		*///
#ifdef JUDY_SLAVE_EDIT_INT

	#ifdef JUDY_SLAVE_SET_INT_FUNC

		// generic_set_bool(id, index, val)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_SET_INT_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell Value = params[3];
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );

			if(Storage == NULL) Storage = new STYPE(Value);
			else Storage->SetInt(Value);

			JUDY_SET_INDEX_P(Unit,Storage,Indice);
		}

	#else

		#error Must Have Set func: JUDY_SLAVE_SET_INT_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_GET_INT_FUNC

		// generic_get_int(id, index, disable_check = 0)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_GET_INT_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			bool disable_check = (params[3] != NULL);
		
			try { Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, disable_check ) ); }
			JUDY_ERROR_CATCH("Judy Error: (Retrieve unset value) - Slave Get Function ");

			if(Storage == NULL) return 0;
			
			return Storage->GetInt();
		}

	#else

		#error Must Have Get func: JUDY_SLAVE_GET_INT_FUNC not defined!

	#endif

///*		End Slave Int Funcs		*///
#endif

///*		Start Slave Float Funcs		*///
#ifdef JUDY_SLAVE_EDIT_FLO

	#ifdef JUDY_SLAVE_SET_FLO_FUNC

		// generic_set_float(id, index, Float:val)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_SET_FLO_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			REAL Value = amx_ctof(params[3]);
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );

			if(Storage == NULL) Storage = new STYPE(Value);
			else Storage->SetFlo(Value);

			JUDY_SET_INDEX_P(Unit,Storage,Indice);
		}

	#else

		#error Must Have Set func: JUDY_SLAVE_SET_FLO_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_GET_FLO_FUNC

		// Float:generic_get_float(id, index, disable_check = 0)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_GET_FLO_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			bool disable_check = (params[3] != NULL);
		
			try { Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, disable_check ) ); }
			JUDY_ERROR_CATCH("Judy Error: (Retrieve unset value) - Slave Get Function ");

			if(Storage == NULL) return 0;
			
			return amx_ftoc(Storage->GetFlo() );
		}

	#else

		#error Must Have Get func: JUDY_SLAVE_GET_FLO_FUNC not defined!

	#endif

///*		End Slave Float Funcs		*///
#endif

///*		Start Slave String Funcs		*///
#ifdef JUDY_SLAVE_EDIT_STR

	#ifdef JUDY_SLAVE_SET_STR_FUNC

		// generic_set_string(id, index, val[])
		static cell AMX_NATIVE_CALL JUDY_SLAVE_SET_STR_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			char* Value = MF_GetAmxString(amx,params[3],3,NULL);
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );

			if(Storage == NULL) Storage = new STYPE(Value);
			else Storage->SetStr(Value);

			JUDY_SET_INDEX_P(Unit,Storage,Indice);
		}

	#else

		#error Must Have Set func: JUDY_SLAVE_SET_STR_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_GET_STR_FUNC

		// generic_get_string(id, index, val[], len, disable_check = 0)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_GET_STR_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			bool disable_check = (params[5] != NULL);
		
			try { Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, disable_check ) ); }
			JUDY_ERROR_CATCH("Judy Error: (Retrieve unset value) - Slave Get Function ");

			if(Storage == NULL) return 0;
			
			return MF_SetAmxString(amx,params[3], Storage->GetStr(), params[4] );
		}

	#else

		#error Must Have Get func: JUDY_SLAVE_GET_STR_FUNC not defined!

	#endif

///*		End Slave String Funcs		*///
#endif

///*		Start Slave Vector Funcs		*///
#ifdef JUDY_SLAVE_EDIT_VEC

	#ifdef JUDY_SLAVE_SET_VEC_FUNC

		// generic_set_vec(id, index, Float:val[3]) 
		static cell AMX_NATIVE_CALL JUDY_SLAVE_SET_VEC_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);

			cell *input_vec = MF_GetAmxAddr(amx, params[3]); 
			JudyVec *Value = new JudyVec(
				amx_ctof(input_vec[0]), 
				amx_ctof(input_vec[1]), 
				amx_ctof(input_vec[2])
				);
		
			Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, true ) );

			if(Storage == NULL) Storage = new STYPE(Value);
			else Storage->SetVec(Value);

			JUDY_SET_INDEX_P(Unit,Storage,Indice);
		}

	#else

		#error Must Have Set func: JUDY_SLAVE_SET_VEC_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_GET_FLO_FUNC

		// generic_get_vec(id,index,Float:vec[3], disable_check = 0)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_GET_VEC_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			STYPE* Storage;

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell *vAmx = MF_GetAmxAddr(amx, params[3]);
			bool disable_check = (params[4] != NULL);
		
			try { Storage = reinterpret_cast<STYPE*>( Unit->Get(Indice, disable_check ) ); }
			JUDY_ERROR_CATCH("Judy Error: (Retrieve unset value) - Slave Get Function ");

			if(Storage == NULL)
			{
				vAmx[0] = amx_ftoc(0);
				vAmx[1] = amx_ftoc(0);
				vAmx[2] = amx_ftoc(0);
				return 0;
			}
			
			JudyVec* Vec = const_cast<JudyVec*>( Storage->GetVec() );
	
			REAL One, Two, Three;
			Vec->Get(One, Two, Three);

			vAmx[0] = amx_ftoc(One);
			vAmx[1] = amx_ftoc(Two);
			vAmx[2] = amx_ftoc(Three);

			return 1;
		}

	#else

		#error Must Have Get func: JUDY_SLAVE_GET_VEC_FUNC not defined!

	#endif

///*		End Slave VEC Funcs		*///
#endif

///*		End Slave Edit Funcs		*///
#endif

///*		Start Slave Search Funcs
#ifdef JUDY_SLAVE_SEARCH_FUNCTIONS

	#ifdef JUDY_SLAVE_FIRST_FUNC
		// generic_first(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_FIRST_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->First(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_FIRST_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_NEXT_FUNC
		// generic_next(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_NEXT_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			JUDY_GET_INDEX(MNAME,Unit, params[1]);

			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->Next(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_NEXT_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_PREV_FUNC
		// generic_prev(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_PREV_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->Prev(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_PREV_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_LAST_FUNC
		// generic_first(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_LAST_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->Last(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_LAST_FUNC not defined!

	#endif

///*		End Slave Search Funcs		*///
#endif


///*		Start Slave Empty Search Funcs
#ifdef JUDY_SLAVE_SEARCH_EMPTY_FUNCTIONS

	#ifdef JUDY_SLAVE_FIRSTEMPTY_FUNC
		// generic_firstempty(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_FIRSTEMPTY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->FirstEmpty(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_FIRSTEMPTY_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_NEXTEMPTY_FUNC
		// generic_next(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_NEXTEMPTY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->NextEmpty(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_NEXTEMPTY_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_PREVEMPTY_FUNC
		// generic_prev(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_PREVEMPTY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->PrevEmpty(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function", *success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_PREVEMPTY_FUNC not defined!

	#endif

	#ifdef JUDY_SLAVE_LASTEMPTY_FUNC
		// generic_first(id, index,...)
		static cell AMX_NATIVE_CALL JUDY_SLAVE_LASTEMPTY_FUNC(AMX *amx,cell *params)
		{
			DTYPE* Unit = NULL;
			

			JUDY_GET_INDEX(MNAME,Unit, params[1]);
			ITYPE Indice = JUDY_GET_KEY(params,2);
			cell* success = MF_GetAmxAddr(amx, params[3 + SE_OFFSET]); 
		
			*success = 1;
			try { return JUDY_SET_KEY(Unit->LastEmpty(Indice),3); }
			JUDY_SEARCH_ERROR_CATCH("Judy Error (Search failed) - Slave Search Function",*success);
		}

	#else

		#error Must Have Search func: JUDY_SLAVE_LASTEMPTY_FUNC not defined!

	#endif

///*		End Slave Search Empty Funcs		*///
#endif

AMX_NATIVE_INFO EXPORT_NAME[] = 
{

#ifdef JUDY_MASTER_EDIT_FUNCTIONS

	{ JUDY_MASTER_CLEAR_STR	, JUDY_MASTER_CLEAR_FUNC },
	{ JUDY_MASTER_DELETE_STR , JUDY_MASTER_DELETE_FUNC },

#endif

#ifdef JUDY_MASTER_IO_FUNCTIONS

	{ JUDY_MASTER_SAVE_STR , JUDY_MASTER_SAVE_FUNC },
	{ JUDY_MASTER_LOAD_STR , JUDY_MASTER_LOAD_FUNC },

#endif

#ifdef JUDY_MASTER_AMOUNT_FUNCTIONS

	{ JUDY_MASTER_COUNT_STR , JUDY_MASTER_COUNT_FUNC },
	{ JUDY_MASTER_BYCOUNT_STR , JUDY_MASTER_BYCOUNT_FUNC },

#endif

#ifdef JUDY_SLAVE_AMOUNT_FUNCTIONS

	{ JUDY_SLAVE_COUNT_STR , JUDY_SLAVE_COUNT_FUNC },
	{ JUDY_SLAVE_BYCOUNT_STR , JUDY_SLAVE_BYCOUNT_FUNC },

#endif

#ifdef JUDY_SLAVE_EDIT_FUNCTIONS

	{ JUDY_SLAVE_MEMORY_STR , JUDY_SLAVE_MEMORY_FUNC },
	{ JUDY_SLAVE_ISFILLED_STR , JUDY_SLAVE_ISFILLED_FUNC },
	{ JUDY_SLAVE_ISEMPTY_STR , JUDY_SLAVE_ISEMPTY_FUNC },

#ifndef NO_JUDY_SLAVE_REMOVE_FUNC
	{ JUDY_SLAVE_REMOVE_STR , JUDY_SLAVE_REMOVE_FUNC },
#endif

#ifdef JUDY_SLAVE_EDIT_BOOL

	{ JUDY_SLAVE_GET_BOOL_STR , JUDY_SLAVE_GET_BOOL_FUNC },
	{ JUDY_SLAVE_SET_BOOL_STR , JUDY_SLAVE_SET_BOOL_FUNC },

#endif

#ifdef JUDY_SLAVE_EDIT_INT

	{ JUDY_SLAVE_GET_INT_STR , JUDY_SLAVE_GET_INT_FUNC },
	{ JUDY_SLAVE_SET_INT_STR , JUDY_SLAVE_SET_INT_FUNC },

#endif

#ifdef JUDY_SLAVE_EDIT_FLO

	{ JUDY_SLAVE_GET_FLO_STR , JUDY_SLAVE_GET_FLO_FUNC },
	{ JUDY_SLAVE_SET_FLO_STR , JUDY_SLAVE_SET_FLO_FUNC },

#endif

#ifdef JUDY_SLAVE_EDIT_STR

	{ JUDY_SLAVE_GET_STR_STR , JUDY_SLAVE_GET_STR_FUNC },
	{ JUDY_SLAVE_SET_STR_STR , JUDY_SLAVE_SET_STR_FUNC },

#endif

#ifdef JUDY_SLAVE_EDIT_VEC

	{ JUDY_SLAVE_GET_VEC_STR , JUDY_SLAVE_GET_VEC_FUNC },
	{ JUDY_SLAVE_SET_VEC_STR , JUDY_SLAVE_SET_VEC_FUNC },

#endif

// End all edit functions
#endif

#ifdef JUDY_SLAVE_SEARCH_FUNCTIONS

	{ JUDY_SLAVE_FIRST_STR , JUDY_SLAVE_FIRST_FUNC },
	{ JUDY_SLAVE_LAST_STR , JUDY_SLAVE_LAST_FUNC },

	{ JUDY_SLAVE_NEXT_STR , JUDY_SLAVE_NEXT_FUNC },
	{ JUDY_SLAVE_PREV_STR , JUDY_SLAVE_PREV_FUNC },

#endif

#ifdef JUDY_SLAVE_SEARCH_EMPTY_FUNCTIONS

	{ JUDY_SLAVE_FIRSTEMPTY_STR , JUDY_SLAVE_FIRSTEMPTY_FUNC },
	{ JUDY_SLAVE_LASTEMPTY_STR , JUDY_SLAVE_LASTEMPTY_FUNC },

	{ JUDY_SLAVE_NEXTEMPTY_STR , JUDY_SLAVE_NEXTEMPTY_FUNC },
	{ JUDY_SLAVE_PREVEMPTY_STR , JUDY_SLAVE_PREVEMPTY_FUNC },

#endif

  { NULL, NULL }
};

#endif