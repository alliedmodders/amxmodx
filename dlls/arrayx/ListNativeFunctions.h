#ifndef _LIST_NATIVE_FUNC_INC_H
#define _LIST_NATIVE_FUNC_INC_H

#define JUDY_GLUE_FUNC( x , y ) x ## y

#define JUDY_GLUE_STR( x, y ) #x#y

#define JUDY_MASTER_EDIT_FUNCTIONS
#define JUDY_MASTER_CLEAR_FUNC		JUDY_GLUE_FUNC( list ,	_clear		)
#define JUDY_MASTER_CLEAR_STR		JUDY_GLUE_STR (	list ,	_clear		)

#define JUDY_MASTER_DELETE_FUNC		JUDY_GLUE_FUNC( list ,	_delete		)
#define JUDY_MASTER_DELETE_STR		JUDY_GLUE_STR (	list ,	_delete		)

#define JUDY_MASTER_IO_FUNCTIONS

#define JUDY_MASTER_SAVE_FUNC		JUDY_GLUE_FUNC( list ,	_save		)
#define JUDY_MASTER_SAVE_STR		JUDY_GLUE_STR (	list ,	_save		)
#define JUDY_SAVE_FUNC(list, file)	JudySaveList  ( list ,	file		)

#define JUDY_MASTER_LOAD_FUNC		JUDY_GLUE_FUNC( list ,	_load		)
#define JUDY_MASTER_LOAD_STR		JUDY_GLUE_STR (	list ,	_load		)
#define JUDY_LOAD_FUNC(list, file)	JudyLoadList  ( list ,	file		)

#define JUDY_MASTER_AMOUNT_FUNCTIONS
#define JUDY_MASTER_COUNT_FUNC		JUDY_GLUE_FUNC( list ,	_count		)
#define JUDY_MASTER_COUNT_STR		JUDY_GLUE_STR (	list ,	_count		)

#define JUDY_MASTER_BYCOUNT_FUNC	JUDY_GLUE_FUNC( list ,	_bycount	)
#define JUDY_MASTER_BYCOUNT_STR		JUDY_GLUE_STR (	list ,	_bycount	)

#define JUDY_SLAVE_AMOUNT_FUNCTIONS

#define JUDY_SLAVE_COUNT_FUNC		JUDY_GLUE_FUNC( list ,	_size		)
#define JUDY_SLAVE_COUNT_STR		JUDY_GLUE_STR (	list ,	_size		)

#define JUDY_SLAVE_BYCOUNT_FUNC		JUDY_GLUE_FUNC( list ,	_get_nth	)
#define JUDY_SLAVE_BYCOUNT_STR		JUDY_GLUE_STR (	list ,	_get_nth	)

#define JUDY_SLAVE_EDIT_FUNCTIONS

#define JUDY_SLAVE_MEMORY_FUNC		JUDY_GLUE_FUNC( list ,	_memory		)
#define JUDY_SLAVE_MEMORY_STR		JUDY_GLUE_STR (	list ,	_memory		)

#define JUDY_SLAVE_ISFILLED_FUNC	JUDY_GLUE_FUNC( list ,	_isfilled	)
#define JUDY_SLAVE_ISFILLED_STR		JUDY_GLUE_STR (	list ,	_isfilled	)

#define JUDY_SLAVE_ISEMPTY_FUNC		JUDY_GLUE_FUNC( list ,	_isempty	)
#define JUDY_SLAVE_ISEMPTY_STR		JUDY_GLUE_STR (	list ,	_isempty	)

#define JUDY_SLAVE_REMOVE_FUNC		JUDY_GLUE_FUNC( list ,	_remove		)
#define JUDY_SLAVE_REMOVE_STR		JUDY_GLUE_STR (	list ,	_remove		)

#define JUDY_SLAVE_EDIT_BOOL
#define JUDY_SLAVE_GET_BOOL_FUNC	JUDY_GLUE_FUNC( list ,	_get_bool	)
#define JUDY_SLAVE_SET_BOOL_FUNC	JUDY_GLUE_FUNC( list ,	_set_bool	)

#define JUDY_SLAVE_GET_BOOL_STR		JUDY_GLUE_STR ( list ,	_get_bool	)
#define JUDY_SLAVE_SET_BOOL_STR		JUDY_GLUE_STR ( list ,	_set_bool	)

#define JUDY_SLAVE_EDIT_INT
#define JUDY_SLAVE_GET_INT_FUNC		JUDY_GLUE_FUNC( list ,	_get_int	)
#define JUDY_SLAVE_SET_INT_FUNC		JUDY_GLUE_FUNC( list ,	_set_int	)

#define JUDY_SLAVE_GET_INT_STR		JUDY_GLUE_STR ( list ,	_get_int	)
#define JUDY_SLAVE_SET_INT_STR		JUDY_GLUE_STR ( list ,	_set_int	)

#define JUDY_SLAVE_EDIT_FLO
#define JUDY_SLAVE_GET_FLO_FUNC		JUDY_GLUE_FUNC( list ,	_get_float	)
#define JUDY_SLAVE_SET_FLO_FUNC		JUDY_GLUE_FUNC( list ,	_set_float	)

#define JUDY_SLAVE_GET_FLO_STR		JUDY_GLUE_STR ( list ,	_get_float	)
#define JUDY_SLAVE_SET_FLO_STR		JUDY_GLUE_STR ( list ,	_set_float	)

#define JUDY_SLAVE_EDIT_STR
#define JUDY_SLAVE_GET_STR_FUNC		JUDY_GLUE_FUNC( list ,	_get_string	)
#define JUDY_SLAVE_SET_STR_FUNC		JUDY_GLUE_FUNC( list ,	_set_string	)

#define JUDY_SLAVE_GET_STR_STR		JUDY_GLUE_STR ( list ,	_get_string	)
#define JUDY_SLAVE_SET_STR_STR		JUDY_GLUE_STR ( list ,	_set_string	)

#define JUDY_SLAVE_EDIT_VEC
#define JUDY_SLAVE_GET_VEC_FUNC		JUDY_GLUE_FUNC( list ,	_get_vector	)
#define JUDY_SLAVE_SET_VEC_FUNC		JUDY_GLUE_FUNC( list ,	_set_vector	)

#define JUDY_SLAVE_GET_VEC_STR		JUDY_GLUE_STR ( list ,	_get_vector	)
#define JUDY_SLAVE_SET_VEC_STR		JUDY_GLUE_STR ( list ,	_set_vector	)

#define JUDY_SLAVE_SEARCH_FUNCTIONS
#define JUDY_SLAVE_FIRST_FUNC		JUDY_GLUE_FUNC( list ,	_first		)
#define JUDY_SLAVE_LAST_FUNC		JUDY_GLUE_FUNC( list ,	_last		)

#define JUDY_SLAVE_FIRST_STR		JUDY_GLUE_STR ( list ,	_first		)
#define JUDY_SLAVE_LAST_STR			JUDY_GLUE_STR ( list ,	_last		)

#define JUDY_SLAVE_NEXT_FUNC		JUDY_GLUE_FUNC( list ,	_next		)
#define JUDY_SLAVE_PREV_FUNC		JUDY_GLUE_FUNC( list ,	_prev		)

#define JUDY_SLAVE_NEXT_STR			JUDY_GLUE_STR ( list ,	_next		)
#define JUDY_SLAVE_PREV_STR			JUDY_GLUE_STR ( list ,	_prev		)

#define JUDY_SLAVE_SEARCH_EMPTY_FUNCTIONS
#define JUDY_SLAVE_FIRSTEMPTY_FUNC	JUDY_GLUE_FUNC( list ,	_firstempty		)
#define JUDY_SLAVE_LASTEMPTY_FUNC	JUDY_GLUE_FUNC( list ,	_lastempty		)

#define JUDY_SLAVE_FIRSTEMPTY_STR	JUDY_GLUE_STR ( list ,	_firstempty		)
#define JUDY_SLAVE_LASTEMPTY_STR	JUDY_GLUE_STR ( list ,	_lastempty		)

#define JUDY_SLAVE_NEXTEMPTY_FUNC	JUDY_GLUE_FUNC( list ,	_nextempty		)
#define JUDY_SLAVE_PREVEMPTY_FUNC	JUDY_GLUE_FUNC( list ,	_prevempty		)

#define JUDY_SLAVE_NEXTEMPTY_STR	JUDY_GLUE_STR ( list ,	_nextempty		)
#define JUDY_SLAVE_PREVEMPTY_STR	JUDY_GLUE_STR ( list ,	_prevempty		)
#endif