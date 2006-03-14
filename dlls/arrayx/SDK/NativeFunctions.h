#ifndef _NATIVE_FUNC_INC_H
#define _NATIVE_FUNC_INC_H

#define JUDY_GLUE_FUNC( x , y ) x ## y

#define JUDY_MASTER_FUNCTIONS
#define JUDY_MASTER_CREATE_FUNC		JUDY_GLUE_FUNC( array , _create		)
#define JUDY_MASTER_CLEAR_FUNC		JUDY_GLUE_FUNC( array ,	_clear		)
#define JUDY_MASTER_DELETE_FUNC		JUDY_GLUE_FUNC( array ,	_delete		)

#define JUDY_MASTER_IO_FUNCTIONS
#define JUDY_MASTER_SAVE_FUNC		JUDY_GLUE_FUNC( array ,	_save		)
#define JUDY_MASTER_LOAD_FUNC		JUDY_GLUE_FUNC( array ,	_load		)

#define JUDY_MASTER_AMOUNT_FUNCTIONS
#define JUDY_MASTER_COUNT_FUNC		JUDY_GLUE_FUNC( array ,	_count		)
#define JUDY_MASTER_BYCOUNT_FUNC	JUDY_GLUE_FUNC( array ,	_bycount	)
#define JUDY_MASTER_MEMORY_FUNC		JUDY_GLUE_FUNC( array ,	_memory		)

#define JUDY_SLAVE_AMOUNT_FUNCTIONS
#define JUDY_SLAVE_COUNT_FUNC		JUDY_GLUE_FUNC( array ,	_size		)
#define JUDY_SLAVE_BYCOUNT_FUNC		JUDY_GLUE_FUNC( array ,	_get_nth	)

#define JUDY_SLAVE_EDIT_FUNCTIONS
#define JUDY_SLAVE_GET_BOOL_FUNC	JUDY_GLUE_FUNC( array ,	_get_bool	)
#define JUDY_SLAVE_SET_BOOL_FUNC	JUDY_GLUE_FUNC( array ,	_set_bool	)

#define JUDY_SLAVE_GET_INT_FUNC		JUDY_GLUE_FUNC( array ,	_get_int	)
#define JUDY_SLAVE_SET_INT_FUNC		JUDY_GLUE_FUNC( array ,	_set_int	)

#define JUDY_SLAVE_GET_FLO_FUNC		JUDY_GLUE_FUNC( array ,	_get_float	)
#define JUDY_SLAVE_SET_FLO_FUNC		JUDY_GLUE_FUNC( array ,	_set_float	)

#define JUDY_SLAVE_GET_STR_FUNC		JUDY_GLUE_FUNC( array ,	_get_string	)
#define JUDY_SLAVE_SET_STR_FUNC		JUDY_GLUE_FUNC( array ,	_set_string	)

#define JUDY_SLAVE_GET_VEC_FUNC		JUDY_GLUE_FUNC( array ,	_get_vector	)
#define JUDY_SLAVE_SET_VEC_FUNC		JUDY_GLUE_FUNC( array ,	_set_vector	)

#define JUDY_SLAVE_ISFILLED_FUNC	JUDY_GLUE_FUNC( array ,	_isfilled	)
#define JUDY_SLAVE_ISEMPTY_FUNC		JUDY_GLUE_FUNC( array ,	_isempty	)

#define JUDY_SLAVE_REMOVE_FUNC		JUDY_GLUE_FUNC( array ,	_remove		)

#define JUDY_SLAVE_SEARCH_FUNCTIONS
#define JUDY_SLAVE_GET_FIRST_FUNC	JUDY_GLUE_FUNC( array ,	_first		)
#define JUDY_SLAVE_SET_LAST_FUNC	JUDY_GLUE_FUNC( array ,	_last		)

#define JUDY_SLAVE_GET_NEXT_FUNC	JUDY_GLUE_FUNC( array ,	_next		)
#define JUDY_SLAVE_SET_PREV_FUNC	JUDY_GLUE_FUNC( array ,	_prev		)

#define JUDY_SLAVE_SEARCH_EMPTY_FUNCTIONS
#define JUDY_SLAVE_GET_FIRSTEMPTY_FUNC	JUDY_GLUE_FUNC( array ,	_firstempty		)
#define JUDY_SLAVE_SET_LASTEMPTY_FUNC	JUDY_GLUE_FUNC( array ,	_lastempty		)

#define JUDY_SLAVE_GET_NEXTEMPTY_FUNC	JUDY_GLUE_FUNC( array ,	_nextempty		)
#define JUDY_SLAVE_SET_PREVEMPTY_FUNC	JUDY_GLUE_FUNC( array ,	_prevempty		)
#endif