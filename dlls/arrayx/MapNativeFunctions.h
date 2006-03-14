#ifndef _MAP_NATIVE_FUNC_INC_H
#define _MAP_NATIVE_FUNC_INC_H

#define JUDY_GLUE_FUNC( x , y ) x ## y

#define JUDY_GLUE_STR( x, y ) #x#y

#define JUDY_MASTER_EDIT_FUNCTIONS
#define JUDY_MASTER_CLEAR_FUNC		JUDY_GLUE_FUNC( map ,	_clear		)
#define JUDY_MASTER_CLEAR_STR		JUDY_GLUE_STR (	map ,	_clear		)

#define JUDY_MASTER_DELETE_FUNC		JUDY_GLUE_FUNC( map ,	_delete		)
#define JUDY_MASTER_DELETE_STR		JUDY_GLUE_STR (	map ,	_delete		)

#define JUDY_MASTER_IO_FUNCTIONS

#define JUDY_MASTER_SAVE_FUNC		JUDY_GLUE_FUNC( map ,	_save		)
#define JUDY_MASTER_SAVE_STR		JUDY_GLUE_STR (	map ,	_save		)
#define JUDY_SAVE_FUNC(map, file)	JudySaveMap  ( map ,	file		)

#define JUDY_MASTER_LOAD_FUNC		JUDY_GLUE_FUNC( map ,	_load		)
#define JUDY_MASTER_LOAD_STR		JUDY_GLUE_STR (	map ,	_load		)
#define JUDY_LOAD_FUNC(map, file)	JudyLoadMap  ( map ,	file		)

#define JUDY_MASTER_AMOUNT_FUNCTIONS
#define JUDY_MASTER_COUNT_FUNC		JUDY_GLUE_FUNC( map ,	_count		)
#define JUDY_MASTER_COUNT_STR		JUDY_GLUE_STR (	map ,	_count		)

#define JUDY_MASTER_BYCOUNT_FUNC	JUDY_GLUE_FUNC( map ,	_bycount	)
#define JUDY_MASTER_BYCOUNT_STR		JUDY_GLUE_STR (	map ,	_bycount	)

#define JUDY_SLAVE_EDIT_FUNCTIONS

#define JUDY_SLAVE_MEMORY_FUNC		JUDY_GLUE_FUNC( map ,	_memory		)
#define JUDY_SLAVE_MEMORY_STR		JUDY_GLUE_STR (	map ,	_memory		)

#define JUDY_SLAVE_ISFILLED_FUNC	JUDY_GLUE_FUNC( map ,	_isfilled	)
#define JUDY_SLAVE_ISFILLED_STR		JUDY_GLUE_STR (	map ,	_isfilled	)

#define JUDY_SLAVE_ISEMPTY_FUNC		JUDY_GLUE_FUNC( map ,	_isempty	)
#define JUDY_SLAVE_ISEMPTY_STR		JUDY_GLUE_STR (	map ,	_isempty	)

#define JUDY_SLAVE_REMOVE_FUNC		JUDY_GLUE_FUNC( map ,	_remove		)
#define JUDY_SLAVE_REMOVE_STR		JUDY_GLUE_STR (	map ,	_remove		)

#define JUDY_SLAVE_EDIT_BOOL
#define JUDY_SLAVE_GET_BOOL_FUNC	JUDY_GLUE_FUNC( map ,	_get_bool	)
#define JUDY_SLAVE_SET_BOOL_FUNC	JUDY_GLUE_FUNC( map ,	_set_bool	)

#define JUDY_SLAVE_GET_BOOL_STR		JUDY_GLUE_STR ( map ,	_get_bool	)
#define JUDY_SLAVE_SET_BOOL_STR		JUDY_GLUE_STR ( map ,	_set_bool	)

#define JUDY_SLAVE_EDIT_INT
#define JUDY_SLAVE_GET_INT_FUNC		JUDY_GLUE_FUNC( map ,	_get_int	)
#define JUDY_SLAVE_SET_INT_FUNC		JUDY_GLUE_FUNC( map ,	_set_int	)

#define JUDY_SLAVE_GET_INT_STR		JUDY_GLUE_STR ( map ,	_get_int	)
#define JUDY_SLAVE_SET_INT_STR		JUDY_GLUE_STR ( map ,	_set_int	)

#define JUDY_SLAVE_EDIT_FLO
#define JUDY_SLAVE_GET_FLO_FUNC		JUDY_GLUE_FUNC( map ,	_get_float	)
#define JUDY_SLAVE_SET_FLO_FUNC		JUDY_GLUE_FUNC( map ,	_set_float	)

#define JUDY_SLAVE_GET_FLO_STR		JUDY_GLUE_STR ( map ,	_get_float	)
#define JUDY_SLAVE_SET_FLO_STR		JUDY_GLUE_STR ( map ,	_set_float	)

#define JUDY_SLAVE_EDIT_STR
#define JUDY_SLAVE_GET_STR_FUNC		JUDY_GLUE_FUNC( map ,	_get_string	)
#define JUDY_SLAVE_SET_STR_FUNC		JUDY_GLUE_FUNC( map ,	_set_string	)

#define JUDY_SLAVE_GET_STR_STR		JUDY_GLUE_STR ( map ,	_get_string	)
#define JUDY_SLAVE_SET_STR_STR		JUDY_GLUE_STR ( map ,	_set_string	)

#define JUDY_SLAVE_EDIT_VEC
#define JUDY_SLAVE_GET_VEC_FUNC		JUDY_GLUE_FUNC( map ,	_get_vector	)
#define JUDY_SLAVE_SET_VEC_FUNC		JUDY_GLUE_FUNC( map ,	_set_vector	)

#define JUDY_SLAVE_GET_VEC_STR		JUDY_GLUE_STR ( map ,	_get_vector	)
#define JUDY_SLAVE_SET_VEC_STR		JUDY_GLUE_STR ( map ,	_set_vector	)

#define JUDY_SLAVE_SEARCH_FUNCTIONS
#define JUDY_SLAVE_FIRST_FUNC		JUDY_GLUE_FUNC( map ,	_first		)
#define JUDY_SLAVE_LAST_FUNC		JUDY_GLUE_FUNC( map ,	_last		)

#define JUDY_SLAVE_FIRST_STR		JUDY_GLUE_STR ( map ,	_first		)
#define JUDY_SLAVE_LAST_STR			JUDY_GLUE_STR ( map ,	_last		)

#define JUDY_SLAVE_NEXT_FUNC		JUDY_GLUE_FUNC( map ,	_next		)
#define JUDY_SLAVE_PREV_FUNC		JUDY_GLUE_FUNC( map ,	_prev		)

#define JUDY_SLAVE_NEXT_STR			JUDY_GLUE_STR ( map ,	_next		)
#define JUDY_SLAVE_PREV_STR			JUDY_GLUE_STR ( map ,	_prev		)

#endif