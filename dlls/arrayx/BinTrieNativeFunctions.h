#ifndef _bintrie_NATIVE_FUNC_INC_H
#define _bintrie_NATIVE_FUNC_INC_H

#define JUDY_GLUE_FUNC( x , y ) x ## y

#define JUDY_GLUE_STR( x, y ) #x#y

#define JUDY_MASTER_EDIT_FUNCTIONS
#define JUDY_MASTER_CLEAR_FUNC		JUDY_GLUE_FUNC( bintrie ,	_clear		)
#define JUDY_MASTER_CLEAR_STR		JUDY_GLUE_STR (	bintrie ,	_clear		)

#define JUDY_MASTER_DELETE_FUNC		JUDY_GLUE_FUNC( bintrie ,	_delete		)
#define JUDY_MASTER_DELETE_STR		JUDY_GLUE_STR (	bintrie ,	_delete		)

#define JUDY_MASTER_IO_FUNCTIONS

#define JUDY_MASTER_SAVE_FUNC		JUDY_GLUE_FUNC( bintrie ,	_save		)
#define JUDY_MASTER_SAVE_STR		JUDY_GLUE_STR (	bintrie ,	_save		)
#define JUDY_SAVE_FUNC(bin,file)	JudySaveBinTrie(bin		,	file		)

#define JUDY_MASTER_LOAD_FUNC		JUDY_GLUE_FUNC( bintrie ,	_load		)
#define JUDY_MASTER_LOAD_STR		JUDY_GLUE_STR (	bintrie ,	_load		)
#define JUDY_LOAD_FUNC(bin, file)	JudyLoadBinTrie(bin		,	file		)

#define JUDY_MASTER_AMOUNT_FUNCTIONS
#define JUDY_MASTER_COUNT_FUNC		JUDY_GLUE_FUNC( bintrie ,	_count		)
#define JUDY_MASTER_COUNT_STR		JUDY_GLUE_STR (	bintrie ,	_count		)

#define JUDY_MASTER_BYCOUNT_FUNC	JUDY_GLUE_FUNC( bintrie ,	_bycount	)
#define JUDY_MASTER_BYCOUNT_STR		JUDY_GLUE_STR (	bintrie ,	_bycount	)

#define JUDY_SLAVE_AMOUNT_FUNCTIONS

#define JUDY_SLAVE_COUNT_FUNC		JUDY_GLUE_FUNC( bintrie ,	_size		)
#define JUDY_SLAVE_COUNT_STR		JUDY_GLUE_STR (	bintrie ,	_size		)

#define JUDY_SLAVE_BYCOUNT_FUNC		JUDY_GLUE_FUNC( bintrie ,	_get_nth	)
#define JUDY_SLAVE_BYCOUNT_STR		JUDY_GLUE_STR (	bintrie ,	_get_nth	)

#define JUDY_SLAVE_EDIT_FUNCTIONS

#define JUDY_SLAVE_MEMORY_FUNC		JUDY_GLUE_FUNC( bintrie ,	_memory		)
#define JUDY_SLAVE_MEMORY_STR		JUDY_GLUE_STR (	bintrie ,	_memory		)

#define JUDY_SLAVE_ISFILLED_FUNC	JUDY_GLUE_FUNC( bintrie ,	_isfilled	)
#define JUDY_SLAVE_ISFILLED_STR		JUDY_GLUE_STR (	bintrie ,	_isfilled	)

#define JUDY_SLAVE_ISEMPTY_FUNC		JUDY_GLUE_FUNC( bintrie ,	_isempty	)
#define JUDY_SLAVE_ISEMPTY_STR		JUDY_GLUE_STR (	bintrie ,	_isempty	)

#define JUDY_SLAVE_REMOVE_FUNC		JUDY_GLUE_FUNC( bintrie ,	_remove		)
#define JUDY_SLAVE_REMOVE_STR		JUDY_GLUE_STR (	bintrie ,	_remove		)

#define JUDY_SLAVE_SEARCH_FUNCTIONS
#define JUDY_SLAVE_FIRST_FUNC		JUDY_GLUE_FUNC( bintrie ,	_first		)
#define JUDY_SLAVE_LAST_FUNC		JUDY_GLUE_FUNC( bintrie ,	_last		)

#define JUDY_SLAVE_FIRST_STR		JUDY_GLUE_STR ( bintrie ,	_first		)
#define JUDY_SLAVE_LAST_STR			JUDY_GLUE_STR ( bintrie ,	_last		)

#define JUDY_SLAVE_NEXT_FUNC		JUDY_GLUE_FUNC( bintrie ,	_next		)
#define JUDY_SLAVE_PREV_FUNC		JUDY_GLUE_FUNC( bintrie ,	_prev		)

#define JUDY_SLAVE_NEXT_STR			JUDY_GLUE_STR ( bintrie ,	_next		)
#define JUDY_SLAVE_PREV_STR			JUDY_GLUE_STR ( bintrie ,	_prev		)

#define JUDY_SLAVE_SEARCH_EMPTY_FUNCTIONS
#define JUDY_SLAVE_FIRSTEMPTY_FUNC	JUDY_GLUE_FUNC( bintrie ,	_firstempty	)
#define JUDY_SLAVE_LASTEMPTY_FUNC	JUDY_GLUE_FUNC( bintrie ,	_lastempty	)

#define JUDY_SLAVE_FIRSTEMPTY_STR	JUDY_GLUE_STR ( bintrie ,	_firstempty	)
#define JUDY_SLAVE_LASTEMPTY_STR	JUDY_GLUE_STR ( bintrie ,	_lastempty	)

#define JUDY_SLAVE_NEXTEMPTY_FUNC	JUDY_GLUE_FUNC( bintrie ,	_nextempty	)
#define JUDY_SLAVE_PREVEMPTY_FUNC	JUDY_GLUE_FUNC( bintrie ,	_prevempty	)

#define JUDY_SLAVE_NEXTEMPTY_STR	JUDY_GLUE_STR ( bintrie ,	_nextempty	)
#define JUDY_SLAVE_PREVEMPTY_STR	JUDY_GLUE_STR ( bintrie ,	_prevempty	)
#endif