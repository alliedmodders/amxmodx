#ifndef _NATIVE_INC_H
#define _NATIVE_INC_H

#include "ComboArray.h"

#ifndef SEARCH_ERROR_OFFSET
#define SEARCH_ERROR_OFFSET 0
#endif

#define SE_OFFSET SEARCH_ERROR_OFFSET

#ifndef STORAGE_TYPE
#error No storage type declared
#endif

#define STYPE STORAGE_TYPE

#ifndef KEY_TYPE
#error No key type declared
#endif

#define ITYPE KEY_TYPE

#ifndef DYNAMIC_UNIT_TYPE
#error No Dynamic type declared
#endif

#define DTYPE DYNAMIC_UNIT_TYPE

#ifndef MASTER_NAME
#error No master name declared
#endif

#define MNAME MASTER_NAME
#define M_ITYPE cell

#ifndef GET_KEY
#error No method of getting keys has been declared
#endif

#ifndef EXPORT_NAME
#define EXPORT_NAME MNAME_exports
#endif

#define SE_OFFSET SEARCH_ERROR_OFFSET

#define JUDY_GET_KEY GET_KEY

#ifndef SET_KEY
#error No method of setting keys has been declared
#endif

#define JUDY_SET_KEY SET_KEY

#ifndef JUDY_ERROR_CATCH
#define JUDY_ERROR_CATCH(x_str) \
	catch(JudyEx& e) \
	{ \
		if(e.IsFatal() )\
		{\
			MF_LogError(amx,AMX_ERR_NATIVE, "%s || Judy Error: %s", x_str, e.ErrorMessage());\
			return NULL;\
		};\
		return NULL;\
	}\
	catch(...) { MF_LogError(amx,AMX_ERR_NATIVE,"Unknown error occured, please post a bug report at www.amxmodx.org/forums"); return NULL; }
#endif

#ifndef JUDY_SEARCH_ERROR_CATCH
#define JUDY_SEARCH_ERROR_CATCH(x_str, success) \
	catch(JudyEx& e) \
	{ \
		if(e.IsFatal() )\
		{\
			MF_LogError(amx,AMX_ERR_NATIVE, "%s || Judy Error: %s", x_str, e.ErrorMessage());\
			return NULL;\
		};\
		return (success = 0);\
	}\
	catch(...) { MF_LogError(amx,AMX_ERR_NATIVE,"Unknown error occured, please post a bug report at www.amxmodx.org/forums"); return NULL; }
#endif

#ifndef JUDY_CREATE_INDEX
#define JUDY_CREATE_INDEX(master, slave, slave_type, index)\
\
	if( index == NULL )\
	{\
		try { index = master.FirstEmpty(); }\
		JUDY_ERROR_CATCH("Unable to create new unit (out of memory)");\
\
		slave = new slave_type;\
		master.Set(index,slave);\
	} \
	else if(master.IsEmpty(index) )\
	{\
		slave = new slave_type;\
		master.Set(index,slave);\
	}\
	else slave = master.Get(index, slave );

#endif

#ifndef JUDY_GET_INDEX
#define JUDY_GET_INDEX(master,slave, where) \
	try { slave = master.Get(where,slave); } \
	JUDY_ERROR_CATCH("Unable to access old unit (invalid index)");
#endif 

#ifndef JUDY_SET_INDEX
#define JUDY_SET_INDEX(master,slave, where) \
	try { master.Set(where,slave); } \
	JUDY_ERROR_CATCH("Unknown Error occured (No error possible) - Set Function");\
	return 1;
#endif 

#ifndef JUDY_SET_INDEX_P
#define JUDY_SET_INDEX_P(master,slave, where) \
	try { master->Set(where,slave); } \
	JUDY_ERROR_CATCH("Unknown Error occured (No error possible) - Set Function");\
	return 1;
#endif 

inline char* JUDY_BUILD_PATH(AMX *amx, cell param, int buffer = 0)
{
	char *file = MF_GetAmxString(amx, param, 0, NULL);
	return MF_BuildPathname("%s", file);
}

#include "GenericNatives.h"
#endif