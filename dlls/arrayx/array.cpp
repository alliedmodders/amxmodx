#include "osdefs.h"
#include <string.h>
#include <extdll.h>
#include "amxxmodule.h"

#ifdef __WIN32__
#define JU_WIN
#endif

#define JUDYERROR_NOTEST 1
#include <Judy.h>

#include "element.h"

#include "CKeytable.h"
#include "CArray.h"
#include "CHashtable.h"

void OnAmxxAttach()
{
	MF_AddNatives(array_exports);
	MF_AddNatives(keytable_exports);
	MF_AddNatives(hashtable_exports);
}

void OnAmxxDetach()
{
	Delete_MasterArray();
	Delete_MasterKeytable();
	Delete_MasterHashtable();
}
