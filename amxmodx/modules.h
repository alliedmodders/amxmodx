/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#ifndef __MODULES_H__
#define __MODULES_H__

#include "amx.h"

#undef DLLEXPORT
#ifndef __linux__
  #define DLLEXPORT   __declspec(dllexport)
#else
  #define DLLEXPORT
  #define WINAPI
#endif

#undef C_DLLEXPORT
#define C_DLLEXPORT extern "C" DLLEXPORT

#define AMX_INTERFACE_VERSION 6

#define RELOAD_MODULE 0
#define STATIC_MODULE 1

struct module_info_s {
  const char* name;
  const char* author;
  const char* version;
  int ivers;
  int type;
  long int serial;
};

// Small scripting language
struct pfnamx_engine_g {
  uint16_t*   (*pfnamx_Align16)(uint16_t *);                    // value
  uint32_t*   (*pfnamx_Align32)(uint32_t *);                    // value
  int     (*pfnamx_Allot)(AMX*, int, cell*, cell**);              // amx, length, amx_addr, phys_addr
  int     (*pfnamx_Callback)(AMX*, cell , cell*, cell*);            // amx, index,result,params
  int     (*pfnamx_Clone)(AMX*, AMX*, void*);                 // amxClone, amxSrc, data
  int     (*pfnamx_Debug)(AMX*); // default debug procedure, does nothing   // amx
  int     (*pfnamx_Exec)(AMX*, cell*, int , int , ...);           // amx, return val, index, num_params, ...
  int     (*pfnamx_Execv)(AMX*, cell*, int , int, cell[]);          // amx, return val, index, num_params, param[]
  int     (*pfnamx_FindPublic)(AMX*, char*, int*);              // amx, func name, index
  int     (*pfnamx_FindPubVar)(AMX*, char*, cell*);             // anx, var name, amx_addr
  int     (*pfnamx_FindTagId)(AMX*, cell , char*);              // amx. tag_id, tagname
  int     (*pfnamx_Flags)(AMX*,uint16_t *);                 // amx, flags
  int     (*pfnamx_GetAddr)(AMX*,cell ,cell**);               // amx, amx_addr, phys_addr
  int     (*pfnamx_GetPublic)(AMX*, int , char*);               // amx, index, funcname
  int     (*pfnamx_GetPubVar)(AMX*, int , char*, cell*);            // amx, index, varname, amx_addr
  int     (*pfnamx_GetString)(char*dest,cell*);               // dest, source
  int     (*pfnamx_GetTag)(AMX*, int , char*, cell*);             // amx, index, tagname, tag_id
  int     (*pfnamx_GetUserData)(AMX*, long , void **);            // amx, tag, ptr
  int     (*pfnamx_Init)(AMX*, void *);                   // amx, program
  int     (*pfnamx_InitJIT)(AMX*, void *, void *);              // amx, reloc_table, native_code
  int     (*pfnamx_MemInfo)(AMX*, long*, long*, long*);           // amx, codesize, datasize, stackheap
  int     (*pfnamx_NameLength)(AMX*, int*);                 // amx, length
  AMX_NATIVE_INFO * (*pfnamx_NativeInfo)(char*,AMX_NATIVE );            // name, func
  int     (*pfnamx_NumPublics)(AMX*, int*);                 // amx, number
  int     (*pfnamx_NumPubVars)(AMX*, int*);                 // amx, number
  int     (*pfnamx_NumTags)(AMX*, int*);                    // amx, number
  int     (*pfnamx_RaiseError)(AMX*, int );                 // amx, error
  int     (*pfnamx_Register)(AMX*, AMX_NATIVE_INFO*, int );         // amx, nativelist, number
  int     (*pfnamx_Release)(AMX*, cell );                   // amx, amx_addr
  int     (*pfnamx_SetCallback)(AMX*, AMX_CALLBACK );             // amx, callback
  int     (*pfnamx_SetDebugHook)(AMX*, AMX_DEBUG );             // amx, debug
  int     (*pfnamx_SetString)(cell*, char*, int );              // dest, source, pack
  int     (*pfnamx_SetUserData)(AMX*, long , void*);              // amx, tag, prt
  int     (*pfnamx_StrLen)(cell*, int*);                    // amx, cstring, length
};
extern pfnamx_engine_g* g_engAmxFunc;

#define AMX_ALIGN16       (*g_engAmxFunc->pfnamx_Align16)
#define AMX_ALIGN32       (*g_engAmxFunc->pfnamx_Align32)
#define AMX_ALLOT         (*g_engAmxFunc->pfnamx_Allot)
#define AMX_CALLBACK      (*g_engAmxFunc->pfnamx_Callback)
#define AMX_CLONE         (*g_engAmxFunc->pfnamx_Clone)
#define AMX_DEBUG         (*g_engAmxFunc->pfnamx_Debug)
#define AMX_EXEC          (*g_engAmxFunc->pfnamx_Exec)
#define AMX_EXECV         (*g_engAmxFunc->pfnamx_Execv)
#define AMX_FINDPUBLIC    (*g_engAmxFunc->pfnamx_FindPublic)
#define AMX_FINDPUBVAR    (*g_engAmxFunc->pfnamx_FindPubVar)
#define AMX_FINDTAGID     (*g_engAmxFunc->pfnamx_FindTagId)
#define AMX_FLAGS         (*g_engAmxFunc->pfnamx_Flags)
#define AMX_GETADDR       (*g_engAmxFunc->pfnamx_GetAddr)
#define AMX_GETPUBLIC     (*g_engAmxFunc->pfnamx_GetPublic)
#define AMX_GETPUBVAR     (*g_engAmxFunc->pfnamx_GetPubVar)
#define AMX_GETSTRING     (*g_engAmxFunc->pfnamx_GetString)
#define AMX_GETTAG        (*g_engAmxFunc->pfnamx_GetTag)
#define AMX_GETUSERDATA   (*g_engAmxFunc->pfnamx_GetUserData)
#define AMX_INIT          (*g_engAmxFunc->pfnamx_Init)
#define AMX_INITJIT       (*g_engAmxFunc->pfnamx_InitJIT)
#define AMX_MEMINFO       (*g_engAmxFunc->pfnamx_MemInfo)
#define AMX_NAMELENGTH    (*g_engAmxFunc->pfnamx_NameLength)
#define AMX_NATIVEINFO    (*g_engAmxFunc->pfnamx_NativeInfo)
#define AMX_NUMPUBLICS    (*g_engAmxFunc->pfnamx_NumPublics)
#define AMX_NUMPUBVARS    (*g_engAmxFunc->pfnamx_NumPubVars)
#define AMX_NUMTAGS       (*g_engAmxFunc->pfnamx_NumTags)
#define AMX_RAISEERROR    (*g_engAmxFunc->pfnamx_RaiseError)
#define AMX_REGISTER      (*g_engAmxFunc->pfnamx_Register)
#define AMX_RELEASE       (*g_engAmxFunc->pfnamx_Release)
#define AMX_SETCALLBACK   (*g_engAmxFunc->pfnamx_SetCallback)
#define AMX_SETDEBUGHOOK  (*g_engAmxFunc->pfnamx_SetDebugHook)
#define AMX_SETSTRING     (*g_engAmxFunc->pfnamx_SetString)
#define AMX_SETUSERDATA   (*g_engAmxFunc->pfnamx_SetUserData)
#define AMX_STRLEN        (*g_engAmxFunc->pfnamx_StrLen)

// Modules API
struct pfnmodule_engine_g {
  int         (*pfnadd_amxnatives)(module_info_s*,AMX_NATIVE_INFO*);      // list
  char*       (*pfnbuild_pathname)(char*, ...);                           // format, ....
  void        (*pfncopy_amxmemory)(cell*,cell*,int);                      // dest, src, len
  char*       (*pfnformat_amxstring)(AMX*, cell*, int ,int& );            // amx, format, start pos, len
  cell*       (*pfnget_amxaddr)(AMX*,cell );                              // amx, cell
  AMX*        (*pfnget_amxscript)(int, void**,const char**);              // id, code, name
  const char* (*pfnget_amxscriptname)(AMX* amx);                          // amx
  char*       (*pfnget_amxstring)(AMX*,cell,int, int&);                   // amx, src, buffer (0-3), len
  void        (*pfnget_modname)(char*);                                   // modname
  int         (*pfnload_amxscript)(AMX*, void**, const char*, char[64]);  // amx, code, path, error info
  void        (*pfnprint_console)(char*, ...);                            // format, ....
  void      (*pfnreport_error)(int code, char*, ... );
  int         (*pfnset_amxnatives)(AMX*,char[64]);                        // amx, error info
  int         (*pfnset_amxstring)(AMX*,cell ,const char*,int);            // amx, dest, string, maxlen
  int         (*pfnamxstring_length)(cell*);                              // src
  int         (*pfnunload_amxscript)(AMX* amx,void**);                    // amx, code
  void*     (*pfnalloc_amxmemory)(void**,int size);
  void      (*pfnfree_amxmemory)(void**);
};
extern pfnmodule_engine_g* g_engModuleFunc;

#define ADD_AMXNATIVES          (*g_engModuleFunc->pfnadd_amxnatives)
#define AMXSTRING_LENGTH        (*g_engModuleFunc->pfnamxstring_length)
#define BUILD_PATHNAME          (*g_engModuleFunc->pfnbuild_pathname)
#define COPY_AMXMEMORY          (*g_engModuleFunc->pfncopy_amxmemory)
#define FORMAT_AMXSTRING        (*g_engModuleFunc->pfnformat_amxstring)
#define GET_AMXADDR             (*g_engModuleFunc->pfnget_amxaddr)
#define GET_AMXSCRIPT           (*g_engModuleFunc->pfnget_amxscript)
#define GET_AMXSCRIPTNAME       (*g_engModuleFunc->pfnget_amxscriptname)
#define GET_AMXSTRING           (*g_engModuleFunc->pfnget_amxstring)
#define GET_MODNAME             (*g_engModuleFunc->pfnget_modname)
#define LOAD_AMXSCRIPT          (*g_engModuleFunc->pfnload_amxscript)
#define PRINT_CONSOLE           (*g_engModuleFunc->pfnprint_console)
#define REPORT_ERROR      (*g_engModuleFunc->pfnreport_error)
#define SET_AMXNATIVES          (*g_engModuleFunc->pfnset_amxnatives)
#define SET_AMXSTRING           (*g_engModuleFunc->pfnset_amxstring)
#define UNLOAD_AMXSCRIPT        (*g_engModuleFunc->pfnunload_amxscript)
#define ALLOC_AMXMEMORY         (*g_engModuleFunc->pfnalloc_amxmemory)
#define FREE_AMXMEMORY        (*g_engModuleFunc->pfnfree_amxmemory)


#endif // __MODULES_H__
