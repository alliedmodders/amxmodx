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

#include <extdll.h>
#include <meta_api.h>
#include "amxmodx.h"
#include "osdep.h"			// sleep, etc
#include "CFile.h"

CList<CModule> g_modules;
CList<CScript,AMX*> g_loadedscripts;

#ifdef  __cplusplus
extern "C" {
#endif
extern const char* no_function; // stupid work around
#ifdef  __cplusplus
}
#endif

void report_error( int code, char* fmt, ... )
{
	va_list argptr;
	char string[256];
	*string = 0;
	va_start (argptr, fmt);
	vsnprintf (string, 255, fmt,argptr);
	string[255] = 0;
	va_end (argptr);
	if ( *string ) {
		//File fp( "error_amx.log","a" );
		//fp << string;
		print_srvconsole( string );
		UTIL_Log("[AMXX] Make sure that modules are compatible with AMX Mod X %s" , AMX_VERSION );
		UTIL_Log("[AMXX] Please fix the problem then start the server again" );
	}
	sleep( 5 );
	exit( code );
}

void print_srvconsole( char *fmt, ... )
{
  va_list argptr;
  char string[256];
  va_start (argptr, fmt);
  vsnprintf (string, 255, fmt,argptr);
  string[255] = 0;
  va_end (argptr);
  SERVER_PRINT(string);
}

void* alloc_amxmemory(void** p, int size)
{
	*p = new unsigned char[ size ];
	return *p;
}

void free_amxmemory(void **ptr)
{
	delete[] *ptr;
	*ptr = 0;
}

int load_amxscript(AMX *amx, void **program, const char *filename, char error[64]){
  
  AMX_HEADER hdr; 
  int err; 
  FILE *fp;

  memset(amx, 0, sizeof(*amx));
  *program = 0;
  *error = 0;

  if ( (fp = fopen( filename, "rb" )) == NULL)
  {
    strcpy(error,"Plugin file open error");
    return (amx->error = AMX_ERR_NOTFOUND);
  }

  fread(&hdr, sizeof(hdr), 1, fp);

  amx_Align16(&hdr.magic);

  if (hdr.magic!=AMX_MAGIC) 
  {
    strcpy(error,"Invalid plugin");
    return (amx->error = AMX_ERR_FORMAT);
  }

  amx_Align32((uint32_t *)&hdr.stp);
  amx_Align32((uint32_t *)&hdr.size);

  if ( (*program  = new unsigned char[ (int)hdr.stp ]) == 0 )
  //if ( (*program = malloc( (int)hdr.stp )) == 0 )
  {
    strcpy(error,"Failed to allocate memory");
    return (amx->error = AMX_ERR_MEMORY);
  }

  rewind(fp);
  fread(*program, 1, (size_t)hdr.size, fp);
  fclose(fp);

  if ((err = amx_Init( amx, *program )) != AMX_ERR_NONE)
  {
    sprintf(error,"Load error %d (invalid file format or version)", err);
    return (amx->error = AMX_ERR_INIT);
  }


#ifdef JIT
  void *np = new unsigned char[ amx->code_size ];
  void *rt = new unsigned char[ amx->reloc_size ];
  if ( !np || (!rt && amx->reloc_size > 0) )
  {
    delete[] np;
    delete[] rt;
    strcpy(error,"Failed to initialize plugin");
    return (amx->error = AMX_ERR_INIT);
  }

  if (amx_InitJIT(amx, rt, np) == AMX_ERR_NONE) 
  {
    //amx->base = (unsigned char FAR *)realloc( np, amx->code_size );
	amx->base = new unsigned char FAR[ amx->code_size ];
	if ( amx->base )
		memcpy( amx->base , np , amx->code_size );
	delete[] np;
    delete[] rt;
    delete[] *program;
    (*program) = amx->base;
	if ( *program == 0 ){
		strcpy(error,"Failed to allocate memory");
		return (amx->error = AMX_ERR_MEMORY);
	}
  }
  else 
  {
    delete[] np;
    delete[] rt;
    strcpy(error,"Failed to initialize plugin");
    return (amx->error = AMX_ERR_INIT_JIT);
  }

#endif

  CScript* aa  =  new CScript(amx,*program,filename);

  if ( aa == 0 )
  {
	strcpy(error,"Failed to allocate memory");
	return (amx->error = AMX_ERR_MEMORY);
  }

  g_loadedscripts.put( aa );
  return set_amxnatives(amx,error);
}

int set_amxnatives(AMX* amx,char error[64])
{
	for ( CList<CModule>::iterator  a  = g_modules.begin(); a ; ++a )
	{
		for( CList<AMX_NATIVE_INFO*>::iterator cc = 
			(*a).natives.begin(); cc; ++cc )
			amx_Register(amx, *cc , -1);
	}
		
	amx_Register(amx, string_Natives, -1);
	amx_Register(amx, float_Natives, -1);
	amx_Register(amx, file_Natives, -1);
	amx_Register(amx, amxmod_Natives, -1);
	amx_Register(amx, power_Natives, -1);
	amx_Register(amx, time_Natives, -1);
	amx_Register(amx, vault_Natives, -1);
	
	if ( amx_Register(amx, core_Natives, -1) != AMX_ERR_NONE )
	{
		sprintf(error,"Function not found (name \"%s\")",no_function);
		return (amx->error = AMX_ERR_NATIVE);
	}
	
	return AMX_ERR_NONE;
}


int unload_amxscript(AMX* amx, void** program)
{
  CList<CScript,AMX*>::iterator a = g_loadedscripts.find( amx  );
  if ( a ) a.remove();
  delete[] *program;
  //free( *program );
  *program = 0;
  return AMX_ERR_NONE;
}


AMX* get_amxscript(int id , void** code, const char** filename)
{
	CList<CScript,AMX*>::iterator a = g_loadedscripts.begin();
	while ( a && id-- )
		++a;
	if ( a ){
		*filename = (*a).getName();
		*code = (*a).getCode();
		return (*a).getAMX();
	}
	return 0;
}

const char* get_amxscriptname(AMX* amx)
{
	CList<CScript,AMX*>::iterator a = g_loadedscripts.find( amx  );
	return a ? (*a).getName() : "";
}

void get_modname(char* buffer )
{
	strcpy( buffer , g_mod_name.str() );
}

char* build_pathname(char *fmt, ... )
{
	static char string[256];
	
	int b;

	int a = b = snprintf(string , 255 ,

#ifndef __linux__
		"%s\\",
#else
		"%s/",
#endif
		g_mod_name.str());
	
	va_list argptr;
	va_start (argptr, fmt);
	a += vsnprintf (&string[a], 255 - a , fmt,argptr);
	string[ a ] = 0;
	va_end (argptr);
	
	char* path = &string[b];
	
	while (*path) 
	{
#ifndef __linux__
		if (*path == '/') *path = '\\';
#else
		if (*path == '\\') *path = '/';
#endif
		++path;
	}
	
	return string;
}


// build pathname based on addons dir
char* build_pathname_addons(char *fmt, ... )
{
	static char string[256];
	
	va_list argptr;
	va_start (argptr, fmt);
	vsnprintf (string, 255, fmt, argptr);
	va_end (argptr);
	
	char* path = string;
	
	while (*path) 
	{
#ifndef __linux__
		if (*path == '/') *path = '\\';
#else
		if (*path == '\\') *path = '/';
#endif
		++path;
	}
	
	return string;
}

int add_amxnatives(module_info_s* info,AMX_NATIVE_INFO*natives)
{
	CList<CModule>::iterator  a  = g_modules.begin();
		
	while ( a )
	{
		if (  (*a).getInfo() == info )
		{
			AMX_NATIVE_INFO** aa = new AMX_NATIVE_INFO*(natives);
			if ( aa == 0 ) return AMX_ERR_NATIVE;
			(*a).natives.put( aa  );
			return AMX_ERR_NONE;
		}
			
		++a;
	}

  return AMX_ERR_NATIVE;
}


bool validFile(const char* file)
{
  const char* a = 0;
  while(*file)
    if (*file++=='.')
      a = file;
#ifndef __linux__
  return (a && !strcmp(a,"dll"));
#else
  return (a && !strcmp(a,"so"));
#endif
}

int loadModules(const char* filename)
{
  File fp( build_pathname("%s",filename), "r"  );

  if ( !fp )
  {
    UTIL_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
    return 0;
  }

  char line[256], moduleName[256];
  int loaded = 0;
  
  while ( fp.getline( line ,  255  ) )
  {
	  *moduleName = 0;
	  sscanf(line,"%s",moduleName);
	  if (!isalnum(*moduleName) || !validFile(moduleName) )  
		  continue;
	  
	  char* pathname = build_pathname("addons/amxx/modules/%s", line);

	  CList<CModule>::iterator a = g_modules.find(  pathname  );

	  if ( a ) continue; // already loaded

	  CModule* cc = new CModule( pathname  );

	  if ( cc == 0 ) return loaded;

	  cc->queryModule();

	  switch(  cc->getStatusValue()  )  {
	  case MODULE_BADLOAD:
        report_error( 1 , "[AMXX] Module is not a valid library (file \"%s\")",pathname );
		break;
	  case MODULE_NOINFO:
        report_error( 1 ,"[AMXX] Couldn't find info. about module (file \"%s\")",pathname );
		break;
	  case MODULE_NOQUERY:
		report_error( 1 , "[AMXX] Couldn't find \"AMX_Query\" (file \"%s\")",  pathname );
		break;
	  case MODULE_NOATTACH:
		report_error( 1 , "[AMXX] Couldn't find \"AMX_Attach\" (file \"%s\")",  pathname );
		break;
	  case MODULE_OLD:
        report_error( 1 , "[AMXX] Module has a different interface version (file \"%s\")",pathname );
		break;
	  default:
		++loaded; 
	  }


	  g_modules.put( cc );
  }

  return loaded;
}

void dettachModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();
	
	while ( a )
	{
		(*a).detachModule();
		a.remove();
	}
}

void dettachReloadModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();
	
	while ( a )
	{
		if ( (*a).isReloadable() )
		{
			(*a).detachModule();
			a.remove();
			continue;
		}

		++a;
	}
	
}

void attachModules()
{
	CList<CModule>::iterator  a  = g_modules.begin();
	
	while ( a )
	{
		(*a).attachModule();

		++a;
	}
}

const char* strip_name( const char* a )
{
  const char* ret = a;
  while(*a){
     if ( *a== '/' || *a=='\\' ){
      ret = ++a;
      continue;
    }
     ++a;
  }
  return ret;
}

void dettachMetaModModules( const char* filename )
{
  File fp( build_pathname("%s",filename), "r"  );

  if ( !fp )
  {
    UTIL_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
    return;
  }

  char line[256], moduleName[256], cmdline[256];
  DLHANDLE module;
  
  while ( fp.getline( line ,  255  ) )
  {
	  *moduleName = 0;
	  sscanf(line,"%s",moduleName);
	  
	  if (!isalnum(*moduleName) || !validFile(moduleName) )  
		  continue;
	  
	  char* pathname = build_pathname_addons("addons/amxx/modules/%s", line);
	  char* mmpathname = build_pathname_addons("addons/amxx/modules/%s", line);

	  module = DLLOAD( pathname ); // link dll

	  if ( module )
	  {
		int a = (int)DLPROC(module,"Meta_Attach");

		if ( a )
		{
			snprintf(cmdline,255, "meta unload %s\n", strip_name(mmpathname) );
			cmdline[255] = 0;
			SERVER_COMMAND( cmdline );
		}

		DLFREE(module);
	  }
  }
}

void attachMetaModModules( const char* filename )
{
  File fp( build_pathname("%s",filename), "r"  );

  if ( !fp )
  {
    UTIL_Log( "[AMXX] Modules list not found (file \"%s\")",filename);
    return;
  }

  char line[256], moduleName[256], cmdline[256];
  DLHANDLE module;

  int loaded = 0;
  
  while ( fp.getline( line ,  255  ) )
  {
	  *moduleName = 0;
	  sscanf(line,"%s",moduleName);
	  
	  if (!isalnum(*moduleName) || !validFile(moduleName) )  
		  continue;
	  
	  char* pathname = build_pathname("addons/amxx/modules/%s", line);
	  char* mmpathname = build_pathname_addons("addons/amxx/modules/%s", line);
	  module = DLLOAD( pathname ); // link dll

	  if ( module )
	  {
		int a = (int)DLPROC(module,"Meta_Attach");

		if ( a )
		{
			snprintf(cmdline,255, "meta load %s\n", mmpathname );
			cmdline[255] = 0;
			SERVER_COMMAND( cmdline );
			++loaded;
		}

		DLFREE(module);
	  }
  }

  if ( loaded ) 
  {
	SERVER_COMMAND( "restart\n" );
	/* must be or modules can cause crash
	since they were not initialized with all routines (spawn, server active
	players think, etc.) and metamod calls other routines
    like nothing has never happened. */
  }
}

