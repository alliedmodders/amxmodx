/*
 * Copyright (c) 2002-2003 Aleksander Naszko
 *
 *    This file is part of AMX Mod.
 *
 *    AMX Mod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    AMX Mod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with AMX Mod; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

// *****************************************************
// class CModule
// *****************************************************

#ifndef CMODULE_H
#define CMODULE_H

enum MODULE_STATUS {
  MODULE_NONE,
  MODULE_QUERY,
  MODULE_BADLOAD,
  MODULE_LOADED,
  MODULE_NOINFO,
  MODULE_NOQUERY,
  MODULE_NOATTACH,
  MODULE_OLD
};

class CModule 
{
	String filename;
	bool metamod;
	module_info_s* info;
	DLHANDLE module;
	MODULE_STATUS status;

public:

	CModule(const char* fname);
	~CModule();

	// Interface
	bool attachModule();
	bool queryModule();
	bool detachModule();
	const char* getStatus() const;
	inline const char* getType() const { return metamod ? "amx&mm" : "amx"; }
	inline const char* getAuthor() const { return info ? info->author : "unknown"; }
	inline const char* getVersion() const { return info ? info->version : "unknown"; }
	inline const char* getName() const { return info ? info->name : "unknown"; }	
	inline module_info_s* getInfo() const { return info; }
	inline int getStatusValue() { return status; }
	inline bool operator==( void* fname ) { return !strcmp( filename.str() , (char*)fname );  }
	inline bool isReloadable() { return ( (status==MODULE_LOADED) && (info->type==RELOAD_MODULE)); }
	CList<AMX_NATIVE_INFO*> natives;
};



#endif



