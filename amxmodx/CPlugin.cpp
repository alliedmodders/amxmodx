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

#include "amxmodx.h"
#include "CPlugin.h"
#include "CForward.h"
#include "CFile.h"

CPluginMngr::CPlugin* CPluginMngr::loadPlugin(const char* path, const char* name, char* error) {	
	CPlugin** a = &head;
	while( *a ) a = &(*a)->next;
	*a = new CPlugin( pCounter++ ,path,name,error);
	return *error ? 0 : *a;
}

void CPluginMngr::unloadPlugin( CPlugin** a ) {
	CPlugin* next = (*a)->next;
	delete *a;
	*a = next;
	--pCounter;
}

int  CPluginMngr::loadPluginsFromFile( const char* filename )
{
	File fp( build_pathname("%s",filename) , "r" );

	if ( !fp ) 
	{
		AMXXLOG_Log( "[AMXX] Plugins list not found (file \"%s\")",filename);
		return 1;
	}
	
	// Find now folder
	char pluginName[256], line[256], error[256];
	const char *pluginsDir = get_localinfo("amxx_pluginsdir", "addons/amxx/plugins");
	
	
	while ( fp.getline(line , 255 ) ) 
	{
		*pluginName = 0;
		sscanf(line,"%s",pluginName);
		if (!isalnum(*pluginName))  continue;
				
		CPlugin* plugin = loadPlugin( pluginsDir , pluginName  , error  );
		
		if (!plugin)
			AMXXLOG_Log("[AMXX] %s (plugin \"%s\")", error, pluginName );
	}

	return pCounter;
}

void CPluginMngr::clear() {
	CPlugin**a = &head;	
	while ( *a )
		unloadPlugin(a);
}

CPluginMngr::CPlugin* CPluginMngr::findPluginFast(AMX *amx) 
{ 
	return (CPlugin*)(amx->userdata[3]); 
	/*CPlugin*a = head;
	while ( a && &a->amx != amx )
		a=a->next;
	return a;*/
}

CPluginMngr::CPlugin* CPluginMngr::findPlugin(AMX *amx) {
	CPlugin*a = head;
	while ( a && &a->amx != amx )
		a=a->next;
	return a;
}
	
CPluginMngr::CPlugin* CPluginMngr::findPlugin(int index){
	CPlugin*a = head;
	while ( a && index--)
		a=a->next;
	return a;
}
	
CPluginMngr::CPlugin* CPluginMngr::findPlugin(const char* name) {
	if (!name) return 0;
	int len = strlen(name);
	if (!len) return 0;
	CPlugin*a = head;
	while( a && strncmp(a->name.c_str(), name,len) )
		a=a->next;
	return a;
}

const char* CPluginMngr::CPlugin::getStatus() const {
	switch(status){
	case ps_running: return "running";
	case ps_paused: return "paused";
	case ps_bad_load: return "bad load";
	case ps_stopped: return "stopped";
	case ps_locked: return "locked";
	}
	return "error";
}

CPluginMngr::CPlugin::CPlugin(int i, const char* p,const char* n, char* e) : name(n), title(n) {
	const char* unk = "unknown";
	title.assign(unk);
	author.assign(unk);
	version.assign(unk);
	char* path = build_pathname("%s/%s",p,n);
	code = 0;
	int err = load_amxscript(&amx,&code,path,e );
	if ( err == AMX_ERR_NONE ) status = ps_running;
	else status = ps_bad_load;
	amx.userdata[3] = this;
	paused_fun = 0;
	next = 0;
	id = i;
}
CPluginMngr::CPlugin::~CPlugin( ){
	unload_amxscript( &amx, &code );
}

void CPluginMngr::CPlugin::pauseFunction( int id ) { 
	if (isValid()){
		paused_fun |= (1<<id);
		g_commands.clearBufforedInfo();
	}
}

void CPluginMngr::CPlugin::unpauseFunction( int id ) {
	if (isValid()) {
		paused_fun &= ~(1<<id); 
		g_commands.clearBufforedInfo();
	}
}

void CPluginMngr::CPlugin::setStatus( int a   ) { 
	status = a; 
	g_commands.clearBufforedInfo(); // ugly way
}

// Pause a plugin
void CPluginMngr::CPlugin::pausePlugin()
{
	if (isValid())
	{
		// call plugin_pause if provided
		int func;
		cell retval;
		if (amx_FindPublic(&amx, "plugin_pause", &func) == AMX_ERR_NONE)
		{
			if (isExecutable(func))
			{

				amx_Exec(&amx, &retval, func, 0);
			}
		}
	
		setStatus(ps_paused);
	}
}

// Unpause a plugin
void CPluginMngr::CPlugin::unpausePlugin()
{
	if (isValid())
	{
		// set status first so the function will be marked executable

		setStatus(ps_running);
		// call plugin_unpause if provided
		int func;
		cell retval;
		if (amx_FindPublic(&amx, "plugin_unpause", &func) == AMX_ERR_NONE)
		{
			if (isExecutable(func))
			{
				amx_Exec(&amx, &retval, func, 0);
			}
		}
	}
}