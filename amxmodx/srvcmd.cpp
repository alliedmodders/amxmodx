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
#include "amxmod.h"

void amx_command(){
	
	const char* cmd = CMD_ARGV(1);
	
	if (!strcmp(cmd,"plugins") || !strcmp(cmd,"list"))
	{

		print_srvconsole( "Currently loaded plugins:\n");
		print_srvconsole( "       %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s\n",
			"name","version","author","file","status");

		int plugins = 0;
		int	running = 0;


		CPluginMngr::iterator a = g_plugins.begin();
		
		while (a) 
		{
			++plugins;

			if ( (*a).isValid() && !(*a).isPaused() ) 
				++running;

			print_srvconsole( " [%3d] %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s\n",
				plugins,(*a).getTitle(),(*a).getVersion(),
				(*a).getAuthor(), (*a).getName(), (*a).getStatus() );

			++a;
		}

		print_srvconsole( "%d plugins, %d running\n",plugins,running );

	}
	else if (!strcmp(cmd,"pause") && CMD_ARGC() > 2) 
	{
		const char* sPlugin = CMD_ARGV(2);

		CPluginMngr::CPlugin *plugin = g_plugins.findPlugin(sPlugin);

		if ( plugin && plugin->isValid() ) 
		{
			plugin->pausePlugin();
			print_srvconsole("Paused plugin \"%s\"\n",plugin->getName() );
		}
		else print_srvconsole("Couldn't find plugin matching \"%s\"\n",sPlugin);

	}
	else if (!strcmp(cmd,"unpause") && CMD_ARGC() > 2) 
	{
		const char* sPlugin = CMD_ARGV(2);

		CPluginMngr::CPlugin *plugin = g_plugins.findPlugin(sPlugin);

		if ( plugin && plugin->isValid() ) 
		{
			plugin->unpausePlugin();
			print_srvconsole("Unpaused plugin \"%s\"\n",plugin->getName() );
		}
		else print_srvconsole("Couldn't find plugin matching \"%s\"\n",sPlugin);

	}
	else if (!strcmp(cmd,"cvars")) 
	{
		print_srvconsole( "Registered cvars:\n");
		print_srvconsole( "       %-24.23s %-24.23s %-16.15s\n",
			"name","value","plugin");

		int ammount = 0;

		for( CList<CCVar>::iterator a = g_cvars.begin(); a ; ++a )
		{
			print_srvconsole( " [%3d] %-24.23s %-24.23s %-16.15s\n",++ammount,
			(*a).getName() ,CVAR_GET_STRING( (*a).getName() ),(*a).getPluginName() );
		}

		print_srvconsole( "%d cvars\n",ammount);
	}
	else if ( !strcmp(cmd,"cmds") ) 
	{
		
		print_srvconsole( "Registered commands:\n");
		print_srvconsole( "       %-24.23s %-16.15s %-8.7s %-16.15s\n", 
			"name","access" ,"type" ,"plugin");
		
		int ammount = 0;
		
		char access[32];

		CmdMngr::iterator a = g_commands.begin( CMD_ConsoleCommand );

		while( a )
		{
			UTIL_GetFlags( access , (*a).getFlags() );
			print_srvconsole( " [%3d] %-24.23s %-16.15s %-8.7s %-16.15s\n",
				++ammount,(*a).getCmdLine() , access , (*a).getCmdType() , (*a).getPlugin()->getName());
			++a;
		}

		print_srvconsole( "%d commands\n",ammount);
	}
	else if (!strcmp(cmd,"version")) 
	{

		print_srvconsole( "%s %s\n", Plugin_info.name, Plugin_info.version);
		print_srvconsole(   "author: %s (%s)\n", Plugin_info.author, Plugin_info.url);
		print_srvconsole(   "compiled: %s\n", __DATE__ ", " __TIME__);
	
	}
	else if (!strcmp(cmd,"modules"))
	{
		print_srvconsole( "Currently loaded modules:\n");
		print_srvconsole( "      %-23.22s %-7.8s %-8.7s %-20.19s %-11.10s\n",
			"name","type","version", "author", "status");

		int running = 0;
		int modules = 0;

		CList<CModule>::iterator a = g_modules.begin();

		while ( a )
		{
			if ( (*a).getStatusValue() == MODULE_LOADED )
				++running;

			++modules;

			print_srvconsole( " [%2d] %-23.22s %-7.6s %-8.7s %-20.19s %-11.10s\n",modules,
				(*a).getName(), (*a).getType(), (*a).getVersion(), (*a).getAuthor() , (*a).getStatus() );
		
			++a;
		}

		print_srvconsole( "%d modules, %d correct\n",modules,running);
	}
	else 
	{


		print_srvconsole("Usage: amxx < command > [ argument ]\n");
		print_srvconsole("Commands:\n");
		print_srvconsole("   version                - display amxx version info\n");
		print_srvconsole("   plugins                - list plugins currently loaded\n");
		print_srvconsole("   modules                - list modules currently loaded\n");
		print_srvconsole("   cvars                  - list cvars registered by plugins\n");
		print_srvconsole("   cmds                   - list commands registered by plugins\n");
		print_srvconsole("   pause < plugin >       - pause a running plugin\n");
		print_srvconsole("   unpause < plugin >     - unpause a previously paused plugin\n");

	
	}
}


void plugin_srvcmd()
{

	cell ret = 0;
	int err;
	const char* cmd = CMD_ARGV(0);
	
#ifdef ENABLEEXEPTIONS
	try{
#endif
		
		CmdMngr::iterator a = g_commands.srvcmdbegin();
		
		while ( a )
		{
			if (  (*a).matchCommand( cmd )  && 
				(*a).getPlugin()->isExecutable( (*a).getFunction() ) )
			{
				
				if ((err = amx_Exec( (*a).getPlugin()->getAMX(), &ret , (*a).getFunction() 
					, 3 , g_srvindex , (*a).getFlags() , (*a).getId() )) != AMX_ERR_NONE)
					
					UTIL_Log("[AMXX] Run time error %d on line %ld (plugin \"%s\")",
					err,(*a).getPlugin()->getAMX()->curline,(*a).getPlugin()->getName());
				
				if ( ret ) break;
			}
			
			++a;
		}
		
#ifdef ENABLEEXEPTIONS
	}catch( ... )
	{
		UTIL_Log( "[AMXX] fatal error at forward function execution");
	}
#endif
	
}
