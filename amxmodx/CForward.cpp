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

#include <extdll.h>
#include <meta_api.h>
#include "amxmod.h"
#include "CForward.h"

void CForwardMngr::registerForward(  CPluginMngr::CPlugin* p, int func , int type  ){
	
	CForward** a = &head[ type ];
	while(*a) a = &(*a)->next;
	*a = new CForward( p , func );
	
}

void CForwardMngr::clearForwards( CForward** a ){
	while( *a )	{
		CForward* b = (*a)->next;
		delete *a;
		*a = b;
	}
}

void CForwardMngr::clear()
{
	for ( int a = 0; a < FORWARD_NUM; ++a )
		clearForwards( &head[ a ] );
}

void CForwardMngr::executeForwards( int type , int num , int player  ) {
	
	cell ret = 0;
	int err;
	CForward* a = head[ type ];
	
	while ( a )
	{
		if ( a->getPlugin()->isExecutable( a->getFunction() ) )
		{
			
			if ((err = amx_Exec(a->getPlugin()->getAMX(), &ret, a->getFunction() , num, player)) != AMX_ERR_NONE)
				print_srvconsole("[AMX] Run time error %d on line %ld (plugin \"%s\")\n",	err,a->getPlugin()->getAMX()->curline,a->getPlugin()->getName());
			
			if ( ret )
				break;
			
		}
		
		a = a->next;
	}
}