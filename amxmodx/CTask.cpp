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
#include "CTask.h"


CTaskMngr::CTask::CTask( CPluginMngr::CPlugin* p, int f, int flags, 
						int i, float base,  float exec,	int parlen , 
						const cell* par, int r){
	plugin = p;
	func = f;
	id = i;
	next = 0;
	prev = 0;
	param_len = 0;
	param = 0;
	base_time = base;
	exec_time = exec;
	repeat = (flags & 1) ? r : 0;
	loop = (flags & 2) ?  true : false;
	afterstart = (flags & 4) ?  true : false;
	beforeend = (flags & 8) ?  true : false;

	if ( parlen )
	{
		param = new cell[ parlen + 1 ];

		if ( param ){
			param_len = parlen + 1;
			memcpy( param , par , sizeof( cell ) * parlen );
			param[ parlen ] = 0; 
		}
	}
}

CTaskMngr::CTask* CTaskMngr::getFirstValidTask(CTask* h){
	CTask* a = h;
	while( a ) {
		if ( a->isRemoved() )  {
			CTask* b = a->next;
			unlink( a );
			delete a;
			a = b;
			continue;
		}
		else if (  a->afterstart  ){
			if ( *m_timer - *m_timeleft + 1 < a->base_time ) {
				a = a->next;
				continue;
			}
		}
		else if ( a->beforeend ){
			if ( *m_timelimit == 0 ){
				a = a->next;
				continue;
			}
			if (  (*m_timeleft + *m_timelimit * 60.0) - *m_timer - 1 >
				a->base_time ){
				a = a->next;
				continue;
			}
		}
		else if ( a->exec_time > *m_timer ) {
			a = a->next;
			continue;
		}
		return a;
	}
	return 0;
}

CTaskMngr::CTask* CTaskMngr::getNextTask(CTask* a) {
	if ( a->isRemoved() )
		return a->next;
	if ( a->loop || a->isToReply() ){
		a->exec_time = *m_timer + a->base_time;
		return a->next;
	}
	a->setToRemove();
	return a->next;
}


CTaskMngr::CTaskMngr() {
	head = 0;
	tail = 0;
	m_timer = 0;
	m_timelimit = 0;
	m_timeleft = 0;
}

CTaskMngr::~CTaskMngr() {
	clear();
}

void CTaskMngr::clear() {
	while ( head )  {
		tail = head->next;
		delete head;
		head = tail;
	}
}

void CTaskMngr::registerTimers( float* timer , float* timelimit, float* timeleft ) {
	m_timer = timer;
	m_timelimit = timelimit;
	m_timeleft = timeleft;
}

void CTaskMngr::registerTask( CPluginMngr::CPlugin* plugin, int func,
							 int flags, int i, float base,  float exec, 
							 int parlen , const cell* par, int repeat ){

	CTask* a = new CTask(plugin,func,flags,i,base,exec,parlen,par,repeat );

	if ( a == 0 ) return;

	if ( tail ) 
	{
		tail->next = a;
		a->prev = tail;
		tail = a;
	}
	else {
		head = a;
		tail = a;
	}
}

CTaskMngr::CTask* CTaskMngr::findTask( int id , AMX* amx )
{
	for (CTask* a = head; a ; a = a->next) 
	{
		if ( !a->isRemoved() && (a->getTaskId() == id) && (!amx || 
			(a->getPlugin()->getAMX() == amx)) )
			return a;
	}
	
	return 0;
}

void CTaskMngr::unlink(CTask* a){
	if ( a->prev ) a->prev->next = a->next;
	else head = a->next;
	if ( a->next ) a->next->prev  = a->prev;
	else tail = a->prev;
}

int CTaskMngr::removeTasks( int id , AMX* amx )
{
	CTask* a;
	int i = 0;
	
	while ( (a = findTask(id, amx )) != 0 )	{
		a->setToRemove();
		++i;
	}
	
	return i;
}

