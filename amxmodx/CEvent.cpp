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
#include "CEvent.h"

// *****************************************************
// class EventsMngr
// *****************************************************
EventsMngr::EventsMngr() 
{ 
	memset( modMsgsFunCall, 0 , sizeof(modMsgsFunCall)  ); 
}
EventsMngr::~EventsMngr()
{ 
	clearEvents();
}

EventsMngr::ClEvent::ClEvent( CPluginMngr::CPlugin* amxplugin,  int function, int flags  )
{	
	plugin = amxplugin;
	func = function;
	stamp = 0.0;
	next = 0;
	done = false;
	alive=true;
	dead=true;
	if ( flags & 24 ){
		alive=(flags&16)?true:false; //e
		dead=(flags&8)?true:false; //d
	}
	world=(flags&1)?true:false; //a
	player=(flags&2)?true:false; //b
	once=(flags&4)?true:false; //c	
	memset(cond,0,sizeof(cond));	
}

void EventsMngr::ClEvent::registerFilter( char* filter )
{
	if ( filter == 0 ) return;

	char* value = filter;

	while ( isdigit(*value) )
		++value;

	if ( *value == 0 ) return;

	cond_t* b = new cond_t;

	if ( b == 0 ) return;

	b->type = *value;

	*value++ = 0;

	b->sValue.set(value);
	b->fValue = atof(value);
	b->iValue = atoi(value);

	int i = atoi(filter);
	if (i >= 0 && i < MAX_PARSE_VALUES) {
		b->next = cond[i];
		cond[i] = b;
	}
	else delete b;
}

EventsMngr::ClEvent* EventsMngr::registerEvent( CPluginMngr::CPlugin* p,  int f, int flags, int pos )
{
	ClEvent* a = new ClEvent( p , f , flags );
	if  ( a == 0 ) return 0;
	ClEvent** end = &modMsgsFunCall[pos];
	while( *end ) end = &(*end)->next;
	return *end = a;
}

void EventsMngr::parserInit(int msg_type, float* tim, CPlayer *pPlayer, int index) {
  parseNotDone = false;
  timer = tim;
  if ( (parseFun = modMsgsFunCall[msg_type]) == 0 ) return;

  for(EventsMngr::ClEvent*p=parseFun;p;p=p->next){
    if ( p->done )  continue;
    if ( !p->plugin->isExecutable(p->func) ){
      p->done = true;
      continue;
    }
    if ( pPlayer ) {
      if ( !p->player || ( pPlayer->IsAlive() ? !p->alive : !p->dead  ) ) {
        p->done = true;
        continue;
      }
    }
    else if ( !p->world ){
      p->done = true;
      continue;
    }
    if ( p->once && p->stamp == (float)(*timer) ){
      p->done = true;
      continue;
    }
    parseNotDone = true;
  }
  if ( parseNotDone ) {
    parseVault[parsePos = 0].type = MSG_INTEGER;
    parseVault[parsePos].iValue = index;
  }
}


const char* EventsMngr::getArgString(int a)
{
	if ( a < 0 || a > parsePos ) return "";
	static char var[32];
	switch(parseVault[a].type){
	case MSG_INTEGER: 
		sprintf( var, "%d", parseVault[a].iValue );
		return var;
	case MSG_STRING: 
		return parseVault[a].sValue;
	default:
		sprintf( var, "%g", parseVault[a].fValue );
		return var;
	}
}

int EventsMngr::getArgInteger(int a)
{
	if ( a < 0 || a > parsePos ) return 0;
	switch(parseVault[a].type){
	case MSG_INTEGER: return parseVault[a].iValue;
	case MSG_STRING: return atoi(parseVault[a].sValue);
	default: return (int)parseVault[a].fValue; 
	}
}

float EventsMngr::getArgFloat(int a)
{
	if ( a < 0 || a > parsePos ) return 0.0f;
	switch(parseVault[a].type){
	case MSG_INTEGER: return parseVault[a].iValue;
	case MSG_STRING: return atof(parseVault[a].sValue);
	default: return parseVault[a].fValue;
	}
}



void EventsMngr::executeEvents() {
	int err;
	
#ifdef ENABLEEXEPTIONS
	try
	{
#endif
		
		for ( ClEvent*p = parseFun ; p ; p = p->next ) 
		{
			
			if ( p->done ) 
			{
				p->done = false;
				continue;
			}
			
			p->stamp = *timer;
			
			if ((err = amx_Exec(p->plugin->getAMX(), NULL , p->func , 1,parseVault[0].iValue)) != AMX_ERR_NONE)
				print_srvconsole("[AMX] Run time error %d on line %ld (plugin \"%s\")\n",err,p->plugin->getAMX()->curline,p->plugin->getName());
			
		}
		
#ifdef ENABLEEXEPTIONS
	}
	catch( ... )
	{
		print_srvconsole( "[AMX] fatal error at event execution\n");
	}
#endif
	
}

void EventsMngr::parseValue(int iValue) {
  if ( !parseNotDone ) return;
  parseVault[++parsePos].type = MSG_INTEGER;
  parseVault[parsePos].iValue = iValue;
  bool skip;
  for (ClEvent*p=parseFun;p;p=p->next){
    if ( p->done || !p->cond[parsePos] ) continue;
    skip = false;
	ClEvent::cond_t* a = p->cond[parsePos];
	do {
      switch(a->type){
      case '=': if (a->iValue == iValue) skip=true; break;
      case '!': if (a->iValue != iValue) skip=true; break;
      case '&': if (iValue & a->iValue) skip=true; break;
      case '<': if (iValue < a->iValue) skip=true; break;
      case '>': if (iValue > a->iValue) skip=true; break;
      }
      if (skip) break;
    } while ( a = a->next );
    if (skip) continue;
    p->done = true;
  }
}

void EventsMngr::parseValue(float flValue) {
  if ( !parseNotDone ) return;
  parseVault[++parsePos].type = MSG_FLOAT;
  parseVault[parsePos].fValue = flValue;
  bool skip;
  for (ClEvent*p=parseFun;p;p=p->next){
    if ( p->done || !p->cond[parsePos] ) continue;
    skip = false;
	ClEvent::cond_t* a = p->cond[parsePos];
	do {
      switch(a->type){
      case '=': if (a->fValue == flValue) skip=true; break;
      case '!': if (a->fValue != flValue) skip=true; break;
      case '<': if (flValue < a->fValue) skip=true; break;
      case '>': if (flValue > a->fValue) skip=true; break;
      }
      if (skip) break;
	} while ( a = a->next );
    if (skip) continue;
    p->done = true;
  }
}

void EventsMngr::parseValue(const char *sz) {
  if ( !parseNotDone ) return;
  parseVault[++parsePos].type = MSG_STRING;
  parseVault[parsePos].sValue = sz;
  bool skip;
  for (ClEvent*p=parseFun;p;p=p->next){
    if ( p->done || !p->cond[parsePos] ) continue;
    skip = false;
	ClEvent::cond_t* a = p->cond[parsePos];
	do {
      switch(a->type){
      case '=': if (!strcmp(sz,a->sValue.str())) skip=true; break;
      case '!': if (strcmp(sz,a->sValue.str())) skip=true; break;
      case '&': if (strstr(sz,a->sValue.str())) skip=true; break;
      }
      if (skip) break;
    } while ( a = a->next );
    if (skip) continue;
    p->done = true;
  }
}

void EventsMngr::clearEvents()
{
	for(int i=0;i<MAX_AMX_REG_MSG;++i){
		ClEvent**b = &modMsgsFunCall[i];
		while (*b){
			ClEvent*aa = (*b)->next;
			delete *b;
			*b = aa;
		}
	}
}

EventsMngr::ClEvent::~ClEvent(){
	for(int a = 0; a < MAX_PARSE_VALUES; ++a){
		cond_t** b = &cond[a];
		while(*b){
			cond_t* nn = (*b)->next;
			delete *b;
			*b = nn;
		}
	}
}

EventsMngr::ClEvent* EventsMngr::getValidEvent(ClEvent* a )	{
	while(a){
		if ( a->done ) {
			a->done = false;
			a = a->next;
			continue;
		}
		a->stamp = *timer;
		return a;
	}
	return 0;
}

int EventsMngr::getEventId( const char* msg ){
	struct CS_Events {
		const char* name;
		CS_EventsIds id;
	} table[] = {
		{ "CS_DeathMsg" ,	CS_DeathMsg  },
//		{ "CS_RoundEnd" ,	CS_RoundEnd  },
//		{ "CS_RoundStart" , CS_RoundStart  },
//		{ "CS_Restart" ,	CS_Restart  },
		{ "" ,				CS_Null  }
	};
	int pos;
	if (  (pos = atoi( msg )) != 0 )
		return pos;
	for (pos = 0; table[ pos ].id != CS_Null; ++pos )
		if ( !strcmp( table[ pos ].name , msg ) )
			return table[ pos ].id;
		return pos = GET_USER_MSG_ID(PLID, msg , 0 );
}
