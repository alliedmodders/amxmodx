/*
 * DoDX 
 * Copyright (c) 2004 Lukasz Wlasinski
 *
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software Foundation,
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

#include "amxxmodule.h"
#include "CRank.h"
#include "dodx.h"

// *****************************************************
// class Stats
// *****************************************************
Stats::Stats(){
	hits = shots = damage = hs = tks = points = kills = deaths = 0;
	memset( bodyHits , 0 ,sizeof( bodyHits ) );
}
void Stats::commit(Stats* a){
	hits += a->hits;
	shots += a->shots;
	damage += a->damage;
	hs += a->hs;
	tks += a->tks;
	points += a->points;
	kills += a->kills;
	deaths += a->deaths;
	for(int i = 1; i < 8; ++i)
		bodyHits[i] += a->bodyHits[i];
}

// *****************************************************
// class RankSystem
// *****************************************************
RankSystem::RankStats::RankStats( const char* uu, const char* nn, RankSystem* pp ) {
	name = 0;
	namelen = 0;
	unique = 0;
	uniquelen = 0;
	score = 0;
	parent = pp;
	id = ++parent->rankNum;
	next = prev = 0;
	setName( nn );
	setUnique( uu );
}
RankSystem::RankStats::~RankStats() {
	delete[] name;
	delete[] unique;
	--parent->rankNum;
}
void RankSystem::RankStats::setName( const char* nn  )	{
	delete[] name;
	namelen = strlen(nn) + 1;
	name = new char[namelen];
	if ( name )
		strcpy( name , nn );
	else
		namelen = 0;
}
void RankSystem::RankStats::setUnique( const char* nn  )	{
	delete[] unique;
	uniquelen = strlen(nn) + 1;
	unique = new char[uniquelen];
	if ( unique )
		strcpy( unique , nn );	
	else
		uniquelen = 0;
}

RankSystem::RankSystem() { 
	head = 0; 
	tail = 0; 
	rankNum = 0;
	calc.code = 0;
}

RankSystem::~RankSystem() {
	clear();
}

void RankSystem::put_before( RankStats* a, RankStats* ptr ){
	a->next = ptr;
	if ( ptr ){
		a->prev = ptr->prev;
		ptr->prev = a;
	}
	else{
		a->prev = head;
		head = a;
	}
	if ( a->prev )	a->prev->next = a;
	else tail = a;
}
void  RankSystem::put_after( RankStats* a, RankStats* ptr ) {
	a->prev = ptr;
	if ( ptr ){
		a->next = ptr->next;
		ptr->next = a;
	}
	else{
		a->next = tail;
		tail = a;
	}
	if ( a->next )	a->next->prev = a;
	else head = a;
}

void RankSystem::unlink( RankStats* ptr ){
	if (ptr->prev) ptr->prev->next = ptr->next;
	else tail = ptr->next;
	if (ptr->next) ptr->next->prev = ptr->prev;
	else head = ptr->prev;
}

void RankSystem::clear(){
	while( tail ){
		head = tail->next;
		delete tail;
		tail = head;
	}
}

bool RankSystem::loadCalc(const char* filename, char* error)
{
	if ((MF_LoadAmxScript(&calc.amx,&calc.code,filename,error,0)!=AMX_ERR_NONE)||
		(MF_AmxAllot(&calc.amx, 8 , &calc.amxAddr1, &calc.physAddr1)!=AMX_ERR_NONE)||
		(MF_AmxAllot(&calc.amx, 8 , &calc.amxAddr2, &calc.physAddr2)!=AMX_ERR_NONE)||
		(MF_AmxFindPublic(&calc.amx,"get_score",&calc.func)!=AMX_ERR_NONE)){
		LOG_CONSOLE( PLID, "Couldn't load plugin (file \"%s\")",filename);
		MF_UnloadAmxScript(&calc.amx, &calc.code);
		return false;
	}
	return true;
}

void RankSystem::unloadCalc()
{
	MF_UnloadAmxScript(&calc.amx , &calc.code);
}

RankSystem::RankStats* RankSystem::findEntryInRank(const char* unique, const char* name, bool isip)
{
	RankStats* a = head;
	
	if (isip) // IP lookups need to strip the port from already saved instances.
	{         // Otherwise the stats file would be essentially reset.
	
		// The IP passed does not contain the port any more for unique
		size_t iplen = strlen(unique);
		
		
		while ( a )
		{
			const char* targetUnique = a->getUnique();
			if ( strncmp( targetUnique, unique, iplen) == 0 )
			{
				// It mostly matches, make sure this isn't a false match
				// eg: checking 4.2.2.2 would match 4.2.2.24 here.
				
				// Get the next character stored in targetUnique
				char c = targetUnique[iplen];
				
				// If c is either a colon or end of line, then this
				// is a valid match.
				if (c == ':' ||
					c == '\0')
				{
					// Yes, this is a match.
					return a;
				}
				// Any other case was a false match.
				a = a->prev;
			}
		}
	}
	else // No special case
	{
		while ( a )
		{
			if (  strcmp( a->getUnique() ,unique ) == 0 )
				return a;

			a = a->prev;
		}
	}
	a = new RankStats( unique ,name,this );
	if ( a == 0 ) return 0;
	put_after( a  , 0 );
	return a;
}

void RankSystem::updatePos(  RankStats* rr ,  Stats* s )
{
	rr->addStats( s );

	if ( calc.code ) {
		calc.physAddr1[0] = rr->kills;
		calc.physAddr1[1] = rr->deaths;
		calc.physAddr1[2] = rr->hs;
		calc.physAddr1[3] = rr->tks;
		calc.physAddr1[4] = rr->shots;
		calc.physAddr1[5] = rr->hits;
		calc.physAddr1[6] = rr->damage;
		calc.physAddr1[7] = rr->points;
		for(int i = 1; i < 8; ++i)
			calc.physAddr2[i] = rr->bodyHits[i];
		cell result = 0;
		int err;
		MF_AmxPush(&calc.amx, calc.amxAddr2);
		MF_AmxPush(&calc.amx, calc.amxAddr1);
		if ((err = MF_AmxExec(&calc.amx,&result, calc.func)) != AMX_ERR_NONE)
			MF_LogError(&calc.amx, err, "Fatal error calculating stats");
		rr->score = result;
	}
	else rr->score = rr->kills - rr->deaths;

	RankStats* aa = rr->next;
	while ( aa && (aa->score <= rr->score) ) { // try to nominate
		rr->goUp();
		aa->goDown();
		aa = aa->next;		// go to next rank
	}
	if ( aa != rr->next )
	{
		unlink( rr );
		put_before( rr, aa );
	}
	else
	{
		aa = rr->prev;
		while ( aa && (aa->score > rr->score) ) { // go down
			rr->goDown();
			aa->goUp();
			aa = aa->prev;	// go to prev rank
		}
		if ( aa != rr->prev ){
			unlink( rr );
			put_after( rr, aa );
		}
	}

}

/** 
 * Who put these backwards...
 */
#define TRYREAD(t_var, t_num, t_size, t_file) \
	if (fread(t_var, t_size, t_num, t_file) != static_cast<size_t>(t_num)) { \
		break; \
	}

void RankSystem::loadRank(const char* filename)
{
	FILE *bfp = fopen(filename , "rb");
	
	if (!bfp)
	{
		MF_Log("Could not load stats file: %s", filename);
		return;
	}
	
	short int i = 0;
	if (fread(&i, sizeof(short int), 1, bfp) != 1)
	{
		fclose(bfp);
		return;
	}

	
	if (i == RANK_VERSION)
	{
		Stats d;
		char unique[64], name[64];
		if (fread(&i, sizeof(short int), 1, bfp) != 1)
		{
			fclose(bfp);
			return;
		}

		while(i && !feof(bfp))
		{
			TRYREAD(name, i, sizeof(char), bfp);
			TRYREAD(&i, 1, sizeof(short int), bfp);
			TRYREAD(unique, i, sizeof(char) , bfp);
			TRYREAD(&d.tks, 1, sizeof(int), bfp);
			TRYREAD(&d.damage, 1, sizeof(int), bfp);
			TRYREAD(&d.deaths, 1, sizeof(int), bfp);
			TRYREAD(&d.kills, 1, sizeof(int), bfp);
			TRYREAD(&d.shots, 1, sizeof(int), bfp);
			TRYREAD(&d.hits, 1, sizeof(int), bfp);
			TRYREAD(&d.hs, 1, sizeof(int), bfp);
			TRYREAD(&d.points, 1, sizeof(int), bfp);
			TRYREAD(d.bodyHits, 1, sizeof(d.bodyHits), bfp);
			TRYREAD(&i, 1, sizeof(short int), bfp);

			RankSystem::RankStats* a = findEntryInRank( unique , name );
			if ( a ) a->updatePosition( &d );
		}
	}
	
	fclose(bfp);
}

void RankSystem::saveRank( const char* filename )
{
	FILE *bfp = fopen(filename, "wb");
	
	if ( !bfp ) return;
	
	short int i = RANK_VERSION;
	
	fwrite(&i, 1, sizeof(short int) , bfp);
	
	RankSystem::iterator a = front();
	
	while ( a )
	{
		if ( (*a).score != (1<<31) ) // score must be different than mincell
		{
			fwrite( &(*a).namelen , 1, sizeof(short int), bfp);
			fwrite( (*a).name , (*a).namelen , sizeof(char) , bfp);
			fwrite( &(*a).uniquelen , 1, sizeof(short int), bfp);
			fwrite( (*a).unique ,  (*a).uniquelen , sizeof(char) , bfp);
			fwrite( &(*a).tks, 1, sizeof(int), bfp);
			fwrite( &(*a).damage, 1, sizeof(int), bfp);
			fwrite( &(*a).deaths, 1, sizeof(int), bfp);
			fwrite( &(*a).kills, 1, sizeof(int), bfp);
			fwrite( &(*a).shots, 1, sizeof(int), bfp);
			fwrite( &(*a).hits, 1, sizeof(int), bfp);
			fwrite( &(*a).hs, 1, sizeof(int), bfp);
			fwrite( &(*a).points, 1,sizeof(int), bfp);
			fwrite( (*a).bodyHits, 1, sizeof((*a).bodyHits), bfp);
		}
		
		--a;
	}

	i = 0;
	fwrite( &i , 1, sizeof(short int), bfp); // null terminator
	
	fclose(bfp);
}
