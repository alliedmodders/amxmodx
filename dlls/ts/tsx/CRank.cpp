// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TSX Module
//

#include "amxxmodule.h"
#include "CRank.h"
#include "tsx.h"

// *****************************************************
// class Stats
// *****************************************************
Stats::Stats(){
	hits = shots = damage = hs = tks = kills = deaths = 0;
	memset( bodyHits , 0 ,sizeof( bodyHits ) );
}
void Stats::commit(Stats* a){
	hits += a->hits;
	shots += a->shots;
	damage += a->damage;
	hs += a->hs;
	tks += a->tks;
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
		MF_PrintSrvConsole("Couldn't load plugin (file \"%s\")",filename);
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
		for(int i = 1; i < 8; ++i)
			calc.physAddr2[i] = rr->bodyHits[i];
		cell result = 0;
		int err;
		MF_AmxPush(&calc.amx, calc.amxAddr2);
		MF_AmxPush(&calc.amx, calc.amxAddr1);
		if ((err = MF_AmxExec(&calc.amx, &result, calc.func)) != AMX_ERR_NONE)
			MF_Log("Run time error %d on line (plugin \"%s\")",	err, LOCALINFO("csstats_score"));
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
			TRYREAD(unique, i, sizeof(char), bfp);
			TRYREAD(&d.tks, 1, sizeof(int), bfp);
			TRYREAD(&d.damage, 1, sizeof(int), bfp);
			TRYREAD(&d.deaths, 1, sizeof(int), bfp);
			TRYREAD(&d.kills, 1, sizeof(int), bfp);
			TRYREAD(&d.shots, 1, sizeof(int), bfp);
			TRYREAD(&d.hits, 1, sizeof(int), bfp);
			TRYREAD(&d.hs, 1, sizeof(int), bfp);
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
			fwrite( &(*a).kills, 1, sizeof(int), bfp);
			fwrite( &(*a).deaths, 1, sizeof(int), bfp);
			fwrite( &(*a).hs, 1, sizeof(int), bfp);
			fwrite( &(*a).tks, 1, sizeof(int), bfp);
			fwrite( &(*a).damage, 1, sizeof(int), bfp);
			fwrite( &(*a).hits, 1, sizeof(int), bfp);
			fwrite( &(*a).shots, 1, sizeof(int), bfp);
			fwrite( (*a).bodyHits, 1, sizeof((*a).bodyHits), bfp);
		}
		
		--a;
	}

	i = 0;
	fwrite( &i , 1, sizeof(short int), bfp); // null terminator
	
	fclose(bfp);
}
