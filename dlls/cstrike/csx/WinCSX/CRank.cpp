// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "stdafx.h"

#include "CRank.h"
#include <stdio.h>

// *****************************************************
// class Stats
// *****************************************************
Stats::Stats(){
	hits = shots = damage = hs = tks = kills = deaths = bDefusions = bDefused = bPlants = bExplosions = 0;
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

	bDefusions += a->bDefusions;
	bDefused += a->bDefused;
	bPlants += a->bPlants;
	bExplosions += a->bExplosions;

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
	namelen = (short)strlen(nn) + 1;
	name = new char[namelen];
	if ( name )
		strcpy( name , nn );
	else
		namelen = 0;
}

void RankSystem::RankStats::setUnique( const char* nn  )	{
	delete[] unique;
	uniquelen = (short)strlen(nn) + 1;
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


/*
bool RankSystem::loadCalc(const char* filename, char* error)
{
	if ((MF_LoadAmxScript(&calc.amx,&calc.code,filename,error,0)!=AMX_ERR_NONE)||
		(MF_AmxAllot(&calc.amx, 8 , &calc.amxAddr1, &calc.physAddr1)!=AMX_ERR_NONE)||
		(MF_AmxAllot(&calc.amx, 8 , &calc.amxAddr2, &calc.physAddr2)!=AMX_ERR_NONE)||
		(MF_AmxFindPublic(&calc.amx,"get_score",&calc.func)!=AMX_ERR_NONE)){
		//LOG_CONSOLE( PLID, "Couldn't load plugin (file \"%s\")",filename);
		MF_UnloadAmxScript(&calc.amx, &calc.code);
		return false;
	}
	return true;
}

void RankSystem::unloadCalc()
{
	MF_UnloadAmxScript(&calc.amx , &calc.code);
}
*/
RankSystem::RankStats* RankSystem::findEntryInRank(const char* unique, const char* name )
{
	RankStats* a = head;
	
	while ( a )
	{
		if (  strcmp( a->getUnique(), unique ) == 0 )
			return a;

		a = a->prev;
	}

	a = new RankStats( unique ,name,this );
	if ( a == 0 ) return 0;
	put_after( a  , 0 );
	return a;
}

RankSystem::RankStats* RankSystem::findEntryInRankByUnique(const char* unique)
{
	RankStats* a = head;
	
	while ( a )
	{
		if (  strcmp( a->getUnique(), unique ) == 0 )
			return a;

		a = a->prev;
	}

	return NULL; // none found
}
RankSystem::RankStats* RankSystem::findEntryInRankByPos(int position)
{
	RankStats* a = head;
	
	while ( a )
	{
		if (a->getPosition() == position)
			return a;

		a = a->prev;
	}

	return NULL;
}

int RankSystem::updatePos(  RankStats* rr ,  Stats* s )
{
	RankStats* rrFirst = rr;
	if (s != NULL)
		rr->addStats( s );
	if ( calc.code ) {
		calc.physAddr1[0] = rr->kills;
		calc.physAddr1[1] = rr->deaths;
		calc.physAddr1[2] = rr->hs;
		calc.physAddr1[3] = rr->tks;
		calc.physAddr1[4] = rr->shots;
		calc.physAddr1[5] = rr->hits;
		calc.physAddr1[6] = rr->damage;

		calc.physAddr1[7] = rr->bDefusions;
		calc.physAddr1[8] = rr->bDefused;
		calc.physAddr1[9] = rr->bPlants;
		calc.physAddr1[10] = rr->bExplosions;

		for(int i = 1; i < 8; ++i)
			calc.physAddr2[i] = rr->bodyHits[i];
		cell result = 0;
		//int err;
		//if ((err = MF_AmxExec(&calc.amx,&result, calc.func ,2,calc.amxAddr1,calc.amxAddr2 )) != AMX_ERR_NONE)
			//LOG_CONSOLE( PLID, "Run time error %d on line %ld (plugin \"%s\")",	err,calc.amx.curline,LOCALINFO("csstats_score"));
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
	return rrFirst->getPosition();
}

bool RankSystem::loadRank( const char* filename )
{
	FILE *bfp = fopen( filename , "rb" );
	
	if ( !bfp ) {
		return false;
	}
	
	short int i = 0;
	fread(&i, 1 , sizeof(short int) , bfp);
	
	if (i == RANK_VERSION)
	{
		Stats d;
		char unique[64], name[64];
		fread(&i , 1, sizeof(short int), bfp);

		while( i )
		{
			fread(name , i,sizeof(char) , bfp);
			fread(&i , 1, sizeof(short int), bfp);
			fread(unique , i,sizeof(char) , bfp);
			fread(&d.tks, 1,sizeof(int), bfp);
			fread(&d.damage, 1,sizeof(int), bfp);
			fread(&d.deaths, 1,sizeof(int), bfp);
			fread(&d.kills, 1,sizeof(int), bfp);
			fread(&d.shots, 1,sizeof(int), bfp);
			fread(&d.hits, 1,sizeof(int), bfp);
			fread(&d.hs, 1,sizeof(int), bfp);

			fread(&d.bDefusions, 1,sizeof(int), bfp);
			fread(&d.bDefused, 1,sizeof(int), bfp);
			fread(&d.bPlants, 1,sizeof(int), bfp);
			fread(&d.bExplosions, 1,sizeof(int), bfp);

			fread(d.bodyHits, 1,sizeof(d.bodyHits), bfp);
			fread(&i , 1, sizeof(short int), bfp);

			RankSystem::RankStats* a = findEntryInRank( unique , name );

			if ( a ) a->updatePosition( &d );
		}
	}
	fclose(bfp);

	return true;
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

			fwrite( &(*a).bDefusions, 1, sizeof(int), bfp);
			fwrite( &(*a).bDefused, 1, sizeof(int), bfp);
			fwrite( &(*a).bPlants, 1, sizeof(int), bfp);
			fwrite( &(*a).bExplosions, 1, sizeof(int), bfp);

			fwrite( (*a).bodyHits, 1, sizeof((*a).bodyHits), bfp);
		}
		
		--a;
	}

	i = 0;
	fwrite( &i , 1, sizeof(short int), bfp); // null terminator
	
	fclose(bfp);
}
