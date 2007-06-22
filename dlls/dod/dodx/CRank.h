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

#ifndef CRANK_H
#define CRANK_H

#define RANK_VERSION 5

#include "amxxmodule.h"

// *****************************************************
// class Stats
// *****************************************************

struct Stats {
	int hits;
	int shots;
	int damage;
	int hs;
	int tks;
	int points; // DoD score
	int kills;
	int deaths;
	int bodyHits[8];
	Stats();
	void commit(Stats* a);
};

// *****************************************************
// class RankSystem
// *****************************************************

class RankSystem
{
public:
	class RankStats;
	friend class RankStats;
	class iterator;

	class RankStats : public Stats {
		friend class RankSystem;
		friend class iterator;
		RankSystem*	parent;
		RankStats*	next;
		RankStats*	prev;
		char*		unique;
		short int	uniquelen;
		char*		name;
		short int	namelen;
		int			score;
		int			id;
		RankStats( const char* uu, const char* nn,  RankSystem* pp );
		~RankStats();
		void setUnique( const char* nn  );
		inline void goDown() {++id;}
		inline void goUp() {--id;}
		inline void addStats(Stats* a) { commit( a ); }
	public:
		void setName( const char* nn  );
		inline const char* getName() const { return name ? name : ""; }
		inline const char* getUnique() const { return unique ? unique : ""; }
		inline int getPosition() const { return id; }
		inline void updatePosition( Stats* points ) {
			parent->updatePos( this , points );
		}
	};

private:
	RankStats* head;
	RankStats* tail;
	int rankNum;

	struct scoreCalc{
		AMX amx;
		void* code;
		int func;
		cell amxAddr1;
		cell amxAddr2;
		cell *physAddr1;
		cell *physAddr2;
	} calc;

	void put_before( RankStats* a, RankStats* ptr );
	void put_after( RankStats* a, RankStats* ptr );
	void unlink( RankStats* ptr );
	void updatePos( RankStats* r ,  Stats* s );
	
public:

	RankSystem();
	~RankSystem();

	void saveRank( const char* filename );
	void loadRank( const char* filename );
	RankStats* findEntryInRank(const char* unique, const char* name , bool isip = false);
	bool loadCalc(const char* filename, char* error);
	inline int getRankNum( ) const { return rankNum; }
	void clear();
	void unloadCalc();

	class iterator {
		RankStats* ptr;
	public:
		iterator(RankStats* a): ptr(a){}
		inline iterator& operator--() { ptr = ptr->prev; return *this;}
		inline iterator& operator++() {	ptr = ptr->next; return *this; }
		inline RankStats& operator*() {	return *ptr;}
		operator bool () { return (ptr != 0); }
	};

	inline iterator front() {  return iterator(head);  }
	inline iterator begin() {  return iterator(tail);  }
};


#endif

