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

#ifndef CLIST_H
#define CLIST_H

// *****************************************************
// class CList
// *****************************************************

template <typename T, typename F = char* >
class CList {
public:
	class iterator;
	class CListEle {
		friend class CList<T,F>;
		friend class iterator;
		T* obj;
		CListEle* next;
		CListEle( T* a , CListEle* nn ) : obj(a) , next(nn) {}
	public:
		T& operator* () { return *obj; }
	};
private:
	CListEle *head;
public:
	CList<T,F>() { head = 0; }
	~CList<T,F>() {	clear(); }
	void clear() {
		iterator a = begin();
		while( a ) a.remove();
	}
	void put( T* a ) {	
		head = new CListEle( a , head ); 
	}
	class iterator {
		CListEle** a;
	public:
		iterator(CListEle** i=0) : a(i){}
		T& operator*() const {	return *(*a)->obj;}
		inline operator bool () const { return (a && *a); }
		inline iterator& operator++() {
			a = &(*a)->next;
			return *this;
		}
		inline iterator operator++(int) {
			iterator tmp(a);
			a = &(*a)->next;
			return tmp;
		}
		iterator& remove(){
			CListEle* aa = (*a)->next;
			delete (*a)->obj;
			delete *a;
			*a = aa;
			return *this;	
		}
		iterator& put( T* aa ){
			*a = new CListEle( aa , *a );
			return *this;
		}
	};
	inline iterator begin() { return iterator(&head); }
	iterator find( F a ){
		iterator cc = begin();
		while(cc){
			if ( *cc == a )
				break;
			++cc;
		}
		return cc;
	}
};

#endif

