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

#ifndef VAULT_CUSTOM_H
#define VAULT_CUSTOM_H

#include "CString.h"
#include "CList.h"

// *****************************************************
// class Vault
// *****************************************************

class Vault
{
  struct Obj
  {
	CString key;
	CString value;
	int number;
    Obj *next;
	Obj( const char* k,  const char* v);
  } *head;

  CString path;

  Obj** find( const char* n );

public:

  Vault() {head=0;}
  ~Vault() { clear();}

	// Interface

  bool exists( const char* k );
  void put(const char* k, const char* v);
  void remove( const char* k );
  const char* get( const char* n );
  int get_number( const char* n );
  void setSource( const char* n );
  bool loadVault( );
  bool saveVault( );
  void clear();


  class iterator {
	Obj * a;
  public:
	iterator(Obj*aa) : a(aa) {}
	iterator& operator++() { if ( a ) a = a->next; return *this; }
	bool operator==(const iterator& b) const { return a == b.a; }
	bool operator!=(const iterator& b) const { return !operator==(b); }
	CString& key() const { return a->key; }
	CString& value() const { return a->value; }
  };

  inline iterator begin() const { return iterator(head); }
  inline iterator end() const { return iterator(0); }
};

#endif



