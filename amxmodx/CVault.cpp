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

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "amxmodx.h"
#include "CVault.h"
#include "CFile.h"

// *****************************************************
// class Vault
// *****************************************************
bool Vault::exists( const char* k )
{
	if ( *k == 0 ) return false;

	return *find( k ) != 0;
}

void Vault::put( const char* k, const char* v )
{
	if ( *k == 0 ) return;

	if ( *v == 0 )
	{
		remove( k );
		return;
	}

	Obj** a = find( k );

	if ( *a )
	{
		(*a)->value.set(v);
		(*a)->number = atoi( v );
	}
	else
		*a = new Obj( k , v );

}

Vault::Obj::Obj( const char* k,  const char* v): key(k) , value(v) , next(0) {
	number = atoi(v);
}

Vault::Obj** Vault::find( const char* n )
{
	Obj** a = &head;

	while( *a )
	{
		if ( strcmp((*a)->key.str(), n) == 0 )
			return a;

		a = &(*a)->next;
	}

	return a;
}


int Vault::get_number( const char* n )
{
	if ( *n == 0 ) return 0;

	Obj* b = *find( n );

	if ( b == 0 ) return 0;

	return b->number;
}

const char* Vault::get( const char* n )
{
	if ( *n == 0 ) return "";

	Obj* b = *find( n );

	if ( b == 0 ) return "";

	return b->value.str();
}

void Vault::clear()
{
	while ( head )
	{
		Obj* a = head->next;
		delete head;
		head =  a;
	}
}

void Vault::remove( const char* n )
{
	Obj** b = find( n );

	if ( *b == 0 ) return;

	Obj* a = (*b)->next;
	delete *b;
	*b = a;
}

void Vault::setSource( const char* n )
{
	path.set(n);
}


bool Vault::loadVault(  )
{
	if ( path.empty() ) return false;

	clear();

	File a( path.str() , "r" );

	if ( !a ) return false;

	const int sz = 512;
	char value[sz+1];
	char key[sz+1];

	while ( a >> key && a.skipWs() && a.getline( value , sz ) )
	{
		if ( isalpha ( *key ) )
			put( key, value );
	}

	return true;

}

bool Vault::saveVault( )
{
	if ( path.empty() ) return false;

	File a( path.str() , "w" );

	if ( !a ) return false;

	a << "; Don't modify!" << '\n';

	for (Obj* b = head; b ;b = b->next)
		a << b->key << '\t' << b->value << '\n';

	return true;
}
