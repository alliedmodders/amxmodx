// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef VAULT_CUSTOM_H
#define VAULT_CUSTOM_H

#include "CList.h"

// *****************************************************
// class Vault
// *****************************************************

class Vault
{
	struct Obj
	{
		ke::AString key;
		ke::AString value;

		int number;
		Obj *next;
		Obj(const char* k, const char* v);
	} *head;

	ke::AString path;

	Obj** find(const char* n);

public:

	Vault() { head = 0; }
	~Vault() { clear(); }

	// Interface

	bool exists(const char* k);
	
	void put(const char* k, const char* v);
	void remove(const char* k);
	
	const char* get(const char* n);
	int get_number(const char* n);
	void setSource(const char* n);
	
	bool loadVault();
	bool saveVault();
	
	void clear();

	class iterator
	{
		Obj * a;
	public:
		iterator(Obj* aa) : a(aa) {}
		iterator& operator++() { if (a) a = a->next; return *this; }
		bool operator==(const iterator& b) const { return a == b.a; }
		bool operator!=(const iterator& b) const { return !operator==(b); }
		ke::AString& key() const { return a->key; }
		ke::AString& value() const { return a->value; }
	};

	inline iterator begin() const { return iterator(head); }
	inline iterator end() const { return iterator(0); }
};

#endif //VAULT_CUSTOM_H
