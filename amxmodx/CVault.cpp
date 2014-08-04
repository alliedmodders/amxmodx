// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "amxmodx.h"
#include "CVault.h"
#include "CFile.h"

// *****************************************************
// class Vault
// *****************************************************

bool Vault::exists(const char* k)
{
	if (*k == 0) return false;

	return *find(k) != 0;
}

void Vault::put(const char* k, const char* v)
{
	if (*k == 0) return;

	if (*v == 0)
	{
		remove(k);
		return;
	}

	Obj** a = find(k);

	if (*a)
	{
		(*a)->value.assign(v);
		(*a)->number = atoi(v);
	}
	else
		*a = new Obj(k, v);
}

Vault::Obj::Obj(const char* k, const char* v): key(k), value(v), next(0)
{
	number = atoi(v);
}

Vault::Obj** Vault::find(const char* n)
{
	Obj** a = &head;

	while (*a)
	{
		if (strcmp((*a)->key.c_str(), n) == 0)
			return a;

		a = &(*a)->next;
	}

	return a;
}


int Vault::get_number(const char* n)
{
	if (*n == 0) return 0;

	Obj* b = *find(n);

	if (b == 0) return 0;

	return b->number;
}

const char* Vault::get(const char* n)
{
	if (*n == 0) return "";

	Obj* b = *find(n);

	if (b == 0) return "";

	return b->value.c_str();
}

void Vault::clear()
{
	while (head)
	{
		Obj* a = head->next;
		delete head;
		head = a;
	}
}

void Vault::remove(const char* n)
{
	Obj** b = find(n);

	if (*b == 0) return;

	Obj* a = (*b)->next;
	delete *b;
	*b = a;
}

void Vault::setSource(const char* n)
{
	path.assign(n);
}

bool Vault::loadVault()
{
	if (path.empty()) return false;

	clear();

	File a(path.c_str(), "r");

	if (!a) return false;

	const int sz = 512;
	char value[sz + 1];
	char key[sz + 1];

	while (a >> key && a.skipWs() && a.getline(value, sz))
	{
		if (isalpha(*key))
			put(key, value);
	}

	return true;

}

bool Vault::saveVault()
{
	if (path.empty()) return false;

	File a(path.c_str(), "w");

	if (!a) return false;

	a << "; Don't modify!" << '\n';

	for (Obj* b = head; b; b = b->next)
		a << b->key << '\t' << b->value << '\n';

	return true;
}
