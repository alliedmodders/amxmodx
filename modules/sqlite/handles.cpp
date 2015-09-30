// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLite Module
//

#include <string.h>
#include <sh_stack.h>
#include <amtl/am-vector.h>
#include "sqlite_header.h"

struct QHandle
{
	void *_ptr;
	FREEHANDLE _func;
	HandleType type;
	bool isfree;
};

ke::Vector<QHandle *> g_Handles;
CStack<unsigned int> g_FreeHandles;

unsigned int MakeHandle(void *ptr, HandleType type, FREEHANDLE f)
{
	unsigned int num;
	QHandle *h;

	if (g_FreeHandles.size())
	{
		num = g_FreeHandles.front();
		g_FreeHandles.pop();
		h = g_Handles[num];
	} else {
		h = new QHandle;
		g_Handles.append(h);
		num = static_cast<unsigned int>(g_Handles.length()) - 1;
	}

	h->_ptr = ptr;
	h->type = type;
	h->_func = f;
	h->isfree = false;

	return num + 1;
}

void *GetHandle(unsigned int num, HandleType type)
{
	if (num == 0)
		return NULL;

	num--;
	if (num >= g_Handles.length())
		return NULL;

	QHandle *h = g_Handles[num];
	if (h->isfree || (h->type != type))
		return NULL;

	return h->_ptr;
}

bool FreeHandle(unsigned int num)
{
	if (num == 0)
		return false;

	unsigned int _num = num;

	num--;
	if (num >= g_Handles.length())
		return false;

	QHandle *h = g_Handles[num];
	if (h->isfree)
		return false;

	h->_func(h->_ptr, _num);
	h->_ptr = NULL;
	h->_func = NULL;
	h->isfree = true;
	
	g_FreeHandles.push(num);

	return true;
}

void FreeAllHandles(HandleType type)
{
	QHandle *q;
	for (size_t i = 0; i < g_Handles.length(); i++)
	{
		q = g_Handles[i];
		if (q && !q->isfree && q->type == type)
		{
			FreeHandle((unsigned int)i);
		}
	}
}

void FreeHandleTable()
{
	QHandle *q;
	for (size_t i = 0; i < g_Handles.length(); i++)
	{
		q = g_Handles[i];
		if (q && !q->isfree)
			FreeHandle((unsigned int)i);
	}
	g_Handles.clear();
	while (!g_FreeHandles.empty())
		g_FreeHandles.pop();
}
