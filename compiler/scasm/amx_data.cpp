/* AMX Assembler
 * Copyright (C)2004 David "BAILOPAN" Anderson
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#include "amxasm.h"

DataMngr::~DataMngr()
{
	Clear();
}

void DataMngr::Clear()
{
	std::vector<DataMngr::Datum *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

DataMngr::Datum::Datum()
{
	db = false;
	offset = -1;
	fill = 0;
}

void DataMngr::Add(std::string &s, CExpr &expr, bool db, int fill)
{
	DataMngr::Datum *D = new DataMngr::Datum();

	D->symbol.assign(s);
	D->e = expr;
	D->fill = fill;
	D->db = db;

	int size = 0;

	if (db)
	{
		size = ((D->e.GetType() == Val_Number) ?
					cellsize : D->e.Size() * cellsize);
	} else {
		size = (D->e.GetNumber() * cellsize);
	}

	if (List.size() == 0)
	{
		D->offset = 0;
	} else {
		DataMngr::Datum *p = List[List.size()-1];
		if (p->db)
		{
			D->offset = p->offset + 
				((p->e.GetType() == Val_Number) ? 
				cellsize : p->e.Size() * cellsize);
		} else {
			D->offset = p->offset + (p->e.GetNumber() * cellsize);
		}
	}

	cursize += size;

	List.push_back(D);
}

DataMngr::Datum *DataMngr::FindData(std::string &sym)
{
	std::vector<DataMngr::Datum *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ((*i)->symbol.compare(sym) == 0)
		{
			return (*i);
		}
	}

	return NULL;
}

int DataMngr::GetOffset(std::string &sym)
{
	DataMngr::Datum *D = NULL;

	D = FindData(sym);
	
	if (D == NULL)
		return DataMngr::nof;

	return D->offset;
}

int DataMngr::GetSize()
{
	return cursize;
}

void DataMngr::GetData(std::vector<DataMngr::Datum *> &dList)
{
	std::vector<DataMngr::Datum *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		dList.push_back( (*i) );
	}
}

