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

#ifndef _INCLUDE_AMXDATA_H
#define _INCLUDE_AMXDATA_H

class DataMngr
{
public:
	class Datum
	{
	public:
		Datum();
		std::string symbol;
		CExpr e;
		bool db;
		int offset;
	};
public:
	~DataMngr();
	DataMngr() { cellsize = 4; lastOffset = 0; }
	DataMngr(int cell) { lastOffset = 0; cellsize = cell; }
	void Add(std::string &s, CExpr &expr, bool db = false);
	DataMngr::Datum *FindData(std::string &sym);
	int GetOffset(std::string &sym);
private:
	std::vector<DataMngr::Datum *> List;
	int lastOffset;
	int cellsize;
public:
	static const int nof = -1;
};

#endif //_INCLUDE_AMXDATA_H