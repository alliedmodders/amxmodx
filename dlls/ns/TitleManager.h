/* AMX Mod X 
 *   Natural Selection Module 
 * 
 * by the AMX Mod X Development Team 
 *
 * This file is part of AMX Mod X. 
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

#ifndef TITLEMANAGER_H
#define TITLEMANAGER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "CString.h"
#include "Hash.h"


class TitleManager
{
private:
	int						m_Loaded;

	// Use a string pointer for the data type because the location manager
	// stores a direct pointer to the c_str() of the title
	Hash<String, String*>	m_Hash;

public:

	TitleManager()
	{
		m_Loaded=0;
	};

	inline const char *Lookup(String &input)
	{
		String** ret = m_Hash.find(input);

		if (ret == NULL || *ret == NULL)
		{
			// was not found
			return NULL;
		}


		return (*ret)->c_str();
	};
	void LoadTitles(void);
};

extern TitleManager TitleMan;

#endif // TITLEMANAGER_H
