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

/* This file is a replacement for the engine call of ALLOC_STRING
 * The main difference is that a string will not be allocated twice
 * as to try to avoid wasting the HL zone space.
 *
 * NOTE: The lookup uses strcmp() in a linked list! It is not fast
 *       Its implementation in the NS module is on rarely used
 *       natives.
 */

#ifndef ALLOCSTRING_H
#define ALLOCSTRING_H

#include "CString.h"
#include "sh_list.h"

class StringManager
{
private:
	List<String *>		m_StringList;

public:
	/**
	 * sh_list.h does not delete objects put into
	 * the list, so I need to delete those and clear()
	 */
	inline void Clear(void)
	{
		List<String *>::iterator		 end;
		List<String *>::iterator		 iter;

		iter=m_StringList.begin();
		end=m_StringList.end();

		while (iter!=end)
		{
			delete (*iter++);
		}

		m_StringList.clear();
	}

	/**
	 * Iterate the list to see if the string exists
	 * This is slow and not very ideal, however
	 * this is *very* rarely used so it won't matter
	 */
	inline int Allocate(const char *str)
	{
		List<String *>::iterator		 end;
		List<String *>::iterator		 iter;

		iter=m_StringList.begin();
		end=m_StringList.end();

		while (iter!=end)
		{
			if (strcmp(str,(*iter)->c_str()))
			{
				// String is already in the list, do not allocate it again
				return MAKE_STRING((*iter)->c_str());
			}
			++iter;
		}

		// Was not found in the linked list, allocate it and add it to the list
		String *AllocStr = new String;

		AllocStr->assign(str);

		m_StringList.push_back(AllocStr);

		return MAKE_STRING(AllocStr->c_str());

	};

};

extern StringManager AllocStringList;

/**
 * Simple wrapper to make conversion easier
 */
inline int ALLOC_STRING2(const char *InputString)
{
	return AllocStringList.Allocate(InputString);
}


#endif // ALLOCSTRING_H
