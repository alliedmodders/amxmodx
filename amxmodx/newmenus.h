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

#ifndef _INCLUDE_NEWMENUS_H
#define _INCLUDE_NEWMENUS_H

#define	MENU_EXIT		-3
#define	MENU_BACK		-2
#define	MENU_MORE		-1
#define	ITEM_IGNORE		0
#define	ITEM_ENABLED	1
#define ITEM_DISABLED	2

#define MAX_MENU_ITEMS		10

#define MPROP_PERPAGE	1
#define MPROP_BACKNAME	2
#define MPROP_NEXTNAME	3
#define MPROP_EXITNAME	4
#define MPROP_TITLE		5
#define MPROP_EXITALL	6
#define MPROP_ORDER		7
#define MPROP_NOCOLORS	8
#define MPROP_PADMENU	9
#define MPROP_SET_NUMBER_COLOR	10

typedef int (*MENUITEM_CALLBACK)(int, int, int);

class BlankItem
{
private:
	char *m_text;
	bool m_num;
public:
	BlankItem() : m_text(NULL), m_num(false) { }
	BlankItem(BlankItem &src) { this->copyFrom(src); } 
	~BlankItem() { free(m_text); }

	void copyFrom(BlankItem &src)
	{
		m_text = src.m_text;
		m_num = src.m_num;
		src.m_text = NULL; // stop the src from freeing the buffer
	}
	BlankItem &operator = (const BlankItem &src) { this->copyFrom(const_cast<BlankItem&>(src)); return *this; }

	/* is this text instead of a blank */
	bool IsText() { return m_text != NULL; }

	/* is this a blank instead of text */
	bool IsBlank() { return m_text == NULL; }

	/* does this item take up a number */
	bool EatNumber() { return m_num; }

	/* the text this item is to display */
	const char *GetDisplay() { return m_text == NULL ? "" : m_text; }

	/* sets this item to use a blank */
	void SetBlank() { free(m_text); m_text = NULL; }

	/* sets this item to display text */
	void SetText(const char *text) { free(m_text); m_text = strdup(text);  }

	/* sets whether or not this item takes up a line */
	void SetEatNumber(bool val) { m_num = val; }

};
struct menuitem
{
	String name;
	String cmd;
	
	int access;
	int handler;
	bool isBlank;
	
	MENUITEM_CALLBACK pfn;
	size_t id;

	CVector<BlankItem> blanks;
};

typedef unsigned int menu_t;
typedef unsigned int item_t;
typedef unsigned int page_t;

class Menu
{
public:
	Menu(const char *title, AMX *amx, int fid);
	~Menu();
	
	menuitem *GetMenuItem(item_t item);
	size_t GetPageCount();
	size_t GetItemCount();
	menuitem *AddItem(const char *name, const char *cmd, int access);
	
	const char *GetTextString(int player, page_t page, int &keys);
	bool Display(int player, page_t page);
	
	int PagekeyToItem(page_t page, item_t key);
	int GetMenuMenuid();
public:
	CVector<menuitem * > m_Items;
	String m_Title;
	String m_Text;

	String m_OptNames[4];

	String m_ItemColor;
	bool m_NeverExit;
	bool m_AutoColors;
	
	int menuId;
	int thisId;
	int func;
	bool isDestroying;
public:
	unsigned int items_per_page;
};

void ClearMenus();

extern CVector<Menu *> g_NewMenus;
extern AMX_NATIVE_INFO g_NewMenuNatives[];

#endif //_INCLUDE_NEWMENUS_H
