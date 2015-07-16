// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_NEWMENUS_H
#define _INCLUDE_NEWMENUS_H

#define MENU_TIMEOUT	-4
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
	ke::AString m_text;
	bool m_num;
public:
	BlankItem() : m_num(false) 
	{ 
	}

	BlankItem(BlankItem &&other) 
	{
		m_text = ke::Forward<ke::AString>(other.m_text);
		m_num = other.m_num;
	}

	/* is this text instead of a blank */
	bool IsText() { return m_text.chars() != nullptr; }

	/* is this a blank instead of text */
	bool IsBlank() { return m_text.chars() == nullptr; }

	/* does this item take up a number */
	bool EatNumber() { return m_num; }

	/* the text this item is to display */
	const char *GetDisplay() { return m_text.chars(); }

	/* sets this item to use a blank */
	void SetBlank() { m_text = nullptr; }

	/* sets this item to display text */
	void SetText(const char *text) { m_text = text;  }

	/* sets whether or not this item takes up a line */
	void SetEatNumber(bool val) { m_num = val; }

};
struct menuitem
{
	ke::AString name;
	ke::AString cmd;
	
	int access;
	int handler;
	bool isBlank;
	
	MENUITEM_CALLBACK pfn;
	size_t id;

	ke::Vector<BlankItem> blanks;
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
	void Close(int player);

	int PagekeyToItem(page_t page, item_t key);
	int GetMenuMenuid();
public:
	ke::Vector<menuitem * > m_Items;

	ke::AString m_Title;
	ke::AutoString m_Text;
	ke::AString m_OptNames[4];
	ke::AString m_ItemColor;

	bool m_NeverExit;
	bool m_ForceExit;
	bool m_AutoColors;
	
	int menuId;
	int thisId;
	int func;
	bool isDestroying;
public:
	unsigned int items_per_page;
};

void ClearMenus();
Menu *get_menu_by_id(int id);

extern ke::Vector<Menu *> g_NewMenus;
extern AMX_NATIVE_INFO g_NewMenuNatives[];

#endif //_INCLUDE_NEWMENUS_H
