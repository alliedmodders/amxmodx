#ifndef _INCLUDE_NEWMENUS_H
#define _INCLUDE_NEWMENUS_H

#define	MENU_EXIT	-3
#define	MENU_BACK	-2
#define	MENU_MORE	-1
#define	ITEM_IGNORE		0
#define	ITEM_ENABLED	1
#define ITEM_DISABLED	2

#define	MENUITEMS	7

typedef int (*MENUITEM_CALLBACK)(int, int, int);

struct menuitem
{
	String name;
	String cmd;
	int access;
	int handler;
	MENUITEM_CALLBACK pfn;
	size_t id;
};

typedef unsigned int	menu_t;
typedef unsigned int	item_t;
typedef unsigned int	page_t;

class Menu
{
public:
	Menu(const char *title, int menuId, int thisId);
	~Menu();
	menuitem *GetMenuItem(item_t item);
	size_t GetPageCount();
	size_t GetItemCount();
	menuitem *AddItem(const char *name, const char *cmd, int access);
	const char *GetTextString(int player, page_t page, int &keys);
	bool Display(int player, page_t page);
	int PagekeyToItem(page_t page, item_t key);
	int GetMenuMenuid();
private:
	CVector<menuitem * > m_Items;
	String m_Title;
	String m_Text;
	int menuId;
	int thisId;
};

/*Menu *CreateMenu(const char *title);
Menu *GetMenuById(menu_t menu);
menuitem *GetMenuItem(menu_t menu, item_t item);
size_t GetMenuPages(menu_t menu);
size_t GetMenuItems(menu_t menu);
menuitem *AddMenuItem(menu_t menu, const char *name, const char *cmd, int access);
bool DisplayMenu(menu_t menu, int player, page_t page);
int MenuPagekeyToItem(menu_t menu, page_t page, int key);
int FindByMenuid(int menuid);
int GetMenuMenuid(menu_t menu);
const char *GetItemName(menu_t menu, item_t item);
const char *GetItemCmd(menu_t menu, item_t item);*/

void ClearMenus();

extern AMX_NATIVE_INFO g_NewMenuNatives[];

#endif //_INCLUDE_NEWMENUS_H
