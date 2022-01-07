// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "stdafx.h"
#include "WinCSX.h"
#include <stdio.h>
#include "commctrl.h"
#include <amtl/am-string.h>

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
 	// TODO: Place code here.
	MSG msg;
	HACCEL hAccelTable;

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, g_szTitle, MAX_LOADSTRING);

	LoadString(hInstance, IDC_WINCSX, g_szWindowClass, MAX_LOADSTRING);
	MyRegisterClass(hInstance);

	// Perform application initialization:
	if (!InitInstance (hInstance, nCmdShow)) 
	{
		return FALSE;
	}

	InitCommonControls();

	hAccelTable = LoadAccelerators(hInstance, (LPCTSTR)IDC_WINCSX);

	// Show the dialog box now.
	DialogBox(hInst, (LPCTSTR)IDD_WINCSXBOX, g_hWnd, (DLGPROC)WinCSXBox);

	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0)) 
	{
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg)) 
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return (int) msg.wParam;
}


//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
//  COMMENTS:
//
//    This function and its usage are only necessary if you want this code
//    to be compatible with Win32 systems prior to the 'RegisterClassEx'
//    function that was added to Windows 95. It is important to call this function
//    so that the application will get 'well formed' small icons associated
//    with it.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	wcex.cbSize = sizeof(WNDCLASSEX); 

	wcex.style			= 0; // CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= (WNDPROC)WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= LoadIcon(hInstance, (LPCTSTR)IDI_WINCSX);
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= (LPCTSTR)IDC_WINCSX;
	wcex.lpszClassName	= g_szWindowClass;
	wcex.hIconSm		= LoadIcon(wcex.hInstance, (LPCTSTR)IDI_SMALL);

	return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HANDLE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
	hInst = hInstance; // Store instance handle in our global variable

	g_hWnd = CreateWindow(g_szWindowClass, g_szTitle, WS_DLGFRAME, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, NULL, NULL, hInstance, NULL); // WS_OVERLAPPED WS_MINIMIZE

	if (!g_hWnd)
	{
		MessageBox(g_hWnd, "Failed to create main window!", "A caption", MB_OK);
		return FALSE;
	}
	
	ShowWindow(g_hWnd, SW_SHOWMINNOACTIVE); // nCmdShow SW_SHOWNORMAL were rubbish. SW_SHOWMINNOACTIVE looks ok.
	UpdateWindow(g_hWnd);

	return TRUE;
}

bool LoadRankFromFile(HWND hDlg) {
	if ( !g_rank.begin() )
	{		
		if (!g_rank.loadRank(STATS_FILENAME)) {
			MessageBox(hDlg, "File load failed! Make sure you have csstats.dat in the same directory as this executable. Exiting...", "Where IS that file of yours?", MB_OK);
			PostQuitMessage(0);
			return false;
		}
	}
	return true;
}

//
//  FUNCTION: WndProc(HWND, unsigned, WORD, LONG)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	//int wmId, wmEvent;
	PAINTSTRUCT ps;
	HDC hdc;

	switch (message) 
	{
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		// TODO: Add any drawing code here...
		EndPaint(hWnd, &ps);
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

void UpdateListBox(HWND hDlg) {
	HWND listbox = GetDlgItem(hDlg, IDC_LIST);

	// Clear first if there's anything in here already
	SendMessage(listbox, LB_RESETCONTENT, NULL, NULL);

	if (g_rank.front() == NULL) {
		MessageBox(hDlg, "The stats file is empty", "Emptiness...", MB_OK);
		return;
	}
	// This part copies the occurring authids into the lefthand listbox.
	int index = 10, len = 0;
	char tempbuffer[1024];

	for (RankSystem::iterator b = g_rank.front(); b; --b) {
		//if ((*b).getPosition() < 1) // umm... naaah!
			//continue;

		ke::SafeSprintf(tempbuffer, sizeof(tempbuffer), "%s", (*b).getName());

		SendMessage(      // returns LRESULT in lResult
			listbox,      // handle to destination control
			LB_ADDSTRING,      // message ID
			0,      // = (WPARAM) () wParam;
			(LPARAM) tempbuffer      // = (LPARAM) () lParam;
		);  
	}
}

LRESULT InitWinCSXBox(HWND hDlg) {
	// Load the stats
	if (!LoadRankFromFile(hDlg))
		return TRUE;

	UpdateListBox(hDlg);

	return TRUE;
}

void ClearStatsfields(HWND hDlg) {
	SetDlgItemText(hDlg, IDC_EDIT_POSITION, "");
	SetDlgItemText(hDlg, IDC_EDIT_NAME, "");
	SetDlgItemText(hDlg, IDC_EDIT_AUTHID, "");
	SetDlgItemText(hDlg, IDC_EDIT_FRAGS, "");
	SetDlgItemText(hDlg, IDC_EDIT_DEATHS, "");
	SetDlgItemText(hDlg, IDC_EDIT_HS, "");
	SetDlgItemText(hDlg, IDC_EDIT_TKS, "");
	SetDlgItemText(hDlg, IDC_EDIT_SHOTS, "");
	SetDlgItemText(hDlg, IDC_EDIT_HITS, "");
	SetDlgItemText(hDlg, IDC_EDIT_DAMAGE, "");
	SetDlgItemText(hDlg, IDC_EDIT_PLANTS, "");
	SetDlgItemText(hDlg, IDC_EDIT_EXPLOSIONS, "");
	SetDlgItemText(hDlg, IDC_EDIT_DEFUSIONS, "");
	SetDlgItemText(hDlg, IDC_EDIT_DEFUSED, "");
}

void ListboxItemSelected(HWND hDlg) {
	HWND hwndList = GetDlgItem(hDlg, IDC_LIST); // Get the handle of the listbox
	LRESULT nItem = SendMessage(hwndList, LB_GETCURSEL, 0, 0);  // Get the item # that's selected. First item is prolly 0...
	if (nItem == LB_ERR) {
		// Error, reset the form items...
		//MessageBox(hDlg, "Error: Couldn't find the selected record in the listbox!", "Oh fiddlesticks!", MB_OK);
		ClearStatsfields(hDlg);
		return;
	}
	// Retrieve complete stats record of this position. Position in listbox should be same as rank in our records!
	RankSystem::RankStats* stats = g_rank.findEntryInRankByPos((int)nItem + 1);
	if (stats == NULL) {
		char msg[512];
		sprintf(msg, "Error: Couldn't find the record by position! (nItem = %d)", nItem);
		MessageBox(hDlg, msg, "Oh fiddlesticks!", MB_OK);
		ClearStatsfields(hDlg);
		return;
	}
	// Copy data into form
	SetDlgItemInt(hDlg, IDC_EDIT_POSITION, stats->getPosition(), 0);
	SetDlgItemText(hDlg, IDC_EDIT_NAME, stats->getName());
	SetDlgItemText(hDlg, IDC_EDIT_AUTHID, stats->getUnique());
	SetDlgItemInt(hDlg, IDC_EDIT_FRAGS, stats->kills, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_DEATHS, stats->deaths, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_HS, stats->hs, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_TKS, stats->tks, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_SHOTS, stats->shots, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_HITS, stats->hits, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_DAMAGE, stats->damage, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_PLANTS, stats->bPlants, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_EXPLOSIONS, stats->bExplosions, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_DEFUSIONS, stats->bDefusions, 0);
	SetDlgItemInt(hDlg, IDC_EDIT_DEFUSED, stats->bDefused, 0);
}

void SaveChanges(HWND hDlg) {
	BOOL success;
	int position = GetDlgItemInt(hDlg, IDC_EDIT_POSITION, &success, false);
	if (!success)
		goto BadEnd;
	
	char authid[32]; // "primary key"
	GetDlgItemText(hDlg, IDC_EDIT_AUTHID, authid, sizeof(authid));
	RankSystem::RankStats* entry = g_rank.findEntryInRankByUnique(authid);
	if (!entry) {
		char buffer[256];
		sprintf(buffer, "Authid %s not found!", authid);
		MessageBox(hDlg, buffer, "Update failed", MB_OK);
		return;
	}

	char name[32];
	GetDlgItemText(hDlg, IDC_EDIT_NAME, name, sizeof(name));
	int frags = GetDlgItemInt(hDlg, IDC_EDIT_FRAGS, &success, false);
	if (!success)
		goto BadEnd;
	int deaths = GetDlgItemInt(hDlg, IDC_EDIT_DEATHS, &success, false);
	if (!success)
		goto BadEnd;
	int hs = GetDlgItemInt(hDlg, IDC_EDIT_HS, &success, false);
	if (!success)
		goto BadEnd;
	int tks = GetDlgItemInt(hDlg, IDC_EDIT_TKS, &success, false);
	if (!success)
		goto BadEnd;
	int shots = GetDlgItemInt(hDlg, IDC_EDIT_SHOTS, &success, false);
	if (!success)
		goto BadEnd;
	int hits = GetDlgItemInt(hDlg, IDC_EDIT_HITS, &success, false);
	if (!success)
		goto BadEnd;
	int damage = GetDlgItemInt(hDlg, IDC_EDIT_DAMAGE, &success, false);
	if (!success)
		goto BadEnd;
	int plants = GetDlgItemInt(hDlg, IDC_EDIT_PLANTS, &success, false);
	if (!success)
		goto BadEnd;
	int explosions = GetDlgItemInt(hDlg, IDC_EDIT_EXPLOSIONS, &success, false);
	if (!success)
		goto BadEnd;
	int defusions = GetDlgItemInt(hDlg, IDC_EDIT_DEFUSIONS, &success, false);
	if (!success)
		goto BadEnd;
	int defused = GetDlgItemInt(hDlg, IDC_EDIT_DEFUSED, &success, false);
	if (!success)
		goto BadEnd;

	// Update stats in memory
	entry->setName(name);
	entry->kills = frags;
	entry->deaths = deaths;
	entry->hs = hs;
	entry->tks = tks;
	entry->shots = shots;
	entry->hits = hits;
	entry->damage = damage;
	entry->bPlants = plants;
	entry->bExplosions = explosions;
	entry->bDefusions = defusions;
	entry->bDefused = defused;

	int newPosition = entry->updatePosition(NULL); // Updates rank (prolly just calculates "frags - deaths" and moves up/down in rank)

	g_rank.saveRank(STATS_FILENAME); // Save changes to file

	// Now update our listbox
	UpdateListBox(hDlg);

	char buffer[256];
	ke::SafeSprintf(buffer, sizeof(buffer), "New rank of %s: %d", name, newPosition);
	MessageBox(hDlg, buffer, "Update succeeded", MB_OK);

	// In the listbox, we need to reselect the item we just updated. Use the new name.
	HWND listbox = GetDlgItem(hDlg, IDC_LIST);
	if (SendMessage(listbox, LB_SELECTSTRING, newPosition - 1, (LPARAM)name) == LB_ERR)
		MessageBox(hDlg, "Error selecting item!", "Oh fiddlesticks!", MB_OK);
	else {
		// Update
		ListboxItemSelected(hDlg);
	}

	return;

BadEnd:
	MessageBox(hDlg, "Update failed", "Oh fiddlesticks!", MB_OK);
}

void ClearStats(HWND hDlg) {
	if (MessageBox(hDlg, "Are you sure? If you continue the whole csstats.dat will be wiped out!", "Omg!", MB_OKCANCEL | MB_DEFBUTTON2 | MB_ICONWARNING) != IDOK)
		return;
	g_rank.clear();
	g_rank.saveRank(STATS_FILENAME);

	// Now update our listbox
	UpdateListBox(hDlg);

	// Update
	ListboxItemSelected(hDlg);
}

void DeleteRecord(HWND hDlg) {
	if (MessageBox(hDlg, "Are you sure?", "Omg!", MB_OKCANCEL | MB_DEFBUTTON2 | MB_ICONWARNING) != IDOK)
		return;

	BOOL success;
	int position = GetDlgItemInt(hDlg, IDC_EDIT_POSITION, &success, false);
	if (!success)
		goto BadEnd;
	
	char authid[32]; // "primary key"
	GetDlgItemText(hDlg, IDC_EDIT_AUTHID, authid, sizeof(authid));
	RankSystem::RankStats* entry = g_rank.findEntryInRankByUnique(authid);
	if (!entry) {
		char buffer[256];
		sprintf(buffer, "Authid %s not found!", authid);
		MessageBox(hDlg, buffer, "Update failed", MB_OK);
		return;
	}

	// Mark this record to delete it.
	entry->MarkToDelete();

	// Save ranks from memory to disk.
	g_rank.saveRank(STATS_FILENAME); // Save changes to file

	// Clear memory.
	g_rank.clear();

	// Reload from disk into memory.
	LoadRankFromFile(hDlg);

	// Update list box.
	UpdateListBox(hDlg);

	MessageBox(hDlg, "Deleted record", "Delete succeeded", MB_OK);

	return;

BadEnd:
	MessageBox(hDlg, "Delete failed", "Oh fiddlesticks!", MB_OK);

	return;
}

// Message handler for WinCSXBox.
LRESULT CALLBACK WinCSXBox(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_INITDIALOG:
		return InitWinCSXBox(hDlg); // load all data from file and fill the listbox with the shit

	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
			case IDOK:
			case IDCANCEL:
				EndDialog(hDlg, LOWORD(wParam));
				PostQuitMessage(0);

				return TRUE;
			case IDC_LIST:
				switch (HIWORD(wParam)) 
				{ 
					case LBN_SELCHANGE:
						// Omg omg, a line in linebox was selected.
						ListboxItemSelected(hDlg);

						return TRUE; 
				} 
				break;
			case IDC_ABOUT:
				DialogBox(hInst, (LPCTSTR)IDD_ABOUTBOX, hDlg, (DLGPROC)AboutBox);
				break;
			case IDC_BUTTON_SAVECHANGES:
				SaveChanges(hDlg);
				break;
			case IDC_BUTTON_CLEARSTATS:
				ClearStats(hDlg);
				break;
			case IDC_BUTTON_DELETE:
				DeleteRecord(hDlg);
				//DialogBox(hInst, (LPCTSTR)IDD_DELETEBOX, hDlg, (DLGPROC)DeleteBox);
				break;
		}
		break;
	}

	return FALSE;
}

// Message handler for AboutBox.
LRESULT CALLBACK AboutBox(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
			case IDOK:
			case IDCANCEL:
				EndDialog(hDlg, LOWORD(wParam));
				
				return TRUE;
		}
		break;
	}

	return FALSE;
}

