// WinCSX.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "WinCSX.h"
#include <stdio.h>

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
		if (!g_rank.loadRank("csstats.dat")) {
			HWND listbox = GetDlgItem(hDlg, IDC_LIST);
			SendMessage(      // returns LRESULT in lResult
				listbox,      // handle to destination control
				LB_ADDSTRING,      // message ID
				0,      // = (WPARAM) () wParam;
				(LPARAM) "File load failed!"      // = (LPARAM) () lParam;
			);  
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

LRESULT InitWinCSXBox(HWND hDlg) {
	// Load the stats
	if (!LoadRankFromFile(hDlg))
		return TRUE;

	// This part copies the occurring authids into the lefthand listbox.
	int index = 10, len = 0;
	HWND listbox = GetDlgItem(hDlg, IDC_LIST);

	char tempbuffer[1024];

	for (RankSystem::iterator b = g_rank.front(); b; --b) {
		//if ((*b).getPosition() < 1) // umm... naaah!
			//continue;

		_snprintf(tempbuffer, 1023, "%s", (*b).getName());

		SendMessage(      // returns LRESULT in lResult
			listbox,      // handle to destination control
			LB_ADDSTRING,      // message ID
			0,      // = (WPARAM) () wParam;
			(LPARAM) tempbuffer      // = (LPARAM) () lParam;
		);  

	}

	return TRUE;
}

void ListboxItemSelected(HWND hDlg) {
	HWND hwndList = GetDlgItem(hDlg, IDC_LIST); // Get the handle of the listbox
	LRESULT nItem = SendMessage(hwndList, LB_GETCURSEL, 0, 0);  // Get the item # that's selected. First item is prolly 0...

	// Retrieve complete stats record of this position. Position in listbox should be same as rank in our records!
	RankSystem::RankStats* stats = g_rank.findEntryInRankByPos((int)nItem + 1);
	
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
	case WM_INITDIALOG:
		return InitWinCSXBox(hDlg); // load all data from file and fill the listbox with the shit

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


