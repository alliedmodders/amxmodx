#pragma once

#include "resource.h"
#include "CRank.h"

// Constants
#define MAX_LOADSTRING	100
#define VERSION			"0.2"
#define STATS_FILENAME	"csstats.dat"

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR g_szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR g_szWindowClass[MAX_LOADSTRING];			// the main window class name
RankSystem g_rank;
HWND g_hWnd;

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK	WinCSXBox(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK	AboutBox(HWND, UINT, WPARAM, LPARAM);
bool				LoadRankFromFile(HWND hDlg);

