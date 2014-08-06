// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

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

