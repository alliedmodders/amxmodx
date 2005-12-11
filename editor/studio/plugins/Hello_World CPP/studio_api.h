/* studio_api.h by the AMX Mod X Dev Team */

#ifndef _INCLUDE_STUDIO_H
#define _INCLUDE_STUDIO_H

#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <string.h>

// DLL Export
#define EXPORT extern "C" __declspec(dllexport)
// Return values for functions
#define PLUGIN_CONTINUE 0
#define PLUGIN_STOP 1
#define PLUGIN_HANDLED 2
// Important values for events
#define COMP_DEFAULT 0
#define COMP_STARTHL 1
#define COMP_UPLOAD 2
#define HELP_DEFAULT 0
#define HELP_SEARCH 1
#define HELP_FORUMS 2
#define HELP_ABOUT 3
#define CTRL_OUTPUT 0 // Output list
#define CTRL_CODETOOLS_MAIN 1 // Code-Tools window
#define CTRL_CODETOOLS_ITEM 2 // Code-Tools tab
#define CTRL_NOTES 3 // Notes tab
#define NEW_PAWN_PLUGIN 0
#define NEW_PAWN_EMPTYPLUGIN 1
#define NEW_PAWN_HEADER 2
#define NEW_CPP_MODULE 3
#define NEW_CPP_UNIT 4
#define NEW_CPP_HEADER 5
#define NEW_OTHER_TEXTFILE 6
#define NEW_OTHER_HTML 7
#define NEW_OTHER_SQL 8
#define NEW_OTHER_XML 9

// Messages (see "Functions and Events.txt" for further information)
#define SCM_SHOWPROGRESS				WM_USER + 0x100 // Show progressbar
#define SCM_HIDEPROGRESS				WM_USER + 0x101 // Hide progressbar
#define SCM_UPDATEPROGRESS				WM_USER + 0x102 // Update progress
#define SCM_LOADCODESNIPPETS			WM_USER + 0x103 // Load Code-Snippets of a language
#define SCM_CODESNIPPETCLICK			WM_USER + 0x104 // Simulate click on a Code-Snippet
#define SCM_MIRC_CMD					WM_USER + 0x105 // Send a command to mIRC (like /msg #amxmodx hello)
#define SCM_RELOADINI					WM_USER + 0x106 // Reload configuration ini (Hint: This is not the whole configuration!)
#define SCM_SELECTLANGUAGE				WM_USER + 0x107 // Set the highlighter language of the active document
#define SCM_LOADFILE					WM_USER + 0x108 // Load a custom file
#define SCM_CURRPROJECTS				WM_USER + 0x109 // Returns the current projects index
#define SCM_COMPILE						WM_USER + 0x110 // Simulate click on the "Compile"-button
#define SCM_COMPILE_UPLOAD				WM_USER + 0x111 // Simulate click on the "Compile and upload"-button
#define SCM_COMPILE_STARTHL				WM_USER + 0x112 // Simulate click on the "Compile and Start HL"-button
#define SCM_MENU_LOADIMAGE				WM_USER + 0x113 // Load an image (bitmap), returns the image index
#define SCM_MENU_ADDITEM				WM_USER + 0x114 // Add a menu item
#define SCM_MENU_ADDSUBITEM				WM_USER + 0x115 // Add a submenu item
#define SCM_MENU_FAKECLICK				WM_USER + 0x116 // Fake a click on a custom menu item
#define SCM_MENU_SHOWITEM				WM_USER + 0x117 // Show an item
#define SCM_MENU_HIDEITEM				WM_USER + 0x118 // Hide an item
#define SCM_PLUGIN_LOAD					WM_USER + 0x119 // Load a plugin
#define SCM_PLUGIN_UNLOAD				WM_USER + 0x120 // Unload a plugin
#define SCM_SETTINGS_CREATEPAGE			WM_USER + 0x121 // Create a new settings-page, returns the new HWND
#define SCM_SETTINGS_REMOVEPAGE			WM_USER + 0x194 // Removes a page from the settings dialog
#define SCM_CODEINSPECTOR_CLEAR			WM_USER + 0x122 // Clear the code-inspector
#define SCM_CODEINSPECTOR_ADD			WM_USER + 0x123 // Add a field to the code-inspector
#define SCM_CODEINSPECTOR_ADDCOMBO		WM_USER + 0x124 // Add a combobox-item to the code-inspector
#define SCM_CODEINSPECTOR_SETVALUE		WM_USER + 0x125 // Set a field of the code-inspector
#define SCM_CODEINSPECTOR_SETNAME		WM_USER + 0x126 // Set the name of an item
#define SCM_CODEINSPECTOR_GETVALUE		WM_USER + 0x127 // Get the value of an item 
#define SCM_CODEINSPECTOR_GETNAME		WM_USER + 0x128 // Get the name of an item
#define SCM_CODEINSPECTOR_COUNT			WM_USER + 0x129 // Gets the count
#define SCM_CODEINSPECTOR_BEGINUPDATE	WM_USER + 0x130 // Begin update
#define SCM_CODEINSPECTOR_ENDUPDATE		WM_USER + 0x131 // End update
#define SCM_CODEINSPECTOR_DELETE		WM_USER + 0x132 // Deletes a field in the code-inspector

#define SCM_PAWN_NEWFILE				WM_USER + 0x133 // Create new Pawn-file
#define SCM_PAWN_SAVEFILE				WM_USER + 0x134 // Save a Pawn-file
#define SCM_PAWN_CLOSEFILE				WM_USER + 0x135 // Close a Pawn-file
#define SCM_PAWN_ISUNTITLED				WM_USER + 0x136 // Returns 1 if untitled, otherwise 0
#define SCM_PAWN_ACTIVATE				WM_USER + 0x137 // Activate the Pawn Projects
#define SCM_PAWN_ACTIVATEDOC			WM_USER + 0x138 // Activate a Pawn document
#define SCM_PAWN_GETNOTES				WM_USER + 0x139 // Gets the notes text (RTF)
#define SCM_PAWN_SETNOTES				WM_USER + 0x140 // Set the notes text (RTF)
#define SCM_PAWN_GETFILENAME			WM_USER + 0x141 // Gets the filename of a document
#define SCM_PAWN_SETFILENAME			WM_USER + 0x142 // Sets the filename
#define SCM_PAWN_FILECOUNT				WM_USER + 0x195 // Returns the number of loaded scripts
#define SCM_PAWN_GETTEXT				WM_USER + 0x143 // Gets the text of a document
#define SCM_PAWN_SETTEXT				WM_USER + 0x144 // Sets the text of a document

#define SCM_CPP_NEWFILE					WM_USER + 0x145 // Create new Pawn-file
#define SCM_CPP_SAVEFILE				WM_USER + 0x146 // Save a Pawn-file
#define SCM_CPP_CLOSEFILE				WM_USER + 0x147 // Close a Pawn-file
#define SCM_CPP_ISUNTITLED				WM_USER + 0x148 // Returns 1 if untitled, otherwise 0
#define SCM_CPP_ACTIVATE				WM_USER + 0x149 // Activate the Pawn Projects
#define SCM_CPP_ACTIVATEDOC				WM_USER + 0x150 // Activate a Pawn document
#define SCM_CPP_ACTIVATEIDE				WM_USER + 0x151 // Activate the C++ IDE
#define SCM_CPP_GETNOTES				WM_USER + 0x152 // Gets the notes text (RTF)
#define SCM_CPP_SETNOTES				WM_USER + 0x153 // Set the notes text (RTF)
#define SCM_CPP_GETFILENAME				WM_USER + 0x154 // Gets the filename of a document
#define SCM_CPP_SETFILENAME				WM_USER + 0x155 // Sets the filename
#define SCM_CPP_FILECOUNT				WM_USER + 0x196 // Returns the number of loaded C(++) files
#define SCM_CPP_GETTEXT					WM_USER + 0x156 // Gets the text of a document
#define SCM_CPP_SETTEXT					WM_USER + 0x157 // Sets the text of a document

#define SCM_OTHER_NEWFILE				WM_USER + 0x158 // Create a new file
#define SCM_OTHER_SAVEFILE				WM_USER + 0x159 // Save a file
#define SCM_OTHER_CLOSEFILE				WM_USER + 0x160 // Close a file
#define SCM_OTHER_ISUNTITLED			WM_USER + 0x161 // Returns 1 if untitled, otherwise 0
#define SCM_OTHER_ACTIVATE				WM_USER + 0x162 // Activate a file
#define SCM_OTHER_ACTIVATEDOC			WM_USER + 0x163 // Activate a document
#define SCM_OTHER_GETNOTES				WM_USER + 0x164 // Gets the notes text (RTF)
#define SCM_OTHER_SETNOTES				WM_USER + 0x165 // Set the notes text (RTF)
#define SCM_OTHER_GETFILENAME			WM_USER + 0x166 // Gets the filename of a document
#define SCM_OTHER_SETFILENAME			WM_USER + 0x167 // Sets the filename
#define SCM_OTHER_FILECOUNT				WM_USER + 0x197 // Returns the number of loaded files
#define SCM_OTHER_GETTEXT				WM_USER + 0x168 // Gets the text of a document
#define SCM_OTHER_SETTEXT				WM_USER + 0x169 // Sets the text of a document

#define SCM_OUTPUT_SHOW					WM_USER + 0x170 // Show the output
#define SCM_OUTPUT_HIDE					WM_USER + 0x171 // Hide the output
#define SCM_OUTPUT_ADD					WM_USER + 0x172 // Add an item to the output
#define SCM_OUTPUT_CLEAR				WM_USER + 0x173 // Clear the output
#define SCM_OUTPUT_DELETE				WM_USER + 0x174 // Delete an item in the output
#define SCM_OUTPUT_GETTEXT				WM_USER + 0x175 // Get the whole output
#define SCM_OUTPUT_GETITEM				WM_USER + 0x176 // Get the text of a specific item
#define SCM_OUTPUT_INDEXOF				WM_USER + 0x177 // Gets the item index of an item, returns -1 if not found
#define SCM_ACTIVE_DOCUMENT				WM_USER + 0x178 // Returns the index of the active document
#define SCM_ACTIVE_PROJECTS				WM_USER + 0x179 // Returns the index of the active projects
#define SCM_EDITOR_SETTEXT				WM_USER + 0x180 // Sets the editor's text
#define SCM_EDITOR_GETTEXT				WM_USER + 0x181 // Gets the editor's text
#define SCM_EDTIOR_SETCALLTIPS			WM_USER + 0x182 // Sets the calltips
#define SCM_EDITOR_SHOWCALLTIP			WM_USER + 0x183 // Shows the calltip
#define SCM_EDITOR_SETAUTOCOMPLETE		WM_USER + 0x184 // Sets the autocomplete list
#define SCM_EDITOR_SHOWAUTOCOMPLETE		WM_USER + 0x185 // Show the autocomplete list
#define SCM_EDITOR_GETSELSTART			WM_USER + 0x186 // Gets the sel start
#define SCM_EDITOR_GETSELLENGTH			WM_USER + 0x187 // Gets the sel length
#define SCM_EDITOR_SETSELSTART			WM_USER + 0x188 // Set sel start
#define SCM_EDITOR_SETSELLENGH			WM_USER + 0x189 // Set sel length

#define SCM_REMOVE_MENUITEM				WM_USER + 0x190 // Remove an item from the menu
#define SCM_REMOVE_IMAGE				WM_USER + 0x191 // Remove an image from the image list
#define SCM_SETTHEME					WM_USER + 0x192 // Set theme
#define SCM_GETTHEME					WM_USER + 0x193 // Get theme						


struct load_info {
	/* Plugin Values */
	const CHAR *sPluginName;
	const CHAR *sPluginDescription;
	/* Form Handles */
	HWND *hModuleHandle;
	HWND *hAllFilesForm;
	HWND *hAutoIndent;
	HWND *hClose;
	HWND *hConnGen;
	HWND *hGoToLine;
	HWND *hHTMLPreview;
	HWND *hHudMsgGenerator;
	HWND *hInfo;
	HWND *hIRCPaster;
	HWND *hMainForm;
	HWND *hMenuGenerator;
	HWND *hMOTDGen;
	HWND *hPluginsIniEditor;
	HWND *hReplace;
	HWND *hSearch;
	HWND *hSelectColor;
	HWND *hSettings;
	HWND *hSocketsTerminal;
	HWND *hParamEdit;
	/* Important Control Handles */
	HWND *hOutput;
	HWND *hCodeExplorer;
	HWND *hCodeInspector;
	HWND *hNotes;
	/* Other */
	void *pApplication;
};

int SendStudioMsg(int Message, const CHAR *Data, int IntData) {
	HWND StudioHandle;
	StudioHandle = FindWindow("TfrmMain", "AMXX-Studio");
	if (StudioHandle != 0) {		
		COPYDATASTRUCT sMessage;
		sMessage.dwData = IntData;
		sMessage.cbData = strlen(Data) + 1;
		sMessage.lpData = (void *) Data;
		return SendMessage(StudioHandle, WM_COPYDATA, (WPARAM)Message, (LPARAM)&sMessage);
	}
	else
		return -1;
}

// If you receive strings as integer, simply cast them with (CHAR *) SendStudioMsg(...);

#endif 