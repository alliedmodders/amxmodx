// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// csharp_bridge.h - C# Bridge Header for AMX Mod X Command Registration Interface
// Cross-platform bridge for C# interop

#ifndef CSHARP_BRIDGE_H
#define CSHARP_BRIDGE_H

#include "amxmodx.h"

// Cross-platform export macros
#ifdef _WIN32
    #define CSHARP_EXPORT extern "C" __declspec(dllexport)
    #define CSHARP_CALL __stdcall
#else
    #define CSHARP_EXPORT extern "C" __attribute__((visibility("default")))
    #define CSHARP_CALL
#endif

// Command types enumeration
enum CSharpCommandType
{
    CSHARP_COMMAND_TYPE_CONSOLE = 0,    // Console command (register_concmd)
    CSHARP_COMMAND_TYPE_CLIENT = 1,     // Client command (register_clcmd)
    CSHARP_COMMAND_TYPE_SERVER = 2      // Server command (register_srvcmd)
};

// Command callback delegate signature
typedef void (CSHARP_CALL *CSharpCommandCallback)(int clientId, int commandId, int flags);

// Menu callback delegate signature  
typedef void (CSHARP_CALL *CSharpMenuCallback)(int clientId, int menuId, int key);

// Command information structure for C# interop
struct CSharpCommandInfo
{
    char command[128];
    char info[256];
    int flags;
    int commandId;
    bool infoMultiLang;
    bool listable;
};

// Bridge initialization and cleanup
CSHARP_EXPORT void CSHARP_CALL InitializeCSharpBridge();
CSHARP_EXPORT void CSHARP_CALL CleanupCSharpBridge();

// Command registration functions
CSHARP_EXPORT int CSHARP_CALL RegisterConsoleCommand(
    const char* command, 
    CSharpCommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

CSHARP_EXPORT int CSHARP_CALL RegisterClientCommand(
    const char* command, 
    CSharpCommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

CSHARP_EXPORT int CSHARP_CALL RegisterServerCommand(
    const char* command, 
    CSharpCommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

// Menu command functions
CSHARP_EXPORT int CSHARP_CALL RegisterMenuCommand(
    int menuId, 
    int keyMask, 
    CSharpMenuCallback callback
);

CSHARP_EXPORT int CSHARP_CALL RegisterMenuId(
    const char* menuName, 
    bool global
);

// Command query functions
CSHARP_EXPORT bool CSHARP_CALL GetCommandInfo(
    int commandId, 
    CSharpCommandType commandType, 
    CSharpCommandInfo* outInfo
);

CSHARP_EXPORT int CSHARP_CALL GetCommandCount(
    CSharpCommandType commandType, 
    int accessFlags
);

// Command management functions
CSHARP_EXPORT bool CSHARP_CALL UnregisterCommand(int commandId);

// Internal bridge management
namespace CSharpBridge
{
    // Internal callback storage
    struct CallbackInfo
    {
        CSharpCommandCallback commandCallback;
        CSharpMenuCallback menuCallback;
        ke::AString commandName;
        int commandType;
        int flags;
        ke::AString info;
        bool infoMultiLang;
        int amxForwardId;  // AMX forward ID for integration
    };

    // Bridge state management
    void Initialize();
    void Cleanup();
    
    // Command registration helpers
    int RegisterCommand(const char* command, CSharpCommandCallback callback, 
                       int flags, const char* info, bool infoMultiLang, 
                       CSharpCommandType type);
    
    // Internal callback handlers
    void HandleCommandCallback(int clientId, int commandId, int flags);
    void HandleMenuCallback(int clientId, int menuId, int key);
    
    // Storage management
    extern ke::Vector<CallbackInfo*> g_commandCallbacks;
    extern ke::Vector<ke::AString> g_menuIds;
    extern int g_nextCommandId;
    extern int g_nextMenuId;
    extern bool g_initialized;
}

#endif // CSHARP_BRIDGE_H
