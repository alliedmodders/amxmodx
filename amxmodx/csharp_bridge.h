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

// Event callback delegate signature
typedef void (CSHARP_CALL *CSharpEventCallback)(int eventId, int clientId, int numParams);

// Forward callback delegate signature
typedef int (CSHARP_CALL *CSharpForwardCallback)(int forwardId, int numParams);

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

// Event information structure for C# interop
struct CSharpEventInfo
{
    char eventName[64];
    int eventId;
    int flags;
    int numParams;
    bool isActive;
};

// Forward information structure for C# interop
struct CSharpForwardInfo
{
    char forwardName[64];
    int forwardId;
    int numParams;
    int execType;
    bool isValid;
};

// Event parameter structure for C# interop
struct CSharpEventParam
{
    int type;           // 0=int, 1=float, 2=string
    int intValue;
    float floatValue;
    char stringValue[256];
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

// Command execution functions
CSHARP_EXPORT void CSHARP_CALL ExecuteServerCommand(const char* command);
CSHARP_EXPORT void CSHARP_CALL ExecuteClientCommand(int clientId, const char* command);
CSHARP_EXPORT void CSHARP_CALL ExecuteConsoleCommand(int clientId, const char* command);

// Command argument reading functions
CSHARP_EXPORT int CSHARP_CALL GetCommandArgCount();
CSHARP_EXPORT bool CSHARP_CALL GetCommandArg(int index, char* buffer, int bufferSize);
CSHARP_EXPORT bool CSHARP_CALL GetCommandArgs(char* buffer, int bufferSize);
CSHARP_EXPORT int CSHARP_CALL GetCommandArgInt(int index);
CSHARP_EXPORT float CSHARP_CALL GetCommandArgFloat(int index);

// Command query functions
CSHARP_EXPORT bool CSHARP_CALL FindCommand(const char* commandName, CSharpCommandType commandType, CSharpCommandInfo* outInfo);
CSHARP_EXPORT int CSHARP_CALL GetCommandsCount(CSharpCommandType commandType, int accessFlags);
CSHARP_EXPORT bool CSHARP_CALL GetCommandByIndex(int index, CSharpCommandType commandType, int accessFlags, CSharpCommandInfo* outInfo);

// Event system functions
CSHARP_EXPORT int CSHARP_CALL RegisterEvent(const char* eventName, CSharpEventCallback callback, int flags, const char* conditions);
CSHARP_EXPORT bool CSHARP_CALL UnregisterEvent(int eventHandle);
CSHARP_EXPORT int CSHARP_CALL GetEventId(const char* eventName);
CSHARP_EXPORT bool CSHARP_CALL GetEventInfo(int eventId, CSharpEventInfo* outInfo);

// Event parameter reading functions
CSHARP_EXPORT int CSHARP_CALL GetEventArgCount();
CSHARP_EXPORT bool CSHARP_CALL GetEventArg(int index, CSharpEventParam* outParam);
CSHARP_EXPORT int CSHARP_CALL GetEventArgInt(int index);
CSHARP_EXPORT float CSHARP_CALL GetEventArgFloat(int index);
CSHARP_EXPORT bool CSHARP_CALL GetEventArgString(int index, char* buffer, int bufferSize);

// Forward system functions
CSHARP_EXPORT int CSHARP_CALL CreateForward(const char* forwardName, int execType, const int* paramTypes, int numParams);
CSHARP_EXPORT int CSHARP_CALL CreateSingleForward(const char* functionName, CSharpForwardCallback callback, const int* paramTypes, int numParams);
CSHARP_EXPORT bool CSHARP_CALL ExecuteForward(int forwardId, const CSharpEventParam* params, int numParams, int* outResult);
CSHARP_EXPORT bool CSHARP_CALL UnregisterForward(int forwardId);
CSHARP_EXPORT bool CSHARP_CALL GetForwardInfo(int forwardId, CSharpForwardInfo* outInfo);

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

    // Event callback storage
    struct EventCallbackInfo
    {
        CSharpEventCallback eventCallback;
        ke::AString eventName;
        int eventId;
        int flags;
        ke::AString conditions;
        int eventHandle;
        int amxForwardId;
    };

    // Forward callback storage
    struct ForwardCallbackInfo
    {
        CSharpForwardCallback forwardCallback;
        ke::AString forwardName;
        int forwardId;
        int execType;
        ke::Vector<int> paramTypes;
        int amxForwardId;
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
    extern ke::Vector<EventCallbackInfo*> g_eventCallbacks;
    extern ke::Vector<ForwardCallbackInfo*> g_forwardCallbacks;
    extern int g_nextCommandId;
    extern int g_nextMenuId;
    extern int g_nextEventHandle;
    extern int g_nextForwardId;
    extern bool g_initialized;

    // Event system helpers
    int RegisterEventInternal(const char* eventName, CSharpEventCallback callback,
                             int flags, const char* conditions);
    void HandleEventCallback(int eventId, int clientId, int numParams);

    // Forward system helpers
    int CreateForwardInternal(const char* forwardName, int execType,
                             const int* paramTypes, int numParams);
    int HandleForwardCallback(int forwardId, int numParams);
}

#endif // CSHARP_BRIDGE_H
