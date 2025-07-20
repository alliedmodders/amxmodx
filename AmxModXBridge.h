// AmxModXBridge.h - C++ Bridge Header for AMX Mod X Command Registration Interface
// Cross-platform bridge for C# interop

#ifndef AMXMODX_BRIDGE_H
#define AMXMODX_BRIDGE_H

#ifdef _WIN32
    #define BRIDGE_EXPORT extern "C" __declspec(dllexport)
    #define BRIDGE_CALL __stdcall
#else
    #define BRIDGE_EXPORT extern "C" __attribute__((visibility("default")))
    #define BRIDGE_CALL
#endif

// Command types enumeration
enum CommandType
{
    COMMAND_TYPE_CONSOLE = 0,    // Console command (register_concmd)
    COMMAND_TYPE_CLIENT = 1,     // Client command (register_clcmd)
    COMMAND_TYPE_SERVER = 2      // Server command (register_srvcmd)
};

// Command callback delegate signature
typedef void (BRIDGE_CALL *CommandCallback)(int clientId, int commandId, int flags);

// Menu callback delegate signature  
typedef void (BRIDGE_CALL *MenuCallback)(int clientId, int menuId, int key);

// Command information structure
struct CommandInfo
{
    char command[128];
    char info[256];
    int flags;
    int commandId;
    bool infoMultiLang;
    bool listable;
};

// Function declarations
BRIDGE_EXPORT int BRIDGE_CALL RegisterConsoleCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

BRIDGE_EXPORT int BRIDGE_CALL RegisterClientCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

BRIDGE_EXPORT int BRIDGE_CALL RegisterServerCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang
);

BRIDGE_EXPORT int BRIDGE_CALL RegisterMenuCommand(
    int menuId, 
    int keyMask, 
    MenuCallback callback
);

BRIDGE_EXPORT int BRIDGE_CALL RegisterMenuId(
    const char* menuName, 
    bool global
);

BRIDGE_EXPORT bool BRIDGE_CALL GetCommandInfo(
    int commandId, 
    CommandType commandType, 
    CommandInfo* outInfo
);

BRIDGE_EXPORT int BRIDGE_CALL GetCommandCount(
    CommandType commandType, 
    int accessFlags
);

BRIDGE_EXPORT bool BRIDGE_CALL UnregisterCommand(int commandId);

BRIDGE_EXPORT void BRIDGE_CALL InitializeBridge();
BRIDGE_EXPORT void BRIDGE_CALL CleanupBridge();

#endif // AMXMODX_BRIDGE_H
