// AmxModXBridge.cpp - C++ Bridge Implementation for AMX Mod X Command Registration Interface
// Cross-platform implementation for C# interop

#include "AmxModXBridge.h"
#include <map>
#include <vector>
#include <string>
#include <cstring>
#include <memory>

#ifdef _WIN32
    #include <windows.h>
#else
    #include <pthread.h>
#endif

// Thread-safe storage for callbacks
struct CallbackInfo
{
    CommandCallback commandCallback;
    MenuCallback menuCallback;
    std::string commandName;
    int commandType;
    int flags;
    std::string info;
    bool infoMultiLang;
};

// Global storage
static std::map<int, CallbackInfo> g_commandCallbacks;
static std::map<int, std::string> g_menuIds;
static int g_nextCommandId = 1;
static int g_nextMenuId = 1;

#ifdef _WIN32
static CRITICAL_SECTION g_criticalSection;
#else
static pthread_mutex_t g_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

// Thread synchronization helpers
class AutoLock
{
public:
    AutoLock()
    {
#ifdef _WIN32
        EnterCriticalSection(&g_criticalSection);
#else
        pthread_mutex_lock(&g_mutex);
#endif
    }
    
    ~AutoLock()
    {
#ifdef _WIN32
        LeaveCriticalSection(&g_criticalSection);
#else
        pthread_mutex_unlock(&g_mutex);
#endif
    }
};

// Internal command handler that bridges to C# callbacks
void InternalCommandHandler(int clientId, int commandId, int flags)
{
    AutoLock lock;
    
    auto it = g_commandCallbacks.find(commandId);
    if (it != g_commandCallbacks.end() && it->second.commandCallback)
    {
        it->second.commandCallback(clientId, commandId, flags);
    }
}

// Internal menu handler that bridges to C# callbacks
void InternalMenuHandler(int clientId, int menuId, int key)
{
    AutoLock lock;
    
    auto it = g_commandCallbacks.find(menuId);
    if (it != g_commandCallbacks.end() && it->second.menuCallback)
    {
        it->second.menuCallback(clientId, menuId, key);
    }
}

BRIDGE_EXPORT int BRIDGE_CALL RegisterConsoleCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang)
{
    if (!command || !callback)
        return -1;
        
    AutoLock lock;
    
    int commandId = g_nextCommandId++;
    
    CallbackInfo callbackInfo;
    callbackInfo.commandCallback = callback;
    callbackInfo.menuCallback = nullptr;
    callbackInfo.commandName = command;
    callbackInfo.commandType = COMMAND_TYPE_CONSOLE;
    callbackInfo.flags = flags;
    callbackInfo.info = info ? info : "";
    callbackInfo.infoMultiLang = infoMultiLang;
    
    g_commandCallbacks[commandId] = callbackInfo;
    
    // TODO: Here you would integrate with actual AMX Mod X command registration
    // For now, we just store the callback information
    
    return commandId;
}

BRIDGE_EXPORT int BRIDGE_CALL RegisterClientCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang)
{
    if (!command || !callback)
        return -1;
        
    AutoLock lock;
    
    int commandId = g_nextCommandId++;
    
    CallbackInfo callbackInfo;
    callbackInfo.commandCallback = callback;
    callbackInfo.menuCallback = nullptr;
    callbackInfo.commandName = command;
    callbackInfo.commandType = COMMAND_TYPE_CLIENT;
    callbackInfo.flags = flags;
    callbackInfo.info = info ? info : "";
    callbackInfo.infoMultiLang = infoMultiLang;
    
    g_commandCallbacks[commandId] = callbackInfo;
    
    return commandId;
}

BRIDGE_EXPORT int BRIDGE_CALL RegisterServerCommand(
    const char* command, 
    CommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang)
{
    if (!command || !callback)
        return -1;
        
    AutoLock lock;
    
    int commandId = g_nextCommandId++;
    
    CallbackInfo callbackInfo;
    callbackInfo.commandCallback = callback;
    callbackInfo.menuCallback = nullptr;
    callbackInfo.commandName = command;
    callbackInfo.commandType = COMMAND_TYPE_SERVER;
    callbackInfo.flags = flags;
    callbackInfo.info = info ? info : "";
    callbackInfo.infoMultiLang = infoMultiLang;
    
    g_commandCallbacks[commandId] = callbackInfo;
    
    return commandId;
}

BRIDGE_EXPORT int BRIDGE_CALL RegisterMenuCommand(
    int menuId, 
    int keyMask, 
    MenuCallback callback)
{
    if (!callback)
        return -1;
        
    AutoLock lock;
    
    int commandId = g_nextCommandId++;
    
    CallbackInfo callbackInfo;
    callbackInfo.commandCallback = nullptr;
    callbackInfo.menuCallback = callback;
    callbackInfo.commandName = "";
    callbackInfo.commandType = -1; // Menu type
    callbackInfo.flags = keyMask;
    callbackInfo.info = "";
    callbackInfo.infoMultiLang = false;
    
    g_commandCallbacks[commandId] = callbackInfo;
    
    return commandId;
}

BRIDGE_EXPORT int BRIDGE_CALL RegisterMenuId(
    const char* menuName, 
    bool global)
{
    if (!menuName)
        return -1;
        
    AutoLock lock;
    
    int menuId = g_nextMenuId++;
    g_menuIds[menuId] = menuName;
    
    return menuId;
}

BRIDGE_EXPORT bool BRIDGE_CALL GetCommandInfo(
    int commandId, 
    CommandType commandType, 
    CommandInfo* outInfo)
{
    if (!outInfo)
        return false;
        
    AutoLock lock;
    
    auto it = g_commandCallbacks.find(commandId);
    if (it == g_commandCallbacks.end())
        return false;
        
    const CallbackInfo& info = it->second;
    
    strncpy(outInfo->command, info.commandName.c_str(), sizeof(outInfo->command) - 1);
    outInfo->command[sizeof(outInfo->command) - 1] = '\0';
    
    strncpy(outInfo->info, info.info.c_str(), sizeof(outInfo->info) - 1);
    outInfo->info[sizeof(outInfo->info) - 1] = '\0';
    
    outInfo->flags = info.flags;
    outInfo->commandId = commandId;
    outInfo->infoMultiLang = info.infoMultiLang;
    outInfo->listable = (info.flags >= 0);
    
    return true;
}

BRIDGE_EXPORT int BRIDGE_CALL GetCommandCount(
    CommandType commandType, 
    int accessFlags)
{
    AutoLock lock;
    
    int count = 0;
    for (const auto& pair : g_commandCallbacks)
    {
        if (pair.second.commandType == commandType)
        {
            if (accessFlags == -1 || (pair.second.flags & accessFlags))
                count++;
        }
    }
    
    return count;
}

BRIDGE_EXPORT bool BRIDGE_CALL UnregisterCommand(int commandId)
{
    AutoLock lock;
    
    auto it = g_commandCallbacks.find(commandId);
    if (it == g_commandCallbacks.end())
        return false;
        
    g_commandCallbacks.erase(it);
    return true;
}

BRIDGE_EXPORT void BRIDGE_CALL InitializeBridge()
{
#ifdef _WIN32
    InitializeCriticalSection(&g_criticalSection);
#endif
}

BRIDGE_EXPORT void BRIDGE_CALL CleanupBridge()
{
    AutoLock lock;
    
    g_commandCallbacks.clear();
    g_menuIds.clear();
    
#ifdef _WIN32
    DeleteCriticalSection(&g_criticalSection);
#endif
}
