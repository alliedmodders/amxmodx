// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// csharp_bridge.cpp - C# Bridge Implementation for AMX Mod X Command Registration Interface

#include "csharp_bridge.h"
#include "CCmd.h"
#include "CMenu.h"
#include <cstring>

// Cross-platform threading support
#ifdef _WIN32
    #include <windows.h>
    static CRITICAL_SECTION g_criticalSection;
    #define LOCK_INIT() InitializeCriticalSection(&g_criticalSection)
    #define LOCK_DESTROY() DeleteCriticalSection(&g_criticalSection)
    #define LOCK_ENTER() EnterCriticalSection(&g_criticalSection)
    #define LOCK_LEAVE() LeaveCriticalSection(&g_criticalSection)
#else
    #include <pthread.h>
    static pthread_mutex_t g_mutex = PTHREAD_MUTEX_INITIALIZER;
    #define LOCK_INIT() pthread_mutex_init(&g_mutex, NULL)
    #define LOCK_DESTROY() pthread_mutex_destroy(&g_mutex)
    #define LOCK_ENTER() pthread_mutex_lock(&g_mutex)
    #define LOCK_LEAVE() pthread_mutex_unlock(&g_mutex)
#endif

namespace CSharpBridge
{
    // Global storage
    ke::Vector<CallbackInfo*> g_commandCallbacks;
    ke::Vector<ke::AString> g_menuIds;
    int g_nextCommandId = 1;
    int g_nextMenuId = 1;
    bool g_initialized = false;

    // Thread-safe helper class
    class AutoLock
    {
    public:
        AutoLock() { LOCK_ENTER(); }
        ~AutoLock() { LOCK_LEAVE(); }
    };

    // AMX forward callback for console commands
    static cell AMX_NATIVE_CALL CSharpConsoleCommandHandler(AMX *amx, cell *params)
    {
        int clientId = params[1];
        int flags = params[2];
        int commandId = params[3];
        
        HandleCommandCallback(clientId, commandId, flags);
        return 1;
    }

    // AMX forward callback for client commands
    static cell AMX_NATIVE_CALL CSharpClientCommandHandler(AMX *amx, cell *params)
    {
        int clientId = params[1];
        int flags = params[2];
        int commandId = params[3];
        
        HandleCommandCallback(clientId, commandId, flags);
        return 1;
    }

    // AMX forward callback for server commands
    static cell AMX_NATIVE_CALL CSharpServerCommandHandler(AMX *amx, cell *params)
    {
        int clientId = params[1];
        int flags = params[2];
        int commandId = params[3];
        
        HandleCommandCallback(clientId, commandId, flags);
        return 1;
    }

    // AMX forward callback for menu commands
    static cell AMX_NATIVE_CALL CSharpMenuCommandHandler(AMX *amx, cell *params)
    {
        int clientId = params[1];
        int menuId = params[2];
        int key = params[3];
        
        HandleMenuCallback(clientId, menuId, key);
        return 1;
    }

    void Initialize()
    {
        if (g_initialized)
            return;

        LOCK_INIT();
        g_commandCallbacks.clear();
        g_menuIds.clear();
        g_nextCommandId = 1;
        g_nextMenuId = 1;
        g_initialized = true;
    }

    void Cleanup()
    {
        if (!g_initialized)
            return;

        AutoLock lock;
        
        // Clean up command callbacks
        for (size_t i = 0; i < g_commandCallbacks.length(); i++)
        {
            delete g_commandCallbacks[i];
        }
        g_commandCallbacks.clear();
        g_menuIds.clear();
        
        g_initialized = false;
        LOCK_DESTROY();
    }

    int RegisterCommand(const char* command, CSharpCommandCallback callback, 
                       int flags, const char* info, bool infoMultiLang, 
                       CSharpCommandType type)
    {
        if (!g_initialized || !command || !callback)
            return -1;

        AutoLock lock;

        int commandId = g_nextCommandId++;
        
        CallbackInfo* callbackInfo = new CallbackInfo();
        callbackInfo->commandCallback = callback;
        callbackInfo->menuCallback = nullptr;
        callbackInfo->commandName = command;
        callbackInfo->commandType = type;
        callbackInfo->flags = flags;
        callbackInfo->info = info ? info : "";
        callbackInfo->infoMultiLang = infoMultiLang;
        callbackInfo->amxForwardId = -1;

        // Create AMX forward based on command type
        const char* handlerName = nullptr;
        switch (type)
        {
            case CSHARP_COMMAND_TYPE_CONSOLE:
                handlerName = "CSharpConsoleCommandHandler";
                break;
            case CSHARP_COMMAND_TYPE_CLIENT:
                handlerName = "CSharpClientCommandHandler";
                break;
            case CSHARP_COMMAND_TYPE_SERVER:
                handlerName = "CSharpServerCommandHandler";
                break;
        }

        if (handlerName)
        {
            // Register the command with AMX Mod X command system
            // This integrates with the existing command infrastructure
            CmdMngr::Command* cmd = nullptr;
            
            // Find a plugin to register the command with (use first available)
            CPluginMngr::CPlugin* plugin = nullptr;
            for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
            {
                if ((*iter).isValid())
                {
                    plugin = &(*iter);
                    break;
                }
            }

            if (plugin)
            {
                // Create a forward for this command
                int forwardId = registerSPForwardByName(plugin->getAMX(), handlerName, 
                                                       FP_CELL, FP_CELL, FP_CELL, FP_DONE);
                if (forwardId != -1)
                {
                    callbackInfo->amxForwardId = forwardId;
                    
                    // Register with command manager
                    bool listable = (flags >= 0);
                    cmd = g_commands.registerCommand(plugin, forwardId, command, 
                                                   callbackInfo->info.chars(), 
                                                   flags < 0 ? 0 : flags, 
                                                   listable, infoMultiLang);
                    
                    if (cmd)
                    {
                        switch (type)
                        {
                            case CSHARP_COMMAND_TYPE_CONSOLE:
                                cmd->setCmdType(CMD_ConsoleCommand);
                                REG_SVR_COMMAND((char*)cmd->getCommand(), plugin_srvcmd);
                                break;
                            case CSHARP_COMMAND_TYPE_CLIENT:
                                cmd->setCmdType(CMD_ClientCommand);
                                break;
                            case CSHARP_COMMAND_TYPE_SERVER:
                                cmd->setCmdType(CMD_ServerCommand);
                                REG_SVR_COMMAND((char*)cmd->getCommand(), plugin_srvcmd);
                                break;
                        }
                    }
                }
            }
        }

        g_commandCallbacks.append(callbackInfo);
        return commandId;
    }

    void HandleCommandCallback(int clientId, int commandId, int flags)
    {
        AutoLock lock;
        
        // Find the callback by command ID
        for (size_t i = 0; i < g_commandCallbacks.length(); i++)
        {
            CallbackInfo* info = g_commandCallbacks[i];
            if (info && info->commandCallback && (i + 1) == commandId)
            {
                info->commandCallback(clientId, commandId, flags);
                break;
            }
        }
    }

    void HandleMenuCallback(int clientId, int menuId, int key)
    {
        AutoLock lock;
        
        // Find the callback by menu ID
        for (size_t i = 0; i < g_commandCallbacks.length(); i++)
        {
            CallbackInfo* info = g_commandCallbacks[i];
            if (info && info->menuCallback && info->commandType == -1) // Menu type
            {
                info->menuCallback(clientId, menuId, key);
                break;
            }
        }
    }
}

// Exported C functions for C# interop
CSHARP_EXPORT void CSHARP_CALL InitializeCSharpBridge()
{
    CSharpBridge::Initialize();
}

CSHARP_EXPORT void CSHARP_CALL CleanupCSharpBridge()
{
    CSharpBridge::Cleanup();
}

CSHARP_EXPORT int CSHARP_CALL RegisterConsoleCommand(
    const char* command, 
    CSharpCommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang)
{
    return CSharpBridge::RegisterCommand(command, callback, flags, info, 
                                        infoMultiLang, CSHARP_COMMAND_TYPE_CONSOLE);
}

CSHARP_EXPORT int CSHARP_CALL RegisterClientCommand(
    const char* command, 
    CSharpCommandCallback callback, 
    int flags, 
    const char* info, 
    bool infoMultiLang)
{
    return CSharpBridge::RegisterCommand(command, callback, flags, info, 
                                        infoMultiLang, CSHARP_COMMAND_TYPE_CLIENT);
}

CSHARP_EXPORT int CSHARP_CALL RegisterServerCommand(
    const char* command,
    CSharpCommandCallback callback,
    int flags,
    const char* info,
    bool infoMultiLang)
{
    return CSharpBridge::RegisterCommand(command, callback, flags, info,
                                        infoMultiLang, CSHARP_COMMAND_TYPE_SERVER);
}

CSHARP_EXPORT int CSHARP_CALL RegisterMenuCommand(
    int menuId,
    int keyMask,
    CSharpMenuCallback callback)
{
    if (!CSharpBridge::g_initialized || !callback)
        return -1;

    CSharpBridge::AutoLock lock;

    int commandId = CSharpBridge::g_nextCommandId++;

    CSharpBridge::CallbackInfo* callbackInfo = new CSharpBridge::CallbackInfo();
    callbackInfo->commandCallback = nullptr;
    callbackInfo->menuCallback = callback;
    callbackInfo->commandName = "";
    callbackInfo->commandType = -1; // Menu type
    callbackInfo->flags = keyMask;
    callbackInfo->info = "";
    callbackInfo->infoMultiLang = false;
    callbackInfo->amxForwardId = -1;

    // Register with menu system
    CPluginMngr::CPlugin* plugin = nullptr;
    for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
    {
        if ((*iter).isValid())
        {
            plugin = &(*iter);
            break;
        }
    }

    if (plugin)
    {
        int forwardId = registerSPForwardByName(plugin->getAMX(), "CSharpMenuCommandHandler",
                                               FP_CELL, FP_CELL, FP_CELL, FP_DONE);
        if (forwardId != -1)
        {
            callbackInfo->amxForwardId = forwardId;
            g_menucmds.registerMenuCmd(plugin, menuId, keyMask, forwardId);
        }
    }

    CSharpBridge::g_commandCallbacks.append(callbackInfo);
    return commandId;
}

CSHARP_EXPORT int CSHARP_CALL RegisterMenuId(
    const char* menuName,
    bool global)
{
    if (!CSharpBridge::g_initialized || !menuName)
        return -1;

    CSharpBridge::AutoLock lock;

    int menuId = CSharpBridge::g_nextMenuId++;
    CSharpBridge::g_menuIds.append(ke::AString(menuName));

    // Register with menu ID system
    AMX* amx = global ? nullptr : (g_plugins.begin() ? (*g_plugins.begin()).getAMX() : nullptr);
    int registeredId = g_menucmds.registerMenuId(menuName, amx);

    return registeredId != 0 ? menuId : -1;
}

CSHARP_EXPORT bool CSHARP_CALL GetCommandInfo(
    int commandId,
    CSharpCommandType commandType,
    CSharpCommandInfo* outInfo)
{
    if (!CSharpBridge::g_initialized || !outInfo || commandId <= 0)
        return false;

    CSharpBridge::AutoLock lock;

    size_t index = commandId - 1;
    if (index >= CSharpBridge::g_commandCallbacks.length())
        return false;

    CSharpBridge::CallbackInfo* info = CSharpBridge::g_commandCallbacks[index];
    if (!info || info->commandType != commandType)
        return false;

    // Copy information to output structure
    strncpy(outInfo->command, info->commandName.chars(), sizeof(outInfo->command) - 1);
    outInfo->command[sizeof(outInfo->command) - 1] = '\0';

    strncpy(outInfo->info, info->info.chars(), sizeof(outInfo->info) - 1);
    outInfo->info[sizeof(outInfo->info) - 1] = '\0';

    outInfo->flags = info->flags;
    outInfo->commandId = commandId;
    outInfo->infoMultiLang = info->infoMultiLang;
    outInfo->listable = (info->flags >= 0);

    return true;
}

CSHARP_EXPORT int CSHARP_CALL GetCommandCount(
    CSharpCommandType commandType,
    int accessFlags)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    CSharpBridge::AutoLock lock;

    int count = 0;
    for (size_t i = 0; i < CSharpBridge::g_commandCallbacks.length(); i++)
    {
        CSharpBridge::CallbackInfo* info = CSharpBridge::g_commandCallbacks[i];
        if (info && info->commandType == commandType)
        {
            if (accessFlags == -1 || (info->flags & accessFlags))
                count++;
        }
    }

    return count;
}

CSHARP_EXPORT bool CSHARP_CALL UnregisterCommand(int commandId)
{
    if (!CSharpBridge::g_initialized || commandId <= 0)
        return false;

    CSharpBridge::AutoLock lock;

    size_t index = commandId - 1;
    if (index >= CSharpBridge::g_commandCallbacks.length())
        return false;

    CSharpBridge::CallbackInfo* info = CSharpBridge::g_commandCallbacks[index];
    if (!info)
        return false;

    // Unregister from AMX forward system
    if (info->amxForwardId != -1)
    {
        unregisterSPForward(info->amxForwardId);
    }

    // Clean up
    delete info;
    CSharpBridge::g_commandCallbacks[index] = nullptr;

    return true;
}
