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
    ke::Vector<EventCallbackInfo*> g_eventCallbacks;
    ke::Vector<ForwardCallbackInfo*> g_forwardCallbacks;
    int g_nextCommandId = 1;
    int g_nextMenuId = 1;
    int g_nextEventHandle = 1;
    int g_nextForwardId = 1;
    bool g_initialized = false;

    // Current event context for parameter reading
    static int g_currentEventId = -1;
    static int g_currentEventParams = 0;
    static CSharpEventParam g_eventParamBuffer[32]; // Max 32 parameters

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

    // AMX forward callback for events
    static cell AMX_NATIVE_CALL CSharpEventHandler(AMX *amx, cell *params)
    {
        int eventId = params[1];
        int clientId = params[2];
        int numParams = params[3];

        // Store event parameters for reading
        g_currentEventId = eventId;
        g_currentEventParams = numParams;

        // Copy event parameters from AMX
        for (int i = 0; i < numParams && i < 32; i++)
        {
            cell* paramPtr = get_amxaddr(amx, params[4 + i]);
            if (paramPtr)
            {
                // Determine parameter type and store
                if (*paramPtr >= -2147483648 && *paramPtr <= 2147483647)
                {
                    g_eventParamBuffer[i].type = 0; // int
                    g_eventParamBuffer[i].intValue = *paramPtr;
                }
                else
                {
                    g_eventParamBuffer[i].type = 1; // float
                    g_eventParamBuffer[i].floatValue = amx_ctof(*paramPtr);
                }
            }
        }

        HandleEventCallback(eventId, clientId, numParams);
        return 1;
    }

    // AMX forward callback for custom forwards
    static cell AMX_NATIVE_CALL CSharpForwardHandler(AMX *amx, cell *params)
    {
        int forwardId = params[1];
        int numParams = params[2];

        return HandleForwardCallback(forwardId, numParams);
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

        // Clean up event callbacks
        for (size_t i = 0; i < g_eventCallbacks.length(); i++)
        {
            delete g_eventCallbacks[i];
        }
        g_eventCallbacks.clear();

        // Clean up forward callbacks
        for (size_t i = 0; i < g_forwardCallbacks.length(); i++)
        {
            delete g_forwardCallbacks[i];
        }
        g_forwardCallbacks.clear();
        
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

    void HandleEventCallback(int eventId, int clientId, int numParams)
    {
        AutoLock lock;

        // Find the callback by event ID
        for (size_t i = 0; i < g_eventCallbacks.length(); i++)
        {
            EventCallbackInfo* info = g_eventCallbacks[i];
            if (info && info->eventCallback && info->eventId == eventId)
            {
                info->eventCallback(eventId, clientId, numParams);
                break;
            }
        }
    }

    int HandleForwardCallback(int forwardId, int numParams)
    {
        AutoLock lock;

        // Find the callback by forward ID
        for (size_t i = 0; i < g_forwardCallbacks.length(); i++)
        {
            ForwardCallbackInfo* info = g_forwardCallbacks[i];
            if (info && info->forwardCallback && info->forwardId == forwardId)
            {
                return info->forwardCallback(forwardId, numParams);
            }
        }

        return 0; // Default return value
    }

    int RegisterEventInternal(const char* eventName, CSharpEventCallback callback,
                             int flags, const char* conditions)
    {
        if (!g_initialized || !eventName || !callback)
            return -1;

        AutoLock lock;

        int eventHandle = g_nextEventHandle++;

        EventCallbackInfo* callbackInfo = new EventCallbackInfo();
        callbackInfo->eventCallback = callback;
        callbackInfo->eventName = eventName;
        callbackInfo->eventId = -1; // Will be set when event is found
        callbackInfo->flags = flags;
        callbackInfo->conditions = conditions ? conditions : "";
        callbackInfo->eventHandle = eventHandle;
        callbackInfo->amxForwardId = -1;

        // Register with AMX event system
        // This would integrate with the existing event infrastructure
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
            // Create a forward for this event
            int forwardId = registerSPForwardByName(plugin->getAMX(), "CSharpEventHandler",
                                                   FP_CELL, FP_CELL, FP_CELL, FP_DONE);
            if (forwardId != -1)
            {
                callbackInfo->amxForwardId = forwardId;

                // Register with event manager (simplified - actual implementation would vary)
                // This would typically involve registering with the game event system
                callbackInfo->eventId = g_nextEventHandle; // Simplified ID assignment
            }
        }

        g_eventCallbacks.append(callbackInfo);
        return eventHandle;
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

// Command execution functions implementation
CSHARP_EXPORT void CSHARP_CALL ExecuteServerCommand(const char* command)
{
    if (!command || !CSharpBridge::g_initialized)
        return;

    // Create a copy of the command and add newline
    size_t len = strlen(command);
    char* cmd = new char[len + 2];
    strcpy(cmd, command);
    cmd[len] = '\n';
    cmd[len + 1] = '\0';

    // Execute server command
    SERVER_COMMAND(cmd);

    delete[] cmd;
}

CSHARP_EXPORT void CSHARP_CALL ExecuteClientCommand(int clientId, const char* command)
{
    if (!command || !CSharpBridge::g_initialized)
        return;

    if (clientId < 0 || clientId > gpGlobals->maxClients)
        return;

    // Create a copy of the command and add newline
    size_t len = strlen(command);
    char* cmd = new char[len + 2];
    strcpy(cmd, command);
    cmd[len] = '\n';
    cmd[len + 1] = '\0';

    if (clientId == 0)
    {
        // Send to all clients
        for (int i = 1; i <= gpGlobals->maxClients; ++i)
        {
            CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
            if (!pPlayer->IsBot() && pPlayer->initialized)
                CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
        }
    }
    else
    {
        // Send to specific client
        CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
        if (!pPlayer->IsBot() && pPlayer->initialized)
            CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
    }

    delete[] cmd;
}

CSHARP_EXPORT void CSHARP_CALL ExecuteConsoleCommand(int clientId, const char* command)
{
    if (!command || !CSharpBridge::g_initialized)
        return;

    // Create a copy of the command and add newline
    size_t len = strlen(command);
    char* cmd = new char[len + 2];
    strcpy(cmd, command);
    cmd[len] = '\n';
    cmd[len + 1] = '\0';

    if (clientId < 1 || clientId > gpGlobals->maxClients)
    {
        // Execute as server command
        SERVER_COMMAND(cmd);
    }
    else
    {
        // Execute as client console command
        CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
        if (!pPlayer->IsBot() && pPlayer->initialized)
            CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
    }

    delete[] cmd;
}

// Command argument reading functions implementation
CSHARP_EXPORT int CSHARP_CALL GetCommandArgCount()
{
    if (!CSharpBridge::g_initialized)
        return 0;

    return CMD_ARGC();
}

CSHARP_EXPORT bool CSHARP_CALL GetCommandArg(int index, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    if (index < 0)
        return false;

    const char* arg = CMD_ARGV(index);
    if (!arg)
    {
        buffer[0] = '\0';
        return false;
    }

    // Copy argument to buffer with bounds checking
    size_t argLen = strlen(arg);
    size_t copyLen = (argLen < (size_t)(bufferSize - 1)) ? argLen : (bufferSize - 1);

    strncpy(buffer, arg, copyLen);
    buffer[copyLen] = '\0';

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL GetCommandArgs(char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    const char* args = CMD_ARGS();
    if (!args)
    {
        buffer[0] = '\0';
        return false;
    }

    // Copy arguments to buffer with bounds checking
    size_t argsLen = strlen(args);
    size_t copyLen = (argsLen < (size_t)(bufferSize - 1)) ? argsLen : (bufferSize - 1);

    strncpy(buffer, args, copyLen);
    buffer[copyLen] = '\0';

    return true;
}

CSHARP_EXPORT int CSHARP_CALL GetCommandArgInt(int index)
{
    if (!CSharpBridge::g_initialized || index < 0)
        return 0;

    const char* arg = CMD_ARGV(index);
    if (!arg)
        return 0;

    return atoi(arg);
}

CSHARP_EXPORT float CSHARP_CALL GetCommandArgFloat(int index)
{
    if (!CSharpBridge::g_initialized || index < 0)
        return 0.0f;

    const char* arg = CMD_ARGV(index);
    if (!arg)
        return 0.0f;

    return (float)atof(arg);
}

// Command query functions implementation
CSHARP_EXPORT bool CSHARP_CALL FindCommand(const char* commandName, CSharpCommandType commandType, CSharpCommandInfo* outInfo)
{
    if (!commandName || !outInfo || !CSharpBridge::g_initialized)
        return false;

    // Convert C# command type to AMX command type
    int amxCmdType;
    switch (commandType)
    {
        case CSHARP_COMMAND_TYPE_CONSOLE:
            amxCmdType = CMD_ConsoleCommand;
            break;
        case CSHARP_COMMAND_TYPE_CLIENT:
            amxCmdType = CMD_ClientCommand;
            break;
        case CSHARP_COMMAND_TYPE_SERVER:
            amxCmdType = CMD_ServerCommand;
            break;
        default:
            return false;
    }

    // Search for command in AMX command manager
    CmdMngr::iterator iter;
    switch (commandType)
    {
        case CSHARP_COMMAND_TYPE_CONSOLE:
            iter = g_commands.concmdbegin();
            break;
        case CSHARP_COMMAND_TYPE_CLIENT:
            iter = g_commands.clcmdbegin();
            break;
        case CSHARP_COMMAND_TYPE_SERVER:
            iter = g_commands.srvcmdbegin();
            break;
        default:
            return false;
    }

    while (iter)
    {
        if ((*iter).matchCommand(commandName))
        {
            // Found the command, fill output structure
            const CmdMngr::Command& cmd = *iter;

            strncpy(outInfo->command, cmd.getCmdLine(), sizeof(outInfo->command) - 1);
            outInfo->command[sizeof(outInfo->command) - 1] = '\0';

            strncpy(outInfo->info, cmd.getCmdInfo(), sizeof(outInfo->info) - 1);
            outInfo->info[sizeof(outInfo->info) - 1] = '\0';

            outInfo->flags = cmd.getFlags();
            outInfo->commandId = cmd.getId();
            outInfo->infoMultiLang = cmd.isInfoML();
            outInfo->listable = true; // Commands found in manager are listable

            return true;
        }
        ++iter;
    }

    return false;
}

CSHARP_EXPORT int CSHARP_CALL GetCommandsCount(CSharpCommandType commandType, int accessFlags)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    // Convert C# command type to AMX command type
    int amxCmdType;
    switch (commandType)
    {
        case CSHARP_COMMAND_TYPE_CONSOLE:
            amxCmdType = CMD_ConsoleCommand;
            break;
        case CSHARP_COMMAND_TYPE_CLIENT:
            amxCmdType = CMD_ClientCommand;
            break;
        case CSHARP_COMMAND_TYPE_SERVER:
            amxCmdType = CMD_ServerCommand;
            break;
        default:
            return 0;
    }

    return g_commands.getCmdNum(amxCmdType, accessFlags);
}

CSHARP_EXPORT bool CSHARP_CALL GetCommandByIndex(int index, CSharpCommandType commandType, int accessFlags, CSharpCommandInfo* outInfo)
{
    if (!outInfo || !CSharpBridge::g_initialized || index < 0)
        return false;

    // Convert C# command type to AMX command type
    int amxCmdType;
    switch (commandType)
    {
        case CSHARP_COMMAND_TYPE_CONSOLE:
            amxCmdType = CMD_ConsoleCommand;
            break;
        case CSHARP_COMMAND_TYPE_CLIENT:
            amxCmdType = CMD_ClientCommand;
            break;
        case CSHARP_COMMAND_TYPE_SERVER:
            amxCmdType = CMD_ServerCommand;
            break;
        default:
            return false;
    }

    // Get command by index
    CmdMngr::Command* cmd = g_commands.getCmd(index, amxCmdType, accessFlags);
    if (!cmd)
        return false;

    // Fill output structure
    strncpy(outInfo->command, cmd->getCmdLine(), sizeof(outInfo->command) - 1);
    outInfo->command[sizeof(outInfo->command) - 1] = '\0';

    strncpy(outInfo->info, cmd->getCmdInfo(), sizeof(outInfo->info) - 1);
    outInfo->info[sizeof(outInfo->info) - 1] = '\0';

    outInfo->flags = cmd->getFlags();
    outInfo->commandId = cmd->getId();
    outInfo->infoMultiLang = cmd->isInfoML();
    outInfo->listable = true;

    return true;
}

// Event system functions implementation
CSHARP_EXPORT int CSHARP_CALL RegisterEvent(const char* eventName, CSharpEventCallback callback, int flags, const char* conditions)
{
    return CSharpBridge::RegisterEventInternal(eventName, callback, flags, conditions);
}

CSHARP_EXPORT bool CSHARP_CALL UnregisterEvent(int eventHandle)
{
    if (!CSharpBridge::g_initialized || eventHandle <= 0)
        return false;

    CSharpBridge::AutoLock lock;

    // Find and remove the event callback
    for (size_t i = 0; i < CSharpBridge::g_eventCallbacks.length(); i++)
    {
        CSharpBridge::EventCallbackInfo* info = CSharpBridge::g_eventCallbacks[i];
        if (info && info->eventHandle == eventHandle)
        {
            // Unregister from AMX forward system
            if (info->amxForwardId != -1)
            {
                unregisterSPForward(info->amxForwardId);
            }

            delete info;
            CSharpBridge::g_eventCallbacks[i] = nullptr;
            return true;
        }
    }

    return false;
}

CSHARP_EXPORT int CSHARP_CALL GetEventId(const char* eventName)
{
    if (!eventName || !CSharpBridge::g_initialized)
        return -1;

    // Search for event by name in registered events
    CSharpBridge::AutoLock lock;

    for (size_t i = 0; i < CSharpBridge::g_eventCallbacks.length(); i++)
    {
        CSharpBridge::EventCallbackInfo* info = CSharpBridge::g_eventCallbacks[i];
        if (info && strcmp(info->eventName.chars(), eventName) == 0)
        {
            return info->eventId;
        }
    }

    return -1;
}

CSHARP_EXPORT bool CSHARP_CALL GetEventInfo(int eventId, CSharpEventInfo* outInfo)
{
    if (!outInfo || !CSharpBridge::g_initialized || eventId < 0)
        return false;

    CSharpBridge::AutoLock lock;

    // Find event by ID
    for (size_t i = 0; i < CSharpBridge::g_eventCallbacks.length(); i++)
    {
        CSharpBridge::EventCallbackInfo* info = CSharpBridge::g_eventCallbacks[i];
        if (info && info->eventId == eventId)
        {
            strncpy(outInfo->eventName, info->eventName.chars(), sizeof(outInfo->eventName) - 1);
            outInfo->eventName[sizeof(outInfo->eventName) - 1] = '\0';

            outInfo->eventId = info->eventId;
            outInfo->flags = info->flags;
            outInfo->numParams = 0; // Would be determined by event type
            outInfo->isActive = (info->amxForwardId != -1);

            return true;
        }
    }

    return false;
}

// Event parameter reading functions implementation
CSHARP_EXPORT int CSHARP_CALL GetEventArgCount()
{
    if (!CSharpBridge::g_initialized)
        return 0;

    return CSharpBridge::g_currentEventParams;
}

CSHARP_EXPORT bool CSHARP_CALL GetEventArg(int index, CSharpEventParam* outParam)
{
    if (!outParam || !CSharpBridge::g_initialized || index < 0 || index >= CSharpBridge::g_currentEventParams)
        return false;

    if (index >= 32) // Buffer limit
        return false;

    *outParam = CSharpBridge::g_eventParamBuffer[index];
    return true;
}

CSHARP_EXPORT int CSHARP_CALL GetEventArgInt(int index)
{
    if (!CSharpBridge::g_initialized || index < 0 || index >= CSharpBridge::g_currentEventParams || index >= 32)
        return 0;

    return CSharpBridge::g_eventParamBuffer[index].intValue;
}

CSHARP_EXPORT float CSHARP_CALL GetEventArgFloat(int index)
{
    if (!CSharpBridge::g_initialized || index < 0 || index >= CSharpBridge::g_currentEventParams || index >= 32)
        return 0.0f;

    if (CSharpBridge::g_eventParamBuffer[index].type == 1)
        return CSharpBridge::g_eventParamBuffer[index].floatValue;
    else
        return (float)CSharpBridge::g_eventParamBuffer[index].intValue;
}

CSHARP_EXPORT bool CSHARP_CALL GetEventArgString(int index, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized ||
        index < 0 || index >= CSharpBridge::g_currentEventParams || index >= 32)
        return false;

    if (CSharpBridge::g_eventParamBuffer[index].type == 2)
    {
        strncpy(buffer, CSharpBridge::g_eventParamBuffer[index].stringValue, bufferSize - 1);
        buffer[bufferSize - 1] = '\0';
        return true;
    }
    else
    {
        // Convert number to string
        if (CSharpBridge::g_eventParamBuffer[index].type == 0)
        {
            snprintf(buffer, bufferSize, "%d", CSharpBridge::g_eventParamBuffer[index].intValue);
        }
        else
        {
            snprintf(buffer, bufferSize, "%.2f", CSharpBridge::g_eventParamBuffer[index].floatValue);
        }
        return true;
    }
}

// Forward system functions implementation
CSHARP_EXPORT int CSHARP_CALL CreateForward(const char* forwardName, int execType, const int* paramTypes, int numParams)
{
    if (!forwardName || !CSharpBridge::g_initialized || numParams < 0 || numParams > 16)
        return -1;

    CSharpBridge::AutoLock lock;

    int forwardId = CSharpBridge::g_nextForwardId++;

    CSharpBridge::ForwardCallbackInfo* callbackInfo = new CSharpBridge::ForwardCallbackInfo();
    callbackInfo->forwardCallback = nullptr; // Global forward, no specific callback
    callbackInfo->forwardName = forwardName;
    callbackInfo->forwardId = forwardId;
    callbackInfo->execType = execType;
    callbackInfo->amxForwardId = -1;

    // Copy parameter types
    for (int i = 0; i < numParams; i++)
    {
        callbackInfo->paramTypes.append(paramTypes[i]);
    }

    // Create AMX forward
    ForwardParam amxParams[16];
    for (int i = 0; i < numParams; i++)
    {
        amxParams[i] = static_cast<ForwardParam>(paramTypes[i]);
    }

    int amxForwardId = registerForwardC(forwardName, static_cast<ForwardExecType>(execType),
                                       reinterpret_cast<cell*>(amxParams), numParams);

    if (amxForwardId != -1)
    {
        callbackInfo->amxForwardId = amxForwardId;
    }

    CSharpBridge::g_forwardCallbacks.append(callbackInfo);
    return forwardId;
}

CSHARP_EXPORT int CSHARP_CALL CreateSingleForward(const char* functionName, CSharpForwardCallback callback, const int* paramTypes, int numParams)
{
    if (!functionName || !callback || !CSharpBridge::g_initialized || numParams < 0 || numParams > 16)
        return -1;

    CSharpBridge::AutoLock lock;

    int forwardId = CSharpBridge::g_nextForwardId++;

    CSharpBridge::ForwardCallbackInfo* callbackInfo = new CSharpBridge::ForwardCallbackInfo();
    callbackInfo->forwardCallback = callback;
    callbackInfo->forwardName = functionName;
    callbackInfo->forwardId = forwardId;
    callbackInfo->execType = 0; // Single forward
    callbackInfo->amxForwardId = -1;

    // Copy parameter types
    for (int i = 0; i < numParams; i++)
    {
        callbackInfo->paramTypes.append(paramTypes[i]);
    }

    // Create single plugin forward (simplified - would need actual plugin context)
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
        ForwardParam amxParams[16];
        for (int i = 0; i < numParams; i++)
        {
            amxParams[i] = static_cast<ForwardParam>(paramTypes[i]);
        }

        int amxForwardId = registerSPForwardByNameC(plugin->getAMX(), functionName,
                                                   reinterpret_cast<cell*>(amxParams), numParams);

        if (amxForwardId != -1)
        {
            callbackInfo->amxForwardId = amxForwardId;
        }
    }

    CSharpBridge::g_forwardCallbacks.append(callbackInfo);
    return forwardId;
}

CSHARP_EXPORT bool CSHARP_CALL ExecuteForward(int forwardId, const CSharpEventParam* params, int numParams, int* outResult)
{
    if (!CSharpBridge::g_initialized || forwardId < 0 || numParams < 0)
        return false;

    CSharpBridge::AutoLock lock;

    // Find forward by ID
    CSharpBridge::ForwardCallbackInfo* info = nullptr;
    for (size_t i = 0; i < CSharpBridge::g_forwardCallbacks.length(); i++)
    {
        if (CSharpBridge::g_forwardCallbacks[i] && CSharpBridge::g_forwardCallbacks[i]->forwardId == forwardId)
        {
            info = CSharpBridge::g_forwardCallbacks[i];
            break;
        }
    }

    if (!info || info->amxForwardId == -1)
        return false;

    // Prepare parameters for AMX forward execution
    cell amxParams[16];
    for (int i = 0; i < numParams && i < 16; i++)
    {
        switch (params[i].type)
        {
            case 0: // int
                amxParams[i] = params[i].intValue;
                break;
            case 1: // float
                amxParams[i] = amx_ftoc(params[i].floatValue);
                break;
            case 2: // string
                // String handling would require more complex parameter preparation
                amxParams[i] = 0; // Simplified
                break;
            default:
                amxParams[i] = 0;
                break;
        }
    }

    // Execute the forward
    cell result = executeForwards(info->amxForwardId, amxParams[0], amxParams[1], amxParams[2],
                                 amxParams[3], amxParams[4], amxParams[5], amxParams[6], amxParams[7]);

    if (outResult)
        *outResult = result;

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL UnregisterForward(int forwardId)
{
    if (!CSharpBridge::g_initialized || forwardId < 0)
        return false;

    CSharpBridge::AutoLock lock;

    // Find and remove the forward callback
    for (size_t i = 0; i < CSharpBridge::g_forwardCallbacks.length(); i++)
    {
        CSharpBridge::ForwardCallbackInfo* info = CSharpBridge::g_forwardCallbacks[i];
        if (info && info->forwardId == forwardId)
        {
            // Unregister from AMX forward system
            if (info->amxForwardId != -1)
            {
                if (info->forwardCallback) // Single forward
                {
                    unregisterSPForward(info->amxForwardId);
                }
                // Global forwards are not unregistered individually
            }

            delete info;
            CSharpBridge::g_forwardCallbacks[i] = nullptr;
            return true;
        }
    }

    return false;
}

CSHARP_EXPORT bool CSHARP_CALL GetForwardInfo(int forwardId, CSharpForwardInfo* outInfo)
{
    if (!outInfo || !CSharpBridge::g_initialized || forwardId < 0)
        return false;

    CSharpBridge::AutoLock lock;

    // Find forward by ID
    for (size_t i = 0; i < CSharpBridge::g_forwardCallbacks.length(); i++)
    {
        CSharpBridge::ForwardCallbackInfo* info = CSharpBridge::g_forwardCallbacks[i];
        if (info && info->forwardId == forwardId)
        {
            strncpy(outInfo->forwardName, info->forwardName.chars(), sizeof(outInfo->forwardName) - 1);
            outInfo->forwardName[sizeof(outInfo->forwardName) - 1] = '\0';

            outInfo->forwardId = info->forwardId;
            outInfo->numParams = info->paramTypes.length();
            outInfo->execType = info->execType;
            outInfo->isValid = (info->amxForwardId != -1);

            return true;
        }
    }

    return false;
}

// Player information functions implementation
CSHARP_EXPORT bool CSHARP_CALL IsPlayerValid(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    return (clientId >= 1 && clientId <= gpGlobals->maxClients);
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerInfo(int clientId, CSharpPlayerInfo* outInfo)
{
    if (!outInfo || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    // Fill player information
    strncpy(outInfo->name, pPlayer->name.chars(), sizeof(outInfo->name) - 1);
    outInfo->name[sizeof(outInfo->name) - 1] = '\0';

    strncpy(outInfo->ip, pPlayer->ip.chars(), sizeof(outInfo->ip) - 1);
    outInfo->ip[sizeof(outInfo->ip) - 1] = '\0';

    const char* authId = GETPLAYERAUTHID(pPlayer->pEdict);
    if (authId)
    {
        strncpy(outInfo->authId, authId, sizeof(outInfo->authId) - 1);
        outInfo->authId[sizeof(outInfo->authId) - 1] = '\0';
    }
    else
    {
        outInfo->authId[0] = '\0';
    }

    strncpy(outInfo->team, pPlayer->team.chars(), sizeof(outInfo->team) - 1);
    outInfo->team[sizeof(outInfo->team) - 1] = '\0';

    outInfo->index = clientId;
    outInfo->teamId = pPlayer->teamId;
    outInfo->userId = GETPLAYERUSERID(pPlayer->pEdict);
    outInfo->flags = pPlayer->flags[0]; // First flag set
    outInfo->connectTime = pPlayer->time;
    outInfo->playTime = pPlayer->playtime;
    outInfo->isInGame = pPlayer->ingame;
    outInfo->isBot = pPlayer->IsBot();
    outInfo->isAlive = pPlayer->IsAlive();
    outInfo->isAuthorized = pPlayer->authorized;
    outInfo->isConnecting = (!pPlayer->ingame && pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0));
    outInfo->isHLTV = (pPlayer->pEdict->v.flags & FL_PROXY) ? true : false;
    outInfo->hasVGUI = pPlayer->vgui;

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerStats(int clientId, CSharpPlayerStats* outStats)
{
    if (!outStats || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    // Fill player statistics
    outStats->deaths = pPlayer->deaths;
    outStats->kills = 0; // Would need to be calculated from game state
    outStats->frags = pPlayer->pEdict->v.frags;
    outStats->currentWeapon = pPlayer->current;
    outStats->menu = pPlayer->menu;
    outStats->keys = pPlayer->keys;
    outStats->health = pPlayer->pEdict->v.health;
    outStats->armor = pPlayer->pEdict->v.armorvalue;
    outStats->aiming = pPlayer->aiming;
    outStats->menuExpire = pPlayer->menuexpire;

    // Copy weapon data
    for (int i = 0; i < 32 && i < MAX_WEAPONS; i++)
    {
        outStats->weapons[i] = pPlayer->weapons[i].ammo;
        outStats->clips[i] = pPlayer->weapons[i].clip;
    }

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerName(int clientId, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    strncpy(buffer, pPlayer->name.chars(), bufferSize - 1);
    buffer[bufferSize - 1] = '\0';

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerIP(int clientId, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    strncpy(buffer, pPlayer->ip.chars(), bufferSize - 1);
    buffer[bufferSize - 1] = '\0';

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerAuthId(int clientId, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    const char* authId = GETPLAYERAUTHID(pPlayer->pEdict);
    if (authId)
    {
        strncpy(buffer, authId, bufferSize - 1);
        buffer[bufferSize - 1] = '\0';
        return true;
    }

    buffer[0] = '\0';
    return false;
}

CSHARP_EXPORT bool CSHARP_CALL GetPlayerTeam(int clientId, char* buffer, int bufferSize)
{
    if (!buffer || bufferSize <= 0 || !CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    strncpy(buffer, pPlayer->team.chars(), bufferSize - 1);
    buffer[bufferSize - 1] = '\0';

    return true;
}

// Player state functions implementation
CSHARP_EXPORT bool CSHARP_CALL IsPlayerInGame(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && pPlayer->ingame);
}

CSHARP_EXPORT bool CSHARP_CALL IsPlayerBot(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && pPlayer->IsBot());
}

CSHARP_EXPORT bool CSHARP_CALL IsPlayerAlive(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && pPlayer->IsAlive());
}

CSHARP_EXPORT bool CSHARP_CALL IsPlayerAuthorized(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && pPlayer->authorized);
}

CSHARP_EXPORT bool CSHARP_CALL IsPlayerConnecting(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && !pPlayer->ingame && pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0));
}

CSHARP_EXPORT bool CSHARP_CALL IsPlayerHLTV(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer && pPlayer->pEdict && (pPlayer->pEdict->v.flags & FL_PROXY));
}

// Player property getters implementation
CSHARP_EXPORT int CSHARP_CALL GetPlayerUserId(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return 0;

    return GETPLAYERUSERID(pPlayer->pEdict);
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerTeamId(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->teamId : 0;
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerFlags(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->flags[0] : 0;
}

CSHARP_EXPORT float CSHARP_CALL GetPlayerConnectTime(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0.0f;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0.0f;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->time : 0.0f;
}

CSHARP_EXPORT float CSHARP_CALL GetPlayerPlayTime(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0.0f;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0.0f;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->playtime : 0.0f;
}

CSHARP_EXPORT float CSHARP_CALL GetPlayerHealth(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0.0f;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0.0f;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return 0.0f;

    return pPlayer->pEdict->v.health;
}

CSHARP_EXPORT float CSHARP_CALL GetPlayerArmor(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0.0f;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0.0f;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return 0.0f;

    return pPlayer->pEdict->v.armorvalue;
}

CSHARP_EXPORT float CSHARP_CALL GetPlayerFrags(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0.0f;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0.0f;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return 0.0f;

    return pPlayer->pEdict->v.frags;
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerDeaths(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->deaths : 0;
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerCurrentWeapon(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->current : 0;
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerMenu(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->menu : 0;
}

CSHARP_EXPORT int CSHARP_CALL GetPlayerKeys(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return 0;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return 0;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    return (pPlayer) ? pPlayer->keys : 0;
}

// Player property setters implementation
CSHARP_EXPORT bool CSHARP_CALL SetPlayerHealth(int clientId, float health)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    pPlayer->pEdict->v.health = health;
    return true;
}

CSHARP_EXPORT bool CSHARP_CALL SetPlayerArmor(int clientId, float armor)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    pPlayer->pEdict->v.armorvalue = armor;
    return true;
}

CSHARP_EXPORT bool CSHARP_CALL SetPlayerFrags(int clientId, float frags)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    pPlayer->pEdict->v.frags = frags;
    return true;
}

CSHARP_EXPORT bool CSHARP_CALL SetPlayerTeamInfo(int clientId, int teamId, const char* teamName)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->ingame)
        return false;

    pPlayer->teamId = teamId;
    if (teamName != nullptr)
    {
        pPlayer->team = teamName;
        // Register team if not already registered
        g_teamsIds.registerTeam(teamName, teamId);
    }

    return true;
}

CSHARP_EXPORT bool CSHARP_CALL SetPlayerFlags(int clientId, int flags)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer)
        return false;

    pPlayer->flags[0] = flags;
    return true;
}

// Player utility functions implementation
CSHARP_EXPORT int CSHARP_CALL GetMaxClients()
{
    if (!CSharpBridge::g_initialized)
        return 0;

    return gpGlobals->maxClients;
}

CSHARP_EXPORT int CSHARP_CALL GetConnectedPlayersCount()
{
    if (!CSharpBridge::g_initialized)
        return 0;

    int count = 0;
    for (int i = 1; i <= gpGlobals->maxClients; i++)
    {
        CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
        if (pPlayer && pPlayer->ingame)
            count++;
    }

    return count;
}

CSHARP_EXPORT bool CSHARP_CALL GetConnectedPlayers(int* playerIds, int maxPlayers, int* outCount)
{
    if (!playerIds || !outCount || !CSharpBridge::g_initialized)
        return false;

    int count = 0;
    for (int i = 1; i <= gpGlobals->maxClients && count < maxPlayers; i++)
    {
        CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
        if (pPlayer && pPlayer->ingame)
        {
            playerIds[count] = i;
            count++;
        }
    }

    *outCount = count;
    return true;
}

CSHARP_EXPORT bool CSHARP_CALL KickPlayer(int clientId, const char* reason)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict)
        return false;

    // Create kick command
    char kickCmd[256];
    if (reason && strlen(reason) > 0)
    {
        snprintf(kickCmd, sizeof(kickCmd), "kick #%d \"%s\"", GETPLAYERUSERID(pPlayer->pEdict), reason);
    }
    else
    {
        snprintf(kickCmd, sizeof(kickCmd), "kick #%d", GETPLAYERUSERID(pPlayer->pEdict));
    }

    SERVER_COMMAND(kickCmd);
    return true;
}

CSHARP_EXPORT bool CSHARP_CALL SlayPlayer(int clientId)
{
    if (!CSharpBridge::g_initialized)
        return false;

    if (clientId < 1 || clientId > gpGlobals->maxClients)
        return false;

    CPlayer* pPlayer = GET_PLAYER_POINTER_I(clientId);
    if (!pPlayer || !pPlayer->pEdict || !pPlayer->IsAlive())
        return false;

    // Kill the player
    pPlayer->pEdict->v.health = 0.0f;
    pPlayer->pEdict->v.deadflag = DEAD_DEAD;

    // Call the killed function to properly handle death
    MDLL_Killed(pPlayer->pEdict, nullptr, 0);

    return true;
}
