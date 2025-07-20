// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// AmxModXExample.cs - Usage examples for AMX Mod X Command Registration Interface
// 演示如何使用 AMX Mod X 命令注册接口

using System;
using System.Collections.Generic;
using AmxModX.Interop;

namespace AmxModX.Examples
{
    /// <summary>
    /// AMX Mod X 命令注册使用示例 / AMX Mod X command registration usage examples
    /// 展示如何在实际项目中使用命令注册接口 / Demonstrates how to use command registration interface in real projects
    /// </summary>
    public class AmxModXExample
    {
        // 存储注册的命令ID / Store registered command IDs
        private static readonly Dictionary<string, int> RegisteredCommands = new Dictionary<string, int>();
        private static int _mainMenuId = -1;

        /// <summary>
        /// 初始化示例系统 / Initialize example system
        /// 注册所有示例命令和菜单 / Register all example commands and menus
        /// </summary>
        public static void Initialize()
        {
            try
            {
                Console.WriteLine("Initializing AMX Mod X C# Command Example...");

                // 初始化命令系统 / Initialize command system
                AmxModXCommands.Initialize();

                // 注册各种类型的命令 / Register various types of commands
                RegisterClientCommands();
                RegisterConsoleCommands();
                RegisterServerCommands();
                RegisterMenuSystem();

                Console.WriteLine("AMX Mod X commands registered successfully!");
                DisplayRegisteredCommands();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to initialize AMX Mod X commands: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 注册客户端命令示例 / Register client commands example
        /// 客户端命令由玩家在游戏中执行 / Client commands are executed by players in-game
        /// </summary>
        private static void RegisterClientCommands()
        {
            // 帮助命令 - 所有玩家可用 / Help command - available to all players
            int helpCmd = AmxModXCommands.RegisterClientCommand(
                command: "help",
                callback: OnHelpCommand,
                flags: CommandFlags.All,
                info: "Display available commands and their usage",
                infoMultiLang: false
            );
            RegisteredCommands["help"] = helpCmd;

            // 状态命令 - 显示玩家状态 / Status command - show player status
            int statusCmd = AmxModXCommands.RegisterClientCommand(
                command: "status",
                callback: OnStatusCommand,
                flags: CommandFlags.All,
                info: "Display your current game status",
                infoMultiLang: false
            );
            RegisteredCommands["status"] = statusCmd;

            // VIP命令 - 仅VIP用户可用 / VIP command - VIP users only
            int vipCmd = AmxModXCommands.RegisterClientCommand(
                command: "vip",
                callback: OnVipCommand,
                flags: CommandFlags.Vip,
                info: "Access VIP features",
                infoMultiLang: false
            );
            RegisteredCommands["vip"] = vipCmd;
        }

        /// <summary>
        /// 注册控制台命令示例 / Register console commands example
        /// 控制台命令从服务器控制台执行 / Console commands are executed from server console
        /// </summary>
        private static void RegisterConsoleCommands()
        {
            // 踢出玩家命令 - 需要管理员权限 / Kick player command - requires admin privileges
            int kickCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_kick",
                callback: OnKickCommand,
                flags: CommandFlags.Admin,
                info: "Kick a player from the server",
                infoMultiLang: false
            );
            RegisteredCommands["amx_kick"] = kickCmd;

            // 重启服务器命令 - 需要超级管理员权限 / Restart server command - requires super admin privileges
            int restartCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_restart",
                callback: OnRestartCommand,
                flags: CommandFlags.SuperAdmin,
                info: "Restart the game server",
                infoMultiLang: false
            );
            RegisteredCommands["amx_restart"] = restartCmd;
        }

        /// <summary>
        /// 注册服务器命令示例 / Register server commands example
        /// 服务器命令在服务器端执行 / Server commands execute on server side
        /// </summary>
        private static void RegisterServerCommands()
        {
            // 配置重载命令 / Config reload command
            int reloadCmd = AmxModXCommands.RegisterServerCommand(
                command: "amx_reloadcfg",
                callback: OnReloadConfigCommand,
                flags: CommandFlags.Admin,
                info: "Reload server configuration",
                infoMultiLang: false
            );
            RegisteredCommands["amx_reloadcfg"] = reloadCmd;
        }

        /// <summary>
        /// 注册菜单系统示例 / Register menu system example
        /// 演示菜单命令的注册和处理 / Demonstrates menu command registration and handling
        /// </summary>
        private static void RegisterMenuSystem()
        {
            // 注册主菜单ID / Register main menu ID
            _mainMenuId = AmxModXCommands.RegisterMenuId("MainMenu", global: false);

            if (_mainMenuId != -1)
            {
                // 注册菜单命令处理器 / Register menu command handler
                int menuCmd = AmxModXCommands.RegisterMenuCommand(
                    menuId: _mainMenuId,
                    keyMask: MenuKeys.AllNumbers,
                    callback: OnMainMenuCommand
                );
                RegisteredCommands["MainMenu"] = menuCmd;
            }
        }

        // ========== 命令回调函数 / Command Callback Functions ==========

        /// <summary>
        /// 帮助命令回调 / Help command callback
        /// </summary>
        private static void OnHelpCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[HELP] Client {clientId} requested help (Command ID: {commandId}, Flags: {flags})");
            
            // 这里可以向客户端发送帮助信息 / Here you can send help information to the client
            // 示例：显示可用命令列表 / Example: show available command list
            ShowAvailableCommands(clientId);
        }

        /// <summary>
        /// 状态命令回调 / Status command callback
        /// </summary>
        private static void OnStatusCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[STATUS] Client {clientId} requested status (Command ID: {commandId}, Flags: {flags})");
            
            // 这里可以显示玩家状态信息 / Here you can display player status information
            // 示例：显示玩家血量、护甲、武器等 / Example: show player health, armor, weapons, etc.
            ShowPlayerStatus(clientId);
        }

        /// <summary>
        /// VIP命令回调 / VIP command callback
        /// </summary>
        private static void OnVipCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[VIP] Client {clientId} accessed VIP features (Command ID: {commandId}, Flags: {flags})");
            
            // VIP功能处理 / VIP feature handling
            HandleVipFeatures(clientId);
        }

        /// <summary>
        /// 踢出命令回调 / Kick command callback
        /// </summary>
        private static void OnKickCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[KICK] Admin {clientId} executed kick command (Command ID: {commandId}, Flags: {flags})");
            
            // 这里可以实现踢出玩家的逻辑 / Here you can implement player kicking logic
            // 需要解析命令参数，获取目标玩家ID等 / Need to parse command arguments, get target player ID, etc.
            HandleKickPlayer(clientId);
        }

        /// <summary>
        /// 重启命令回调 / Restart command callback
        /// </summary>
        private static void OnRestartCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[RESTART] Super Admin {clientId} initiated server restart (Command ID: {commandId}, Flags: {flags})");
            
            // 服务器重启逻辑 / Server restart logic
            HandleServerRestart(clientId);
        }

        /// <summary>
        /// 配置重载命令回调 / Config reload command callback
        /// </summary>
        private static void OnReloadConfigCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[RELOAD] Admin {clientId} reloaded configuration (Command ID: {commandId}, Flags: {flags})");
            
            // 配置重载逻辑 / Configuration reload logic
            HandleConfigReload(clientId);
        }

        /// <summary>
        /// 主菜单命令回调 / Main menu command callback
        /// </summary>
        private static void OnMainMenuCommand(int clientId, int menuId, int key)
        {
            Console.WriteLine($"[MENU] Client {clientId} selected option {key} in menu {menuId}");
            
            // 根据按键处理不同的菜单选项 / Handle different menu options based on key
            HandleMenuSelection(clientId, menuId, key);
        }

        // ========== 辅助方法 / Helper Methods ==========

        /// <summary>
        /// 显示可用命令 / Show available commands
        /// </summary>
        private static void ShowAvailableCommands(int clientId)
        {
            Console.WriteLine($"Showing available commands to client {clientId}:");
            Console.WriteLine("  help - Display this help message");
            Console.WriteLine("  status - Show your current status");
            Console.WriteLine("  vip - Access VIP features (VIP only)");
        }

        /// <summary>
        /// 显示玩家状态 / Show player status
        /// </summary>
        private static void ShowPlayerStatus(int clientId)
        {
            Console.WriteLine($"Player {clientId} status:");
            Console.WriteLine("  Health: 100/100");
            Console.WriteLine("  Armor: 50/100");
            Console.WriteLine("  Weapon: AK-47");
            Console.WriteLine("  Score: 15/3");
        }

        /// <summary>
        /// 处理VIP功能 / Handle VIP features
        /// </summary>
        private static void HandleVipFeatures(int clientId)
        {
            Console.WriteLine($"Activating VIP features for client {clientId}");
            // VIP功能实现 / VIP feature implementation
        }

        /// <summary>
        /// 处理踢出玩家 / Handle kick player
        /// </summary>
        private static void HandleKickPlayer(int clientId)
        {
            Console.WriteLine($"Admin {clientId} is kicking a player");
            // 踢出逻辑实现 / Kick logic implementation
        }

        /// <summary>
        /// 处理服务器重启 / Handle server restart
        /// </summary>
        private static void HandleServerRestart(int clientId)
        {
            Console.WriteLine($"Server restart initiated by {clientId}");
            // 重启逻辑实现 / Restart logic implementation
        }

        /// <summary>
        /// 处理配置重载 / Handle config reload
        /// </summary>
        private static void HandleConfigReload(int clientId)
        {
            Console.WriteLine($"Configuration reloaded by {clientId}");
            // 配置重载实现 / Config reload implementation
        }

        /// <summary>
        /// 处理菜单选择 / Handle menu selection
        /// </summary>
        private static void HandleMenuSelection(int clientId, int menuId, int key)
        {
            switch (key)
            {
                case 1:
                    Console.WriteLine($"Client {clientId} selected: Player Management");
                    break;
                case 2:
                    Console.WriteLine($"Client {clientId} selected: Server Settings");
                    break;
                case 3:
                    Console.WriteLine($"Client {clientId} selected: Game Options");
                    break;
                case 0:
                    Console.WriteLine($"Client {clientId} selected: Exit Menu");
                    break;
                default:
                    Console.WriteLine($"Client {clientId} selected: Option {key}");
                    break;
            }
        }

        /// <summary>
        /// 显示已注册的命令信息 / Display registered command information
        /// </summary>
        private static void DisplayRegisteredCommands()
        {
            Console.WriteLine("\n========== Registered Commands ==========");

            foreach (var kvp in RegisteredCommands)
            {
                string commandName = kvp.Key;
                int commandId = kvp.Value;

                Console.WriteLine($"Command: {commandName} (ID: {commandId})");

                // 尝试获取命令详细信息 / Try to get command details
                if (commandName != "MainMenu") // 菜单不是标准命令 / Menu is not a standard command
                {
                    CommandType type = DetermineCommandType(commandName);
                    var info = AmxModXCommands.GetCommandInfo(commandId, type);

                    if (info.HasValue)
                    {
                        var cmdInfo = info.Value;
                        Console.WriteLine($"  Info: {cmdInfo.Info}");
                        Console.WriteLine($"  Flags: {cmdInfo.Flags}");
                        Console.WriteLine($"  Listable: {cmdInfo.Listable}");
                        Console.WriteLine($"  Multi-language: {cmdInfo.InfoMultiLang}");
                    }
                }
                Console.WriteLine();
            }

            // 显示命令统计 / Display command statistics
            DisplayCommandStatistics();
        }

        /// <summary>
        /// 确定命令类型 / Determine command type
        /// </summary>
        private static CommandType DetermineCommandType(string commandName)
        {
            if (commandName.StartsWith("amx_"))
            {
                return commandName.Contains("restart") || commandName.Contains("kick")
                    ? CommandType.Console
                    : CommandType.Server;
            }
            return CommandType.Client;
        }

        /// <summary>
        /// 显示命令统计信息 / Display command statistics
        /// </summary>
        private static void DisplayCommandStatistics()
        {
            Console.WriteLine("========== Command Statistics ==========");

            int clientCommands = AmxModXCommands.GetCommandCount(CommandType.Client);
            int consoleCommands = AmxModXCommands.GetCommandCount(CommandType.Console);
            int serverCommands = AmxModXCommands.GetCommandCount(CommandType.Server);

            Console.WriteLine($"Client Commands: {clientCommands}");
            Console.WriteLine($"Console Commands: {consoleCommands}");
            Console.WriteLine($"Server Commands: {serverCommands}");
            Console.WriteLine($"Total Commands: {clientCommands + consoleCommands + serverCommands}");

            // 按权限级别统计 / Statistics by permission level
            int adminCommands = AmxModXCommands.GetCommandCount(CommandType.Console, CommandFlags.Admin);
            int vipCommands = AmxModXCommands.GetCommandCount(CommandType.Client, CommandFlags.Vip);

            Console.WriteLine($"Admin Commands: {adminCommands}");
            Console.WriteLine($"VIP Commands: {vipCommands}");
        }

        /// <summary>
        /// 测试命令功能 / Test command functionality
        /// 模拟命令执行以验证功能 / Simulate command execution to verify functionality
        /// </summary>
        public static void TestCommands()
        {
            Console.WriteLine("\n========== Testing Commands ==========");

            // 模拟客户端命令执行 / Simulate client command execution
            Console.WriteLine("Testing client commands...");
            OnHelpCommand(1, RegisteredCommands["help"], CommandFlags.All);
            OnStatusCommand(1, RegisteredCommands["status"], CommandFlags.All);
            OnVipCommand(2, RegisteredCommands["vip"], CommandFlags.Vip);

            // 模拟控制台命令执行 / Simulate console command execution
            Console.WriteLine("\nTesting console commands...");
            OnKickCommand(0, RegisteredCommands["amx_kick"], CommandFlags.Admin);
            OnRestartCommand(0, RegisteredCommands["amx_restart"], CommandFlags.SuperAdmin);

            // 模拟服务器命令执行 / Simulate server command execution
            Console.WriteLine("\nTesting server commands...");
            OnReloadConfigCommand(0, RegisteredCommands["amx_reloadcfg"], CommandFlags.Admin);

            // 模拟菜单命令执行 / Simulate menu command execution
            Console.WriteLine("\nTesting menu commands...");
            OnMainMenuCommand(1, _mainMenuId, 1);
            OnMainMenuCommand(1, _mainMenuId, 2);
            OnMainMenuCommand(1, _mainMenuId, 0);
        }

        /// <summary>
        /// 清理资源 / Cleanup resources
        /// 注销所有命令并清理系统 / Unregister all commands and cleanup system
        /// </summary>
        public static void Cleanup()
        {
            try
            {
                Console.WriteLine("\nCleaning up AMX Mod X commands...");

                // 注销所有命令 / Unregister all commands
                foreach (var kvp in RegisteredCommands)
                {
                    string commandName = kvp.Key;
                    int commandId = kvp.Value;

                    bool success = AmxModXCommands.UnregisterCommand(commandId);
                    Console.WriteLine($"Unregistered {commandName}: {(success ? "Success" : "Failed")}");
                }

                RegisteredCommands.Clear();

                // 清理命令系统 / Cleanup command system
                AmxModXCommands.Cleanup();

                Console.WriteLine("AMX Mod X commands cleaned up successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to cleanup AMX Mod X commands: {ex.Message}");
            }
        }

        /// <summary>
        /// 检查系统状态 / Check system status
        /// </summary>
        public static void CheckSystemStatus()
        {
            Console.WriteLine("\n========== System Status ==========");
            Console.WriteLine($"System Initialized: {AmxModXCommands.IsInitialized}");
            Console.WriteLine($"Registered Commands: {RegisteredCommands.Count}");
            Console.WriteLine($"Main Menu ID: {_mainMenuId}");
        }
    }

    /// <summary>
    /// 程序入口点 / Program entry point
    /// 演示完整的使用流程 / Demonstrates complete usage workflow
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("AMX Mod X C# Command Registration Example");
            Console.WriteLine("==========================================");

            try
            {
                // 初始化系统 / Initialize system
                AmxModXExample.Initialize();

                // 检查系统状态 / Check system status
                AmxModXExample.CheckSystemStatus();

                // 测试命令功能 / Test command functionality
                AmxModXExample.TestCommands();

                Console.WriteLine("\nPress any key to exit...");
                Console.ReadKey();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
                Console.WriteLine($"Stack Trace: {ex.StackTrace}");
            }
            finally
            {
                // 清理资源 / Cleanup resources
                AmxModXExample.Cleanup();
            }
        }
    }
}
