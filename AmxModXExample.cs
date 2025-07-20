// AmxModXExample.cs - Usage examples for AMX Mod X Command Registration Interface
// 演示如何使用 AMX Mod X 命令注册接口

using System;
using AmxModX.Interop;

namespace AmxModX.Examples
{
    /// <summary>
    /// AMX Mod X 命令注册使用示例 / AMX Mod X command registration usage examples
    /// </summary>
    public class AmxModXExample
    {
        // 存储注册的命令ID / Store registered command IDs
        private static int _helpCommandId = -1;
        private static int _statusCommandId = -1;
        private static int _kickCommandId = -1;
        private static int _menuCommandId = -1;
        private static int _mainMenuId = -1;

        /// <summary>
        /// 初始化示例 / Initialize example
        /// </summary>
        public static void Initialize()
        {
            try
            {
                // 初始化命令系统 / Initialize command system
                AmxModXCommands.Initialize();

                // 注册各种类型的命令 / Register various types of commands
                RegisterCommands();
                RegisterMenus();

                Console.WriteLine("AMX Mod X commands registered successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to initialize AMX Mod X commands: {ex.Message}");
            }
        }

        /// <summary>
        /// 注册命令示例 / Register commands example
        /// </summary>
        private static void RegisterCommands()
        {
            // 注册客户端命令：help - 显示帮助信息 / Register client command: help - show help information
            _helpCommandId = AmxModXCommands.RegisterClientCommand(
                command: "help",
                callback: OnHelpCommand,
                flags: 0, // 所有玩家都可以使用 / All players can use
                info: "Display help information",
                infoMultiLang: false
            );

            // 注册客户端命令：status - 显示玩家状态 / Register client command: status - show player status
            _statusCommandId = AmxModXCommands.RegisterClientCommand(
                command: "status",
                callback: OnStatusCommand,
                flags: 0, // 所有玩家都可以使用 / All players can use
                info: "Display player status",
                infoMultiLang: false
            );

            // 注册控制台命令：kick - 踢出玩家（需要管理员权限） / Register console command: kick - kick player (admin required)
            _kickCommandId = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_kick",
                callback: OnKickCommand,
                flags: 1, // 需要管理员权限 / Requires admin privileges
                info: "Kick a player from the server",
                infoMultiLang: false
            );

            Console.WriteLine($"Registered commands - Help: {_helpCommandId}, Status: {_statusCommandId}, Kick: {_kickCommandId}");
        }

        /// <summary>
        /// 注册菜单示例 / Register menus example
        /// </summary>
        private static void RegisterMenus()
        {
            // 注册菜单ID / Register menu ID
            _mainMenuId = AmxModXCommands.RegisterMenuId("MainMenu", global: false);

            // 注册菜单命令处理器 / Register menu command handler
            _menuCommandId = AmxModXCommands.RegisterMenuCommand(
                menuId: _mainMenuId,
                keyMask: 1023, // 支持按键1-0 / Support keys 1-0
                callback: OnMenuCommand
            );

            Console.WriteLine($"Registered menu - Menu ID: {_mainMenuId}, Command ID: {_menuCommandId}");
        }

        /// <summary>
        /// 帮助命令回调 / Help command callback
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="flags">标志位 / Flags</param>
        private static void OnHelpCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"Help command executed by client {clientId}");
            
            // 这里可以向客户端发送帮助信息 / Here you can send help information to the client
            // 例如：AmxModXClient.PrintToChat(clientId, "Available commands: help, status");
        }

        /// <summary>
        /// 状态命令回调 / Status command callback
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="flags">标志位 / Flags</param>
        private static void OnStatusCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"Status command executed by client {clientId}");
            
            // 这里可以显示玩家状态信息 / Here you can display player status information
            // 例如：显示玩家血量、护甲、武器等 / For example: show player health, armor, weapons, etc.
        }

        /// <summary>
        /// 踢出命令回调 / Kick command callback
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="flags">标志位 / Flags</param>
        private static void OnKickCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"Kick command executed by client {clientId} with flags {flags}");
            
            // 这里可以实现踢出玩家的逻辑 / Here you can implement player kicking logic
            // 需要解析命令参数，获取目标玩家ID等 / Need to parse command arguments, get target player ID, etc.
        }

        /// <summary>
        /// 菜单命令回调 / Menu command callback
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="menuId">菜单ID / Menu ID</param>
        /// <param name="key">按键 / Key</param>
        private static void OnMenuCommand(int clientId, int menuId, int key)
        {
            Console.WriteLine($"Menu command executed by client {clientId}, menu {menuId}, key {key}");
            
            // 根据按键处理不同的菜单选项 / Handle different menu options based on key
            switch (key)
            {
                case 1:
                    Console.WriteLine("Player selected option 1");
                    break;
                case 2:
                    Console.WriteLine("Player selected option 2");
                    break;
                default:
                    Console.WriteLine($"Player selected option {key}");
                    break;
            }
        }

        /// <summary>
        /// 获取命令信息示例 / Get command information example
        /// </summary>
        public static void DisplayCommandInfo()
        {
            if (_helpCommandId != -1)
            {
                var commandInfo = AmxModXCommands.GetCommandInfo(_helpCommandId, CommandType.Client);
                if (commandInfo.HasValue)
                {
                    var info = commandInfo.Value;
                    Console.WriteLine($"Command: {info.Command}");
                    Console.WriteLine($"Info: {info.Info}");
                    Console.WriteLine($"Flags: {info.Flags}");
                    Console.WriteLine($"Listable: {info.Listable}");
                    Console.WriteLine($"Multi-language: {info.InfoMultiLang}");
                }
            }

            // 获取命令数量 / Get command count
            int clientCommandCount = AmxModXCommands.GetCommandCount(CommandType.Client);
            int consoleCommandCount = AmxModXCommands.GetCommandCount(CommandType.Console);
            
            Console.WriteLine($"Client commands: {clientCommandCount}");
            Console.WriteLine($"Console commands: {consoleCommandCount}");
        }

        /// <summary>
        /// 清理资源 / Cleanup resources
        /// </summary>
        public static void Cleanup()
        {
            try
            {
                // 注销命令 / Unregister commands
                if (_helpCommandId != -1)
                {
                    AmxModXCommands.UnregisterCommand(_helpCommandId);
                    _helpCommandId = -1;
                }

                if (_statusCommandId != -1)
                {
                    AmxModXCommands.UnregisterCommand(_statusCommandId);
                    _statusCommandId = -1;
                }

                if (_kickCommandId != -1)
                {
                    AmxModXCommands.UnregisterCommand(_kickCommandId);
                    _kickCommandId = -1;
                }

                if (_menuCommandId != -1)
                {
                    AmxModXCommands.UnregisterCommand(_menuCommandId);
                    _menuCommandId = -1;
                }

                // 清理命令系统 / Cleanup command system
                AmxModXCommands.Cleanup();

                Console.WriteLine("AMX Mod X commands cleaned up successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to cleanup AMX Mod X commands: {ex.Message}");
            }
        }
    }

    /// <summary>
    /// 程序入口点 / Program entry point
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("AMX Mod X Command Registration Example");
            Console.WriteLine("=====================================");

            // 初始化 / Initialize
            AmxModXExample.Initialize();

            // 显示命令信息 / Display command information
            AmxModXExample.DisplayCommandInfo();

            Console.WriteLine("\nPress any key to exit...");
            Console.ReadKey();

            // 清理 / Cleanup
            AmxModXExample.Cleanup();
        }
    }
}
