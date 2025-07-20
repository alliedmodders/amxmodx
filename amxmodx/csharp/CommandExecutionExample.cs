// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// CommandExecutionExample.cs - Command execution examples for AMX Mod X C# Interface
// 命令执行示例，演示如何使用C#接口执行和管理AMX Mod X命令

using System;
using System.Collections.Generic;
using System.Linq;
using AmxModX.Interop;

namespace AmxModX.Examples
{
    /// <summary>
    /// AMX Mod X 命令执行示例 / AMX Mod X command execution examples
    /// 演示命令执行、参数读取和命令查询功能 / Demonstrates command execution, argument reading, and command query functionality
    /// </summary>
    public static class CommandExecutionExample
    {
        private static readonly Dictionary<string, int> _registeredCommands = new Dictionary<string, int>();

        /// <summary>
        /// 初始化命令执行示例 / Initialize command execution example
        /// </summary>
        public static void Initialize()
        {
            Console.WriteLine("Initializing Command Execution Example...");

            try
            {
                // 初始化AMX Mod X命令系统 / Initialize AMX Mod X command system
                AmxModXCommands.Initialize();

                // 注册演示命令 / Register demonstration commands
                RegisterDemoCommands();

                Console.WriteLine("Command Execution Example initialized successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to initialize Command Execution Example: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 注册演示命令 / Register demonstration commands
        /// </summary>
        private static void RegisterDemoCommands()
        {
            // 注册执行命令的演示命令 / Register command to demonstrate command execution
            int execCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_exec_demo",
                callback: OnExecuteDemo,
                flags: CommandFlags.Admin,
                info: "Demonstrate command execution from C#"
            );
            _registeredCommands["amx_exec_demo"] = execCmd;

            // 注册参数解析演示命令 / Register command to demonstrate argument parsing
            int argsCmd = AmxModXCommands.RegisterClientCommand(
                command: "test_args",
                callback: OnTestArgs,
                flags: CommandFlags.All,
                info: "Test command argument parsing - Usage: test_args <string> <int> <float>"
            );
            _registeredCommands["test_args"] = argsCmd;

            // 注册命令查询演示命令 / Register command to demonstrate command querying
            int queryCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_query_demo",
                callback: OnQueryDemo,
                flags: CommandFlags.Admin,
                info: "Demonstrate command querying functionality"
            );
            _registeredCommands["amx_query_demo"] = queryCmd;

            Console.WriteLine($"Registered {_registeredCommands.Count} demonstration commands");
        }

        // ========== 命令回调函数 / Command Callback Functions ==========

        /// <summary>
        /// 执行演示命令回调 / Execute demonstration command callback
        /// </summary>
        private static void OnExecuteDemo(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[EXEC DEMO] Executed by client {clientId}");

            try
            {
                // 演示各种命令执行 / Demonstrate various command executions
                Console.WriteLine("[EXEC DEMO] Demonstrating command execution...");

                // 执行服务器命令 / Execute server commands
                Console.WriteLine("[EXEC DEMO] Executing server commands:");
                AmxModXCommands.ExecuteServerCommand("echo [C# DEMO] Server command executed");
                AmxModXCommands.ExecuteServerCommand("users");

                // 执行客户端命令 / Execute client commands
                Console.WriteLine("[EXEC DEMO] Executing client commands:");
                if (clientId > 0)
                {
                    AmxModXCommands.ExecuteClientCommand(clientId, "say [C# DEMO] Hello from C# command execution!");
                }
                else
                {
                    AmxModXCommands.ExecuteClientCommand(0, "say [C# DEMO] Broadcast message from C#");
                }

                // 执行控制台命令 / Execute console commands
                Console.WriteLine("[EXEC DEMO] Executing console commands:");
                AmxModXCommands.ExecuteConsoleCommand(0, "version");

                Console.WriteLine("[EXEC DEMO] Command execution demonstration completed");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[EXEC DEMO] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 测试参数命令回调 / Test arguments command callback
        /// </summary>
        private static void OnTestArgs(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[ARGS TEST] Executed by client {clientId}");

            try
            {
                // 读取命令参数 / Read command arguments
                int argc = AmxModXCommands.GetCommandArgCount();
                Console.WriteLine($"[ARGS TEST] Argument count: {argc}");

                if (argc < 4)
                {
                    Console.WriteLine("[ARGS TEST] Usage: test_args <string> <int> <float>");
                    Console.WriteLine("[ARGS TEST] Example: test_args hello 123 45.67");
                    return;
                }

                // 读取各种类型的参数 / Read various types of arguments
                string command = AmxModXCommands.GetCommandArg(0);
                string stringArg = AmxModXCommands.GetCommandArg(1);
                string intArgStr = AmxModXCommands.GetCommandArg(2);
                string floatArgStr = AmxModXCommands.GetCommandArg(3);

                // 使用类型化读取方法 / Use typed reading methods
                int intArg = AmxModXCommands.GetCommandArgInt(2);
                float floatArg = AmxModXCommands.GetCommandArgFloat(3);

                // 获取所有参数 / Get all arguments
                string allArgs = AmxModXCommands.GetCommandArgs();

                // 显示解析结果 / Display parsing results
                Console.WriteLine($"[ARGS TEST] Command: '{command}'");
                Console.WriteLine($"[ARGS TEST] String arg: '{stringArg}'");
                Console.WriteLine($"[ARGS TEST] Int arg: '{intArgStr}' -> {intArg}");
                Console.WriteLine($"[ARGS TEST] Float arg: '{floatArgStr}' -> {floatArg}");
                Console.WriteLine($"[ARGS TEST] All args: '{allArgs}'");

                // 验证解析 / Validate parsing
                if (int.TryParse(intArgStr, out int expectedInt) && expectedInt == intArg)
                {
                    Console.WriteLine("[ARGS TEST] ✓ Integer parsing correct");
                }
                else
                {
                    Console.WriteLine("[ARGS TEST] ✗ Integer parsing mismatch");
                }

                if (float.TryParse(floatArgStr, out float expectedFloat) && Math.Abs(expectedFloat - floatArg) < 0.001f)
                {
                    Console.WriteLine("[ARGS TEST] ✓ Float parsing correct");
                }
                else
                {
                    Console.WriteLine("[ARGS TEST] ✗ Float parsing mismatch");
                }

                Console.WriteLine("[ARGS TEST] Argument parsing test completed");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ARGS TEST] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 查询演示命令回调 / Query demonstration command callback
        /// </summary>
        private static void OnQueryDemo(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[QUERY DEMO] Executed by client {clientId}");

            try
            {
                // 演示命令查询功能 / Demonstrate command query functionality
                Console.WriteLine("[QUERY DEMO] Demonstrating command querying...");

                // 查找特定命令 / Find specific commands
                Console.WriteLine("[QUERY DEMO] Searching for specific commands:");
                
                var commands = new[] { "help", "status", "amx_kick", "amx_restart" };
                var types = new[] { CommandType.Client, CommandType.Client, CommandType.Console, CommandType.Console };

                for (int i = 0; i < commands.Length; i++)
                {
                    var cmdInfo = AmxModXCommands.FindCommand(commands[i], types[i]);
                    if (cmdInfo.HasValue)
                    {
                        var cmd = cmdInfo.Value;
                        Console.WriteLine($"[QUERY DEMO]   Found '{cmd.Command}': {cmd.Info} (Flags: {cmd.Flags})");
                    }
                    else
                    {
                        Console.WriteLine($"[QUERY DEMO]   Command '{commands[i]}' not found");
                    }
                }

                // 获取命令统计 / Get command statistics
                Console.WriteLine("[QUERY DEMO] Command statistics:");
                int clientCmds = AmxModXCommands.GetCommandsCount(CommandType.Client);
                int consoleCmds = AmxModXCommands.GetCommandsCount(CommandType.Console);
                int serverCmds = AmxModXCommands.GetCommandsCount(CommandType.Server);

                Console.WriteLine($"[QUERY DEMO]   Client commands: {clientCmds}");
                Console.WriteLine($"[QUERY DEMO]   Console commands: {consoleCmds}");
                Console.WriteLine($"[QUERY DEMO]   Server commands: {serverCmds}");
                Console.WriteLine($"[QUERY DEMO]   Total commands: {clientCmds + consoleCmds + serverCmds}");

                // 列出前几个客户端命令 / List first few client commands
                Console.WriteLine("[QUERY DEMO] First 5 client commands:");
                var allClientCmds = AmxModXCommands.GetAllCommands(CommandType.Client);
                foreach (var cmd in allClientCmds.Take(5))
                {
                    Console.WriteLine($"[QUERY DEMO]   {cmd.Command} - {cmd.Info}");
                }

                // 按权限过滤命令 / Filter commands by permissions
                Console.WriteLine("[QUERY DEMO] Admin-only commands:");
                int adminCmds = AmxModXCommands.GetCommandsCount(CommandType.Console, CommandFlags.Admin);
                Console.WriteLine($"[QUERY DEMO]   Admin console commands: {adminCmds}");

                // 检查命令存在性 / Check command existence
                Console.WriteLine("[QUERY DEMO] Command existence checks:");
                bool helpExists = AmxModXCommands.CommandExists("help", CommandType.Client);
                bool fakeExists = AmxModXCommands.CommandExists("nonexistent_cmd", CommandType.Client);
                Console.WriteLine($"[QUERY DEMO]   'help' exists: {helpExists}");
                Console.WriteLine($"[QUERY DEMO]   'nonexistent_cmd' exists: {fakeExists}");

                Console.WriteLine("[QUERY DEMO] Command querying demonstration completed");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[QUERY DEMO] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 运行完整的命令执行演示 / Run complete command execution demonstration
        /// </summary>
        public static void RunFullDemo()
        {
            Console.WriteLine("\n========== Full Command Execution Demo ==========");

            try
            {
                // 模拟执行注册的演示命令 / Simulate executing registered demo commands
                Console.WriteLine("Simulating demo command executions...");

                OnExecuteDemo(0, _registeredCommands["amx_exec_demo"], CommandFlags.Admin);
                OnTestArgs(1, _registeredCommands["test_args"], CommandFlags.All);
                OnQueryDemo(0, _registeredCommands["amx_query_demo"], CommandFlags.Admin);

                Console.WriteLine("Full command execution demo completed!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Demo execution error: {ex.Message}");
            }
        }

        /// <summary>
        /// 清理资源 / Cleanup resources
        /// </summary>
        public static void Cleanup()
        {
            try
            {
                Console.WriteLine("Cleaning up Command Execution Example...");

                // 注销所有演示命令 / Unregister all demo commands
                foreach (var kvp in _registeredCommands)
                {
                    bool success = AmxModXCommands.UnregisterCommand(kvp.Value);
                    Console.WriteLine($"Unregistered '{kvp.Key}': {(success ? "Success" : "Failed")}");
                }

                _registeredCommands.Clear();

                // 清理AMX Mod X命令系统 / Cleanup AMX Mod X command system
                AmxModXCommands.Cleanup();

                Console.WriteLine("Command Execution Example cleaned up successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Cleanup error: {ex.Message}");
            }
        }
    }
}
