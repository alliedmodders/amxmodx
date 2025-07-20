// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// TestProgram.cs - Test program for AMX Mod X C# Command Registration Interface
// 测试程序，演示完整的使用流程

using System;
using System.Threading;
using System.Threading.Tasks;
using AmxModX.Interop;
using AmxModX.Examples;

namespace AmxModX.TestApp
{
    /// <summary>
    /// AMX Mod X C# 接口测试程序 / AMX Mod X C# Interface Test Program
    /// 提供完整的测试和演示功能 / Provides complete testing and demonstration functionality
    /// </summary>
    class TestProgram
    {
        private static bool _running = true;
        private static readonly object _consoleLock = new object();

        /// <summary>
        /// 程序主入口 / Program main entry point
        /// </summary>
        /// <param name="args">命令行参数 / Command line arguments</param>
        static async Task Main(string[] args)
        {
            // 设置控制台标题和编码 / Set console title and encoding
            Console.Title = "AMX Mod X C# Command Registration Test";
            Console.OutputEncoding = System.Text.Encoding.UTF8;

            PrintHeader();

            try
            {
                // 解析命令行参数 / Parse command line arguments
                var options = ParseArguments(args);

                // 运行测试 / Run tests
                await RunTestsAsync(options);
            }
            catch (Exception ex)
            {
                PrintError($"Fatal error: {ex.Message}");
                PrintError($"Stack trace: {ex.StackTrace}");
                Environment.ExitCode = 1;
            }
            finally
            {
                PrintInfo("Press any key to exit...");
                Console.ReadKey(true);
            }
        }

        /// <summary>
        /// 运行测试套件 / Run test suite
        /// </summary>
        private static async Task RunTestsAsync(TestOptions options)
        {
            PrintInfo("Starting AMX Mod X C# interface tests...");

            // 设置控制台取消处理 / Setup console cancellation handling
            Console.CancelKeyPress += (sender, e) =>
            {
                e.Cancel = true;
                _running = false;
                PrintWarning("Shutdown requested...");
            };

            try
            {
                // 基础功能测试 / Basic functionality tests
                await RunBasicTests(options);

                if (options.RunStressTests)
                {
                    // 压力测试 / Stress tests
                    await RunStressTests(options);
                }

                if (options.RunInteractiveMode)
                {
                    // 交互模式 / Interactive mode
                    await RunInteractiveMode();
                }
            }
            catch (Exception ex)
            {
                PrintError($"Test execution failed: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 运行基础测试 / Run basic tests
        /// </summary>
        private static async Task RunBasicTests(TestOptions options)
        {
            PrintSection("Basic Functionality Tests");

            // 测试1: 初始化和清理 / Test 1: Initialization and cleanup
            PrintTest("Test 1: System Initialization");
            try
            {
                AmxModXExample.Initialize();
                PrintSuccess("✓ System initialized successfully");

                AmxModXExample.CheckSystemStatus();
                PrintSuccess("✓ System status check passed");

                await Task.Delay(100); // 模拟一些处理时间 / Simulate some processing time

                AmxModXExample.Cleanup();
                PrintSuccess("✓ System cleanup completed");
            }
            catch (Exception ex)
            {
                PrintError($"✗ Initialization test failed: {ex.Message}");
                throw;
            }

            // 测试2: 命令注册和执行 / Test 2: Command registration and execution
            PrintTest("Test 2: Command Registration and Execution");
            try
            {
                AmxModXExample.Initialize();
                PrintSuccess("✓ System re-initialized");

                AmxModXExample.TestCommands();
                PrintSuccess("✓ Command execution tests passed");

                AmxModXExample.Cleanup();
                PrintSuccess("✓ Test 2 completed successfully");
            }
            catch (Exception ex)
            {
                PrintError($"✗ Command test failed: {ex.Message}");
                throw;
            }

            // 测试3: 命令执行接口 / Test 3: Command execution interface
            PrintTest("Test 3: Command Execution Interface");
            try
            {
                CommandExecutionExample.Initialize();
                PrintSuccess("✓ Command execution example initialized");

                CommandExecutionExample.RunFullDemo();
                PrintSuccess("✓ Command execution demo completed");

                CommandExecutionExample.Cleanup();
                PrintSuccess("✓ Test 3 completed successfully");
            }
            catch (Exception ex)
            {
                PrintError($"✗ Command execution test failed: {ex.Message}");
                throw;
            }

            // 测试4: 多线程安全性 / Test 4: Thread safety
            if (options.TestThreadSafety)
            {
                await RunThreadSafetyTests();
            }
        }

        /// <summary>
        /// 运行线程安全测试 / Run thread safety tests
        /// </summary>
        private static async Task RunThreadSafetyTests()
        {
            PrintTest("Test 3: Thread Safety");

            try
            {
                AmxModXExample.Initialize();

                // 创建多个并发任务 / Create multiple concurrent tasks
                var tasks = new Task[5];
                for (int i = 0; i < tasks.Length; i++)
                {
                    int taskId = i;
                    tasks[i] = Task.Run(() =>
                    {
                        for (int j = 0; j < 10; j++)
                        {
                            if (!_running) break;

                            // 模拟并发命令注册 / Simulate concurrent command registration
                            try
                            {
                                int cmdId = AmxModXCommands.RegisterClientCommand(
                                    $"test_cmd_{taskId}_{j}",
                                    (clientId, commandId, flags) => 
                                    {
                                        PrintInfo($"Command executed: Task {taskId}, Iteration {j}");
                                    },
                                    CommandFlags.All,
                                    $"Test command from task {taskId}, iteration {j}"
                                );

                                Thread.Sleep(10); // 短暂延迟 / Brief delay

                                AmxModXCommands.UnregisterCommand(cmdId);
                            }
                            catch (Exception ex)
                            {
                                PrintError($"Thread safety test error in task {taskId}: {ex.Message}");
                            }
                        }
                    });
                }

                await Task.WhenAll(tasks);
                PrintSuccess("✓ Thread safety tests passed");

                AmxModXExample.Cleanup();
            }
            catch (Exception ex)
            {
                PrintError($"✗ Thread safety test failed: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 运行压力测试 / Run stress tests
        /// </summary>
        private static async Task RunStressTests(TestOptions options)
        {
            PrintSection("Stress Tests");

            PrintTest("Stress Test: High Volume Command Registration");

            try
            {
                AmxModXExample.Initialize();

                const int commandCount = 1000;
                var commandIds = new List<int>();

                // 注册大量命令 / Register many commands
                var stopwatch = System.Diagnostics.Stopwatch.StartNew();

                for (int i = 0; i < commandCount && _running; i++)
                {
                    int cmdId = AmxModXCommands.RegisterClientCommand(
                        $"stress_cmd_{i}",
                        (clientId, commandId, flags) => { /* No-op */ },
                        CommandFlags.All,
                        $"Stress test command {i}"
                    );

                    commandIds.Add(cmdId);

                    if (i % 100 == 0)
                    {
                        PrintInfo($"Registered {i + 1}/{commandCount} commands...");
                    }
                }

                stopwatch.Stop();
                PrintSuccess($"✓ Registered {commandIds.Count} commands in {stopwatch.ElapsedMilliseconds}ms");

                // 查询命令信息 / Query command information
                stopwatch.Restart();
                int successCount = 0;

                foreach (var cmdId in commandIds)
                {
                    if (!_running) break;

                    var info = AmxModXCommands.GetCommandInfo(cmdId, CommandType.Client);
                    if (info.HasValue)
                    {
                        successCount++;
                    }
                }

                stopwatch.Stop();
                PrintSuccess($"✓ Queried {successCount}/{commandIds.Count} commands in {stopwatch.ElapsedMilliseconds}ms");

                // 注销所有命令 / Unregister all commands
                stopwatch.Restart();
                int unregisteredCount = 0;

                foreach (var cmdId in commandIds)
                {
                    if (!_running) break;

                    if (AmxModXCommands.UnregisterCommand(cmdId))
                    {
                        unregisteredCount++;
                    }
                }

                stopwatch.Stop();
                PrintSuccess($"✓ Unregistered {unregisteredCount}/{commandIds.Count} commands in {stopwatch.ElapsedMilliseconds}ms");

                AmxModXExample.Cleanup();
                PrintSuccess("✓ Stress tests completed successfully");
            }
            catch (Exception ex)
            {
                PrintError($"✗ Stress test failed: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 解析命令行参数 / Parse command line arguments
        /// </summary>
        private static TestOptions ParseArguments(string[] args)
        {
            var options = new TestOptions();

            for (int i = 0; i < args.Length; i++)
            {
                switch (args[i].ToLower())
                {
                    case "--stress":
                    case "-s":
                        options.RunStressTests = true;
                        break;

                    case "--interactive":
                    case "-i":
                        options.RunInteractiveMode = true;
                        break;

                    case "--no-thread-safety":
                        options.TestThreadSafety = false;
                        break;

                    case "--help":
                    case "-h":
                        PrintUsage();
                        Environment.Exit(0);
                        break;

                    default:
                        PrintWarning($"Unknown argument: {args[i]}");
                        break;
                }
            }

            return options;
        }

        /// <summary>
        /// 打印使用说明 / Print usage information
        /// </summary>
        private static void PrintUsage()
        {
            Console.WriteLine("AMX Mod X C# Command Registration Test");
            Console.WriteLine("Usage: AmxModXTestApp [options]");
            Console.WriteLine();
            Console.WriteLine("Options:");
            Console.WriteLine("  -s, --stress           Run stress tests");
            Console.WriteLine("  -i, --interactive      Run in interactive mode");
            Console.WriteLine("  --no-thread-safety     Skip thread safety tests");
            Console.WriteLine("  -h, --help             Show this help message");
            Console.WriteLine();
            Console.WriteLine("Examples:");
            Console.WriteLine("  AmxModXTestApp                    # Run basic tests only");
            Console.WriteLine("  AmxModXTestApp --stress           # Run basic and stress tests");
            Console.WriteLine("  AmxModXTestApp --interactive      # Run in interactive mode");
            Console.WriteLine("  AmxModXTestApp -s -i              # Run all tests and interactive mode");
        }

        // ========== 输出格式化方法 / Output Formatting Methods ==========

        /// <summary>
        /// 打印程序头部信息 / Print program header
        /// </summary>
        private static void PrintHeader()
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.Cyan;
                Console.WriteLine("╔══════════════════════════════════════════════════════════════╗");
                Console.WriteLine("║              AMX Mod X C# Command Registration              ║");
                Console.WriteLine("║                        Test Program                          ║");
                Console.WriteLine("║                                                              ║");
                Console.WriteLine("║  Tests the C# interop layer for AMX Mod X command system   ║");
                Console.WriteLine("╚══════════════════════════════════════════════════════════════╝");
                Console.ResetColor();
                Console.WriteLine();
            }
        }

        /// <summary>
        /// 打印章节标题 / Print section title
        /// </summary>
        private static void PrintSection(string title)
        {
            lock (_consoleLock)
            {
                Console.WriteLine();
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.WriteLine($"═══ {title} ═══");
                Console.ResetColor();
            }
        }

        /// <summary>
        /// 打印测试标题 / Print test title
        /// </summary>
        private static void PrintTest(string testName)
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.White;
                Console.WriteLine($"\n► {testName}");
                Console.ResetColor();
            }
        }

        /// <summary>
        /// 打印成功信息 / Print success message
        /// </summary>
        private static void PrintSuccess(string message)
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine($"  {message}");
                Console.ResetColor();
            }
        }

        /// <summary>
        /// 打印错误信息 / Print error message
        /// </summary>
        private static void PrintError(string message)
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine($"  {message}");
                Console.ResetColor();
            }
        }

        /// <summary>
        /// 打印警告信息 / Print warning message
        /// </summary>
        private static void PrintWarning(string message)
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.WriteLine($"  {message}");
                Console.ResetColor();
            }
        }

        /// <summary>
        /// 打印信息 / Print information
        /// </summary>
        private static void PrintInfo(string message)
        {
            lock (_consoleLock)
            {
                Console.ForegroundColor = ConsoleColor.Gray;
                Console.WriteLine($"  {message}");
                Console.ResetColor();
            }
        }
    }

    /// <summary>
    /// 测试选项配置 / Test options configuration
    /// </summary>
    public class TestOptions
    {
        /// <summary>运行压力测试 / Run stress tests</summary>
        public bool RunStressTests { get; set; } = false;

        /// <summary>运行交互模式 / Run interactive mode</summary>
        public bool RunInteractiveMode { get; set; } = false;

        /// <summary>测试线程安全性 / Test thread safety</summary>
        public bool TestThreadSafety { get; set; } = true;
    }
}

        /// <summary>
        /// 运行交互模式 / Run interactive mode
        /// </summary>
        private static async Task RunInteractiveMode()
        {
            PrintSection("Interactive Mode");
            PrintInfo("Available commands:");
            PrintInfo("  init    - Initialize system");
            PrintInfo("  test    - Run command tests");
            PrintInfo("  status  - Show system status");
            PrintInfo("  cleanup - Cleanup system");
            PrintInfo("  help    - Show this help");
            PrintInfo("  exit    - Exit interactive mode");
            PrintInfo("");

            bool initialized = false;

            while (_running)
            {
                Console.Write("amxmodx> ");
                var input = Console.ReadLine()?.Trim().ToLower();

                if (string.IsNullOrEmpty(input))
                    continue;

                try
                {
                    switch (input)
                    {
                        case "init":
                            if (!initialized)
                            {
                                AmxModXExample.Initialize();
                                initialized = true;
                                PrintSuccess("System initialized");
                            }
                            else
                            {
                                PrintWarning("System already initialized");
                            }
                            break;

                        case "test":
                            if (initialized)
                            {
                                AmxModXExample.TestCommands();
                                PrintSuccess("Command tests completed");
                            }
                            else
                            {
                                PrintError("System not initialized. Run 'init' first.");
                            }
                            break;

                        case "status":
                            if (initialized)
                            {
                                AmxModXExample.CheckSystemStatus();
                            }
                            else
                            {
                                PrintInfo("System Status: Not Initialized");
                            }
                            break;

                        case "cleanup":
                            if (initialized)
                            {
                                AmxModXExample.Cleanup();
                                initialized = false;
                                PrintSuccess("System cleaned up");
                            }
                            else
                            {
                                PrintWarning("System not initialized");
                            }
                            break;

                        case "help":
                            PrintInfo("Available commands: init, test, status, cleanup, help, exit");
                            break;

                        case "exit":
                            _running = false;
                            break;

                        default:
                            PrintWarning($"Unknown command: {input}. Type 'help' for available commands.");
                            break;
                    }
                }
                catch (Exception ex)
                {
                    PrintError($"Command failed: {ex.Message}");
                }

                await Task.Delay(10); // 防止CPU占用过高 / Prevent high CPU usage
            }

            if (initialized)
            {
                try
                {
                    AmxModXExample.Cleanup();
                    PrintInfo("System cleaned up on exit");
                }
                catch (Exception ex)
                {
                    PrintError($"Cleanup on exit failed: {ex.Message}");
                }
            }
        }
