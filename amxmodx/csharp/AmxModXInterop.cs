// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// AmxModXInterop.cs - C# Interop Layer for AMX Mod X Command Registration Interface
// Provides managed interface for command registration with AMX Mod X

using System;
using System.Runtime.InteropServices;

namespace AmxModX.Interop
{
    /// <summary>
    /// 命令类型枚举 / Command type enumeration
    /// </summary>
    public enum CommandType
    {
        /// <summary>控制台命令 / Console command</summary>
        Console = 0,
        /// <summary>客户端命令 / Client command</summary>
        Client = 1,
        /// <summary>服务器命令 / Server command</summary>
        Server = 2
    }

    /// <summary>
    /// 命令回调委托 / Command callback delegate
    /// </summary>
    /// <param name="clientId">客户端ID / Client ID</param>
    /// <param name="commandId">命令ID / Command ID</param>
    /// <param name="flags">标志位 / Flags</param>
    public delegate void CommandCallback(int clientId, int commandId, int flags);

    /// <summary>
    /// 菜单回调委托 / Menu callback delegate
    /// </summary>
    /// <param name="clientId">客户端ID / Client ID</param>
    /// <param name="menuId">菜单ID / Menu ID</param>
    /// <param name="key">按键 / Key</param>
    public delegate void MenuCallback(int clientId, int menuId, int key);

    /// <summary>
    /// 命令信息结构 / Command information structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct CommandInfo
    {
        /// <summary>命令字符串 / Command string</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
        public string Command;

        /// <summary>命令信息 / Command info</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
        public string Info;

        /// <summary>访问标志 / Access flags</summary>
        public int Flags;

        /// <summary>命令ID / Command ID</summary>
        public int CommandId;

        /// <summary>信息是否支持多语言 / Whether info supports multi-language</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool InfoMultiLang;

        /// <summary>是否可列出 / Whether listable</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool Listable;
    }

    /// <summary>
    /// AMX Mod X 原生函数导入 / AMX Mod X native function imports
    /// 统一管理所有DLL导入声明 / Centralized management of all DLL import declarations
    /// </summary>
    internal static class NativeMethods
    {
        // 根据平台选择正确的DLL名称 / Choose correct DLL name based on platform
#if UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || NETFRAMEWORK
        private const string DllName = "amxmodx_mm.dll";
#elif UNITY_EDITOR_OSX || UNITY_STANDALONE_OSX
        private const string DllName = "amxmodx_mm.dylib";
#else
        private const string DllName = "amxmodx_mm.so";
#endif

        /// <summary>
        /// 初始化C#桥接层 / Initialize C# bridge
        /// </summary>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "InitializeCSharpBridge")]
        internal static extern void InitializeCSharpBridge();

        /// <summary>
        /// 清理C#桥接层 / Cleanup C# bridge
        /// </summary>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "CleanupCSharpBridge")]
        internal static extern void CleanupCSharpBridge();

        /// <summary>
        /// 注册控制台命令 / Register console command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志 / Access flags</param>
        /// <param name="info">命令信息 / Command info</param>
        /// <param name="infoMultiLang">信息是否支持多语言 / Whether info supports multi-language</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "RegisterConsoleCommand")]
        internal static extern int RegisterConsoleCommand(
            [MarshalAs(UnmanagedType.LPStr)] string command,
            CommandCallback callback,
            int flags,
            [MarshalAs(UnmanagedType.LPStr)] string info,
            [MarshalAs(UnmanagedType.I1)] bool infoMultiLang);

        /// <summary>
        /// 注册客户端命令 / Register client command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志 / Access flags</param>
        /// <param name="info">命令信息 / Command info</param>
        /// <param name="infoMultiLang">信息是否支持多语言 / Whether info supports multi-language</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "RegisterClientCommand")]
        internal static extern int RegisterClientCommand(
            [MarshalAs(UnmanagedType.LPStr)] string command,
            CommandCallback callback,
            int flags,
            [MarshalAs(UnmanagedType.LPStr)] string info,
            [MarshalAs(UnmanagedType.I1)] bool infoMultiLang);

        /// <summary>
        /// 注册服务器命令 / Register server command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志 / Access flags</param>
        /// <param name="info">命令信息 / Command info</param>
        /// <param name="infoMultiLang">信息是否支持多语言 / Whether info supports multi-language</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "RegisterServerCommand")]
        internal static extern int RegisterServerCommand(
            [MarshalAs(UnmanagedType.LPStr)] string command,
            CommandCallback callback,
            int flags,
            [MarshalAs(UnmanagedType.LPStr)] string info,
            [MarshalAs(UnmanagedType.I1)] bool infoMultiLang);

        /// <summary>
        /// 注册菜单命令 / Register menu command
        /// </summary>
        /// <param name="menuId">菜单ID / Menu ID</param>
        /// <param name="keyMask">按键掩码 / Key mask</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "RegisterMenuCommand")]
        internal static extern int RegisterMenuCommand(
            int menuId,
            int keyMask,
            MenuCallback callback);

        /// <summary>
        /// 注册菜单ID / Register menu ID
        /// </summary>
        /// <param name="menuName">菜单名称 / Menu name</param>
        /// <param name="global">是否全局 / Whether global</param>
        /// <returns>菜单ID，失败返回-1 / Menu ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "RegisterMenuId")]
        internal static extern int RegisterMenuId(
            [MarshalAs(UnmanagedType.LPStr)] string menuName,
            [MarshalAs(UnmanagedType.I1)] bool global);

        /// <summary>
        /// 获取命令信息 / Get command information
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="outInfo">输出命令信息 / Output command info</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandInfo")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetCommandInfo(
            int commandId,
            CommandType commandType,
            out CommandInfo outInfo);

        /// <summary>
        /// 获取命令数量 / Get command count
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志 / Access flags</param>
        /// <returns>命令数量 / Command count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandCount")]
        internal static extern int GetCommandCount(
            CommandType commandType,
            int accessFlags);

        /// <summary>
        /// 注销命令 / Unregister command
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "UnregisterCommand")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool UnregisterCommand(int commandId);
    }

    /// <summary>
    /// AMX Mod X 命令注册管理器 / AMX Mod X command registration manager
    /// 提供高级的命令注册和管理接口 / Provides high-level command registration and management interface
    /// </summary>
    public static class AmxModXCommands
    {
        private static bool _initialized = false;
        private static readonly object _lockObject = new object();

        /// <summary>
        /// 初始化命令系统 / Initialize command system
        /// 必须在使用其他接口前调用 / Must be called before using other interfaces
        /// </summary>
        /// <exception cref="InvalidOperationException">当初始化失败时抛出 / Thrown when initialization fails</exception>
        public static void Initialize()
        {
            lock (_lockObject)
            {
                if (!_initialized)
                {
                    try
                    {
                        NativeMethods.InitializeCSharpBridge();
                        _initialized = true;
                    }
                    catch (Exception ex)
                    {
                        throw new InvalidOperationException("Failed to initialize AMX Mod X command system.", ex);
                    }
                }
            }
        }

        /// <summary>
        /// 清理命令系统 / Cleanup command system
        /// 释放所有资源并注销所有命令 / Release all resources and unregister all commands
        /// </summary>
        public static void Cleanup()
        {
            lock (_lockObject)
            {
                if (_initialized)
                {
                    try
                    {
                        NativeMethods.CleanupCSharpBridge();
                        _initialized = false;
                    }
                    catch (Exception)
                    {
                        // 忽略清理时的异常 / Ignore exceptions during cleanup
                        _initialized = false;
                    }
                }
            }
        }

        /// <summary>
        /// 检查系统是否已初始化 / Check if system is initialized
        /// </summary>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        private static void EnsureInitialized()
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");
        }

        /// <summary>
        /// 注册控制台命令 / Register console command
        /// 控制台命令可以从服务器控制台执行 / Console commands can be executed from server console
        /// </summary>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <param name="callback">回调函数，不能为null / Callback function, cannot be null</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        /// <exception cref="ArgumentNullException">当回调函数为null时抛出 / Thrown when callback is null</exception>
        public static int RegisterConsoleCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterConsoleCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册客户端命令 / Register client command
        /// 客户端命令可以由玩家在游戏中执行 / Client commands can be executed by players in-game
        /// </summary>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <param name="callback">回调函数，不能为null / Callback function, cannot be null</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        /// <exception cref="ArgumentNullException">当回调函数为null时抛出 / Thrown when callback is null</exception>
        public static int RegisterClientCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterClientCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册服务器命令 / Register server command
        /// 服务器命令在服务器端执行，通常用于管理功能 / Server commands execute on server side, typically for admin functions
        /// </summary>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <param name="callback">回调函数，不能为null / Callback function, cannot be null</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        /// <exception cref="ArgumentNullException">当回调函数为null时抛出 / Thrown when callback is null</exception>
        public static int RegisterServerCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterServerCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册菜单命令 / Register menu command
        /// 菜单命令处理玩家在菜单中的按键选择 / Menu commands handle player key selections in menus
        /// </summary>
        /// <param name="menuId">菜单ID / Menu ID</param>
        /// <param name="keyMask">按键掩码，指定支持的按键 / Key mask, specifies supported keys</param>
        /// <param name="callback">回调函数，不能为null / Callback function, cannot be null</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentNullException">当回调函数为null时抛出 / Thrown when callback is null</exception>
        public static int RegisterMenuCommand(int menuId, int keyMask, MenuCallback callback)
        {
            EnsureInitialized();

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterMenuCommand(menuId, keyMask, callback);
        }

        /// <summary>
        /// 注册菜单ID / Register menu ID
        /// 创建一个新的菜单标识符 / Create a new menu identifier
        /// </summary>
        /// <param name="menuName">菜单名称，不能为空 / Menu name, cannot be empty</param>
        /// <param name="global">是否全局菜单，默认为false / Whether global menu, default false</param>
        /// <returns>菜单ID，失败返回-1 / Menu ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当菜单名称为空时抛出 / Thrown when menu name is empty</exception>
        public static int RegisterMenuId(string menuName, bool global = false)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(menuName))
                throw new ArgumentException("Menu name cannot be null or empty.", nameof(menuName));

            return NativeMethods.RegisterMenuId(menuName, global);
        }

        /// <summary>
        /// 获取命令信息 / Get command information
        /// 检索指定命令的详细信息 / Retrieve detailed information for specified command
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <returns>命令信息，失败返回null / Command info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static CommandInfo? GetCommandInfo(int commandId, CommandType commandType)
        {
            EnsureInitialized();

            if (NativeMethods.GetCommandInfo(commandId, commandType, out CommandInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 获取命令数量 / Get command count
        /// 统计指定类型的命令数量 / Count commands of specified type
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志过滤，默认为-1（所有） / Access flags filter, default -1 (all)</param>
        /// <returns>命令数量 / Command count</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetCommandCount(CommandType commandType, int accessFlags = -1)
        {
            EnsureInitialized();

            return NativeMethods.GetCommandCount(commandType, accessFlags);
        }

        /// <summary>
        /// 注销命令 / Unregister command
        /// 移除指定的命令注册 / Remove specified command registration
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <returns>是否成功注销 / Whether successfully unregistered</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool UnregisterCommand(int commandId)
        {
            EnsureInitialized();

            return NativeMethods.UnregisterCommand(commandId);
        }

        /// <summary>
        /// 获取系统初始化状态 / Get system initialization status
        /// </summary>
        /// <returns>是否已初始化 / Whether initialized</returns>
        public static bool IsInitialized => _initialized;
    }

    /// <summary>
    /// 命令访问标志常量 / Command access flag constants
    /// 定义常用的访问权限标志 / Define common access permission flags
    /// </summary>
    public static class CommandFlags
    {
        /// <summary>所有用户可访问 / Accessible to all users</summary>
        public const int All = 0;

        /// <summary>管理员权限 / Admin privileges</summary>
        public const int Admin = 1;

        /// <summary>隐藏命令（不在列表中显示） / Hidden command (not shown in lists)</summary>
        public const int Hidden = -1;

        /// <summary>超级管理员权限 / Super admin privileges</summary>
        public const int SuperAdmin = 2;

        /// <summary>VIP用户权限 / VIP user privileges</summary>
        public const int Vip = 4;

        /// <summary>调试权限 / Debug privileges</summary>
        public const int Debug = 8;
    }

    /// <summary>
    /// 菜单按键掩码常量 / Menu key mask constants
    /// 定义菜单支持的按键组合 / Define supported key combinations for menus
    /// </summary>
    public static class MenuKeys
    {
        /// <summary>按键1 / Key 1</summary>
        public const int Key1 = 1 << 0;

        /// <summary>按键2 / Key 2</summary>
        public const int Key2 = 1 << 1;

        /// <summary>按键3 / Key 3</summary>
        public const int Key3 = 1 << 2;

        /// <summary>按键4 / Key 4</summary>
        public const int Key4 = 1 << 3;

        /// <summary>按键5 / Key 5</summary>
        public const int Key5 = 1 << 4;

        /// <summary>按键6 / Key 6</summary>
        public const int Key6 = 1 << 5;

        /// <summary>按键7 / Key 7</summary>
        public const int Key7 = 1 << 6;

        /// <summary>按键8 / Key 8</summary>
        public const int Key8 = 1 << 7;

        /// <summary>按键9 / Key 9</summary>
        public const int Key9 = 1 << 8;

        /// <summary>按键0 / Key 0</summary>
        public const int Key0 = 1 << 9;

        /// <summary>所有数字键 / All number keys</summary>
        public const int AllNumbers = Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | Key0;
    }
}
