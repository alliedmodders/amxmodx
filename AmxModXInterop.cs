// AmxModXInterop.cs - C# Interop Layer for AMX Mod X Command Registration Interface
// Provides managed interface for command registration with AMX Mod X

using System;
using System.Runtime.InteropServices;
using System.Text;

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
    /// </summary>
    internal static class NativeMethods
    {
        private const string DllName = "AmxModXBridge";

        /// <summary>
        /// 注册控制台命令 / Register console command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志 / Access flags</param>
        /// <param name="info">命令信息 / Command info</param>
        /// <param name="infoMultiLang">信息是否支持多语言 / Whether info supports multi-language</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
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
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
        internal static extern int GetCommandCount(
            CommandType commandType,
            int accessFlags);

        /// <summary>
        /// 注销命令 / Unregister command
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool UnregisterCommand(int commandId);

        /// <summary>
        /// 初始化桥接层 / Initialize bridge
        /// </summary>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
        internal static extern void InitializeBridge();

        /// <summary>
        /// 清理桥接层 / Cleanup bridge
        /// </summary>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall)]
        internal static extern void CleanupBridge();
    }

    /// <summary>
    /// AMX Mod X 命令注册管理器 / AMX Mod X command registration manager
    /// </summary>
    public static class AmxModXCommands
    {
        private static bool _initialized = false;

        /// <summary>
        /// 初始化命令系统 / Initialize command system
        /// </summary>
        public static void Initialize()
        {
            if (!_initialized)
            {
                NativeMethods.InitializeBridge();
                _initialized = true;
            }
        }

        /// <summary>
        /// 清理命令系统 / Cleanup command system
        /// </summary>
        public static void Cleanup()
        {
            if (_initialized)
            {
                NativeMethods.CleanupBridge();
                _initialized = false;
            }
        }

        /// <summary>
        /// 注册控制台命令 / Register console command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        public static int RegisterConsoleCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterConsoleCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册客户端命令 / Register client command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        public static int RegisterClientCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterClientCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册服务器命令 / Register server command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <param name="flags">访问标志，默认为-1（隐藏） / Access flags, default -1 (hidden)</param>
        /// <param name="info">命令信息，默认为空 / Command info, default empty</param>
        /// <param name="infoMultiLang">信息是否支持多语言，默认为false / Whether info supports multi-language, default false</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        public static int RegisterServerCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterServerCommand(command, callback, flags, info ?? "", infoMultiLang);
        }

        /// <summary>
        /// 注册菜单命令 / Register menu command
        /// </summary>
        /// <param name="menuId">菜单ID / Menu ID</param>
        /// <param name="keyMask">按键掩码 / Key mask</param>
        /// <param name="callback">回调函数 / Callback function</param>
        /// <returns>命令ID，失败返回-1 / Command ID, returns -1 on failure</returns>
        public static int RegisterMenuCommand(int menuId, int keyMask, MenuCallback callback)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterMenuCommand(menuId, keyMask, callback);
        }

        /// <summary>
        /// 注册菜单ID / Register menu ID
        /// </summary>
        /// <param name="menuName">菜单名称 / Menu name</param>
        /// <param name="global">是否全局，默认为false / Whether global, default false</param>
        /// <returns>菜单ID，失败返回-1 / Menu ID, returns -1 on failure</returns>
        public static int RegisterMenuId(string menuName, bool global = false)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (string.IsNullOrEmpty(menuName))
                throw new ArgumentException("Menu name cannot be null or empty.", nameof(menuName));

            return NativeMethods.RegisterMenuId(menuName, global);
        }

        /// <summary>
        /// 获取命令信息 / Get command information
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <returns>命令信息，失败返回null / Command info, returns null on failure</returns>
        public static CommandInfo? GetCommandInfo(int commandId, CommandType commandType)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            if (NativeMethods.GetCommandInfo(commandId, commandType, out CommandInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 获取命令数量 / Get command count
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志，默认为-1（所有） / Access flags, default -1 (all)</param>
        /// <returns>命令数量 / Command count</returns>
        public static int GetCommandCount(CommandType commandType, int accessFlags = -1)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            return NativeMethods.GetCommandCount(commandType, accessFlags);
        }

        /// <summary>
        /// 注销命令 / Unregister command
        /// </summary>
        /// <param name="commandId">命令ID / Command ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        public static bool UnregisterCommand(int commandId)
        {
            if (!_initialized)
                throw new InvalidOperationException("AmxModXCommands not initialized. Call Initialize() first.");

            return NativeMethods.UnregisterCommand(commandId);
        }
    }
}
}
