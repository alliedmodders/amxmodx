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
using System.Collections.Generic;
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
    /// 事件回调委托 / Event callback delegate
    /// 处理游戏事件 / Handle game events
    /// </summary>
    /// <param name="eventId">事件ID / Event ID</param>
    /// <param name="clientId">客户端ID / Client ID</param>
    /// <param name="numParams">参数数量 / Number of parameters</param>
    public delegate void EventCallback(int eventId, int clientId, int numParams);

    /// <summary>
    /// Forward回调委托 / Forward callback delegate
    /// 处理Forward调用 / Handle forward calls
    /// </summary>
    /// <param name="forwardId">Forward ID</param>
    /// <param name="numParams">参数数量 / Number of parameters</param>
    /// <returns>返回值 / Return value</returns>
    public delegate int ForwardCallback(int forwardId, int numParams);

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
    /// 事件信息结构 / Event information structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct EventInfo
    {
        /// <summary>事件名称 / Event name</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string EventName;

        /// <summary>事件ID / Event ID</summary>
        public int EventId;

        /// <summary>事件标志 / Event flags</summary>
        public int Flags;

        /// <summary>参数数量 / Number of parameters</summary>
        public int NumParams;

        /// <summary>是否激活 / Whether active</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsActive;
    }

    /// <summary>
    /// Forward信息结构 / Forward information structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct ForwardInfo
    {
        /// <summary>Forward名称 / Forward name</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string ForwardName;

        /// <summary>Forward ID</summary>
        public int ForwardId;

        /// <summary>参数数量 / Number of parameters</summary>
        public int NumParams;

        /// <summary>执行类型 / Execution type</summary>
        public int ExecType;

        /// <summary>是否有效 / Whether valid</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsValid;
    }

    /// <summary>
    /// 事件参数结构 / Event parameter structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct EventParam
    {
        /// <summary>参数类型：0=整数，1=浮点数，2=字符串 / Parameter type: 0=int, 1=float, 2=string</summary>
        public int Type;

        /// <summary>整数值 / Integer value</summary>
        public int IntValue;

        /// <summary>浮点数值 / Float value</summary>
        public float FloatValue;

        /// <summary>字符串值 / String value</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
        public string StringValue;
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

        // ========== 命令执行接口 / Command Execution Interfaces ==========

        /// <summary>
        /// 执行服务器命令 / Execute server command
        /// </summary>
        /// <param name="command">命令字符串 / Command string</param>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "ExecuteServerCommand")]
        internal static extern void ExecuteServerCommand([MarshalAs(UnmanagedType.LPStr)] string command);

        /// <summary>
        /// 执行客户端命令 / Execute client command
        /// </summary>
        /// <param name="clientId">客户端ID，0表示所有客户端 / Client ID, 0 means all clients</param>
        /// <param name="command">命令字符串 / Command string</param>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "ExecuteClientCommand")]
        internal static extern void ExecuteClientCommand(int clientId, [MarshalAs(UnmanagedType.LPStr)] string command);

        /// <summary>
        /// 执行控制台命令 / Execute console command
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="command">命令字符串 / Command string</param>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "ExecuteConsoleCommand")]
        internal static extern void ExecuteConsoleCommand(int clientId, [MarshalAs(UnmanagedType.LPStr)] string command);

        // ========== 命令参数读取接口 / Command Argument Reading Interfaces ==========

        /// <summary>
        /// 获取命令参数数量 / Get command argument count
        /// </summary>
        /// <returns>参数数量 / Argument count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandArgCount")]
        internal static extern int GetCommandArgCount();

        /// <summary>
        /// 获取指定索引的命令参数 / Get command argument by index
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandArg")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetCommandArg(int index, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        /// <summary>
        /// 获取所有命令参数 / Get all command arguments
        /// </summary>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandArgs")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetCommandArgs([MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        /// <summary>
        /// 获取指定索引的命令参数（整数） / Get command argument by index (integer)
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>整数值 / Integer value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandArgInt")]
        internal static extern int GetCommandArgInt(int index);

        /// <summary>
        /// 获取指定索引的命令参数（浮点数） / Get command argument by index (float)
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>浮点数值 / Float value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandArgFloat")]
        internal static extern float GetCommandArgFloat(int index);

        // ========== 命令查询接口 / Command Query Interfaces ==========

        /// <summary>
        /// 查找指定名称的命令 / Find command by name
        /// </summary>
        /// <param name="commandName">命令名称 / Command name</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="outInfo">输出命令信息 / Output command info</param>
        /// <returns>是否找到 / Whether found</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "FindCommand")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool FindCommand(
            [MarshalAs(UnmanagedType.LPStr)] string commandName,
            CommandType commandType,
            out CommandInfo outInfo);

        /// <summary>
        /// 获取指定类型的命令数量 / Get command count by type
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志 / Access flags</param>
        /// <returns>命令数量 / Command count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandsCount")]
        internal static extern int GetCommandsCount(CommandType commandType, int accessFlags);

        /// <summary>
        /// 根据索引获取命令信息 / Get command info by index
        /// </summary>
        /// <param name="index">命令索引 / Command index</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志 / Access flags</param>
        /// <param name="outInfo">输出命令信息 / Output command info</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetCommandByIndex")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetCommandByIndex(
            int index,
            CommandType commandType,
            int accessFlags,
            out CommandInfo outInfo);

        // ========== 事件系统接口 / Event System Interfaces ==========

        /// <summary>
        /// 注册事件监听器 / Register event listener
        /// </summary>
        /// <param name="eventName">事件名称 / Event name</param>
        /// <param name="callback">事件回调函数 / Event callback function</param>
        /// <param name="flags">事件标志 / Event flags</param>
        /// <param name="conditions">事件条件 / Event conditions</param>
        /// <returns>事件句柄 / Event handle</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "RegisterEvent")]
        internal static extern int RegisterEvent(
            [MarshalAs(UnmanagedType.LPStr)] string eventName,
            EventCallback callback,
            int flags,
            [MarshalAs(UnmanagedType.LPStr)] string conditions);

        /// <summary>
        /// 注销事件监听器 / Unregister event listener
        /// </summary>
        /// <param name="eventHandle">事件句柄 / Event handle</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "UnregisterEvent")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool UnregisterEvent(int eventHandle);

        /// <summary>
        /// 获取事件ID / Get event ID
        /// </summary>
        /// <param name="eventName">事件名称 / Event name</param>
        /// <returns>事件ID / Event ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "GetEventId")]
        internal static extern int GetEventId([MarshalAs(UnmanagedType.LPStr)] string eventName);

        /// <summary>
        /// 获取事件信息 / Get event information
        /// </summary>
        /// <param name="eventId">事件ID / Event ID</param>
        /// <param name="outInfo">输出事件信息 / Output event info</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventInfo")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetEventInfo(int eventId, out EventInfo outInfo);

        // ========== 事件参数读取接口 / Event Parameter Reading Interfaces ==========

        /// <summary>
        /// 获取事件参数数量 / Get event argument count
        /// </summary>
        /// <returns>参数数量 / Argument count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventArgCount")]
        internal static extern int GetEventArgCount();

        /// <summary>
        /// 获取事件参数 / Get event argument
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <param name="outParam">输出参数 / Output parameter</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventArg")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetEventArg(int index, out EventParam outParam);

        /// <summary>
        /// 获取事件参数（整数） / Get event argument (integer)
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>整数值 / Integer value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventArgInt")]
        internal static extern int GetEventArgInt(int index);

        /// <summary>
        /// 获取事件参数（浮点数） / Get event argument (float)
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>浮点数值 / Float value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventArgFloat")]
        internal static extern float GetEventArgFloat(int index);

        /// <summary>
        /// 获取事件参数（字符串） / Get event argument (string)
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetEventArgString")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetEventArgString(int index, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        // ========== Forward系统接口 / Forward System Interfaces ==========

        /// <summary>
        /// 创建全局Forward / Create global forward
        /// </summary>
        /// <param name="forwardName">Forward名称 / Forward name</param>
        /// <param name="execType">执行类型 / Execution type</param>
        /// <param name="paramTypes">参数类型数组 / Parameter types array</param>
        /// <param name="numParams">参数数量 / Number of parameters</param>
        /// <returns>Forward ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "CreateForward")]
        internal static extern int CreateForward(
            [MarshalAs(UnmanagedType.LPStr)] string forwardName,
            int execType,
            [MarshalAs(UnmanagedType.LPArray)] int[] paramTypes,
            int numParams);

        /// <summary>
        /// 创建单插件Forward / Create single plugin forward
        /// </summary>
        /// <param name="functionName">函数名称 / Function name</param>
        /// <param name="callback">Forward回调 / Forward callback</param>
        /// <param name="paramTypes">参数类型数组 / Parameter types array</param>
        /// <param name="numParams">参数数量 / Number of parameters</param>
        /// <returns>Forward ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "CreateSingleForward")]
        internal static extern int CreateSingleForward(
            [MarshalAs(UnmanagedType.LPStr)] string functionName,
            ForwardCallback callback,
            [MarshalAs(UnmanagedType.LPArray)] int[] paramTypes,
            int numParams);

        /// <summary>
        /// 执行Forward / Execute forward
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <param name="parameters">参数数组 / Parameters array</param>
        /// <param name="numParams">参数数量 / Number of parameters</param>
        /// <param name="outResult">输出结果 / Output result</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "ExecuteForward")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool ExecuteForward(
            int forwardId,
            [MarshalAs(UnmanagedType.LPArray)] EventParam[] parameters,
            int numParams,
            out int outResult);

        /// <summary>
        /// 注销Forward / Unregister forward
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "UnregisterForward")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool UnregisterForward(int forwardId);

        /// <summary>
        /// 获取Forward信息 / Get forward information
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <param name="outInfo">输出Forward信息 / Output forward info</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetForwardInfo")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetForwardInfo(int forwardId, out ForwardInfo outInfo);

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

        // ========== 命令执行接口 / Command Execution Interfaces ==========

        /// <summary>
        /// 执行服务器命令 / Execute server command
        /// 在服务器控制台执行命令 / Execute command in server console
        /// </summary>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        public static void ExecuteServerCommand(string command)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            NativeMethods.ExecuteServerCommand(command);
        }

        /// <summary>
        /// 执行客户端命令 / Execute client command
        /// 让指定客户端或所有客户端执行命令 / Make specified client or all clients execute command
        /// </summary>
        /// <param name="clientId">客户端ID，0表示所有客户端 / Client ID, 0 means all clients</param>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        public static void ExecuteClientCommand(int clientId, string command)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            NativeMethods.ExecuteClientCommand(clientId, command);
        }

        /// <summary>
        /// 执行控制台命令 / Execute console command
        /// 在指定客户端的控制台执行命令 / Execute command in specified client's console
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="command">命令字符串，不能为空 / Command string, cannot be empty</param>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令字符串为空时抛出 / Thrown when command string is empty</exception>
        public static void ExecuteConsoleCommand(int clientId, string command)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(command))
                throw new ArgumentException("Command cannot be null or empty.", nameof(command));

            NativeMethods.ExecuteConsoleCommand(clientId, command);
        }

        // ========== 命令参数读取接口 / Command Argument Reading Interfaces ==========

        /// <summary>
        /// 获取当前命令的参数数量 / Get current command argument count
        /// 在命令回调函数中使用 / Use in command callback functions
        /// </summary>
        /// <returns>参数数量，包括命令本身 / Argument count, including command itself</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetCommandArgCount()
        {
            EnsureInitialized();
            return NativeMethods.GetCommandArgCount();
        }

        /// <summary>
        /// 获取指定索引的命令参数 / Get command argument by index
        /// 在命令回调函数中使用 / Use in command callback functions
        /// </summary>
        /// <param name="index">参数索引，0为命令本身 / Argument index, 0 is command itself</param>
        /// <returns>参数字符串，失败返回空字符串 / Argument string, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetCommandArg(int index)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(256);
            if (NativeMethods.GetCommandArg(index, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        /// <summary>
        /// 获取所有命令参数 / Get all command arguments
        /// 在命令回调函数中使用，不包括命令本身 / Use in command callback functions, excludes command itself
        /// </summary>
        /// <returns>参数字符串，失败返回空字符串 / Arguments string, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetCommandArgs()
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(512);
            if (NativeMethods.GetCommandArgs(buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        /// <summary>
        /// 获取指定索引的命令参数（整数） / Get command argument by index (integer)
        /// 在命令回调函数中使用 / Use in command callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>整数值，解析失败返回0 / Integer value, returns 0 on parse failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetCommandArgInt(int index)
        {
            EnsureInitialized();
            return NativeMethods.GetCommandArgInt(index);
        }

        /// <summary>
        /// 获取指定索引的命令参数（浮点数） / Get command argument by index (float)
        /// 在命令回调函数中使用 / Use in command callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>浮点数值，解析失败返回0.0 / Float value, returns 0.0 on parse failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static float GetCommandArgFloat(int index)
        {
            EnsureInitialized();
            return NativeMethods.GetCommandArgFloat(index);
        }

        // ========== 命令查询接口 / Command Query Interfaces ==========

        /// <summary>
        /// 查找指定名称的命令 / Find command by name
        /// 在AMX Mod X命令系统中查找命令 / Search for command in AMX Mod X command system
        /// </summary>
        /// <param name="commandName">命令名称，不能为空 / Command name, cannot be empty</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <returns>命令信息，未找到返回null / Command info, returns null if not found</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令名称为空时抛出 / Thrown when command name is empty</exception>
        public static CommandInfo? FindCommand(string commandName, CommandType commandType)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(commandName))
                throw new ArgumentException("Command name cannot be null or empty.", nameof(commandName));

            if (NativeMethods.FindCommand(commandName, commandType, out CommandInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 获取指定类型的命令总数 / Get total command count by type
        /// 统计AMX Mod X系统中的命令数量 / Count commands in AMX Mod X system
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志过滤，默认为-1（所有） / Access flags filter, default -1 (all)</param>
        /// <returns>命令数量 / Command count</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetCommandsCount(CommandType commandType, int accessFlags = -1)
        {
            EnsureInitialized();
            return NativeMethods.GetCommandsCount(commandType, accessFlags);
        }

        /// <summary>
        /// 根据索引获取命令信息 / Get command info by index
        /// 遍历AMX Mod X系统中的命令 / Iterate through commands in AMX Mod X system
        /// </summary>
        /// <param name="index">命令索引 / Command index</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志过滤，默认为-1（所有） / Access flags filter, default -1 (all)</param>
        /// <returns>命令信息，失败返回null / Command info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static CommandInfo? GetCommandByIndex(int index, CommandType commandType, int accessFlags = -1)
        {
            EnsureInitialized();

            if (NativeMethods.GetCommandByIndex(index, commandType, accessFlags, out CommandInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 获取所有指定类型的命令 / Get all commands of specified type
        /// 返回AMX Mod X系统中所有匹配的命令 / Returns all matching commands in AMX Mod X system
        /// </summary>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <param name="accessFlags">访问标志过滤，默认为-1（所有） / Access flags filter, default -1 (all)</param>
        /// <returns>命令信息列表 / List of command info</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static List<CommandInfo> GetAllCommands(CommandType commandType, int accessFlags = -1)
        {
            EnsureInitialized();

            var commands = new List<CommandInfo>();
            int count = GetCommandsCount(commandType, accessFlags);

            for (int i = 0; i < count; i++)
            {
                var cmdInfo = GetCommandByIndex(i, commandType, accessFlags);
                if (cmdInfo.HasValue)
                {
                    commands.Add(cmdInfo.Value);
                }
            }

            return commands;
        }

        /// <summary>
        /// 检查命令是否存在 / Check if command exists
        /// 在AMX Mod X命令系统中检查命令是否已注册 / Check if command is registered in AMX Mod X command system
        /// </summary>
        /// <param name="commandName">命令名称，不能为空 / Command name, cannot be empty</param>
        /// <param name="commandType">命令类型 / Command type</param>
        /// <returns>是否存在 / Whether exists</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当命令名称为空时抛出 / Thrown when command name is empty</exception>
        public static bool CommandExists(string commandName, CommandType commandType)
        {
            return FindCommand(commandName, commandType).HasValue;
        }

        // ========== 事件系统接口 / Event System Interfaces ==========

        /// <summary>
        /// 注册事件监听器 / Register event listener
        /// 监听指定的游戏事件 / Listen to specified game events
        /// </summary>
        /// <param name="eventName">事件名称，不能为空 / Event name, cannot be empty</param>
        /// <param name="callback">事件回调函数，不能为null / Event callback function, cannot be null</param>
        /// <param name="flags">事件标志，默认为无标志 / Event flags, default is no flags</param>
        /// <param name="conditions">事件条件，可选 / Event conditions, optional</param>
        /// <returns>事件句柄，失败返回-1 / Event handle, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当事件名称为空时抛出 / Thrown when event name is empty</exception>
        /// <exception cref="ArgumentNullException">当回调函数为null时抛出 / Thrown when callback is null</exception>
        public static int RegisterEvent(string eventName, EventCallback callback, int flags = EventFlags.None, string conditions = null)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(eventName))
                throw new ArgumentException("Event name cannot be null or empty.", nameof(eventName));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            return NativeMethods.RegisterEvent(eventName, callback, flags, conditions ?? string.Empty);
        }

        /// <summary>
        /// 注销事件监听器 / Unregister event listener
        /// 停止监听指定的事件 / Stop listening to specified event
        /// </summary>
        /// <param name="eventHandle">事件句柄 / Event handle</param>
        /// <returns>是否成功 / Whether successful</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool UnregisterEvent(int eventHandle)
        {
            EnsureInitialized();
            return NativeMethods.UnregisterEvent(eventHandle);
        }

        /// <summary>
        /// 获取事件ID / Get event ID
        /// 根据事件名称获取对应的事件ID / Get event ID by event name
        /// </summary>
        /// <param name="eventName">事件名称，不能为空 / Event name, cannot be empty</param>
        /// <returns>事件ID，未找到返回-1 / Event ID, returns -1 if not found</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当事件名称为空时抛出 / Thrown when event name is empty</exception>
        public static int GetEventId(string eventName)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(eventName))
                throw new ArgumentException("Event name cannot be null or empty.", nameof(eventName));

            return NativeMethods.GetEventId(eventName);
        }

        /// <summary>
        /// 获取事件信息 / Get event information
        /// 获取指定事件ID的详细信息 / Get detailed information for specified event ID
        /// </summary>
        /// <param name="eventId">事件ID / Event ID</param>
        /// <returns>事件信息，失败返回null / Event info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static EventInfo? GetEventInfo(int eventId)
        {
            EnsureInitialized();

            if (NativeMethods.GetEventInfo(eventId, out EventInfo info))
                return info;

            return null;
        }

        // ========== 事件参数读取接口 / Event Parameter Reading Interfaces ==========

        /// <summary>
        /// 获取当前事件的参数数量 / Get current event argument count
        /// 在事件回调函数中使用 / Use in event callback functions
        /// </summary>
        /// <returns>参数数量 / Argument count</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetEventArgCount()
        {
            EnsureInitialized();
            return NativeMethods.GetEventArgCount();
        }

        /// <summary>
        /// 获取指定索引的事件参数 / Get event argument by index
        /// 在事件回调函数中使用 / Use in event callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>事件参数，失败返回null / Event parameter, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static EventParam? GetEventArg(int index)
        {
            EnsureInitialized();

            if (NativeMethods.GetEventArg(index, out EventParam param))
                return param;

            return null;
        }

        /// <summary>
        /// 获取指定索引的事件参数（整数） / Get event argument by index (integer)
        /// 在事件回调函数中使用 / Use in event callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>整数值，失败返回0 / Integer value, returns 0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetEventArgInt(int index)
        {
            EnsureInitialized();
            return NativeMethods.GetEventArgInt(index);
        }

        /// <summary>
        /// 获取指定索引的事件参数（浮点数） / Get event argument by index (float)
        /// 在事件回调函数中使用 / Use in event callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>浮点数值，失败返回0.0 / Float value, returns 0.0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static float GetEventArgFloat(int index)
        {
            EnsureInitialized();
            return NativeMethods.GetEventArgFloat(index);
        }

        /// <summary>
        /// 获取指定索引的事件参数（字符串） / Get event argument by index (string)
        /// 在事件回调函数中使用 / Use in event callback functions
        /// </summary>
        /// <param name="index">参数索引 / Argument index</param>
        /// <returns>字符串值，失败返回空字符串 / String value, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetEventArgString(int index)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(256);
            if (NativeMethods.GetEventArgString(index, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        // ========== Forward系统接口 / Forward System Interfaces ==========

        /// <summary>
        /// 创建全局Forward / Create global forward
        /// 创建一个可以被所有插件调用的Forward / Create a forward that can be called by all plugins
        /// </summary>
        /// <param name="forwardName">Forward名称，不能为空 / Forward name, cannot be empty</param>
        /// <param name="execType">执行类型 / Execution type</param>
        /// <param name="paramTypes">参数类型数组 / Parameter types array</param>
        /// <returns>Forward ID，失败返回-1 / Forward ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当Forward名称为空时抛出 / Thrown when forward name is empty</exception>
        /// <exception cref="ArgumentNullException">当参数类型数组为null时抛出 / Thrown when parameter types array is null</exception>
        public static int CreateForward(string forwardName, int execType, params int[] paramTypes)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(forwardName))
                throw new ArgumentException("Forward name cannot be null or empty.", nameof(forwardName));

            if (paramTypes == null)
                throw new ArgumentNullException(nameof(paramTypes));

            return NativeMethods.CreateForward(forwardName, execType, paramTypes, paramTypes.Length);
        }

        /// <summary>
        /// 创建单插件Forward / Create single plugin forward
        /// 创建一个特定于单个插件的Forward / Create a forward specific to a single plugin
        /// </summary>
        /// <param name="functionName">函数名称，不能为空 / Function name, cannot be empty</param>
        /// <param name="callback">Forward回调函数，不能为null / Forward callback function, cannot be null</param>
        /// <param name="paramTypes">参数类型数组 / Parameter types array</param>
        /// <returns>Forward ID，失败返回-1 / Forward ID, returns -1 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentException">当函数名称为空时抛出 / Thrown when function name is empty</exception>
        /// <exception cref="ArgumentNullException">当回调函数或参数类型数组为null时抛出 / Thrown when callback or parameter types array is null</exception>
        public static int CreateSingleForward(string functionName, ForwardCallback callback, params int[] paramTypes)
        {
            EnsureInitialized();

            if (string.IsNullOrEmpty(functionName))
                throw new ArgumentException("Function name cannot be null or empty.", nameof(functionName));

            if (callback == null)
                throw new ArgumentNullException(nameof(callback));

            if (paramTypes == null)
                throw new ArgumentNullException(nameof(paramTypes));

            return NativeMethods.CreateSingleForward(functionName, callback, paramTypes, paramTypes.Length);
        }

        /// <summary>
        /// 执行Forward / Execute forward
        /// 执行指定的Forward并获取返回值 / Execute specified forward and get return value
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <param name="parameters">参数数组 / Parameters array</param>
        /// <returns>执行结果，包含是否成功和返回值 / Execution result, contains success status and return value</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static (bool Success, int Result) ExecuteForward(int forwardId, params EventParam[] parameters)
        {
            EnsureInitialized();

            if (parameters == null)
                parameters = new EventParam[0];

            bool success = NativeMethods.ExecuteForward(forwardId, parameters, parameters.Length, out int result);
            return (success, result);
        }

        /// <summary>
        /// 注销Forward / Unregister forward
        /// 移除指定的Forward / Remove specified forward
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool UnregisterForward(int forwardId)
        {
            EnsureInitialized();
            return NativeMethods.UnregisterForward(forwardId);
        }

        /// <summary>
        /// 获取Forward信息 / Get forward information
        /// 获取指定Forward的详细信息 / Get detailed information for specified forward
        /// </summary>
        /// <param name="forwardId">Forward ID</param>
        /// <returns>Forward信息，失败返回null / Forward info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static ForwardInfo? GetForwardInfo(int forwardId)
        {
            EnsureInitialized();

            if (NativeMethods.GetForwardInfo(forwardId, out ForwardInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 创建事件参数 / Create event parameter
        /// 创建用于Forward执行的参数 / Create parameter for forward execution
        /// </summary>
        /// <param name="value">整数值 / Integer value</param>
        /// <returns>事件参数 / Event parameter</returns>
        public static EventParam CreateEventParam(int value)
        {
            return new EventParam
            {
                Type = 0,
                IntValue = value,
                FloatValue = 0,
                StringValue = string.Empty
            };
        }

        /// <summary>
        /// 创建事件参数 / Create event parameter
        /// 创建用于Forward执行的参数 / Create parameter for forward execution
        /// </summary>
        /// <param name="value">浮点数值 / Float value</param>
        /// <returns>事件参数 / Event parameter</returns>
        public static EventParam CreateEventParam(float value)
        {
            return new EventParam
            {
                Type = 1,
                IntValue = 0,
                FloatValue = value,
                StringValue = string.Empty
            };
        }

        /// <summary>
        /// 创建事件参数 / Create event parameter
        /// 创建用于Forward执行的参数 / Create parameter for forward execution
        /// </summary>
        /// <param name="value">字符串值 / String value</param>
        /// <returns>事件参数 / Event parameter</returns>
        public static EventParam CreateEventParam(string value)
        {
            return new EventParam
            {
                Type = 2,
                IntValue = 0,
                FloatValue = 0,
                StringValue = value ?? string.Empty
            };
        }
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

    /// <summary>
    /// 事件标志常量 / Event flag constants
    /// 用于事件注册的标志位 / Used for event registration flags
    /// </summary>
    public static class EventFlags
    {
        /// <summary>无标志 / No flags</summary>
        public const int None = 0;
        /// <summary>仅客户端 / Client only</summary>
        public const int Client = 1 << 0;
        /// <summary>仅世界 / World only</summary>
        public const int World = 1 << 1;
        /// <summary>仅一次 / Once only</summary>
        public const int Once = 1 << 2;
        /// <summary>仅死亡玩家 / Dead players only</summary>
        public const int Dead = 1 << 3;
        /// <summary>仅存活玩家 / Alive players only</summary>
        public const int Alive = 1 << 4;
        /// <summary>仅真实玩家 / Real players only</summary>
        public const int Player = 1 << 5;
        /// <summary>仅机器人 / Bots only</summary>
        public const int Bot = 1 << 6;
    }

    /// <summary>
    /// Forward执行类型常量 / Forward execution type constants
    /// </summary>
    public static class ForwardExecType
    {
        /// <summary>忽略返回值 / Ignore return value</summary>
        public const int Ignore = 0;
        /// <summary>停止执行 / Stop execution</summary>
        public const int Stop = 1;
        /// <summary>停止执行（最高优先级） / Stop execution (highest priority)</summary>
        public const int Stop2 = 2;
        /// <summary>继续执行 / Continue execution</summary>
        public const int Continue = 3;
    }

    /// <summary>
    /// Forward参数类型常量 / Forward parameter type constants
    /// </summary>
    public static class ForwardParamType
    {
        /// <summary>整数 / Integer</summary>
        public const int Cell = 0;
        /// <summary>浮点数 / Float</summary>
        public const int Float = 1;
        /// <summary>字符串 / String</summary>
        public const int String = 2;
        /// <summary>数组 / Array</summary>
        public const int Array = 3;
        /// <summary>整数引用 / Integer reference</summary>
        public const int CellByRef = 4;
        /// <summary>浮点数引用 / Float reference</summary>
        public const int FloatByRef = 5;
        /// <summary>字符串引用 / String reference</summary>
        public const int StringEx = 6;
        /// <summary>结束标记 / End marker</summary>
        public const int Done = -1;
    }
}
