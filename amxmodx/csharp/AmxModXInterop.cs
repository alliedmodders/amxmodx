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
    /// 玩家信息结构 / Player information structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PlayerInfo
    {
        /// <summary>玩家名称 / Player name</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string Name;

        /// <summary>IP地址 / IP address</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string IP;

        /// <summary>认证ID / Auth ID</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string AuthId;

        /// <summary>队伍名称 / Team name</summary>
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string Team;

        /// <summary>玩家索引 / Player index</summary>
        public int Index;

        /// <summary>队伍ID / Team ID</summary>
        public int TeamId;

        /// <summary>用户ID / User ID</summary>
        public int UserId;

        /// <summary>权限标志 / Permission flags</summary>
        public int Flags;

        /// <summary>连接时间 / Connect time</summary>
        public float ConnectTime;

        /// <summary>游戏时间 / Play time</summary>
        public float PlayTime;

        /// <summary>是否在游戏中 / Whether in game</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsInGame;

        /// <summary>是否为机器人 / Whether is bot</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsBot;

        /// <summary>是否存活 / Whether alive</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsAlive;

        /// <summary>是否已认证 / Whether authorized</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsAuthorized;

        /// <summary>是否正在连接 / Whether connecting</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsConnecting;

        /// <summary>是否为HLTV / Whether is HLTV</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool IsHLTV;

        /// <summary>是否支持VGUI / Whether has VGUI</summary>
        [MarshalAs(UnmanagedType.I1)]
        public bool HasVGUI;
    }

    /// <summary>
    /// 玩家统计信息结构 / Player statistics structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PlayerStats
    {
        /// <summary>死亡数 / Deaths count</summary>
        public int Deaths;

        /// <summary>击杀数 / Kills count</summary>
        public int Kills;

        /// <summary>击杀分数 / Frags score</summary>
        public float Frags;

        /// <summary>当前武器 / Current weapon</summary>
        public int CurrentWeapon;

        /// <summary>菜单状态 / Menu state</summary>
        public int Menu;

        /// <summary>按键状态 / Keys state</summary>
        public int Keys;

        /// <summary>生命值 / Health value</summary>
        public float Health;

        /// <summary>护甲值 / Armor value</summary>
        public float Armor;

        /// <summary>瞄准目标 / Aiming target</summary>
        public int Aiming;

        /// <summary>菜单过期时间 / Menu expire time</summary>
        public float MenuExpire;

        /// <summary>武器弹药数组 / Weapon ammo array</summary>
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)]
        public int[] Weapons;

        /// <summary>武器弹夹数组 / Weapon clip array</summary>
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)]
        public int[] Clips;
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

        // ========== 玩家信息接口 / Player Information Interfaces ==========

        /// <summary>
        /// 检查玩家ID是否有效 / Check if player ID is valid
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否有效 / Whether valid</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerValid")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerValid(int clientId);

        /// <summary>
        /// 获取玩家完整信息 / Get complete player information
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="outInfo">输出玩家信息 / Output player info</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerInfo")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerInfo(int clientId, out PlayerInfo outInfo);

        /// <summary>
        /// 获取玩家统计信息 / Get player statistics
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="outStats">输出统计信息 / Output statistics</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerStats")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerStats(int clientId, out PlayerStats outStats);

        /// <summary>
        /// 获取玩家名称 / Get player name
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerName")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerName(int clientId, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        /// <summary>
        /// 获取玩家IP地址 / Get player IP address
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerIP")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerIP(int clientId, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        /// <summary>
        /// 获取玩家认证ID / Get player auth ID
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerAuthId")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerAuthId(int clientId, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        /// <summary>
        /// 获取玩家队伍名称 / Get player team name
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="buffer">输出缓冲区 / Output buffer</param>
        /// <param name="bufferSize">缓冲区大小 / Buffer size</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerTeam")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetPlayerTeam(int clientId, [MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder buffer, int bufferSize);

        // ========== 玩家状态接口 / Player State Interfaces ==========

        /// <summary>
        /// 检查玩家是否在游戏中 / Check if player is in game
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否在游戏中 / Whether in game</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerInGame")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerInGame(int clientId);

        /// <summary>
        /// 检查玩家是否为机器人 / Check if player is bot
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否为机器人 / Whether is bot</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerBot")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerBot(int clientId);

        /// <summary>
        /// 检查玩家是否存活 / Check if player is alive
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否存活 / Whether alive</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerAlive")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerAlive(int clientId);

        /// <summary>
        /// 检查玩家是否已认证 / Check if player is authorized
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否已认证 / Whether authorized</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerAuthorized")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerAuthorized(int clientId);

        /// <summary>
        /// 检查玩家是否正在连接 / Check if player is connecting
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否正在连接 / Whether connecting</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerConnecting")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerConnecting(int clientId);

        /// <summary>
        /// 检查玩家是否为HLTV / Check if player is HLTV
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否为HLTV / Whether is HLTV</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "IsPlayerHLTV")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool IsPlayerHLTV(int clientId);

        // ========== 玩家属性获取接口 / Player Property Getter Interfaces ==========

        /// <summary>
        /// 获取玩家用户ID / Get player user ID
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>用户ID / User ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerUserId")]
        internal static extern int GetPlayerUserId(int clientId);

        /// <summary>
        /// 获取玩家队伍ID / Get player team ID
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>队伍ID / Team ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerTeamId")]
        internal static extern int GetPlayerTeamId(int clientId);

        /// <summary>
        /// 获取玩家标志 / Get player flags
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>标志值 / Flags value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerFlags")]
        internal static extern int GetPlayerFlags(int clientId);

        /// <summary>
        /// 获取玩家连接时间 / Get player connect time
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>连接时间 / Connect time</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerConnectTime")]
        internal static extern float GetPlayerConnectTime(int clientId);

        /// <summary>
        /// 获取玩家游戏时间 / Get player play time
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>游戏时间 / Play time</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerPlayTime")]
        internal static extern float GetPlayerPlayTime(int clientId);

        /// <summary>
        /// 获取玩家生命值 / Get player health
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>生命值 / Health value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerHealth")]
        internal static extern float GetPlayerHealth(int clientId);

        /// <summary>
        /// 获取玩家护甲值 / Get player armor
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>护甲值 / Armor value</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerArmor")]
        internal static extern float GetPlayerArmor(int clientId);

        /// <summary>
        /// 获取玩家击杀数 / Get player frags
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>击杀数 / Frags count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerFrags")]
        internal static extern float GetPlayerFrags(int clientId);

        /// <summary>
        /// 获取玩家死亡数 / Get player deaths
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>死亡数 / Deaths count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerDeaths")]
        internal static extern int GetPlayerDeaths(int clientId);

        /// <summary>
        /// 获取玩家当前武器 / Get player current weapon
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>武器ID / Weapon ID</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerCurrentWeapon")]
        internal static extern int GetPlayerCurrentWeapon(int clientId);

        /// <summary>
        /// 获取玩家菜单状态 / Get player menu state
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>菜单状态 / Menu state</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerMenu")]
        internal static extern int GetPlayerMenu(int clientId);

        /// <summary>
        /// 获取玩家按键状态 / Get player keys state
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>按键状态 / Keys state</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetPlayerKeys")]
        internal static extern int GetPlayerKeys(int clientId);

        // ========== 玩家属性设置接口 / Player Property Setter Interfaces ==========

        /// <summary>
        /// 设置玩家生命值 / Set player health
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="health">生命值 / Health value</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "SetPlayerHealth")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SetPlayerHealth(int clientId, float health);

        /// <summary>
        /// 设置玩家护甲值 / Set player armor
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="armor">护甲值 / Armor value</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "SetPlayerArmor")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SetPlayerArmor(int clientId, float armor);

        /// <summary>
        /// 设置玩家击杀数 / Set player frags
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="frags">击杀数 / Frags count</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "SetPlayerFrags")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SetPlayerFrags(int clientId, float frags);

        /// <summary>
        /// 设置玩家队伍信息 / Set player team info
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="teamId">队伍ID / Team ID</param>
        /// <param name="teamName">队伍名称 / Team name</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "SetPlayerTeamInfo")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SetPlayerTeamInfo(int clientId, int teamId, [MarshalAs(UnmanagedType.LPStr)] string teamName);

        /// <summary>
        /// 设置玩家标志 / Set player flags
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="flags">标志值 / Flags value</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "SetPlayerFlags")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SetPlayerFlags(int clientId, int flags);

        // ========== 玩家工具函数接口 / Player Utility Function Interfaces ==========

        /// <summary>
        /// 获取最大客户端数量 / Get maximum clients count
        /// </summary>
        /// <returns>最大客户端数量 / Maximum clients count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetMaxClients")]
        internal static extern int GetMaxClients();

        /// <summary>
        /// 获取已连接玩家数量 / Get connected players count
        /// </summary>
        /// <returns>已连接玩家数量 / Connected players count</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetConnectedPlayersCount")]
        internal static extern int GetConnectedPlayersCount();

        /// <summary>
        /// 获取已连接玩家列表 / Get connected players list
        /// </summary>
        /// <param name="playerIds">玩家ID数组 / Player IDs array</param>
        /// <param name="maxPlayers">最大玩家数 / Maximum players</param>
        /// <param name="outCount">输出玩家数量 / Output players count</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "GetConnectedPlayers")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool GetConnectedPlayers([MarshalAs(UnmanagedType.LPArray)] int[] playerIds, int maxPlayers, out int outCount);

        /// <summary>
        /// 踢出玩家 / Kick player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="reason">踢出原因 / Kick reason</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi, EntryPoint = "KickPlayer")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool KickPlayer(int clientId, [MarshalAs(UnmanagedType.LPStr)] string reason);

        /// <summary>
        /// 杀死玩家 / Slay player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否成功 / Whether successful</returns>
        [DllImport(DllName, CallingConvention = CallingConvention.StdCall, EntryPoint = "SlayPlayer")]
        [return: MarshalAs(UnmanagedType.I1)]
        internal static extern bool SlayPlayer(int clientId);

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

        // ========== 玩家信息管理接口 / Player Information Management Interfaces ==========

        /// <summary>
        /// 检查玩家ID是否有效 / Check if player ID is valid
        /// 验证客户端ID是否在有效范围内 / Validate if client ID is within valid range
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否有效 / Whether valid</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerValid(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerValid(clientId);
        }

        /// <summary>
        /// 获取玩家完整信息 / Get complete player information
        /// 获取玩家的所有基本信息 / Get all basic information of the player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>玩家信息，失败返回null / Player info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static PlayerInfo? GetPlayerInfo(int clientId)
        {
            EnsureInitialized();

            if (NativeMethods.GetPlayerInfo(clientId, out PlayerInfo info))
                return info;

            return null;
        }

        /// <summary>
        /// 获取玩家统计信息 / Get player statistics
        /// 获取玩家的游戏统计数据 / Get player's game statistics
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>统计信息，失败返回null / Statistics info, returns null on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static PlayerStats? GetPlayerStats(int clientId)
        {
            EnsureInitialized();

            if (NativeMethods.GetPlayerStats(clientId, out PlayerStats stats))
                return stats;

            return null;
        }

        /// <summary>
        /// 获取玩家名称 / Get player name
        /// 获取指定玩家的显示名称 / Get display name of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>玩家名称，失败返回空字符串 / Player name, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetPlayerName(int clientId)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(64);
            if (NativeMethods.GetPlayerName(clientId, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        /// <summary>
        /// 获取玩家IP地址 / Get player IP address
        /// 获取指定玩家的IP地址 / Get IP address of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>IP地址，失败返回空字符串 / IP address, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetPlayerIP(int clientId)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(32);
            if (NativeMethods.GetPlayerIP(clientId, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        /// <summary>
        /// 获取玩家认证ID / Get player auth ID
        /// 获取指定玩家的Steam认证ID / Get Steam auth ID of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>认证ID，失败返回空字符串 / Auth ID, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetPlayerAuthId(int clientId)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(64);
            if (NativeMethods.GetPlayerAuthId(clientId, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        /// <summary>
        /// 获取玩家队伍名称 / Get player team name
        /// 获取指定玩家所在队伍的名称 / Get team name of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>队伍名称，失败返回空字符串 / Team name, returns empty string on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static string GetPlayerTeam(int clientId)
        {
            EnsureInitialized();

            var buffer = new System.Text.StringBuilder(32);
            if (NativeMethods.GetPlayerTeam(clientId, buffer, buffer.Capacity))
                return buffer.ToString();

            return string.Empty;
        }

        // ========== 玩家状态查询接口 / Player State Query Interfaces ==========

        /// <summary>
        /// 检查玩家是否在游戏中 / Check if player is in game
        /// 检查指定玩家是否已完全进入游戏 / Check if specified player has fully entered the game
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否在游戏中 / Whether in game</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerInGame(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerInGame(clientId);
        }

        /// <summary>
        /// 检查玩家是否为机器人 / Check if player is bot
        /// 检查指定玩家是否为AI机器人 / Check if specified player is AI bot
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否为机器人 / Whether is bot</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerBot(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerBot(clientId);
        }

        /// <summary>
        /// 检查玩家是否存活 / Check if player is alive
        /// 检查指定玩家是否处于存活状态 / Check if specified player is in alive state
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否存活 / Whether alive</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerAlive(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerAlive(clientId);
        }

        /// <summary>
        /// 检查玩家是否已认证 / Check if player is authorized
        /// 检查指定玩家是否已通过Steam认证 / Check if specified player has passed Steam authorization
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否已认证 / Whether authorized</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerAuthorized(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerAuthorized(clientId);
        }

        /// <summary>
        /// 检查玩家是否正在连接 / Check if player is connecting
        /// 检查指定玩家是否正在连接过程中 / Check if specified player is in connecting process
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否正在连接 / Whether connecting</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerConnecting(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerConnecting(clientId);
        }

        /// <summary>
        /// 检查玩家是否为HLTV / Check if player is HLTV
        /// 检查指定玩家是否为HLTV观察者 / Check if specified player is HLTV observer
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>是否为HLTV / Whether is HLTV</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool IsPlayerHLTV(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.IsPlayerHLTV(clientId);
        }

        // ========== 玩家属性获取接口 / Player Property Getter Interfaces ==========

        /// <summary>
        /// 获取玩家用户ID / Get player user ID
        /// 获取指定玩家的唯一用户ID / Get unique user ID of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>用户ID，失败返回0 / User ID, returns 0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetPlayerUserId(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerUserId(clientId);
        }

        /// <summary>
        /// 获取玩家队伍ID / Get player team ID
        /// 获取指定玩家所在队伍的ID / Get team ID of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>队伍ID，失败返回0 / Team ID, returns 0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetPlayerTeamId(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerTeamId(clientId);
        }

        /// <summary>
        /// 获取玩家权限标志 / Get player permission flags
        /// 获取指定玩家的权限标志位 / Get permission flags of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>权限标志，失败返回0 / Permission flags, returns 0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static int GetPlayerFlags(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerFlags(clientId);
        }

        /// <summary>
        /// 获取玩家生命值 / Get player health
        /// 获取指定玩家的当前生命值 / Get current health of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>生命值，失败返回0.0 / Health value, returns 0.0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static float GetPlayerHealth(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerHealth(clientId);
        }

        /// <summary>
        /// 获取玩家护甲值 / Get player armor
        /// 获取指定玩家的当前护甲值 / Get current armor of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>护甲值，失败返回0.0 / Armor value, returns 0.0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static float GetPlayerArmor(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerArmor(clientId);
        }

        /// <summary>
        /// 获取玩家击杀数 / Get player frags
        /// 获取指定玩家的击杀分数 / Get frag score of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <returns>击杀数，失败返回0.0 / Frags count, returns 0.0 on failure</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static float GetPlayerFrags(int clientId)
        {
            EnsureInitialized();
            return NativeMethods.GetPlayerFrags(clientId);
        }

        /// <summary>
        /// 设置玩家生命值 / Set player health
        /// 设置指定玩家的生命值 / Set health of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="health">生命值，应大于等于0 / Health value, should be >= 0</param>
        /// <returns>是否成功 / Whether successful</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentOutOfRangeException">当生命值小于0时抛出 / Thrown when health is less than 0</exception>
        public static bool SetPlayerHealth(int clientId, float health)
        {
            EnsureInitialized();

            if (health < 0)
                throw new ArgumentOutOfRangeException(nameof(health), "Health cannot be negative.");

            return NativeMethods.SetPlayerHealth(clientId, health);
        }

        /// <summary>
        /// 设置玩家护甲值 / Set player armor
        /// 设置指定玩家的护甲值 / Set armor of specified player
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="armor">护甲值，应大于等于0 / Armor value, should be >= 0</param>
        /// <returns>是否成功 / Whether successful</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        /// <exception cref="ArgumentOutOfRangeException">当护甲值小于0时抛出 / Thrown when armor is less than 0</exception>
        public static bool SetPlayerArmor(int clientId, float armor)
        {
            EnsureInitialized();

            if (armor < 0)
                throw new ArgumentOutOfRangeException(nameof(armor), "Armor cannot be negative.");

            return NativeMethods.SetPlayerArmor(clientId, armor);
        }

        /// <summary>
        /// 踢出玩家 / Kick player
        /// 将指定玩家踢出服务器 / Kick specified player from server
        /// </summary>
        /// <param name="clientId">客户端ID / Client ID</param>
        /// <param name="reason">踢出原因，可选 / Kick reason, optional</param>
        /// <returns>是否成功 / Whether successful</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static bool KickPlayer(int clientId, string reason = null)
        {
            EnsureInitialized();
            return NativeMethods.KickPlayer(clientId, reason ?? string.Empty);
        }

        /// <summary>
        /// 获取已连接玩家列表 / Get connected players list
        /// 获取当前所有已连接玩家的ID列表 / Get ID list of all currently connected players
        /// </summary>
        /// <returns>玩家ID列表 / List of player IDs</returns>
        /// <exception cref="InvalidOperationException">当系统未初始化时抛出 / Thrown when system is not initialized</exception>
        public static List<int> GetConnectedPlayers()
        {
            EnsureInitialized();

            int maxClients = NativeMethods.GetMaxClients();
            int[] playerIds = new int[maxClients];

            if (NativeMethods.GetConnectedPlayers(playerIds, maxClients, out int count))
            {
                var result = new List<int>(count);
                for (int i = 0; i < count; i++)
                {
                    result.Add(playerIds[i]);
                }
                return result;
            }

            return new List<int>();
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
