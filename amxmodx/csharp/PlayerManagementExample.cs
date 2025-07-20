// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// PlayerManagementExample.cs - Player management examples for AMX Mod X C# Interface
// 玩家管理示例，演示如何使用C#接口获取和设置玩家信息

using System;
using System.Collections.Generic;
using System.Linq;
using AmxModX.Interop;

namespace AmxModX.Examples
{
    /// <summary>
    /// AMX Mod X 玩家管理示例 / AMX Mod X player management examples
    /// 演示玩家信息获取、设置和管理功能 / Demonstrates player info retrieval, setting, and management functionality
    /// </summary>
    public static class PlayerManagementExample
    {
        private static readonly Dictionary<string, int> _registeredCommands = new Dictionary<string, int>();
        private static readonly Dictionary<string, int> _registeredEvents = new Dictionary<string, int>();

        /// <summary>
        /// 初始化玩家管理示例 / Initialize player management example
        /// </summary>
        public static void Initialize()
        {
            Console.WriteLine("Initializing Player Management Example...");

            try
            {
                // 初始化AMX Mod X命令系统 / Initialize AMX Mod X command system
                AmxModXCommands.Initialize();

                // 注册玩家管理命令 / Register player management commands
                RegisterPlayerCommands();

                // 注册玩家事件监听器 / Register player event listeners
                RegisterPlayerEvents();

                Console.WriteLine("Player Management Example initialized successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to initialize Player Management Example: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 注册玩家管理命令 / Register player management commands
        /// </summary>
        private static void RegisterPlayerCommands()
        {
            // 注册玩家信息查看命令 / Register player info viewing command
            int playerInfoCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_playerinfo",
                callback: OnPlayerInfoCommand,
                flags: CommandFlags.Admin,
                info: "Display detailed player information - Usage: amx_playerinfo <player>"
            );
            _registeredCommands["amx_playerinfo"] = playerInfoCmd;

            // 注册玩家列表命令 / Register player list command
            int playersCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_players",
                callback: OnPlayersCommand,
                flags: CommandFlags.Admin,
                info: "List all connected players with basic info"
            );
            _registeredCommands["amx_players"] = playersCmd;

            // 注册玩家健康管理命令 / Register player health management command
            int healthCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_sethealth",
                callback: OnSetHealthCommand,
                flags: CommandFlags.Admin,
                info: "Set player health - Usage: amx_sethealth <player> <health>"
            );
            _registeredCommands["amx_sethealth"] = healthCmd;

            // 注册玩家护甲管理命令 / Register player armor management command
            int armorCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_setarmor",
                callback: OnSetArmorCommand,
                flags: CommandFlags.Admin,
                info: "Set player armor - Usage: amx_setarmor <player> <armor>"
            );
            _registeredCommands["amx_setarmor"] = armorCmd;

            // 注册玩家踢出命令 / Register player kick command
            int kickCmd = AmxModXCommands.RegisterConsoleCommand(
                command: "amx_ckick",
                callback: OnKickCommand,
                flags: CommandFlags.Admin,
                info: "Kick player with reason - Usage: amx_ckick <player> [reason]"
            );
            _registeredCommands["amx_ckick"] = kickCmd;

            Console.WriteLine($"Registered {_registeredCommands.Count} player management commands");
        }

        /// <summary>
        /// 注册玩家事件监听器 / Register player event listeners
        /// </summary>
        private static void RegisterPlayerEvents()
        {
            // 注册玩家连接事件 / Register player connect event
            int connectEvent = AmxModXCommands.RegisterEvent(
                eventName: "client_connect",
                callback: OnPlayerConnectEvent,
                flags: EventFlags.Client | EventFlags.Player
            );
            _registeredEvents["client_connect"] = connectEvent;

            // 注册玩家断开连接事件 / Register player disconnect event
            int disconnectEvent = AmxModXCommands.RegisterEvent(
                eventName: "client_disconnect",
                callback: OnPlayerDisconnectEvent,
                flags: EventFlags.Client | EventFlags.Player
            );
            _registeredEvents["client_disconnect"] = disconnectEvent;

            Console.WriteLine($"Registered {_registeredEvents.Count} player event listeners");
        }

        // ========== 命令回调函数 / Command Callback Functions ==========

        /// <summary>
        /// 玩家信息命令回调 / Player info command callback
        /// </summary>
        private static void OnPlayerInfoCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[PLAYER INFO] Executed by client {clientId}");

            try
            {
                int argc = AmxModXCommands.GetCommandArgCount();
                if (argc < 2)
                {
                    Console.WriteLine("[PLAYER INFO] Usage: amx_playerinfo <player>");
                    return;
                }

                string playerTarget = AmxModXCommands.GetCommandArg(1);
                var targetPlayers = FindPlayersByTarget(playerTarget);

                if (targetPlayers.Count == 0)
                {
                    Console.WriteLine($"[PLAYER INFO] No players found matching '{playerTarget}'");
                    return;
                }

                if (targetPlayers.Count > 1)
                {
                    Console.WriteLine($"[PLAYER INFO] Multiple players found matching '{playerTarget}':");
                    foreach (int id in targetPlayers)
                    {
                        string name = AmxModXCommands.GetPlayerName(id);
                        Console.WriteLine($"[PLAYER INFO]   {id}: {name}");
                    }
                    return;
                }

                // 显示详细玩家信息 / Display detailed player information
                DisplayDetailedPlayerInfo(targetPlayers[0]);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PLAYER INFO] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 玩家列表命令回调 / Players list command callback
        /// </summary>
        private static void OnPlayersCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[PLAYERS] Executed by client {clientId}");

            try
            {
                var connectedPlayers = AmxModXCommands.GetConnectedPlayers();
                int totalPlayers = connectedPlayers.Count;
                int maxClients = AmxModXCommands.GetMaxClients();

                Console.WriteLine($"[PLAYERS] Connected players: {totalPlayers}/{maxClients}");
                Console.WriteLine("[PLAYERS] ID | Name                | IP              | Auth ID           | Team | Status");
                Console.WriteLine("[PLAYERS] ---|---------------------|-----------------|-------------------|------|--------");

                foreach (int playerId in connectedPlayers)
                {
                    var playerInfo = AmxModXCommands.GetPlayerInfo(playerId);
                    if (playerInfo.HasValue)
                    {
                        var info = playerInfo.Value;
                        string status = GetPlayerStatusString(playerId);
                        
                        Console.WriteLine($"[PLAYERS] {info.Index,2} | {info.Name,-19} | {info.IP,-15} | {info.AuthId,-17} | {info.Team,-4} | {status}");
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PLAYERS] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 设置生命值命令回调 / Set health command callback
        /// </summary>
        private static void OnSetHealthCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[SET HEALTH] Executed by client {clientId}");

            try
            {
                int argc = AmxModXCommands.GetCommandArgCount();
                if (argc < 3)
                {
                    Console.WriteLine("[SET HEALTH] Usage: amx_sethealth <player> <health>");
                    return;
                }

                string playerTarget = AmxModXCommands.GetCommandArg(1);
                float health = AmxModXCommands.GetCommandArgFloat(2);

                var targetPlayers = FindPlayersByTarget(playerTarget);
                if (targetPlayers.Count == 0)
                {
                    Console.WriteLine($"[SET HEALTH] No players found matching '{playerTarget}'");
                    return;
                }

                foreach (int playerId in targetPlayers)
                {
                    if (AmxModXCommands.IsPlayerAlive(playerId))
                    {
                        bool success = AmxModXCommands.SetPlayerHealth(playerId, health);
                        string playerName = AmxModXCommands.GetPlayerName(playerId);
                        
                        if (success)
                        {
                            Console.WriteLine($"[SET HEALTH] Set {playerName}'s health to {health}");
                        }
                        else
                        {
                            Console.WriteLine($"[SET HEALTH] Failed to set {playerName}'s health");
                        }
                    }
                    else
                    {
                        string playerName = AmxModXCommands.GetPlayerName(playerId);
                        Console.WriteLine($"[SET HEALTH] {playerName} is not alive");
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[SET HEALTH] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 设置护甲命令回调 / Set armor command callback
        /// </summary>
        private static void OnSetArmorCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[SET ARMOR] Executed by client {clientId}");

            try
            {
                int argc = AmxModXCommands.GetCommandArgCount();
                if (argc < 3)
                {
                    Console.WriteLine("[SET ARMOR] Usage: amx_setarmor <player> <armor>");
                    return;
                }

                string playerTarget = AmxModXCommands.GetCommandArg(1);
                float armor = AmxModXCommands.GetCommandArgFloat(2);

                var targetPlayers = FindPlayersByTarget(playerTarget);
                if (targetPlayers.Count == 0)
                {
                    Console.WriteLine($"[SET ARMOR] No players found matching '{playerTarget}'");
                    return;
                }

                foreach (int playerId in targetPlayers)
                {
                    if (AmxModXCommands.IsPlayerAlive(playerId))
                    {
                        bool success = AmxModXCommands.SetPlayerArmor(playerId, armor);
                        string playerName = AmxModXCommands.GetPlayerName(playerId);
                        
                        if (success)
                        {
                            Console.WriteLine($"[SET ARMOR] Set {playerName}'s armor to {armor}");
                        }
                        else
                        {
                            Console.WriteLine($"[SET ARMOR] Failed to set {playerName}'s armor");
                        }
                    }
                    else
                    {
                        string playerName = AmxModXCommands.GetPlayerName(playerId);
                        Console.WriteLine($"[SET ARMOR] {playerName} is not alive");
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[SET ARMOR] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 踢出玩家命令回调 / Kick player command callback
        /// </summary>
        private static void OnKickCommand(int clientId, int commandId, int flags)
        {
            Console.WriteLine($"[KICK] Executed by client {clientId}");

            try
            {
                int argc = AmxModXCommands.GetCommandArgCount();
                if (argc < 2)
                {
                    Console.WriteLine("[KICK] Usage: amx_ckick <player> [reason]");
                    return;
                }

                string playerTarget = AmxModXCommands.GetCommandArg(1);
                string reason = argc > 2 ? AmxModXCommands.GetCommandArgs().Substring(playerTarget.Length).Trim() : "Kicked by admin";

                var targetPlayers = FindPlayersByTarget(playerTarget);
                if (targetPlayers.Count == 0)
                {
                    Console.WriteLine($"[KICK] No players found matching '{playerTarget}'");
                    return;
                }

                foreach (int playerId in targetPlayers)
                {
                    string playerName = AmxModXCommands.GetPlayerName(playerId);
                    bool success = AmxModXCommands.KickPlayer(playerId, reason);
                    
                    if (success)
                    {
                        Console.WriteLine($"[KICK] Kicked {playerName}: {reason}");
                    }
                    else
                    {
                        Console.WriteLine($"[KICK] Failed to kick {playerName}");
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[KICK] Error: {ex.Message}");
            }
        }

        // ========== 事件回调函数 / Event Callback Functions ==========

        /// <summary>
        /// 玩家连接事件回调 / Player connect event callback
        /// </summary>
        private static void OnPlayerConnectEvent(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[CONNECT EVENT] Player {clientId} connecting...");

            try
            {
                // 等待玩家完全连接后显示信息 / Wait for player to fully connect before showing info
                System.Threading.Tasks.Task.Delay(1000).ContinueWith(_ =>
                {
                    if (AmxModXCommands.IsPlayerInGame(clientId))
                    {
                        DisplayPlayerConnectInfo(clientId);
                    }
                });
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[CONNECT EVENT] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 玩家断开连接事件回调 / Player disconnect event callback
        /// </summary>
        private static void OnPlayerDisconnectEvent(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[DISCONNECT EVENT] Player {clientId} disconnecting...");

            try
            {
                string playerName = AmxModXCommands.GetPlayerName(clientId);
                string reason = numParams > 0 ? AmxModXCommands.GetEventArgString(1) : "Unknown";
                
                Console.WriteLine($"[DISCONNECT EVENT] {playerName} left the server: {reason}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[DISCONNECT EVENT] Error: {ex.Message}");
            }
        }

        // ========== 辅助方法 / Helper Methods ==========

        /// <summary>
        /// 显示详细玩家信息 / Display detailed player information
        /// </summary>
        private static void DisplayDetailedPlayerInfo(int clientId)
        {
            try
            {
                var playerInfo = AmxModXCommands.GetPlayerInfo(clientId);
                var playerStats = AmxModXCommands.GetPlayerStats(clientId);

                if (!playerInfo.HasValue)
                {
                    Console.WriteLine($"[PLAYER INFO] Failed to get info for player {clientId}");
                    return;
                }

                var info = playerInfo.Value;
                Console.WriteLine($"[PLAYER INFO] Detailed information for player {clientId}:");
                Console.WriteLine($"[PLAYER INFO]   Name: {info.Name}");
                Console.WriteLine($"[PLAYER INFO]   IP: {info.IP}");
                Console.WriteLine($"[PLAYER INFO]   Auth ID: {info.AuthId}");
                Console.WriteLine($"[PLAYER INFO]   User ID: {info.UserId}");
                Console.WriteLine($"[PLAYER INFO]   Team: {info.Team} (ID: {info.TeamId})");
                Console.WriteLine($"[PLAYER INFO]   Flags: {info.Flags}");
                Console.WriteLine($"[PLAYER INFO]   Connect Time: {info.ConnectTime:F2}s");
                Console.WriteLine($"[PLAYER INFO]   Play Time: {info.PlayTime:F2}s");

                // 状态信息 / Status information
                Console.WriteLine($"[PLAYER INFO]   Status:");
                Console.WriteLine($"[PLAYER INFO]     In Game: {info.IsInGame}");
                Console.WriteLine($"[PLAYER INFO]     Bot: {info.IsBot}");
                Console.WriteLine($"[PLAYER INFO]     Alive: {info.IsAlive}");
                Console.WriteLine($"[PLAYER INFO]     Authorized: {info.IsAuthorized}");
                Console.WriteLine($"[PLAYER INFO]     Connecting: {info.IsConnecting}");
                Console.WriteLine($"[PLAYER INFO]     HLTV: {info.IsHLTV}");
                Console.WriteLine($"[PLAYER INFO]     VGUI: {info.HasVGUI}");

                // 游戏统计 / Game statistics
                if (playerStats.HasValue)
                {
                    var stats = playerStats.Value;
                    Console.WriteLine($"[PLAYER INFO]   Game Stats:");
                    Console.WriteLine($"[PLAYER INFO]     Health: {stats.Health:F1}");
                    Console.WriteLine($"[PLAYER INFO]     Armor: {stats.Armor:F1}");
                    Console.WriteLine($"[PLAYER INFO]     Frags: {stats.Frags:F1}");
                    Console.WriteLine($"[PLAYER INFO]     Deaths: {stats.Deaths}");
                    Console.WriteLine($"[PLAYER INFO]     Current Weapon: {stats.CurrentWeapon}");
                    Console.WriteLine($"[PLAYER INFO]     Menu: {stats.Menu}");
                    Console.WriteLine($"[PLAYER INFO]     Keys: {stats.Keys}");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PLAYER INFO] Error displaying player info: {ex.Message}");
            }
        }

        /// <summary>
        /// 显示玩家连接信息 / Display player connect information
        /// </summary>
        private static void DisplayPlayerConnectInfo(int clientId)
        {
            try
            {
                var playerInfo = AmxModXCommands.GetPlayerInfo(clientId);
                if (playerInfo.HasValue)
                {
                    var info = playerInfo.Value;
                    Console.WriteLine($"[CONNECT INFO] Player connected:");
                    Console.WriteLine($"[CONNECT INFO]   {info.Name} ({info.AuthId}) from {info.IP}");
                    Console.WriteLine($"[CONNECT INFO]   Team: {info.Team}, Bot: {info.IsBot}, HLTV: {info.IsHLTV}");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[CONNECT INFO] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 获取玩家状态字符串 / Get player status string
        /// </summary>
        private static string GetPlayerStatusString(int clientId)
        {
            try
            {
                var status = new List<string>();

                if (AmxModXCommands.IsPlayerBot(clientId))
                    status.Add("BOT");
                if (AmxModXCommands.IsPlayerHLTV(clientId))
                    status.Add("HLTV");
                if (!AmxModXCommands.IsPlayerAuthorized(clientId))
                    status.Add("UNAUTH");
                if (AmxModXCommands.IsPlayerConnecting(clientId))
                    status.Add("CONN");
                if (!AmxModXCommands.IsPlayerAlive(clientId))
                    status.Add("DEAD");

                return status.Count > 0 ? string.Join(",", status) : "OK";
            }
            catch
            {
                return "ERROR";
            }
        }

        /// <summary>
        /// 根据目标字符串查找玩家 / Find players by target string
        /// </summary>
        private static List<int> FindPlayersByTarget(string target)
        {
            try
            {
                // 尝试解析为数字ID / Try parsing as numeric ID
                if (int.TryParse(target, out int playerId))
                {
                    if (AmxModXCommands.IsPlayerValid(playerId) && AmxModXCommands.IsPlayerInGame(playerId))
                    {
                        return new List<int> { playerId };
                    }
                }

                // 按名称查找 / Find by name
                var connectedPlayers = AmxModXCommands.GetConnectedPlayers();
                var matchingPlayers = new List<int>();

                foreach (int clientId in connectedPlayers)
                {
                    string playerName = AmxModXCommands.GetPlayerName(clientId);
                    if (!string.IsNullOrEmpty(playerName))
                    {
                        // 精确匹配 / Exact match
                        if (playerName.Equals(target, StringComparison.OrdinalIgnoreCase))
                        {
                            return new List<int> { clientId };
                        }

                        // 部分匹配 / Partial match
                        if (playerName.IndexOf(target, StringComparison.OrdinalIgnoreCase) >= 0)
                        {
                            matchingPlayers.Add(clientId);
                        }
                    }
                }

                return matchingPlayers;
            }
            catch
            {
                return new List<int>();
            }
        }

        /// <summary>
        /// 运行完整的玩家管理演示 / Run complete player management demonstration
        /// </summary>
        public static void RunFullDemo()
        {
            Console.WriteLine("\n========== Full Player Management Demo ==========");

            try
            {
                // 显示服务器信息 / Display server information
                int maxClients = AmxModXCommands.GetMaxClients();
                int connectedCount = AmxModXCommands.GetConnectedPlayersCount();
                Console.WriteLine($"[DEMO] Server capacity: {connectedCount}/{maxClients} players");

                // 获取所有连接的玩家 / Get all connected players
                var connectedPlayers = AmxModXCommands.GetConnectedPlayers();
                Console.WriteLine($"[DEMO] Connected players: {connectedPlayers.Count}");

                // 显示每个玩家的基本信息 / Display basic info for each player
                foreach (int clientId in connectedPlayers.Take(3)) // 限制显示前3个 / Limit to first 3
                {
                    var playerInfo = AmxModXCommands.GetPlayerInfo(clientId);
                    if (playerInfo.HasValue)
                    {
                        var info = playerInfo.Value;
                        Console.WriteLine($"[DEMO] Player {clientId}: {info.Name} ({info.IP}) - {info.Team}");

                        if (AmxModXCommands.IsPlayerAlive(clientId))
                        {
                            float health = AmxModXCommands.GetPlayerHealth(clientId);
                            float armor = AmxModXCommands.GetPlayerArmor(clientId);
                            float frags = AmxModXCommands.GetPlayerFrags(clientId);
                            Console.WriteLine($"[DEMO]   Health: {health}, Armor: {armor}, Frags: {frags}");
                        }
                    }
                }

                // 演示玩家查找功能 / Demonstrate player finding functionality
                if (connectedPlayers.Count > 0)
                {
                    int firstPlayer = connectedPlayers[0];
                    string firstName = AmxModXCommands.GetPlayerName(firstPlayer);

                    Console.WriteLine($"[DEMO] Testing player search for '{firstName}':");
                    var foundPlayers = FindPlayersByTarget(firstName);
                    Console.WriteLine($"[DEMO] Found {foundPlayers.Count} matching players");
                }

                Console.WriteLine("Full player management demo completed!");
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
                Console.WriteLine("Cleaning up Player Management Example...");

                // 注销所有命令 / Unregister all commands
                foreach (var kvp in _registeredCommands)
                {
                    bool success = AmxModXCommands.UnregisterCommand(kvp.Value);
                    Console.WriteLine($"Unregistered command '{kvp.Key}': {(success ? "Success" : "Failed")}");
                }

                // 注销所有事件 / Unregister all events
                foreach (var kvp in _registeredEvents)
                {
                    bool success = AmxModXCommands.UnregisterEvent(kvp.Value);
                    Console.WriteLine($"Unregistered event '{kvp.Key}': {(success ? "Success" : "Failed")}");
                }

                _registeredCommands.Clear();
                _registeredEvents.Clear();

                // 清理AMX Mod X命令系统 / Cleanup AMX Mod X command system
                AmxModXCommands.Cleanup();

                Console.WriteLine("Player Management Example cleaned up successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Cleanup error: {ex.Message}");
            }
        }
    }
}
