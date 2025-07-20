// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// EventSystemExample.cs - Event system examples for AMX Mod X C# Interface
// 事件系统示例，演示如何使用C#接口处理游戏事件和Forward系统

using System;
using System.Collections.Generic;
using AmxModX.Interop;

namespace AmxModX.Examples
{
    /// <summary>
    /// AMX Mod X 事件系统示例 / AMX Mod X event system examples
    /// 演示事件监听、Forward创建和参数处理功能 / Demonstrates event listening, forward creation, and parameter handling
    /// </summary>
    public static class EventSystemExample
    {
        private static readonly Dictionary<string, int> _registeredEvents = new Dictionary<string, int>();
        private static readonly Dictionary<string, int> _registeredForwards = new Dictionary<string, int>();

        /// <summary>
        /// 初始化事件系统示例 / Initialize event system example
        /// </summary>
        public static void Initialize()
        {
            Console.WriteLine("Initializing Event System Example...");

            try
            {
                // 初始化AMX Mod X命令系统 / Initialize AMX Mod X command system
                AmxModXCommands.Initialize();

                // 注册游戏事件监听器 / Register game event listeners
                RegisterGameEvents();

                // 创建自定义Forward / Create custom forwards
                CreateCustomForwards();

                Console.WriteLine("Event System Example initialized successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to initialize Event System Example: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// 注册游戏事件监听器 / Register game event listeners
        /// </summary>
        private static void RegisterGameEvents()
        {
            Console.WriteLine("Registering game event listeners...");

            // 注册玩家连接事件 / Register player connect event
            int connectEvent = AmxModXCommands.RegisterEvent(
                eventName: "client_connect",
                callback: OnPlayerConnect,
                flags: EventFlags.Client | EventFlags.Player
            );
            _registeredEvents["client_connect"] = connectEvent;

            // 注册玩家断开连接事件 / Register player disconnect event
            int disconnectEvent = AmxModXCommands.RegisterEvent(
                eventName: "client_disconnect",
                callback: OnPlayerDisconnect,
                flags: EventFlags.Client | EventFlags.Player
            );
            _registeredEvents["client_disconnect"] = disconnectEvent;

            // 注册玩家死亡事件 / Register player death event
            int deathEvent = AmxModXCommands.RegisterEvent(
                eventName: "DeathMsg",
                callback: OnPlayerDeath,
                flags: EventFlags.Client,
                conditions: "1>0" // 确保有杀手ID / Ensure killer ID exists
            );
            _registeredEvents["DeathMsg"] = deathEvent;

            // 注册回合开始事件 / Register round start event
            int roundStartEvent = AmxModXCommands.RegisterEvent(
                eventName: "HLTV",
                callback: OnRoundStart,
                flags: EventFlags.World,
                conditions: "1=0&2=0" // HLTV消息的特定条件 / Specific conditions for HLTV message
            );
            _registeredEvents["HLTV"] = roundStartEvent;

            // 注册文本消息事件 / Register text message event
            int textMsgEvent = AmxModXCommands.RegisterEvent(
                eventName: "TextMsg",
                callback: OnTextMessage,
                flags: EventFlags.Client | EventFlags.World
            );
            _registeredEvents["TextMsg"] = textMsgEvent;

            Console.WriteLine($"Registered {_registeredEvents.Count} game event listeners");
        }

        /// <summary>
        /// 创建自定义Forward / Create custom forwards
        /// </summary>
        private static void CreateCustomForwards()
        {
            Console.WriteLine("Creating custom forwards...");

            // 创建玩家伤害Forward / Create player damage forward
            int damageForward = AmxModXCommands.CreateForward(
                forwardName: "player_damage",
                execType: ForwardExecType.Continue,
                ForwardParamType.Cell,    // 受害者ID / Victim ID
                ForwardParamType.Cell,    // 攻击者ID / Attacker ID
                ForwardParamType.Float,   // 伤害值 / Damage amount
                ForwardParamType.Cell     // 武器ID / Weapon ID
            );
            _registeredForwards["player_damage"] = damageForward;

            // 创建物品拾取Forward / Create item pickup forward
            int pickupForward = AmxModXCommands.CreateForward(
                forwardName: "item_pickup",
                execType: ForwardExecType.Stop,
                ForwardParamType.Cell,    // 玩家ID / Player ID
                ForwardParamType.String   // 物品名称 / Item name
            );
            _registeredForwards["item_pickup"] = pickupForward;

            // 创建单插件Forward示例 / Create single plugin forward example
            int singleForward = AmxModXCommands.CreateSingleForward(
                functionName: "on_custom_event",
                callback: OnCustomForwardCallback,
                ForwardParamType.Cell,    // 事件类型 / Event type
                ForwardParamType.String,  // 事件数据 / Event data
                ForwardParamType.Float    // 时间戳 / Timestamp
            );
            _registeredForwards["on_custom_event"] = singleForward;

            Console.WriteLine($"Created {_registeredForwards.Count} custom forwards");
        }

        // ========== 事件回调函数 / Event Callback Functions ==========

        /// <summary>
        /// 玩家连接事件回调 / Player connect event callback
        /// </summary>
        private static void OnPlayerConnect(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[CONNECT] Player {clientId} connected (Event ID: {eventId}, Params: {numParams})");

            try
            {
                // 读取事件参数 / Read event parameters
                DemonstrateEventParameterReading("Player Connect");

                // 获取玩家信息 / Get player information
                if (numParams > 0)
                {
                    string playerName = AmxModXCommands.GetEventArgString(1);
                    string playerAuth = AmxModXCommands.GetEventArgString(2);
                    
                    Console.WriteLine($"[CONNECT]   Player Name: '{playerName}'");
                    Console.WriteLine($"[CONNECT]   Player Auth: '{playerAuth}'");
                }

                // 触发自定义Forward / Trigger custom forward
                TriggerCustomForward("player_connect", clientId);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[CONNECT] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 玩家断开连接事件回调 / Player disconnect event callback
        /// </summary>
        private static void OnPlayerDisconnect(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[DISCONNECT] Player {clientId} disconnected (Event ID: {eventId}, Params: {numParams})");

            try
            {
                // 读取断开连接原因 / Read disconnect reason
                if (numParams > 0)
                {
                    string reason = AmxModXCommands.GetEventArgString(1);
                    Console.WriteLine($"[DISCONNECT]   Reason: '{reason}'");
                }

                // 演示事件参数读取 / Demonstrate event parameter reading
                DemonstrateEventParameterReading("Player Disconnect");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[DISCONNECT] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 玩家死亡事件回调 / Player death event callback
        /// </summary>
        private static void OnPlayerDeath(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[DEATH] Death event (Event ID: {eventId}, Client: {clientId}, Params: {numParams})");

            try
            {
                // 读取死亡信息 / Read death information
                if (numParams >= 3)
                {
                    int killerId = AmxModXCommands.GetEventArgInt(1);
                    int victimId = AmxModXCommands.GetEventArgInt(2);
                    int weaponId = AmxModXCommands.GetEventArgInt(3);
                    
                    Console.WriteLine($"[DEATH]   Killer: {killerId}, Victim: {victimId}, Weapon: {weaponId}");

                    // 触发伤害Forward / Trigger damage forward
                    if (_registeredForwards.ContainsKey("player_damage"))
                    {
                        var result = AmxModXCommands.ExecuteForward(
                            _registeredForwards["player_damage"],
                            AmxModXCommands.CreateEventParam(victimId),
                            AmxModXCommands.CreateEventParam(killerId),
                            AmxModXCommands.CreateEventParam(100.0f), // 致命伤害 / Fatal damage
                            AmxModXCommands.CreateEventParam(weaponId)
                        );
                        
                        Console.WriteLine($"[DEATH]   Damage forward result: Success={result.Success}, Return={result.Result}");
                    }
                }

                DemonstrateEventParameterReading("Player Death");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[DEATH] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 回合开始事件回调 / Round start event callback
        /// </summary>
        private static void OnRoundStart(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[ROUND START] Round started (Event ID: {eventId}, Params: {numParams})");

            try
            {
                DemonstrateEventParameterReading("Round Start");

                // 广播回合开始消息 / Broadcast round start message
                AmxModXCommands.ExecuteServerCommand("echo [C# EVENT] New round started!");
                AmxModXCommands.ExecuteClientCommand(0, "say [C# EVENT] Round started - Good luck!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ROUND START] Error: {ex.Message}");
            }
        }

        /// <summary>
        /// 文本消息事件回调 / Text message event callback
        /// </summary>
        private static void OnTextMessage(int eventId, int clientId, int numParams)
        {
            Console.WriteLine($"[TEXT MSG] Text message (Event ID: {eventId}, Client: {clientId}, Params: {numParams})");

            try
            {
                // 读取文本消息内容 / Read text message content
                if (numParams > 1)
                {
                    int msgType = AmxModXCommands.GetEventArgInt(1);
                    string message = AmxModXCommands.GetEventArgString(2);
                    
                    Console.WriteLine($"[TEXT MSG]   Type: {msgType}, Message: '{message}'");

                    // 检查特定消息 / Check for specific messages
                    if (message.Contains("Terrorists Win") || message.Contains("Counter-Terrorists Win"))
                    {
                        Console.WriteLine("[TEXT MSG]   Round end detected!");
                        TriggerCustomForward("round_end", 0);
                    }
                }

                DemonstrateEventParameterReading("Text Message");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[TEXT MSG] Error: {ex.Message}");
            }
        }

        // ========== Forward回调函数 / Forward Callback Functions ==========

        /// <summary>
        /// 自定义Forward回调 / Custom forward callback
        /// </summary>
        private static int OnCustomForwardCallback(int forwardId, int numParams)
        {
            Console.WriteLine($"[CUSTOM FORWARD] Forward {forwardId} called with {numParams} parameters");

            try
            {
                // 这里可以处理Forward参数 / Here you can process forward parameters
                // 注意：实际的参数读取需要通过特定的API / Note: actual parameter reading requires specific APIs
                
                Console.WriteLine("[CUSTOM FORWARD] Processing custom forward logic...");
                
                // 返回值影响Forward的执行流程 / Return value affects forward execution flow
                return 1; // 继续执行 / Continue execution
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[CUSTOM FORWARD] Error: {ex.Message}");
                return 0; // 停止执行 / Stop execution
            }
        }

        // ========== 辅助方法 / Helper Methods ==========

        /// <summary>
        /// 演示事件参数读取 / Demonstrate event parameter reading
        /// </summary>
        private static void DemonstrateEventParameterReading(string eventContext)
        {
            try
            {
                int argc = AmxModXCommands.GetEventArgCount();
                Console.WriteLine($"[{eventContext}] Event has {argc} parameters:");

                for (int i = 0; i < argc && i < 5; i++) // 限制显示前5个参数 / Limit to first 5 parameters
                {
                    var param = AmxModXCommands.GetEventArg(i);
                    if (param.HasValue)
                    {
                        var p = param.Value;
                        string typeStr = p.Type switch
                        {
                            0 => "int",
                            1 => "float", 
                            2 => "string",
                            _ => "unknown"
                        };

                        Console.WriteLine($"[{eventContext}]   Param[{i}] ({typeStr}): " +
                            $"int={p.IntValue}, float={p.FloatValue:F2}, string='{p.StringValue}'");
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[{eventContext}] Parameter reading error: {ex.Message}");
            }
        }

        /// <summary>
        /// 触发自定义Forward / Trigger custom forward
        /// </summary>
        private static void TriggerCustomForward(string eventName, int clientId)
        {
            try
            {
                if (_registeredForwards.ContainsKey("on_custom_event"))
                {
                    var result = AmxModXCommands.ExecuteForward(
                        _registeredForwards["on_custom_event"],
                        AmxModXCommands.CreateEventParam(1), // 事件类型 / Event type
                        AmxModXCommands.CreateEventParam(eventName), // 事件名称 / Event name
                        AmxModXCommands.CreateEventParam(DateTime.Now.Ticks / 10000000.0f) // 时间戳 / Timestamp
                    );
                    
                    Console.WriteLine($"[FORWARD] Custom forward '{eventName}' result: Success={result.Success}, Return={result.Result}");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[FORWARD] Error triggering custom forward: {ex.Message}");
            }
        }

        /// <summary>
        /// 运行完整的事件系统演示 / Run complete event system demonstration
        /// </summary>
        public static void RunFullDemo()
        {
            Console.WriteLine("\n========== Full Event System Demo ==========");

            try
            {
                // 模拟触发一些事件 / Simulate triggering some events
                Console.WriteLine("Simulating event triggers...");

                // 模拟玩家连接 / Simulate player connect
                OnPlayerConnect(1, 1, 2);

                // 模拟玩家死亡 / Simulate player death  
                OnPlayerDeath(2, 1, 3);

                // 模拟回合开始 / Simulate round start
                OnRoundStart(3, 0, 2);

                // 模拟文本消息 / Simulate text message
                OnTextMessage(4, 0, 2);

                // 演示Forward信息查询 / Demonstrate forward info query
                DemonstrateForwardInfoQuery();

                Console.WriteLine("Full event system demo completed!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Demo execution error: {ex.Message}");
            }
        }

        /// <summary>
        /// 演示Forward信息查询 / Demonstrate forward info query
        /// </summary>
        private static void DemonstrateForwardInfoQuery()
        {
            Console.WriteLine("\n[FORWARD INFO] Querying forward information:");

            foreach (var kvp in _registeredForwards)
            {
                try
                {
                    var info = AmxModXCommands.GetForwardInfo(kvp.Value);
                    if (info.HasValue)
                    {
                        var f = info.Value;
                        Console.WriteLine($"[FORWARD INFO]   {kvp.Key}: ID={f.ForwardId}, " +
                            $"Params={f.NumParams}, ExecType={f.ExecType}, Valid={f.IsValid}");
                    }
                    else
                    {
                        Console.WriteLine($"[FORWARD INFO]   {kvp.Key}: Info not available");
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"[FORWARD INFO]   {kvp.Key}: Error - {ex.Message}");
                }
            }
        }

        /// <summary>
        /// 清理资源 / Cleanup resources
        /// </summary>
        public static void Cleanup()
        {
            try
            {
                Console.WriteLine("Cleaning up Event System Example...");

                // 注销所有事件监听器 / Unregister all event listeners
                foreach (var kvp in _registeredEvents)
                {
                    bool success = AmxModXCommands.UnregisterEvent(kvp.Value);
                    Console.WriteLine($"Unregistered event '{kvp.Key}': {(success ? "Success" : "Failed")}");
                }

                // 注销所有Forward / Unregister all forwards
                foreach (var kvp in _registeredForwards)
                {
                    bool success = AmxModXCommands.UnregisterForward(kvp.Value);
                    Console.WriteLine($"Unregistered forward '{kvp.Key}': {(success ? "Success" : "Failed")}");
                }

                _registeredEvents.Clear();
                _registeredForwards.Clear();

                // 清理AMX Mod X命令系统 / Cleanup AMX Mod X command system
                AmxModXCommands.Cleanup();

                Console.WriteLine("Event System Example cleaned up successfully!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Cleanup error: {ex.Message}");
            }
        }
    }
}
