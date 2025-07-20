# AMX Mod X C# Command Registration Interface

## 概述 / Overview

这是AMX Mod X项目的C#命令注册接口，允许C#应用程序直接与AMX Mod X服务器进行交互，注册和管理各种类型的命令。

This is the C# command registration interface for the AMX Mod X project, allowing C# applications to directly interact with AMX Mod X servers to register and manage various types of commands.

## 特性 / Features

- ✅ **完全集成** / **Full Integration** - 直接集成到AMX Mod X核心，无需额外插件
- ✅ **跨平台支持** / **Cross-Platform** - 支持Windows、Linux、macOS
- ✅ **类型安全** / **Type Safe** - 强类型C#接口，编译时错误检查
- ✅ **高性能** / **High Performance** - 直接调用原生代码，最小化开销
- ✅ **线程安全** / **Thread Safe** - 所有接口都是线程安全的
- ✅ **委托回调** / **Delegate Callbacks** - 使用C#委托处理命令回调
- ✅ **自动内存管理** / **Automatic Memory Management** - 无需手动管理内存
- ✅ **完整文档** / **Complete Documentation** - 中英文XML文档注释

## 架构 / Architecture

```
┌─────────────────────────────────────┐
│          C# Application             │
│     (AmxModXExample.cs)            │
└─────────────────┬───────────────────┘
                  │
┌─────────────────▼───────────────────┐
│       C# Interop Layer              │
│     (AmxModXInterop.cs)            │
│  • AmxModXCommands                 │
│  • CommandCallback                 │
│  • MenuCallback                    │
└─────────────────┬───────────────────┘
                  │ P/Invoke
┌─────────────────▼───────────────────┐
│      C++ Bridge Layer               │
│    (csharp_bridge.cpp)             │
│  • RegisterConsoleCommand          │
│  • RegisterClientCommand           │
│  • RegisterServerCommand           │
└─────────────────┬───────────────────┘
                  │
┌─────────────────▼───────────────────┐
│       AMX Mod X Core                │
│      (amxmodx.cpp)                 │
│  • CmdMngr                         │
│  • MenuMngr                        │
│  • Forward System                  │
└─────────────────────────────────────┘
```

## 快速开始 / Quick Start

### 1. 安装 / Installation

```bash
# 克隆AMX Mod X仓库 / Clone AMX Mod X repository
git clone https://github.com/alliedmodders/amxmodx.git
cd amxmodx

# 编译AMX Mod X（包含C#桥接层） / Build AMX Mod X (including C# bridge)
python configure.py
ambuild
```

### 2. 在C#项目中使用 / Using in C# Project

```xml
<!-- 在你的.csproj文件中添加 / Add to your .csproj file -->
<PackageReference Include="AmxModX.CSharp" Version="1.0.0" />
```

### 3. 基本使用 / Basic Usage

```csharp
using AmxModX.Interop;

class Program
{
    static void Main()
    {
        // 初始化系统 / Initialize system
        AmxModXCommands.Initialize();

        // 注册客户端命令 / Register client command
        int helpCmd = AmxModXCommands.RegisterClientCommand(
            command: "help",
            callback: OnHelpCommand,
            flags: CommandFlags.All,
            info: "Display help information"
        );

        // 注册控制台命令 / Register console command
        int kickCmd = AmxModXCommands.RegisterConsoleCommand(
            command: "amx_kick",
            callback: OnKickCommand,
            flags: CommandFlags.Admin,
            info: "Kick a player"
        );

        Console.WriteLine("Commands registered successfully!");
    }

    static void OnHelpCommand(int clientId, int commandId, int flags)
    {
        Console.WriteLine($"Help requested by client {clientId}");
        // 处理帮助命令 / Handle help command
    }

    static void OnKickCommand(int clientId, int commandId, int flags)
    {
        Console.WriteLine($"Kick command executed by admin {clientId}");
        // 处理踢出命令 / Handle kick command
    }
}
```

## API 参考 / API Reference

### 核心类 / Core Classes

#### `AmxModXCommands`
主要的命令管理类 / Main command management class

```csharp
// 初始化和清理 / Initialization and cleanup
public static void Initialize()
public static void Cleanup()

// 命令注册 / Command registration
public static int RegisterClientCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
public static int RegisterConsoleCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)
public static int RegisterServerCommand(string command, CommandCallback callback, int flags = -1, string info = "", bool infoMultiLang = false)

// 菜单系统 / Menu system
public static int RegisterMenuId(string menuName, bool global = false)
public static int RegisterMenuCommand(int menuId, int keyMask, MenuCallback callback)

// 查询和管理 / Query and management
public static CommandInfo? GetCommandInfo(int commandId, CommandType commandType)
public static int GetCommandCount(CommandType commandType, int accessFlags = -1)
public static bool UnregisterCommand(int commandId)
```

### 委托类型 / Delegate Types

```csharp
// 命令回调 / Command callback
public delegate void CommandCallback(int clientId, int commandId, int flags);

// 菜单回调 / Menu callback
public delegate void MenuCallback(int clientId, int menuId, int key);
```

### 枚举类型 / Enum Types

```csharp
// 命令类型 / Command types
public enum CommandType
{
    Console = 0,    // 控制台命令 / Console command
    Client = 1,     // 客户端命令 / Client command
    Server = 2      // 服务器命令 / Server command
}
```

### 常量类 / Constant Classes

```csharp
// 命令标志 / Command flags
public static class CommandFlags
{
    public const int All = 0;           // 所有用户 / All users
    public const int Admin = 1;         // 管理员 / Admin
    public const int Hidden = -1;       // 隐藏 / Hidden
    public const int SuperAdmin = 2;    // 超级管理员 / Super admin
    public const int Vip = 4;           // VIP用户 / VIP users
    public const int Debug = 8;         // 调试 / Debug
}

// 菜单按键 / Menu keys
public static class MenuKeys
{
    public const int Key1 = 1 << 0;     // 按键1 / Key 1
    public const int Key2 = 1 << 1;     // 按键2 / Key 2
    // ... 更多按键 / More keys
    public const int AllNumbers = 1023;  // 所有数字键 / All number keys
}
```

### 主要方法 / Main Methods

#### 命令注册 / Command Registration
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `Initialize()` | 初始化命令系统 / Initialize command system |
| `RegisterClientCommand()` | 注册客户端命令 / Register client command |
| `RegisterConsoleCommand()` | 注册控制台命令 / Register console command |
| `RegisterServerCommand()` | 注册服务器命令 / Register server command |
| `RegisterMenuCommand()` | 注册菜单命令 / Register menu command |
| `RegisterMenuId()` | 注册菜单ID / Register menu ID |
| `UnregisterCommand()` | 注销命令 / Unregister command |
| `Cleanup()` | 清理资源 / Cleanup resources |

#### 命令执行 / Command Execution
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `ExecuteServerCommand()` | 执行服务器命令 / Execute server command |
| `ExecuteClientCommand()` | 执行客户端命令 / Execute client command |
| `ExecuteConsoleCommand()` | 执行控制台命令 / Execute console command |

#### 命令参数读取 / Command Argument Reading
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `GetCommandArgCount()` | 获取参数数量 / Get argument count |
| `GetCommandArg()` | 获取字符串参数 / Get string argument |
| `GetCommandArgs()` | 获取所有参数 / Get all arguments |
| `GetCommandArgInt()` | 获取整数参数 / Get integer argument |
| `GetCommandArgFloat()` | 获取浮点参数 / Get float argument |

#### 命令查询 / Command Query
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `FindCommand()` | 查找指定命令 / Find specific command |
| `GetCommandsCount()` | 获取命令数量 / Get command count |
| `GetCommandByIndex()` | 根据索引获取命令 / Get command by index |
| `GetAllCommands()` | 获取所有命令 / Get all commands |
| `CommandExists()` | 检查命令是否存在 / Check if command exists |

#### 事件系统 / Event System
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `RegisterEvent()` | 注册事件监听器 / Register event listener |
| `UnregisterEvent()` | 注销事件监听器 / Unregister event listener |
| `GetEventId()` | 获取事件ID / Get event ID |
| `GetEventInfo()` | 获取事件信息 / Get event information |

#### 事件参数读取 / Event Parameter Reading
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `GetEventArgCount()` | 获取事件参数数量 / Get event argument count |
| `GetEventArg()` | 获取事件参数 / Get event argument |
| `GetEventArgInt()` | 获取整数事件参数 / Get integer event argument |
| `GetEventArgFloat()` | 获取浮点事件参数 / Get float event argument |
| `GetEventArgString()` | 获取字符串事件参数 / Get string event argument |

#### Forward系统 / Forward System
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `CreateForward()` | 创建全局Forward / Create global forward |
| `CreateSingleForward()` | 创建单插件Forward / Create single plugin forward |
| `ExecuteForward()` | 执行Forward / Execute forward |
| `UnregisterForward()` | 注销Forward / Unregister forward |
| `GetForwardInfo()` | 获取Forward信息 / Get forward information |
| `CreateEventParam()` | 创建事件参数 / Create event parameter |

#### 玩家信息管理 / Player Information Management
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `IsPlayerValid()` | 检查玩家ID是否有效 / Check if player ID is valid |
| `GetPlayerInfo()` | 获取玩家完整信息 / Get complete player information |
| `GetPlayerStats()` | 获取玩家统计信息 / Get player statistics |
| `GetPlayerName()` | 获取玩家名称 / Get player name |
| `GetPlayerIP()` | 获取玩家IP地址 / Get player IP address |
| `GetPlayerAuthId()` | 获取玩家认证ID / Get player auth ID |
| `GetPlayerTeam()` | 获取玩家队伍 / Get player team |

#### 玩家状态查询 / Player State Query
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `IsPlayerInGame()` | 检查玩家是否在游戏中 / Check if player is in game |
| `IsPlayerBot()` | 检查玩家是否为机器人 / Check if player is bot |
| `IsPlayerAlive()` | 检查玩家是否存活 / Check if player is alive |
| `IsPlayerAuthorized()` | 检查玩家是否已认证 / Check if player is authorized |
| `IsPlayerConnecting()` | 检查玩家是否正在连接 / Check if player is connecting |
| `IsPlayerHLTV()` | 检查玩家是否为HLTV / Check if player is HLTV |

#### 玩家属性操作 / Player Property Operations
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `GetPlayerHealth()` | 获取玩家生命值 / Get player health |
| `GetPlayerArmor()` | 获取玩家护甲值 / Get player armor |
| `GetPlayerFrags()` | 获取玩家击杀数 / Get player frags |
| `SetPlayerHealth()` | 设置玩家生命值 / Set player health |
| `SetPlayerArmor()` | 设置玩家护甲值 / Set player armor |
| `SetPlayerFrags()` | 设置玩家击杀数 / Set player frags |
| `SetPlayerTeamInfo()` | 设置玩家队伍信息 / Set player team info |
| `SetPlayerFlags()` | 设置玩家权限标志 / Set player flags |

#### 玩家管理工具 / Player Management Tools
| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `GetMaxClients()` | 获取最大客户端数量 / Get maximum clients count |
| `GetConnectedPlayersCount()` | 获取已连接玩家数量 / Get connected players count |
| `GetConnectedPlayers()` | 获取已连接玩家列表 / Get connected players list |
| `KickPlayer()` | 踢出玩家 / Kick player |
| `SlayPlayer()` | 杀死玩家 / Slay player |
| `FindPlayersByName()` | 根据名称查找玩家 / Find players by name |

## 高级示例 / Advanced Examples

### 命令执行 / Command Execution

```csharp
// 执行服务器命令 / Execute server command
AmxModXCommands.ExecuteServerCommand("echo Hello from C#");
AmxModXCommands.ExecuteServerCommand("users");

// 执行客户端命令 / Execute client command
AmxModXCommands.ExecuteClientCommand(1, "say Hello from C# to client 1");
AmxModXCommands.ExecuteClientCommand(0, "say Broadcast message to all clients");

// 执行控制台命令 / Execute console command
AmxModXCommands.ExecuteConsoleCommand(0, "version");
```

### 命令参数读取 / Command Argument Reading

```csharp
static void OnMyCommand(int clientId, int commandId, int flags)
{
    // 获取参数数量 / Get argument count
    int argc = AmxModXCommands.GetCommandArgCount();
    Console.WriteLine($"Argument count: {argc}");

    // 读取各个参数 / Read individual arguments
    for (int i = 0; i < argc; i++)
    {
        string arg = AmxModXCommands.GetCommandArg(i);
        Console.WriteLine($"Arg[{i}]: {arg}");
    }

    // 读取类型化参数 / Read typed arguments
    if (argc > 1)
    {
        int intArg = AmxModXCommands.GetCommandArgInt(1);
        float floatArg = AmxModXCommands.GetCommandArgFloat(2);
        Console.WriteLine($"Int: {intArg}, Float: {floatArg}");
    }

    // 获取所有参数字符串 / Get all arguments string
    string allArgs = AmxModXCommands.GetCommandArgs();
    Console.WriteLine($"All args: {allArgs}");
}
```

### 命令查询 / Command Query

```csharp
// 查找特定命令 / Find specific command
var cmdInfo = AmxModXCommands.FindCommand("help", CommandType.Client);
if (cmdInfo.HasValue)
{
    Console.WriteLine($"Found: {cmdInfo.Value.Command} - {cmdInfo.Value.Info}");
}

// 获取所有客户端命令 / Get all client commands
var allCommands = AmxModXCommands.GetAllCommands(CommandType.Client);
foreach (var cmd in allCommands)
{
    Console.WriteLine($"{cmd.Command}: {cmd.Info} (Flags: {cmd.Flags})");
}

// 检查命令是否存在 / Check if command exists
bool exists = AmxModXCommands.CommandExists("amx_kick", CommandType.Console);
Console.WriteLine($"amx_kick exists: {exists}");
```

### 事件系统 / Event System

```csharp
// 注册事件监听器 / Register event listeners
int connectEvent = AmxModXCommands.RegisterEvent(
    eventName: "client_connect",
    callback: OnPlayerConnect,
    flags: EventFlags.Client | EventFlags.Player
);

int deathEvent = AmxModXCommands.RegisterEvent(
    eventName: "DeathMsg",
    callback: OnPlayerDeath,
    flags: EventFlags.Client,
    conditions: "1>0" // 确保有杀手ID / Ensure killer ID exists
);

// 事件回调函数 / Event callback functions
static void OnPlayerConnect(int eventId, int clientId, int numParams)
{
    Console.WriteLine($"Player {clientId} connected");

    // 读取事件参数 / Read event parameters
    int argc = AmxModXCommands.GetEventArgCount();
    for (int i = 0; i < argc; i++)
    {
        string arg = AmxModXCommands.GetEventArgString(i);
        Console.WriteLine($"Param[{i}]: {arg}");
    }
}

static void OnPlayerDeath(int eventId, int clientId, int numParams)
{
    // 读取死亡信息 / Read death information
    int killerId = AmxModXCommands.GetEventArgInt(1);
    int victimId = AmxModXCommands.GetEventArgInt(2);
    int weaponId = AmxModXCommands.GetEventArgInt(3);

    Console.WriteLine($"Player {victimId} killed by {killerId} with weapon {weaponId}");
}
```

### Forward系统 / Forward System

```csharp
// 创建全局Forward / Create global forward
int damageForward = AmxModXCommands.CreateForward(
    forwardName: "player_damage",
    execType: ForwardExecType.Continue,
    ForwardParamType.Cell,    // 受害者ID / Victim ID
    ForwardParamType.Cell,    // 攻击者ID / Attacker ID
    ForwardParamType.Float,   // 伤害值 / Damage amount
    ForwardParamType.Cell     // 武器ID / Weapon ID
);

// 创建单插件Forward / Create single plugin forward
int customForward = AmxModXCommands.CreateSingleForward(
    functionName: "on_custom_event",
    callback: OnCustomForwardCallback,
    ForwardParamType.Cell,    // 事件类型 / Event type
    ForwardParamType.String   // 事件数据 / Event data
);

// Forward回调函数 / Forward callback function
static int OnCustomForwardCallback(int forwardId, int numParams)
{
    Console.WriteLine($"Custom forward {forwardId} called with {numParams} parameters");
    return 1; // 继续执行 / Continue execution
}

// 执行Forward / Execute forward
var result = AmxModXCommands.ExecuteForward(
    damageForward,
    AmxModXCommands.CreateEventParam(1),      // 受害者ID / Victim ID
    AmxModXCommands.CreateEventParam(2),      // 攻击者ID / Attacker ID
    AmxModXCommands.CreateEventParam(50.0f),  // 伤害值 / Damage amount
    AmxModXCommands.CreateEventParam(3)       // 武器ID / Weapon ID
);

Console.WriteLine($"Forward result: Success={result.Success}, Return={result.Result}");
```

### 玩家信息管理 / Player Information Management

```csharp
// 获取玩家完整信息 / Get complete player information
var playerInfo = AmxModXCommands.GetPlayerInfo(clientId);
if (playerInfo.HasValue)
{
    var info = playerInfo.Value;
    Console.WriteLine($"Player: {info.Name} ({info.AuthId})");
    Console.WriteLine($"IP: {info.IP}, Team: {info.Team}");
    Console.WriteLine($"In Game: {info.IsInGame}, Bot: {info.IsBot}, Alive: {info.IsAlive}");
}

// 获取玩家统计信息 / Get player statistics
var playerStats = AmxModXCommands.GetPlayerStats(clientId);
if (playerStats.HasValue)
{
    var stats = playerStats.Value;
    Console.WriteLine($"Health: {stats.Health}, Armor: {stats.Armor}");
    Console.WriteLine($"Frags: {stats.Frags}, Deaths: {stats.Deaths}");
    Console.WriteLine($"Current Weapon: {stats.CurrentWeapon}");
}

// 玩家状态检查 / Player state checking
bool isValid = AmxModXCommands.IsPlayerValid(clientId);
bool isInGame = AmxModXCommands.IsPlayerInGame(clientId);
bool isAlive = AmxModXCommands.IsPlayerAlive(clientId);
bool isBot = AmxModXCommands.IsPlayerBot(clientId);

Console.WriteLine($"Player {clientId}: Valid={isValid}, InGame={isInGame}, Alive={isAlive}, Bot={isBot}");
```

### 玩家属性操作 / Player Property Operations

```csharp
// 设置玩家属性 / Set player properties
if (AmxModXCommands.IsPlayerAlive(clientId))
{
    // 设置生命值和护甲 / Set health and armor
    bool healthSet = AmxModXCommands.SetPlayerHealth(clientId, 100.0f);
    bool armorSet = AmxModXCommands.SetPlayerArmor(clientId, 100.0f);

    Console.WriteLine($"Health set: {healthSet}, Armor set: {armorSet}");

    // 设置击杀数 / Set frags
    AmxModXCommands.SetPlayerFrags(clientId, 10.0f);

    // 设置队伍信息 / Set team info
    AmxModXCommands.SetPlayerTeamInfo(clientId, 2, "Terrorists");
}

// 获取玩家属性 / Get player properties
float health = AmxModXCommands.GetPlayerHealth(clientId);
float armor = AmxModXCommands.GetPlayerArmor(clientId);
float frags = AmxModXCommands.GetPlayerFrags(clientId);
int teamId = AmxModXCommands.GetPlayerTeamId(clientId);

Console.WriteLine($"Health: {health}, Armor: {armor}, Frags: {frags}, Team: {teamId}");
```

### 玩家管理工具 / Player Management Tools

```csharp
// 获取服务器信息 / Get server information
int maxClients = AmxModXCommands.GetMaxClients();
int connectedCount = AmxModXCommands.GetConnectedPlayersCount();
Console.WriteLine($"Players: {connectedCount}/{maxClients}");

// 获取所有连接的玩家 / Get all connected players
var connectedPlayers = AmxModXCommands.GetConnectedPlayers();
foreach (int playerId in connectedPlayers)
{
    string playerName = AmxModXCommands.GetPlayerName(playerId);
    string playerIP = AmxModXCommands.GetPlayerIP(playerId);
    Console.WriteLine($"Player {playerId}: {playerName} ({playerIP})");
}

// 根据名称查找玩家 / Find players by name
var foundPlayers = AmxModXCommands.FindPlayersByName("John", exactMatch: false);
foreach (int playerId in foundPlayers)
{
    string playerName = AmxModXCommands.GetPlayerName(playerId);
    Console.WriteLine($"Found player: {playerId} - {playerName}");
}

// 玩家管理操作 / Player management operations
// 踢出玩家 / Kick player
bool kicked = AmxModXCommands.KickPlayer(clientId, "Kicked by admin");

// 杀死玩家 / Slay player
if (AmxModXCommands.IsPlayerAlive(clientId))
{
    bool slayed = AmxModXCommands.SlayPlayer(clientId);
    Console.WriteLine($"Player slayed: {slayed}");
}
```

### 综合示例：玩家监控系统 / Comprehensive Example: Player Monitoring System

```csharp
// 注册玩家连接事件 / Register player connect event
int connectEvent = AmxModXCommands.RegisterEvent(
    eventName: "client_connect",
    callback: OnPlayerConnect,
    flags: EventFlags.Client | EventFlags.Player
);

static void OnPlayerConnect(int eventId, int clientId, int numParams)
{
    // 等待玩家完全连接 / Wait for player to fully connect
    System.Threading.Tasks.Task.Delay(1000).ContinueWith(_ =>
    {
        if (AmxModXCommands.IsPlayerInGame(clientId))
        {
            var playerInfo = AmxModXCommands.GetPlayerInfo(clientId);
            if (playerInfo.HasValue)
            {
                var info = playerInfo.Value;
                Console.WriteLine($"Player connected: {info.Name} ({info.AuthId}) from {info.IP}");

                // 检查玩家状态 / Check player status
                if (info.IsBot)
                {
                    Console.WriteLine($"  Bot player detected");
                }
                else if (!info.IsAuthorized)
                {
                    Console.WriteLine($"  Warning: Player not authorized");
                }

                // 欢迎消息 / Welcome message
                AmxModXCommands.ExecuteClientCommand(clientId, $"say Welcome {info.Name}!");

                // 设置初始属性 / Set initial properties
                if (AmxModXCommands.IsPlayerAlive(clientId))
                {
                    AmxModXCommands.SetPlayerHealth(clientId, 100.0f);
                    AmxModXCommands.SetPlayerArmor(clientId, 0.0f);
                }
            }
        }
    });
}

// 注册管理员命令 / Register admin command
int playerInfoCmd = AmxModXCommands.RegisterConsoleCommand(
    command: "amx_playerinfo",
    callback: OnPlayerInfoCommand,
    flags: CommandFlags.Admin,
    info: "Display detailed player information"
);

static void OnPlayerInfoCommand(int clientId, int commandId, int flags)
{
    int argc = AmxModXCommands.GetCommandArgCount();
    if (argc < 2)
    {
        Console.WriteLine("Usage: amx_playerinfo <player>");
        return;
    }

    string target = AmxModXCommands.GetCommandArg(1);
    var foundPlayers = AmxModXCommands.FindPlayersByName(target);

    if (foundPlayers.Count == 0)
    {
        Console.WriteLine($"No players found matching '{target}'");
        return;
    }

    foreach (int playerId in foundPlayers)
    {
        var playerInfo = AmxModXCommands.GetPlayerInfo(playerId);
        var playerStats = AmxModXCommands.GetPlayerStats(playerId);

        if (playerInfo.HasValue)
        {
            var info = playerInfo.Value;
            Console.WriteLine($"Player {playerId}: {info.Name}");
            Console.WriteLine($"  Auth ID: {info.AuthId}");
            Console.WriteLine($"  IP: {info.IP}");
            Console.WriteLine($"  Team: {info.Team} (ID: {info.TeamId})");
            Console.WriteLine($"  Status: InGame={info.IsInGame}, Alive={info.IsAlive}, Bot={info.IsBot}");

            if (playerStats.HasValue)
            {
                var stats = playerStats.Value;
                Console.WriteLine($"  Stats: Health={stats.Health}, Armor={stats.Armor}, Frags={stats.Frags}");
            }
        }
    }
}
```

### 菜单系统 / Menu System

```csharp
// 创建菜单 / Create menu
int menuId = AmxModXCommands.RegisterMenuId("AdminMenu", global: false);

// 注册菜单处理器 / Register menu handler
int menuCmd = AmxModXCommands.RegisterMenuCommand(
    menuId: menuId,
    keyMask: MenuKeys.Key1 | MenuKeys.Key2 | MenuKeys.Key3,
    callback: OnAdminMenuCommand
);

static void OnAdminMenuCommand(int clientId, int menuId, int key)
{
    switch (key)
    {
        case 1:
            Console.WriteLine("Player Management selected");
            break;
        case 2:
            Console.WriteLine("Server Settings selected");
            break;
        case 3:
            Console.WriteLine("Exit selected");
            break;
    }
}
```

### 权限检查 / Permission Checking

```csharp
static void OnAdminCommand(int clientId, int commandId, int flags)
{
    // 检查权限 / Check permissions
    if ((flags & CommandFlags.Admin) == 0)
    {
        Console.WriteLine($"Client {clientId} lacks admin privileges");
        return;
    }

    // 执行管理员命令 / Execute admin command
    Console.WriteLine($"Admin command executed by {clientId}");
}
```

### 命令信息查询 / Command Information Query

```csharp
// 获取命令信息 / Get command information
var info = AmxModXCommands.GetCommandInfo(commandId, CommandType.Client);
if (info.HasValue)
{
    Console.WriteLine($"Command: {info.Value.Command}");
    Console.WriteLine($"Info: {info.Value.Info}");
    Console.WriteLine($"Flags: {info.Value.Flags}");
    Console.WriteLine($"Listable: {info.Value.Listable}");
}

// 获取统计信息 / Get statistics
int clientCommands = AmxModXCommands.GetCommandCount(CommandType.Client);
int adminCommands = AmxModXCommands.GetCommandCount(CommandType.Console, CommandFlags.Admin);
Console.WriteLine($"Client commands: {clientCommands}, Admin commands: {adminCommands}");
```

## 构建说明 / Build Instructions

### 前提条件 / Prerequisites

- Python 3.6+
- AMBuild
- C++11兼容编译器 / C++11 compatible compiler
- .NET SDK 5.0+ 或 .NET Framework 4.8+ / .NET SDK 5.0+ or .NET Framework 4.8+

### 编译步骤 / Build Steps

```bash
# 1. 配置构建 / Configure build
python configure.py --enable-optimize

# 2. 编译原生代码 / Build native code
ambuild

# 3. 编译C#代码 / Build C# code
cd amxmodx/csharp
dotnet build AmxModX.CSharp.csproj -c Release

# 4. 打包 / Package
dotnet pack AmxModX.CSharp.csproj -c Release
```

## 注意事项 / Important Notes

1. **初始化顺序** / **Initialization Order**: 必须先调用`Initialize()`再使用其他接口
2. **线程安全** / **Thread Safety**: 所有接口都是线程安全的，可以从多个线程调用
3. **内存管理** / **Memory Management**: 系统自动管理内存，无需手动释放
4. **错误处理** / **Error Handling**: 所有方法都包含适当的错误检查和异常处理
5. **平台兼容性** / **Platform Compatibility**: 支持Windows、Linux、macOS

## 许可证 / License

本项目遵循GNU General Public License v3.0许可证。

This project is licensed under the GNU General Public License v3.0.

## 贡献 / Contributing

欢迎提交Issue和Pull Request！请确保遵循项目的编码规范。

Issues and Pull Requests are welcome! Please ensure you follow the project's coding standards.
