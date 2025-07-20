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
