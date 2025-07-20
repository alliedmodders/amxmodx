# AMX Mod X C# Command Registration Interface

## 概述 / Overview

这是一个为 AMX Mod X 提供的 C# 命令注册接口，允许 C# 应用程序注册和管理 AMX Mod X 服务器命令。

This is a C# command registration interface for AMX Mod X that allows C# applications to register and manage AMX Mod X server commands.

## 特性 / Features

- ✅ 跨平台支持 (Windows, Linux, macOS) / Cross-platform support
- ✅ 类型安全的 C# 接口 / Type-safe C# interface
- ✅ 支持控制台、客户端和服务器命令 / Support for console, client, and server commands
- ✅ 菜单命令支持 / Menu command support
- ✅ 委托回调机制 / Delegate callback mechanism
- ✅ 完整的错误处理 / Complete error handling
- ✅ 多语言注释 / Multi-language comments

## 架构 / Architecture

```
C# Application Layer (AmxModXInterop.cs)
    ↓
P/Invoke Bridge (NativeMethods)
    ↓
C++ Bridge Layer (AmxModXBridge.cpp)
    ↓
AMX Mod X Core (amxmodx.cpp)
```

## 文件结构 / File Structure

```
├── AmxModXBridge.h          # C++ 桥接层头文件 / C++ bridge header
├── AmxModXBridge.cpp        # C++ 桥接层实现 / C++ bridge implementation
├── AmxModXInterop.cs        # C# 互操作层 / C# interop layer
├── AmxModXExample.cs        # 使用示例 / Usage examples
├── CMakeLists.txt           # 构建配置 / Build configuration
└── README.md               # 说明文档 / Documentation
```

## 编译 / Building

### 前提条件 / Prerequisites

- CMake 3.10+
- C++11 兼容编译器 / C++11 compatible compiler
- .NET Framework 4.0+ 或 .NET Core 2.0+ / .NET Framework 4.0+ or .NET Core 2.0+

### 编译步骤 / Build Steps

```bash
# 创建构建目录 / Create build directory
mkdir build
cd build

# 配置项目 / Configure project
cmake ..

# 编译 / Build
cmake --build .

# 安装 (可选) / Install (optional)
cmake --install .
```

### Windows 特定 / Windows Specific

```cmd
# 使用 Visual Studio / Using Visual Studio
cmake .. -G "Visual Studio 16 2019"
cmake --build . --config Release
```

### Linux/macOS 特定 / Linux/macOS Specific

```bash
# 使用 GCC/Clang / Using GCC/Clang
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

## 使用方法 / Usage

### 1. 初始化 / Initialization

```csharp
using AmxModX.Interop;

// 初始化命令系统 / Initialize command system
AmxModXCommands.Initialize();
```

### 2. 注册命令 / Register Commands

#### 客户端命令 / Client Commands

```csharp
// 注册客户端命令 / Register client command
int commandId = AmxModXCommands.RegisterClientCommand(
    command: "help",
    callback: OnHelpCommand,
    flags: 0, // 所有玩家可用 / Available to all players
    info: "Display help information",
    infoMultiLang: false
);

// 回调函数 / Callback function
private static void OnHelpCommand(int clientId, int commandId, int flags)
{
    Console.WriteLine($"Help command executed by client {clientId}");
    // 处理命令逻辑 / Handle command logic
}
```

#### 控制台命令 / Console Commands

```csharp
// 注册控制台命令 / Register console command
int commandId = AmxModXCommands.RegisterConsoleCommand(
    command: "amx_kick",
    callback: OnKickCommand,
    flags: 1, // 需要管理员权限 / Requires admin privileges
    info: "Kick a player from the server",
    infoMultiLang: false
);

// 回调函数 / Callback function
private static void OnKickCommand(int clientId, int commandId, int flags)
{
    Console.WriteLine($"Kick command executed by client {clientId}");
    // 处理踢出逻辑 / Handle kick logic
}
```

#### 服务器命令 / Server Commands

```csharp
// 注册服务器命令 / Register server command
int commandId = AmxModXCommands.RegisterServerCommand(
    command: "restart",
    callback: OnRestartCommand,
    flags: 1, // 需要管理员权限 / Requires admin privileges
    info: "Restart the server",
    infoMultiLang: false
);
```

### 3. 菜单命令 / Menu Commands

```csharp
// 注册菜单ID / Register menu ID
int menuId = AmxModXCommands.RegisterMenuId("MainMenu", global: false);

// 注册菜单命令 / Register menu command
int commandId = AmxModXCommands.RegisterMenuCommand(
    menuId: menuId,
    keyMask: 1023, // 支持按键1-0 / Support keys 1-0
    callback: OnMenuCommand
);

// 菜单回调函数 / Menu callback function
private static void OnMenuCommand(int clientId, int menuId, int key)
{
    switch (key)
    {
        case 1:
            Console.WriteLine("Player selected option 1");
            break;
        case 2:
            Console.WriteLine("Player selected option 2");
            break;
        default:
            Console.WriteLine($"Player selected option {key}");
            break;
    }
}
```

### 4. 查询命令信息 / Query Command Information

```csharp
// 获取命令信息 / Get command information
var commandInfo = AmxModXCommands.GetCommandInfo(commandId, CommandType.Client);
if (commandInfo.HasValue)
{
    var info = commandInfo.Value;
    Console.WriteLine($"Command: {info.Command}");
    Console.WriteLine($"Info: {info.Info}");
    Console.WriteLine($"Flags: {info.Flags}");
}

// 获取命令数量 / Get command count
int clientCommandCount = AmxModXCommands.GetCommandCount(CommandType.Client);
Console.WriteLine($"Client commands: {clientCommandCount}");
```

### 5. 清理 / Cleanup

```csharp
// 注销命令 / Unregister command
AmxModXCommands.UnregisterCommand(commandId);

// 清理命令系统 / Cleanup command system
AmxModXCommands.Cleanup();
```

## API 参考 / API Reference

### 命令类型 / Command Types

```csharp
public enum CommandType
{
    Console = 0,    // 控制台命令 / Console command
    Client = 1,     // 客户端命令 / Client command
    Server = 2      // 服务器命令 / Server command
}
```

### 委托定义 / Delegate Definitions

```csharp
// 命令回调委托 / Command callback delegate
public delegate void CommandCallback(int clientId, int commandId, int flags);

// 菜单回调委托 / Menu callback delegate
public delegate void MenuCallback(int clientId, int menuId, int key);
```

### 主要方法 / Main Methods

| 方法 / Method | 描述 / Description |
|---------------|-------------------|
| `Initialize()` | 初始化命令系统 / Initialize command system |
| `RegisterClientCommand()` | 注册客户端命令 / Register client command |
| `RegisterConsoleCommand()` | 注册控制台命令 / Register console command |
| `RegisterServerCommand()` | 注册服务器命令 / Register server command |
| `RegisterMenuCommand()` | 注册菜单命令 / Register menu command |
| `RegisterMenuId()` | 注册菜单ID / Register menu ID |
| `GetCommandInfo()` | 获取命令信息 / Get command information |
| `GetCommandCount()` | 获取命令数量 / Get command count |
| `UnregisterCommand()` | 注销命令 / Unregister command |
| `Cleanup()` | 清理资源 / Cleanup resources |

## 注意事项 / Notes

1. **线程安全** / **Thread Safety**: 所有接口都是线程安全的 / All interfaces are thread-safe
2. **内存管理** / **Memory Management**: 自动处理内存分配和释放 / Automatic memory allocation and deallocation
3. **错误处理** / **Error Handling**: 所有方法都包含适当的错误检查 / All methods include proper error checking
4. **跨平台** / **Cross-platform**: 支持 Windows、Linux 和 macOS / Supports Windows, Linux, and macOS

## 许可证 / License

本项目遵循 GNU General Public License v3.0 许可证。

This project is licensed under the GNU General Public License v3.0.
