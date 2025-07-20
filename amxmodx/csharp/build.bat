@echo off
REM vim: set ts=4 sw=4 tw=99 noet:
REM
REM AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
REM Copyright (C) The AMX Mod X Development Team.
REM
REM This software is licensed under the GNU General Public License, version 3 or higher.
REM Additional exceptions apply. For full license details, see LICENSE.txt or visit:
REM     https://alliedmods.net/amxmodx-license

REM build.bat - Build script for AMX Mod X C# Bridge (Windows)
REM 构建脚本，用于编译AMX Mod X C#桥接层 (Windows版本)

setlocal enabledelayedexpansion

REM Configuration
set BUILD_TYPE=Release
set CLEAN_BUILD=false
set BUILD_NATIVE=true
set BUILD_CSHARP=true
set RUN_TESTS=false
set VERBOSE=false

REM Parse command line arguments
:parse_args
if "%~1"=="" goto :args_done
if /i "%~1"=="--clean" (
    set CLEAN_BUILD=true
    shift
    goto :parse_args
)
if /i "%~1"=="--debug" (
    set BUILD_TYPE=Debug
    shift
    goto :parse_args
)
if /i "%~1"=="--no-native" (
    set BUILD_NATIVE=false
    shift
    goto :parse_args
)
if /i "%~1"=="--no-csharp" (
    set BUILD_CSHARP=false
    shift
    goto :parse_args
)
if /i "%~1"=="--test" (
    set RUN_TESTS=true
    shift
    goto :parse_args
)
if /i "%~1"=="--verbose" (
    set VERBOSE=true
    shift
    goto :parse_args
)
if /i "%~1"=="--help" (
    goto :show_usage
)
echo [ERROR] Unknown option: %~1
goto :show_usage

:args_done

REM Get script directory
set SCRIPT_DIR=%~dp0
set SCRIPT_DIR=%SCRIPT_DIR:~0,-1%
for %%i in ("%SCRIPT_DIR%") do set AMXMODX_ROOT=%%~dpi
set AMXMODX_ROOT=%AMXMODX_ROOT:~0,-1%
for %%i in ("%AMXMODX_ROOT%") do set PROJECT_ROOT=%%~dpi
set PROJECT_ROOT=%PROJECT_ROOT:~0,-1%

echo.
echo ╔══════════════════════════════════════════════════════════════╗
echo ║              AMX Mod X C# Bridge Build Script               ║
echo ║                        (Windows)                             ║
echo ║  Builds both native C++ bridge and C# interop library      ║
echo ╚══════════════════════════════════════════════════════════════╝
echo.

echo [INFO] Build configuration:
echo [INFO]   Build Type: %BUILD_TYPE%
echo [INFO]   Clean Build: %CLEAN_BUILD%
echo [INFO]   Build Native: %BUILD_NATIVE%
echo [INFO]   Build C#: %BUILD_CSHARP%
echo [INFO]   Run Tests: %RUN_TESTS%
echo [INFO]   Verbose: %VERBOSE%
echo [INFO]   Project Root: %PROJECT_ROOT%
echo.

REM Check prerequisites
echo [INFO] Checking prerequisites...

REM Check for Python
python --version >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Python is required but not installed
    exit /b 1
)

REM Check for .NET SDK
if "%BUILD_CSHARP%"=="true" (
    dotnet --version >nul 2>&1
    if errorlevel 1 (
        echo [ERROR] .NET SDK is required but not installed
        exit /b 1
    )
    
    for /f "tokens=*" %%i in ('dotnet --version') do set DOTNET_VERSION=%%i
    echo [INFO] Found .NET SDK version: !DOTNET_VERSION!
)

REM Check for AMBuild
if "%BUILD_NATIVE%"=="true" (
    ambuild --version >nul 2>&1
    if errorlevel 1 (
        echo [ERROR] AMBuild is required but not installed
        echo [INFO] Install with: pip install ambuild
        exit /b 1
    )
    
    echo [INFO] Found AMBuild
)

echo [SUCCESS] Prerequisites check passed
echo.

REM Clean build if requested
if "%CLEAN_BUILD%"=="true" (
    echo [INFO] Cleaning previous build artifacts...
    
    if exist "%PROJECT_ROOT%\build" (
        rmdir /s /q "%PROJECT_ROOT%\build"
        echo [INFO] Removed build directory
    )
    
    if exist "%SCRIPT_DIR%\bin" (
        rmdir /s /q "%SCRIPT_DIR%\bin"
        echo [INFO] Removed C# bin directory
    )
    
    if exist "%SCRIPT_DIR%\obj" (
        rmdir /s /q "%SCRIPT_DIR%\obj"
        echo [INFO] Removed C# obj directory
    )
    
    echo [SUCCESS] Clean completed
    echo.
)

REM Build native C++ components
if "%BUILD_NATIVE%"=="true" (
    echo [INFO] Building native C++ components...
    
    cd /d "%PROJECT_ROOT%"
    
    REM Configure build
    echo [INFO] Configuring build...
    if "%VERBOSE%"=="true" (
        python configure.py --enable-optimize
    ) else (
        python configure.py --enable-optimize >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Configuration failed
        exit /b 1
    )
    
    REM Build
    echo [INFO] Building with AMBuild...
    cd build
    if "%VERBOSE%"=="true" (
        ambuild
    ) else (
        ambuild >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Native build failed
        exit /b 1
    )
    
    echo [SUCCESS] Native build completed
    echo.
)

REM Build C# components
if "%BUILD_CSHARP%"=="true" (
    echo [INFO] Building C# components...
    
    cd /d "%SCRIPT_DIR%"
    
    REM Restore packages
    echo [INFO] Restoring NuGet packages...
    if "%VERBOSE%"=="true" (
        dotnet restore AmxModX.CSharp.csproj
    ) else (
        dotnet restore AmxModX.CSharp.csproj >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Package restore failed
        exit /b 1
    )
    
    REM Build library
    echo [INFO] Building C# library...
    if "%VERBOSE%"=="true" (
        dotnet build AmxModX.CSharp.csproj -c %BUILD_TYPE%
    ) else (
        dotnet build AmxModX.CSharp.csproj -c %BUILD_TYPE% >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] C# library build failed
        exit /b 1
    )
    
    REM Build test application
    echo [INFO] Building test application...
    if "%VERBOSE%"=="true" (
        dotnet build AmxModXTestApp.csproj -c %BUILD_TYPE%
    ) else (
        dotnet build AmxModXTestApp.csproj -c %BUILD_TYPE% >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Test application build failed
        exit /b 1
    )
    
    REM Create package
    echo [INFO] Creating NuGet package...
    if "%VERBOSE%"=="true" (
        dotnet pack AmxModX.CSharp.csproj -c %BUILD_TYPE% --no-build
    ) else (
        dotnet pack AmxModX.CSharp.csproj -c %BUILD_TYPE% --no-build >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Package creation failed
        exit /b 1
    )
    
    echo [SUCCESS] C# build completed
    echo.
)

REM Run tests if requested
if "%RUN_TESTS%"=="true" (
    echo [INFO] Running tests...
    
    cd /d "%SCRIPT_DIR%"
    
    REM Run basic tests
    echo [INFO] Running basic functionality tests...
    if "%VERBOSE%"=="true" (
        dotnet run --project AmxModXTestApp.csproj -c %BUILD_TYPE%
    ) else (
        dotnet run --project AmxModXTestApp.csproj -c %BUILD_TYPE% >nul 2>&1
    )
    
    if errorlevel 1 (
        echo [ERROR] Tests failed
        exit /b 1
    )
    
    echo [SUCCESS] Tests completed
    echo.
)

REM Show build results
echo [INFO] Build completed successfully!
echo [INFO] Build artifacts:

if "%BUILD_NATIVE%"=="true" (
    if exist "%PROJECT_ROOT%\build\package\addons\amxmodx\modules\amxmodx_mm.dll" (
        echo [INFO]   Native library: %PROJECT_ROOT%\build\package\addons\amxmodx\modules\amxmodx_mm.dll
    )
)

if "%BUILD_CSHARP%"=="true" (
    if exist "%SCRIPT_DIR%\bin\%BUILD_TYPE%\net6.0\AmxModX.CSharp.dll" (
        echo [INFO]   C# library: %SCRIPT_DIR%\bin\%BUILD_TYPE%\net6.0\AmxModX.CSharp.dll
    )
    
    if exist "%SCRIPT_DIR%\bin\%BUILD_TYPE%\net6.0\AmxModXTestApp.dll" (
        echo [INFO]   Test app: %SCRIPT_DIR%\bin\%BUILD_TYPE%\net6.0\AmxModXTestApp.dll
    )
    
    for %%f in ("%SCRIPT_DIR%\bin\%BUILD_TYPE%\AmxModX.CSharp.*.nupkg") do (
        if exist "%%f" echo [INFO]   NuGet package: %%f
    )
)

echo.
echo [SUCCESS] Build script completed successfully!

if "%RUN_TESTS%"=="true" (
    echo [INFO] To run the test application manually:
    echo [INFO]   cd %SCRIPT_DIR%
    echo [INFO]   dotnet run --project AmxModXTestApp.csproj -c %BUILD_TYPE%
)

goto :end

:show_usage
echo Usage: %~nx0 [options]
echo.
echo Options:
echo   --clean              Clean build (remove previous build artifacts)
echo   --debug              Build in Debug mode (default: Release)
echo   --no-native          Skip native C++ build
echo   --no-csharp          Skip C# build
echo   --test               Run tests after build
echo   --verbose            Enable verbose output
echo   --help               Show this help message
echo.
echo Examples:
echo   %~nx0                   # Build everything in Release mode
echo   %~nx0 --clean --debug   # Clean build in Debug mode
echo   %~nx0 --test            # Build and run tests
echo   %~nx0 --no-native       # Build only C# components
exit /b 0

:end
endlocal
