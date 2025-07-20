#!/bin/bash
# vim: set ts=4 sw=4 tw=99 noet:
#
# AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
# Copyright (C) The AMX Mod X Development Team.
#
# This software is licensed under the GNU General Public License, version 3 or higher.
# Additional exceptions apply. For full license details, see LICENSE.txt or visit:
#     https://alliedmods.net/amxmodx-license

# build.sh - Build script for AMX Mod X C# Bridge
# 构建脚本，用于编译AMX Mod X C#桥接层

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BUILD_TYPE="Release"
CLEAN_BUILD=false
BUILD_NATIVE=true
BUILD_CSHARP=true
RUN_TESTS=false
VERBOSE=false

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║              AMX Mod X C# Bridge Build Script               ║${NC}"
    echo -e "${BLUE}║                                                              ║${NC}"
    echo -e "${BLUE}║  Builds both native C++ bridge and C# interop library      ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [options]"
    echo
    echo "Options:"
    echo "  --clean              Clean build (remove previous build artifacts)"
    echo "  --debug              Build in Debug mode (default: Release)"
    echo "  --no-native          Skip native C++ build"
    echo "  --no-csharp          Skip C# build"
    echo "  --test               Run tests after build"
    echo "  --verbose            Enable verbose output"
    echo "  --help               Show this help message"
    echo
    echo "Examples:"
    echo "  $0                   # Build everything in Release mode"
    echo "  $0 --clean --debug   # Clean build in Debug mode"
    echo "  $0 --test            # Build and run tests"
    echo "  $0 --no-native       # Build only C# components"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        --no-native)
            BUILD_NATIVE=false
            shift
            ;;
        --no-csharp)
            BUILD_CSHARP=false
            shift
            ;;
        --test)
            RUN_TESTS=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        --help)
            show_usage
            exit 0
            ;;
        *)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AMXMODX_ROOT="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$AMXMODX_ROOT")"

print_header

print_info "Build configuration:"
print_info "  Build Type: $BUILD_TYPE"
print_info "  Clean Build: $CLEAN_BUILD"
print_info "  Build Native: $BUILD_NATIVE"
print_info "  Build C#: $BUILD_CSHARP"
print_info "  Run Tests: $RUN_TESTS"
print_info "  Verbose: $VERBOSE"
print_info "  Project Root: $PROJECT_ROOT"
echo

# Check prerequisites
print_info "Checking prerequisites..."

# Check for Python
if ! command -v python3 &> /dev/null && ! command -v python &> /dev/null; then
    print_error "Python is required but not installed"
    exit 1
fi

PYTHON_CMD="python3"
if ! command -v python3 &> /dev/null; then
    PYTHON_CMD="python"
fi

# Check for .NET SDK
if $BUILD_CSHARP; then
    if ! command -v dotnet &> /dev/null; then
        print_error ".NET SDK is required but not installed"
        exit 1
    fi
    
    DOTNET_VERSION=$(dotnet --version)
    print_info "Found .NET SDK version: $DOTNET_VERSION"
fi

# Check for AMBuild
if $BUILD_NATIVE; then
    if ! command -v ambuild &> /dev/null; then
        print_error "AMBuild is required but not installed"
        print_info "Install with: pip install ambuild"
        exit 1
    fi
    
    AMBUILD_VERSION=$(ambuild --version 2>&1 | head -n1)
    print_info "Found AMBuild: $AMBUILD_VERSION"
fi

print_success "Prerequisites check passed"
echo

# Clean build if requested
if $CLEAN_BUILD; then
    print_info "Cleaning previous build artifacts..."
    
    if [ -d "$PROJECT_ROOT/build" ]; then
        rm -rf "$PROJECT_ROOT/build"
        print_info "Removed build directory"
    fi
    
    if [ -d "$SCRIPT_DIR/bin" ]; then
        rm -rf "$SCRIPT_DIR/bin"
        print_info "Removed C# bin directory"
    fi
    
    if [ -d "$SCRIPT_DIR/obj" ]; then
        rm -rf "$SCRIPT_DIR/obj"
        print_info "Removed C# obj directory"
    fi
    
    print_success "Clean completed"
    echo
fi

# Build native C++ components
if $BUILD_NATIVE; then
    print_info "Building native C++ components..."
    
    cd "$PROJECT_ROOT"
    
    # Configure build
    print_info "Configuring build..."
    if $VERBOSE; then
        $PYTHON_CMD configure.py --enable-optimize
    else
        $PYTHON_CMD configure.py --enable-optimize > /dev/null 2>&1
    fi
    
    # Build
    print_info "Building with AMBuild..."
    cd build
    if $VERBOSE; then
        ambuild
    else
        ambuild > /dev/null 2>&1
    fi
    
    print_success "Native build completed"
    echo
fi

# Build C# components
if $BUILD_CSHARP; then
    print_info "Building C# components..."
    
    cd "$SCRIPT_DIR"
    
    # Restore packages
    print_info "Restoring NuGet packages..."
    if $VERBOSE; then
        dotnet restore AmxModX.CSharp.csproj
    else
        dotnet restore AmxModX.CSharp.csproj > /dev/null 2>&1
    fi
    
    # Build library
    print_info "Building C# library..."
    if $VERBOSE; then
        dotnet build AmxModX.CSharp.csproj -c $BUILD_TYPE
    else
        dotnet build AmxModX.CSharp.csproj -c $BUILD_TYPE > /dev/null 2>&1
    fi
    
    # Build test application
    print_info "Building test application..."
    if $VERBOSE; then
        dotnet build AmxModXTestApp.csproj -c $BUILD_TYPE
    else
        dotnet build AmxModXTestApp.csproj -c $BUILD_TYPE > /dev/null 2>&1
    fi
    
    # Create package
    print_info "Creating NuGet package..."
    if $VERBOSE; then
        dotnet pack AmxModX.CSharp.csproj -c $BUILD_TYPE --no-build
    else
        dotnet pack AmxModX.CSharp.csproj -c $BUILD_TYPE --no-build > /dev/null 2>&1
    fi
    
    print_success "C# build completed"
    echo
fi

# Run tests if requested
if $RUN_TESTS; then
    print_info "Running tests..."
    
    cd "$SCRIPT_DIR"
    
    # Run basic tests
    print_info "Running basic functionality tests..."
    if $VERBOSE; then
        dotnet run --project AmxModXTestApp.csproj -c $BUILD_TYPE
    else
        dotnet run --project AmxModXTestApp.csproj -c $BUILD_TYPE > /dev/null 2>&1
    fi
    
    print_success "Tests completed"
    echo
fi

# Show build results
print_info "Build completed successfully!"
print_info "Build artifacts:"

if $BUILD_NATIVE; then
    if [ -f "$PROJECT_ROOT/build/package/addons/amxmodx/modules/amxmodx_mm.so" ]; then
        print_info "  Native library: $PROJECT_ROOT/build/package/addons/amxmodx/modules/amxmodx_mm.so"
    elif [ -f "$PROJECT_ROOT/build/package/addons/amxmodx/modules/amxmodx_mm.dll" ]; then
        print_info "  Native library: $PROJECT_ROOT/build/package/addons/amxmodx/modules/amxmodx_mm.dll"
    fi
fi

if $BUILD_CSHARP; then
    if [ -f "$SCRIPT_DIR/bin/$BUILD_TYPE/net6.0/AmxModX.CSharp.dll" ]; then
        print_info "  C# library: $SCRIPT_DIR/bin/$BUILD_TYPE/net6.0/AmxModX.CSharp.dll"
    fi
    
    if [ -f "$SCRIPT_DIR/bin/$BUILD_TYPE/net6.0/AmxModXTestApp.dll" ]; then
        print_info "  Test app: $SCRIPT_DIR/bin/$BUILD_TYPE/net6.0/AmxModXTestApp.dll"
    fi
    
    PACKAGE_FILE=$(find "$SCRIPT_DIR/bin/$BUILD_TYPE" -name "AmxModX.CSharp.*.nupkg" 2>/dev/null | head -n1)
    if [ -n "$PACKAGE_FILE" ]; then
        print_info "  NuGet package: $PACKAGE_FILE"
    fi
fi

echo
print_success "Build script completed successfully!"

if $RUN_TESTS; then
    print_info "To run the test application manually:"
    print_info "  cd $SCRIPT_DIR"
    print_info "  dotnet run --project AmxModXTestApp.csproj -c $BUILD_TYPE"
fi
