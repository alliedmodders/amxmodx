# CMakeLists.txt for AMX Mod X Bridge
# Cross-platform build configuration for C# interop bridge

cmake_minimum_required(VERSION 3.10)
project(AmxModXBridge VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Platform-specific settings
if(WIN32)
    # Windows specific settings
    add_definitions(-DWIN32_LEAN_AND_MEAN)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
elseif(UNIX)
    # Unix/Linux specific settings
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
    if(APPLE)
        # macOS specific settings
        set(CMAKE_MACOSX_RPATH ON)
    endif()
endif()

# Source files
set(BRIDGE_SOURCES
    AmxModXBridge.cpp
    AmxModXBridge.h
)

# Create shared library
add_library(AmxModXBridge SHARED ${BRIDGE_SOURCES})

# Set output name
set_target_properties(AmxModXBridge PROPERTIES
    OUTPUT_NAME "AmxModXBridge"
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
)

# Platform-specific library settings
if(WIN32)
    # Windows: Create .dll
    set_target_properties(AmxModXBridge PROPERTIES
        SUFFIX ".dll"
        PREFIX ""
    )
elseif(APPLE)
    # macOS: Create .dylib
    set_target_properties(AmxModXBridge PROPERTIES
        SUFFIX ".dylib"
        PREFIX "lib"
    )
else()
    # Linux: Create .so
    set_target_properties(AmxModXBridge PROPERTIES
        SUFFIX ".so"
        PREFIX "lib"
    )
endif()

# Compiler-specific options
if(MSVC)
    # Visual Studio specific options
    target_compile_options(AmxModXBridge PRIVATE /W4)
    target_compile_definitions(AmxModXBridge PRIVATE _CRT_SECURE_NO_WARNINGS)
else()
    # GCC/Clang specific options
    target_compile_options(AmxModXBridge PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Install targets
install(TARGETS AmxModXBridge
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(FILES AmxModXBridge.h
    DESTINATION include
)

# Package configuration
set(CPACK_PACKAGE_NAME "AmxModXBridge")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "AMX Mod X C# Interop Bridge")
set(CPACK_PACKAGE_VENDOR "AMX Mod X Community")

include(CPack)
