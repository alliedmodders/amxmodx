# Microsoft Developer Studio Project File - Name="amxmodx_mm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=amxmodx_mm - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "amxmodx_mm.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "amxmodx_mm.mak" CFG="amxmodx_mm - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "amxmodx_mm - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "amxmodx_mm - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "amxmodx_mm - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "release"
# PROP Intermediate_Dir "release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "amxmodx_mm_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\..\metamod\metamod" /I "..\..\hlsdk\sourcecode\common" /I "..\..\hlsdk\sourcecode\engine" /I "..\..\hlsdk\sourcecode\dlls" /I "..\..\hlsdk\sourcecode\pm_shared" /I "..\extra\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "amxmodx_mm_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /def:".\amxmodx_mm.def" /out:"release/amxxmm.dll" /libpath:"..\extra\lib_win32"
# Begin Custom Build
TargetPath=.\release\amxxmm.dll
TargetName=amxxmm
InputPath=.\release\amxxmm.dll
SOURCE="$(InputPath)"

"$(TargetName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(TargetPath) D:\SIERRA\Half-Life\cstrike\addons\amx\dlls

# End Custom Build

!ELSEIF  "$(CFG)" == "amxmodx_mm - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "debug"
# PROP Intermediate_Dir "debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "amxmodx_mm_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /Zp4 /MTd /W3 /Gm /GX /ZI /Od /I "..\..\metamod\metamod" /I "..\...\hlsdk\sourcecode\common" /I "..\...\hlsdk\sourcecode\engine" /I "..\...\hlsdk\sourcecode\dlls" /I "..\...\hlsdk\sourcecode\pm_shared" /I "..\extra\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "amxmodx_mm_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /def:".\amxmodx_mm.def" /out:"debug/amxxmm.dll" /pdbtype:sept /libpath:"..\extra\lib_win32"
# SUBTRACT LINK32 /incremental:no /nodefaultlib
# Begin Custom Build
TargetPath=.\debug\amxxmm.dll
TargetName=amxxmm
InputPath=.\debug\amxxmm.dll
SOURCE="$(InputPath)"

"$(TargetName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(TargetPath) D:\SIERRA\Half-Life\cstrike\addons\amx\dlls

# End Custom Build

!ENDIF 

# Begin Target

# Name "amxmodx_mm - Win32 Release"
# Name "amxmodx_mm - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\amx.c
# End Source File
# Begin Source File

SOURCE=..\amxcore.c
# End Source File
# Begin Source File

SOURCE=..\amxmodx.cpp
# End Source File
# Begin Source File

SOURCE=..\amxtime.c
# End Source File
# Begin Source File

SOURCE=..\CCmd.cpp
# End Source File
# Begin Source File

SOURCE=..\CEvent.cpp
# End Source File
# Begin Source File

SOURCE=..\CFile.cpp
# End Source File
# Begin Source File

SOURCE=..\CForward.cpp
# End Source File
# Begin Source File

SOURCE=..\CLogEvent.cpp
# End Source File
# Begin Source File

SOURCE=..\CMenu.cpp
# End Source File
# Begin Source File

SOURCE=..\CMisc.cpp
# End Source File
# Begin Source File

SOURCE=..\CModule.cpp
# End Source File
# Begin Source File

SOURCE=..\CPlugin.cpp
# End Source File
# Begin Source File

SOURCE=..\CString.cpp
# End Source File
# Begin Source File

SOURCE=..\CTask.cpp
# End Source File
# Begin Source File

SOURCE=..\CVault.cpp
# End Source File
# Begin Source File

SOURCE=..\emsg.cpp
# End Source File
# Begin Source File

SOURCE=..\file.cpp
# End Source File
# Begin Source File

SOURCE=..\float.cpp
# End Source File
# Begin Source File

SOURCE=..\meta_api.cpp
# End Source File
# Begin Source File

SOURCE=..\modules.cpp
# End Source File
# Begin Source File

SOURCE=..\power.c
# End Source File
# Begin Source File

SOURCE=..\srvcmd.cpp
# End Source File
# Begin Source File

SOURCE=..\string.cpp
# End Source File
# Begin Source File

SOURCE=..\strptime.cpp
# End Source File
# Begin Source File

SOURCE=..\util.cpp
# End Source File
# Begin Source File

SOURCE=..\vault.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\amxmodx.h
# End Source File
# Begin Source File

SOURCE=..\CCmd.h
# End Source File
# Begin Source File

SOURCE=..\CEvent.h
# End Source File
# Begin Source File

SOURCE=..\CFile.h
# End Source File
# Begin Source File

SOURCE=..\CForward.h
# End Source File
# Begin Source File

SOURCE=..\CList.h
# End Source File
# Begin Source File

SOURCE=..\CLogEvent.h
# End Source File
# Begin Source File

SOURCE=..\CMenu.h
# End Source File
# Begin Source File

SOURCE=..\CMisc.h
# End Source File
# Begin Source File

SOURCE=..\CModule.h
# End Source File
# Begin Source File

SOURCE=..\CPlugin.h
# End Source File
# Begin Source File

SOURCE=..\CString.h
# End Source File
# Begin Source File

SOURCE=..\CTask.h
# End Source File
# Begin Source File

SOURCE=..\CVault.h
# End Source File
# Begin Source File

SOURCE=..\modules.h
# End Source File
# End Group
# End Target
# End Project
