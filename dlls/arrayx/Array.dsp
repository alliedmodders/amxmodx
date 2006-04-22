# Microsoft Developer Studio Project File - Name="Array" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Array - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Array.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Array.mak" CFG="Array - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Array - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Array - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Array - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ARRAY_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ARRAY_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "Array - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ARRAY_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ARRAY_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Array - Win32 Release"
# Name "Array - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Array.cpp
# End Source File
# Begin Source File

SOURCE=.\BinTrieNatives.cpp
# End Source File
# Begin Source File

SOURCE=.\Capsule.cpp
# End Source File
# Begin Source File

SOURCE=.\CArray.cpp
# End Source File
# Begin Source File

SOURCE=.\CBinTrie.cpp
# End Source File
# Begin Source File

SOURCE=.\CKeytable.cpp
# End Source File
# Begin Source File

SOURCE=.\JudyExtra.cpp
# End Source File
# Begin Source File

SOURCE=.\ListNatives.cpp
# End Source File
# Begin Source File

SOURCE=.\MapNatives.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Capsule.h
# End Source File
# Begin Source File

SOURCE=.\CArray.h
# End Source File
# Begin Source File

SOURCE=.\CBaseList.h
# End Source File
# Begin Source File

SOURCE=.\CBaseMap.h
# End Source File
# Begin Source File

SOURCE=.\CBinTrie.h
# End Source File
# Begin Source File

SOURCE=.\CHashtable.h
# End Source File
# Begin Source File

SOURCE=.\CKeytable.h
# End Source File
# Begin Source File

SOURCE=.\ComboArray.h
# End Source File
# Begin Source File

SOURCE=.\ComboTable.h
# End Source File
# Begin Source File

SOURCE=.\JudyIncludes.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\amxxmodule.cpp
# End Source File
# Begin Source File

SOURCE=.\amxxmodule.h
# End Source File
# Begin Source File

SOURCE=.\BinTrieNativeFunctions.h
# End Source File
# Begin Source File

SOURCE=.\GenericNatives.h
# End Source File
# Begin Source File

SOURCE=.\Judy.h
# End Source File
# Begin Source File

SOURCE=.\JudyEx.h
# End Source File
# Begin Source File

SOURCE=.\JudyExtra.h
# End Source File
# Begin Source File

SOURCE=.\JudyVar.h
# End Source File
# Begin Source File

SOURCE=.\JudyVec.h
# End Source File
# Begin Source File

SOURCE=.\ListNativeFunctions.h
# End Source File
# Begin Source File

SOURCE=.\MapNativeFunctions.h
# End Source File
# Begin Source File

SOURCE=.\moduleconfig.h
# End Source File
# Begin Source File

SOURCE=.\NativeIncludes.h
# End Source File
# Begin Source File

SOURCE=.\osdefs.h
# End Source File
# Begin Source File

SOURCE=.\Judy.lib
# End Source File
# End Group
# End Target
# End Project
