//========= Copyright © 1996-2002, Valve LLC, All rights reserved. ============
//
// Purpose: 
//
// $NoKeywords: $
//=============================================================================

#ifndef VOICE_VGUI_TWEAKDLG_H
#define VOICE_VGUI_TWEAKDLG_H
#ifdef _WIN32
#ifndef __MINGW32__
#pragma once
#endif /* not __MINGW32__ */
#endif


class CMenuPanel;


// Returns true if the tweak dialog is currently up.
bool IsTweakDlgOpen();

// Returns a global instance of the tweak dialog.
CMenuPanel* GetVoiceTweakDlg();


#endif // VOICE_VGUI_TWEAKDLG_H
