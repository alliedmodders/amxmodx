/*  DLL support functions for dynamically loadable extension libraries.
 *
 *  Copyright (c) ITB CompuPhase, 2004-2005
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 */

#if defined _UNICODE || defined __UNICODE__ || defined UNICODE
# if !defined UNICODE   /* for Windows */
#   define UNICODE
# endif
# if !defined _UNICODE  /* for C library */
#   define _UNICODE
# endif
#endif

#include <assert.h>
#include <windows.h>

#if !defined UNUSED_PARAM
  #define UNUSED_PARAM(p) ((void)(p))
#endif

HINSTANCE hinstDLL;

/* Especially Watcom C/C++ does not like DLLs that do not have a LibMain()
 * set. Apparently, the start address is not set well, and some required
 * initializations are not done.
 */
#if defined __WIN32__ || defined _WIN32 || defined WIN32

  BOOL WINAPI DllMain(HINSTANCE hinst, DWORD dwReason, LPVOID lpRes)
  {
    UNUSED_PARAM(lpRes);
    switch (dwReason) {
    case DLL_PROCESS_ATTACH:
      hinstDLL=hinst;
      break;
    case DLL_PROCESS_DETACH:
      break;
    } /* switch */
    return TRUE;
  }

#else

  int FAR PASCAL LibMain(HINSTANCE hinst, WORD wDataSeg, WORD wHeapSize, LPSTR lpszCmdLine)
  {
    UNUSED_PARAM(wDataSeg);
    UNUSED_PARAM(wHeapSize);
    UNUSED_PARAM(lpszCmdLine);
    hinstDLL=hinst;
    return 1;
  }

  int FAR PASCAL _export WEP(int param)
  {
    UNUSED_PARAM(param);
    return 1;
  }

#endif /* __WIN32__ */

