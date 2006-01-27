/* AMX Mod X
*
* by the AMX Mod X Development Team
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"
#include "fakemeta.h"

int LoadMetamodPlugin(const char *path, void **handle, PLUG_LOADTIME now)
{
	if (gpMetaPExtFuncs)
	{
		if(PEXT_LOAD_PLUGIN_BY_NAME(PLID, path, now, handle) || !*handle)
		{
			LOG_MESSAGE(PLID, "Can't Attach metamod-module \"%s\".", path);
			return 0;
		}
		return 1;
	} else if (g_IsNewMM) {
		int err = 0;
		if ( (err = LOAD_PLUGIN(PLID, path, now, handle)) || !*handle)
		{
			LOG_MESSAGE(PLID, "Can't Attach Module \"%s\".", path);
			return 0;
		}
		return 1;
	}
	return 0;
}
int UnloadMetamodPlugin(void *handle)
{
	if (gpMetaPExtFuncs)
	{
		if(PEXT_UNLOAD_PLUGIN_BY_HANDLE(PLID, (void*)handle, PT_ANYTIME, PNL_PLUGIN)) {
			return 0;
		}
		return 1;
	} else if (g_IsNewMM) {
		if (UNLOAD_PLUGIN_BY_HANDLE(PLID, (void *)handle, PT_ANYTIME, PNL_PLUGIN))
		{
			return 0;
		}
		return 1;
	}

	return 0;
}
