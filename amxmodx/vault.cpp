/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#include <extdll.h>
#include <meta_api.h>
#include "CVault.h"
#include "amxmod.h"

Vault g_vault;

static cell AMX_NATIVE_CALL set_vaultdata(AMX *amx,cell *params)
{
  int iLen;

  g_vault.put( get_amxstring(amx,params[1],0,iLen) , get_amxstring(amx,params[2],1,iLen) );
  g_vault.saveVault();

  return 1;
}

static cell AMX_NATIVE_CALL get_vaultdata(AMX *amx,cell *params)
{
  int iLen;

  const char* key = get_amxstring(amx,params[1],0,iLen);

  if ( params[3] )
    return set_amxstring( amx , params[2] , g_vault.get( key ) , params[3] );

  return g_vault.get_number( key );
}

static cell AMX_NATIVE_CALL remove_vaultdata(AMX *amx,cell *params)
{
  int iLen;

  g_vault.remove( get_amxstring(amx,params[1],0,iLen) );
  g_vault.saveVault();

  return 1;
}

static cell AMX_NATIVE_CALL vaultdata_exists(AMX *amx,cell *params)
{
  int iLen;

  return g_vault.exists( get_amxstring(amx,params[1],0,iLen) ) ? 1 : 0;
}

AMX_NATIVE_INFO vault_Natives[] = {
  { "set_vaultdata",  set_vaultdata },
  { "get_vaultdata",  get_vaultdata },
  { "remove_vaultdata", remove_vaultdata  },
  { "delete_vaultdata", remove_vaultdata  },
  { "vaultdata_exists", vaultdata_exists  },
  { 0, 0 }
};