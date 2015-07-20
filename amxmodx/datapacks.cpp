/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 */

#include "CDataPack.h"

NativeHandle<CDataPack> DataPackHandles;

static cell AMX_NATIVE_CALL CreateDataPack(AMX* amx, cell* params)
{
	return static_cast<cell>(DataPackHandles.create());
}

static cell AMX_NATIVE_CALL WritePackCell(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	d->PackCell(params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL WritePackFloat(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	d->PackFloat(amx_ctof(params[2]));

	return 1;
}

static cell AMX_NATIVE_CALL WritePackString(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	const char *str = get_amxstring(amx, params[2], 0, len);

	d->PackString(str);

	return len;
}

static cell AMX_NATIVE_CALL ReadPackCell(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	if (!d->CanReadCell())
	{
		LogError(amx, AMX_ERR_NATIVE, "Datapack operation is invalid.");
		return 0;
	}

	return d->ReadCell();
}

static cell AMX_NATIVE_CALL ReadPackFloat(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	if (!d->CanReadFloat())
	{
		LogError(amx, AMX_ERR_NATIVE, "Datapack operation is invalid.");
		return 0;
	}

	float value = d->ReadFloat();

	return amx_ftoc(value);
}

static cell AMX_NATIVE_CALL ReadPackString(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	if (!d->CanReadString(NULL))
	{
		LogError(amx, AMX_ERR_NATIVE, "Datapack operation is invalid.");
		return 0;
	}

	size_t len;
	const char *str = d->ReadString(&len);

	return set_amxstring_utf8(amx, params[2], str, len, params[3]);
}

static cell AMX_NATIVE_CALL ResetPack(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	d->Reset();

	if (params[2])
	{
		d->ResetSize();
	}

	return 1;
}

static cell AMX_NATIVE_CALL GetPackPosition(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	return static_cast<cell>(d->GetPosition());
}

static cell AMX_NATIVE_CALL SetPackPosition(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	if (!d->SetPosition(params[2]))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid DataPack position, %d is out of bounds", params[2]);
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL IsPackEnded(AMX* amx, cell* params)
{
	CDataPack *d = DataPackHandles.lookup(params[1]);

	if (!d)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid datapack handle provided (%d)", params[1]);
		return 0;
	}

	return d->IsReadable(1) ? false : true;
}

static cell AMX_NATIVE_CALL DestroyDataPack(AMX* amx, cell* params)
{
	cell *ptr = get_amxaddr(amx, params[1]);

	CDataPack *d = DataPackHandles.lookup(*ptr);

	if (!d)
	{
		return 0;
	}

	if (DataPackHandles.destroy(*ptr))
	{
		*ptr = 0;
		return 1;
	}

	return 0;
}


AMX_NATIVE_INFO g_DatapackNatives[] = 
{
	{ "CreateDataPack" , CreateDataPack },
	{ "WritePackCell"  , WritePackCell },
	{ "WritePackFloat" , WritePackFloat },
	{ "WritePackString", WritePackString },
	{ "ReadPackCell"   , ReadPackCell },
	{ "ReadPackFloat"  , ReadPackFloat },
	{ "ReadPackString" , ReadPackString },
	{ "ResetPack"      , ResetPack },
	{ "GetPackPosition", GetPackPosition },
	{ "SetPackPosition", SetPackPosition },
	{ "IsPackEnded"    , IsPackEnded },
	{ "DestroyDataPack", DestroyDataPack },
	{ nullptr          , nullptr}
};
