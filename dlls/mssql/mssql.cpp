/*  by David "BAILOPAN" Anderson
*   http://www.bailopan.com
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
*/
#include "mssql.h"

using namespace std;

std::vector<msdb*> DBList;

std::string error;
int lastError = 0;

bool msdb::Kill()
{
	host.clear();
	pass.clear();
	dbname.clear();
	user.clear();
	free = true;
	try {
		cn->Close();
		cn = NULL;
		if (res)
			res->Close();
		res = NULL;
	} catch(_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		return false;
	}

	return true;
}

bool msdb::Connect()
{
	HRESULT hr;
	cstr.assign("Provider=sqloledb;Network Library=DBMSSOCN;");
	cstr.append("Data Source=");
	cstr.append(host);
	cstr.append(";Initial Catalog=");
	cstr.append(dbname);
	cstr.append(";User ID=");
	cstr.append(user);
	cstr.append(";Password=");
	cstr.append(pass);
	try {
		hr = cn.CreateInstance(__uuidof(ADODB::Connection));
		cn->Open(cstr.c_str(), user.c_str(), pass.c_str(), NULL);
	} catch (_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		return false;
	}

	return 1;
}

int sql_exists(char *user, char *pass, char *db, char *host)
{
	std::vector<msdb*>::iterator i;
	int id = 0;
	for (i=DBList.begin(); i!=DBList.end(); i++) {
		id++;
		if (((*i)->user.compare(user) == 0) &&
			((*i)->pass.compare(pass) == 0) &&
			((*i)->dbname.compare(db) == 0) &&
			((*i)->host.compare(host) == 0) &&
			(!(*i)->free)) {

			return id;
		}
	}

	return -1;
}

static cell AMX_NATIVE_CALL mssql_connect(AMX *amx, cell *params)
{
	int len;
	unsigned int i;
	msdb *c = NULL;
	char *host = MF_GetAmxString(amx, params[1], 0, &len);
	char *user = MF_GetAmxString(amx, params[2], 1, &len);
	char *pass = MF_GetAmxString(amx, params[3], 2, &len);
	char *dbn = MF_GetAmxString(amx, params[4], 3, &len);

	int id = sql_exists(user, pass, dbn, host);
	if (id >= 0)
		return id;
	id = -1;
	for (i=0; i<DBList.size(); i++) {
		if (DBList.at(i)->free) {
			id = i;
			break;
		}
	}

	if (id < 0) {
		c = new msdb;
		DBList.push_back(c);
		id = DBList.size() - 1;
	} else {
		c = DBList.at(id);
	}
	c->host.assign(host);
	c->user.assign(user);
	c->pass.assign(pass);
	c->dbname.assign(dbn);

	try {
		if (c->Connect()) {
			c->free = false;
			return id+1;
		}
	} catch (_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		MF_SetAmxString(amx, params[5], error.c_str(), params[6]);
		return -1;
	}

	c->free = true;
	return -1;
}

static cell AMX_NATIVE_CALL mssql_close(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;
	
	if (id >= DBList.size() || DBList.at(id)->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return DBList.at(id)->Kill();
}

static cell AMX_NATIVE_CALL mssql_query(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;
	
	if (id >= DBList.size() || DBList.at(id)->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	msdb *c = NULL;
	c = DBList.at(id);

	try {
		if (c->res) {
			c->res->Close();
			c->res = NULL;
		}
	} catch (_com_error &e) {
		/* Nothing */
	}

	int len;
	const char *query = MF_FormatAmxString(amx, params, 2, &len);
	c->resStart = false;

	try {
		HRESULT hr = c->res.CreateInstance(__uuidof(ADODB::Recordset));
		if (FAILED(hr)) {
			return 0;
		}
		c->res->CursorType = ADODB::adOpenStatic;
		c->res->Open((_bstr_t)query, (_bstr_t)c->cstr.c_str(), ADODB::adOpenForwardOnly, ADODB::adLockReadOnly, ADODB::adCmdText);
		c->resStart = true;
	} catch (_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		return -1;
	}

	try {
		c->res->MoveFirst();
	} catch (_com_error &e) {
		return 1;
	}

	return 1;
}

static cell AMX_NATIVE_CALL mssql_nextrow(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;
	
	if (id >= DBList.size() || DBList.at(id)->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	msdb *c = NULL;
	c = DBList.at(id);

	if (c->res == NULL)
		return 0;

	try {
		if (c->res->ADOEOF)
			return 0;
		c->res->MoveNext();
		if (c->res->ADOEOF)
			return 0;
		return 1;
	} catch (_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		return -1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL mssql_getfield(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;
	
	if (id >= DBList.size() || DBList.at(id)->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	msdb *c = NULL;
	c = DBList.at(id);

	try {
		if (c->res->ADOEOF)
			return 0;
		ADODB::FieldPtr pFld = NULL;
		_variant_t index = (short)params[2];
		pFld = c->res->Fields->GetItem(&index);
		_variant_t FldVal = pFld->GetValue();
		switch (FldVal.vt)
		{
			case (VT_BOOL):
				if (FldVal.boolVal) {
					MF_SetAmxString(amx, params[3], "1", params[4]);
				} else {
					MF_SetAmxString(amx, params[3], "0", params[4]);
				}
				break;
			case (VT_BSTR):
				MF_SetAmxString(amx, params[3], (LPCSTR)(_bstr_t)FldVal.bstrVal, params[4]);
				break;
			case (VT_I4):
				MF_SetAmxString(amx, params[3], (LPCSTR)FldVal.iVal, params[4]);
				break;
			case (VT_EMPTY):
				MF_SetAmxString(amx, params[3], (LPCSTR)FldVal.iVal, params[4]);
				break;
			default:
				return 0;
				break;
		}
	} catch (_com_error &e) {
		_bstr_t bstrSource(e.Description());
		error.assign((LPCSTR)bstrSource);
		lastError = e.Error();
		return -1;
	}

	return 1;
}

static cell AMX_NATIVE_CALL mssql_error(AMX *amx, cell *params)
{
	MF_SetAmxString(amx, params[2], error.c_str(), params[3]);
	return lastError;
}

void OnAmxxAttach()
{
	MF_AddNatives(mssql_Natives);
	if(FAILED(::CoInitialize(NULL)))
		return;
}

void onAmxxDetach()
{
	std::vector<msdb*>::iterator i;
	for (i=DBList.begin(); i!=DBList.end(); i++) {
		(*i)->Kill();
	}
	::CoUninitialize();
	return;
}

AMX_NATIVE_INFO mssql_Natives[] = {
	{"mssql_connect",		mssql_connect},
	{"dbi_connect",			mssql_connect},
	{"mssql_close",			mssql_close},
	{"dbi_close",			mssql_close},
	{"mssql_query",			mssql_query},
	{"dbi_query",			mssql_query},
	{"mssql_getfield",		mssql_getfield},
	{"dbi_getfield",		mssql_getfield},
	{"mssql_nextrow",		mssql_nextrow},
	{"dbi_nextrow",			mssql_nextrow},
	{"mssql_error",			mssql_error},
	{"dbi_error",			mssql_error},

	{NULL,					NULL},
	  ///////////////////	
};