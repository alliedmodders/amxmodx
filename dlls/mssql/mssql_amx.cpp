#include "mssql_amx.h"

using namespace std;

SQL::SQL()
{
	isFree = true;
}

SQL::~SQL()
{
	if (isFree)
		Disconnect();
}

SQLResult::SQLResult()
{
	isFree = true;
	RowCount = 0;
	FieldCount = 0;
	sql = NULL;
}

SQLResult::~SQLResult()
{
	if (!isFree)
		FreeResult();
}

int SQL::Error(_com_error &e)
{
	if (isFree)
		return 0;
	
	_bstr_t bstrSource(e.Description());
	ErrorStr.assign((LPCSTR)bstrSource);
	ErrorCode = e.Error();
	return ErrorCode;
}

int SQL::Connect(const char *host, const char *user, const char *pass, const char *base)
{
	Username.assign(user);
	Password.assign(pass);
	Database.assign(base);
	Host.assign(host);

	isFree = false;
	int err = 0;

	HRESULT hr;
	cstr.assign("Provider=sqloledb;Network Library=DBMSSOCN;");
	cstr.append("Data Source=");
	cstr.append(Host);
	cstr.append(";Initial Catalog=");
	cstr.append(Database);
	cstr.append(";User ID=");
	cstr.append(Username);
	cstr.append(";Password=");
	cstr.append(Password);
	try {
		hr = cn.CreateInstance(__uuidof(ADODB::Connection));
		cn->Open(cstr.c_str(), Username.c_str(), Password.c_str(), NULL);
	} catch (_com_error &e) {
		Error(e);
		return false;
	}

	return 1;
}

void SQL::Disconnect()
{
	Host.clear();
	Username.clear();
	Password.clear();
	Database.clear();
	isFree = true;

	try {
		cn->Close();
		cn = NULL;
	} catch (_com_error &e) {
		Error(e);
		return;
	}
}

int SQL::Query(const char *query)
{
	if (isFree)
	{
		ErrorCode = -1;
		return -1;
	}

	SQLResult *p = new SQLResult;
	int ret = p->Query(this, query);

	if (ret < 1)
	{
		delete p;
		return ret;
	}

	unsigned int i = 0;
	int id = -1;
	for (i=0; i < Results.size(); i++)
	{
		if (Results[i]->isFree)
		{
			id = i;
			break;
		}
	}

	if (id < 0) {
		Results.push_back(p);
		return Results.size();
	} else {
		SQLResult *r = Results[id];
		Results[id] = p;
		delete r;
		return (id+1);
	}
}

int SQLResult::Query(SQL *cn, const char *query)
{
	isStart = false;

	sql = cn;

	try {
		HRESULT hr = res.CreateInstance(__uuidof(ADODB::Recordset));
		if (FAILED(hr)) {
			return -1;
		}
		res->CursorType = ADODB::adOpenStatic;
		res->Open((_bstr_t)query, (_bstr_t)cn->cstr.c_str(), ADODB::adOpenForwardOnly, ADODB::adLockReadOnly, ADODB::adCmdText);
		isStart = true;
	} catch (_com_error &e) {
		cn->Error(e);
		FreeResult();
		return -1;
	}

	try {
		res->MoveFirst();
	} catch (_com_error &e) {
		cn->Error(e);
		FreeResult();
		return 0;
	}

	if (NumRows()>0)
		return 1;

	return 0;
}

bool SQLResult::Nextrow()
{
	if (isFree)
		return false;

	try {
		if (res->ADOEOF)
			return 0;
		res->MoveNext();
		if (res->ADOEOF)
			return 0;
		return 1;
	} catch (_com_error &e) {
		sql->Error(e);
		return 0;
	}

	return 0;
}

void SQLResult::FreeResult()
{
	if (isFree)
		return;

	isFree = true;
	try {
		res->Close();
	} catch (_com_error &e) {
		sql->Error(e);
		return;
	}
}

const char *SQLResult::GetField(unsigned int field)
{
	if (isFree)
	{
		return "";
	}
	
	char buf[256];

	try {
		if (res->ADOEOF)
		{
			return "";
		}
		ADODB::FieldPtr pFld = NULL;
		_variant_t index = (short)field;
		pFld = res->Fields->GetItem(&index);
		_variant_t FldVal = pFld->GetValue();
		switch (FldVal.vt)
		{
			case (VT_BOOL):
				if (FldVal.boolVal) {
					return "1";
				} else {
					return "0";
				}
				break;
			case (VT_BSTR):
				LastResult.assign((LPCSTR)(_bstr_t)FldVal.bstrVal);
				break;
			case (VT_I4):
				sprintf(buf, "%d", FldVal.iVal);
				buf[255] = 0;
				LastResult.assign(buf);
				break;
			case (VT_EMPTY):
				sprintf(buf, "%d", FldVal.iVal);
				buf[255] = 0;
				LastResult.assign(buf);
				break;
			default:
				LastResult.assign("");
				break;
		}
	} catch (_com_error &e) {
		sql->Error(e);
		return 0;
	}

	return LastResult.c_str();
}

const char *SQLResult::GetField(const char *field)
{
	if (isFree)
	{
		return "";
	}
	
	char buf[256];

	try {
		if (res->ADOEOF)
		{
			return "";
		}
		ADODB::FieldPtr pFld = NULL;
		pFld = res->Fields->GetItem(field);
		_variant_t FldVal = pFld->GetValue();
		switch (FldVal.vt)
		{
			case (VT_BOOL):
				if (FldVal.boolVal) {
					return "1";
				} else {
					return "0";
				}
				break;
			case (VT_BSTR):
				LastResult.assign((_bstr_t)FldVal.bstrVal);
				break;
			case (VT_I4):
				sprintf(buf, "%d", FldVal.iVal);
				buf[255] = 0;
				LastResult.assign(buf);
				break;
			case (VT_EMPTY):
				sprintf(buf, "%d", FldVal.iVal);
				buf[255] = 0;
				LastResult.assign(buf);
				break;
			default:
				LastResult.assign("");
				break;
		}
	} catch (_com_error &e) {
		sql->Error(e);
		return 0;
	}

	return LastResult.c_str();
}

unsigned int SQLResult::NumRows()
{
	if (isFree)
		return 0;

	return (unsigned int)(res->RecordCount);
}