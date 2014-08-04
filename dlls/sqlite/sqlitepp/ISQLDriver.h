// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLite Module
//

#ifndef _INCLUDE_SOURCEMOD_DATABASE2_H
#define _INCLUDE_SOURCEMOD_DATABASE2_H

#include <stdarg.h>

namespace SourceMod
{
	class IResultRow
	{
	public:
		virtual ~IResultRow() { };
	public:
		/**
		 * This will return NULL if the entry is NULL.
		 * Remember that in SQL, a field can have NULL 
		 *  entries, which are not the same as 0 or "".
		 */
		virtual const char *GetString(unsigned int columnId) =0;
		virtual double GetDouble(unsigned int columnId) =0;
		virtual float GetFloat(unsigned int columnId) =0;
		virtual int GetInt(unsigned int columnId) =0;
		virtual bool IsNull(unsigned int columnId) =0;
		/**
		 * NULL can be returned.  The length will be zero if so.
		 */
		virtual const char *GetRaw(unsigned int columnId, size_t *length) =0;
	};

	class IResultSet
	{
	public:
		virtual ~IResultSet() { };
	public:
		//free the handle if necessary (see IQuery).
		virtual void FreeHandle() =0;
	public: //Basic stuff
		virtual unsigned int RowCount() =0;
		virtual unsigned int FieldCount() =0;
		virtual const char *FieldNumToName(unsigned int num) =0;
		virtual bool FieldNameToNum(const char *name, unsigned int *columnId) =0;
	public: //iteration
		/**
		 * Returns true if there are no more handles left.
		 */
		virtual bool IsDone() =0;
		/**
		 * Returns the current row.  If "IsDone()" is false
		 *  this is guaranteed to return non-NULL.
		 * Handles to IResultRow are guaranteed to not leak
		 *  (you don't need to free them), however,
		 *  they should be considered volatile - don't cache
		 *  them.
		 */
		virtual IResultRow *GetRow() =0;
		/**
		 * Advances to the next row.  Note that you need to
		 *  call IsDone() after each call to NextRow().
		 */
		virtual void NextRow() =0;
		/**
		 * Resets back to the first row.
		 */
		virtual void Rewind() =0;

		/* Always returns false in Sqlite */
		virtual bool NextResultSet() =0;
	};

	struct QueryInfo
	{
		IResultSet *rs;
		unsigned long long affected_rows;
		int errorcode;
		bool success;
		unsigned long long insert_id;
	};

	class IQuery
	{
	public:
		virtual ~IQuery() { };
	public:
		//you must free the handle when done
		virtual void FreeHandle() =0;
	public:
		/**
		 * Executes the query.  Specify optional error string buffer.
		 * If "info" is NULL, no results will be stored.
		 * Returns false on failure.
		 * Calling Execute() multiple times will cause each result handle
		 *  to be freed in succession.  That means that you do not need to 
		 *  explicitly free IResultSets when using Execute(), but their
		 *  handles are deep-invalidated on succesive calls, and
		 *  thus Execute() is also not thread safe.
		 */
		virtual bool Execute(QueryInfo *info, char *error, size_t maxlength) =0;
		/**
		 * Same as above, except result handles are not freed for you.
		 */
		virtual bool ExecuteR(QueryInfo *info, char *error, size_t maxlength) =0;
		/**
		 * Returns the query string.
		 */
		virtual const char *GetQueryString() =0;
		/**
		 * Same as execute, but supports insert_id
		 */
		virtual bool Execute2(QueryInfo *info, char *error, size_t maxlength) =0;
	};

	class ISQLDriver;

	class IDatabase
	{
	public:
		virtual ~IDatabase() { };
	public:
		/** 
		 * Closes the database and frees the handle.
		 */
		virtual void FreeHandle() =0;
		/**
		 * Returns the parent driver.
		 */
		virtual ISQLDriver *Driver() =0;
	public:
		/**
		 * Query preparation.
		 */
		virtual IQuery *PrepareQueryFmt(const char *fmt, ...) =0;
		virtual IQuery *PrepareQueryFmt(const char *fmt, va_list ap) =0;
		virtual IQuery *PrepareQuery(const char *query) =0;
		/**
		 * Quotes a string properly.
		 * Returns 0 on success.  On failure, returns
		 *  the size of the buffer needed, or a negative number
		 *  on internal failure.
		 */
		virtual int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize) =0;
		
		/**
		 * @brief Sets the character set of the current connection
		 *
		 * @param characterset  The characterset to switch to. e.g. "utf8".
		 */
		virtual bool SetCharacterSet(const char *characterset) =0;
	};

	struct DatabaseInfo
	{
		DatabaseInfo() : max_timeout(0) { };
		const char *host;
		const char *database;
		const char *user;
		const char *pass;
		unsigned int port;
		unsigned int max_timeout;
		const char *charset;
	};

	class ISQLDriver
	{
	public:
		virtual ~ISQLDriver() { };
	public:
		virtual IDatabase *Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength) =0;
		//Supports the timeout clause
		virtual IDatabase *Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength) =0;
		virtual const char *NameString() =0;
		virtual bool IsCompatDriver(const char *namestring) =0;
	};
};

#endif //_INCLUDE_SOURCEMOD_DATABASE2_H
