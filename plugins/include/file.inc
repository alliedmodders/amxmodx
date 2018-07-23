// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// File Functions
//

#if defined _file_included
	#endinput
#endif
#define _file_included

/**
 * @note All paths in AMX Mod X natives are relative to the mod folder
 * unless otherwise noted.
 *
 * Most functions in AMX Mod X (at least, ones that deal with direct 
 * file manipulation) will support an alternate path specification.
 */
 
/**
 * Maximum path length.
 */
#define PLATFORM_MAX_PATH  256

/**
 * File inode types for use with open_dir() and next_file().
 */
enum FileType
{
	FileType_Unknown,       /* Unknown file type (device/socket) */
	FileType_Directory,     /* File is a directory */
	FileType_File,          /* File is a file */
};

/**
 * File time modes for use with GetFileTime().
 */
enum FileTimeType
{
	FileTime_LastAccess,    /* Last access (not available on FAT) */
	FileTime_Created,       /* Creation (not available on FAT) */
	FileTime_LastChange,    /* Last modification */
};

/**
 * File position modes for use with fseek().
 */
#define SEEK_SET 0          /* Seek from start */
#define SEEK_CUR 1          /* Seek from current position */
#define SEEK_END 2          /* Seek from end position */

/**
 * Options for use with file_size() flag parameter.
 */
#define FSOPT_BYTES_COUNT  0  /* Returns the file size in number of bytes */
#define FSOPT_LINES_COUNT  1  /* Returns how many lines there are in this file */
#define FSOPT_END_WITH_LF  2  /* Returns whether the last line is '\n' */

/**
 * Data block modes for use with fread*() and fwrite*().
 */
#define BLOCK_INT   4
#define BLOCK_SHORT 2
#define BLOCK_CHAR  1
#define BLOCK_BYTE  1

/**
 * File permissions flags for use with mkdir() and SetFilePermissions().
 */
#define FPERM_U_READ       0x0100   /* User can read.    */
#define FPERM_U_WRITE      0x0080   /* User can write.   */
#define FPERM_U_EXEC       0x0040   /* User can exec.    */
#define FPERM_U_RWX        FPERM_U_READ | FPERM_U_WRITE | FPERM_U_EXEC

#define FPERM_G_READ       0x0020   /* Group can read.   */
#define FPERM_G_WRITE      0x0010   /* Group can write.  */
#define FPERM_G_EXEC       0x0008   /* Group can exec.   */
#define FPERM_G_RWX        FPERM_G_READ | FPERM_G_WRITE | FPERM_G_EXEC

#define FPERM_O_READ       0x0004   /* Anyone can read.  */
#define FPERM_O_WRITE      0x0002   /* Anyone can write. */
#define FPERM_O_EXEC       0x0001   /* Anyone can exec.  */
#define FPERM_O_RWX        FPERM_O_READ | FPERM_O_WRITE | FPERM_O_EXEC

#define FPERM_DIR_DEFAULT  FPERM_U_RWX | FPERM_G_RWX | FPERM_O_RWX /* rwx r-x r-x (0755) */


/**
 * Reads content from directory
 *
 * @note This native is expensive. Consider the use of open_dir(), next_file() and close_dir() instead.
 * @note Both the '.' and '..' automatic directory entries will be retrieved for Windows and Linux.
 *
 * @param dirname      Path to open
 * @param pos          Index the element
 * @param output       String buffer to hold content
 * @param len          Maximum size of string buffer
 * @param outlen       Number of characters written to the buffer
 *
 * @return             Returns index of next element, otherwiwe 0 when end of dir is reached
 */
native read_dir(const dirname[], pos, output[], len, &outlen = 0);

/**
 * Reads line from file.
 *
 * @note This native is expensive. Consider the use of new file natives (fopen(), fgets(), etc.)
 *       if purpose is to read several lines of a file.
 *
 * @param file         Path to open
 * @param line         Index of the line, starting to 0
 * @param text         String buffer to hold line read
 * @param len          Maximum size of string buffer
 * @param txtlen       Number of characters written to the buffer
 *
 * @return             Returns index of next line, otherwise 0 when end of file is reached
 * @error              Unable to read the file
 */
native read_file(const file[], line, text[], len, &txtlen = 0);

/**
 * Writes text to file.
 *
 * @note This native is expensive. Consider the use of new file natives (fopen(), fputs(), etc.)
 *       if purpose is to write several lines of a file.
 *
 * @param file         Path to open
 * @param text         String to write to
 * @param line         Index of the line, starting to 0
 *                     If < 0, content will be appended
 *
 * @noreturn
 * @error              Unable to write [temporary] file
 */
native write_file(const file[], const text[], line = -1);

/**
 * Deletes a file.
 *
 * @param file          Path of the file to delete
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to delete files existing in the Valve
 *                      search path, rather than solely files existing directly
 *                      in the gamedir.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for all search paths.
 *
 * @return              1 on success, 0 on failure or if file not immediately removed.
 */
native delete_file(const file[], bool:use_valve_fs = false, const valve_path_id[] = "GAMECONFIG");

/**
 * Checks if a file exists.
 *
 * @param file          Path to the file
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to find files existing in any of
 *                      the Valve search paths, rather than solely files
 *                      existing directly in the gamedir.
 *
 * @return              1 if the file exists, 0 otherwise
 */
native file_exists(const file[], bool:use_valve_fs = false);

/**
  * Renames a file.
  *
  * @param oldname      New path to the file
  * @param newname      Path to the existing file
  * @param relative     If true, native  will act like other natives which
  *                     use the moddir as a base directory. Otherwise, the
  *                     current directory is undefined (but assumed to be hlds).
  *
  * @return             1 on success, 0 otherwise
  */
native rename_file(const oldname[], const newname[], relative = 0);

/**
 * Checks if a directory exists.
 *
 * @param dir           Path to the directory
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to find files existing in any of
 *                      the Valve search paths, rather than solely files
 *                      existing directly in the gamedir.
 *
 * @return              1 if the directory exists, 0 otherwise
 */
native dir_exists(const dir[], bool:use_valve_fs = false);

/**
 * Get the file size in bytes.
 *
 * @param file          Path to the file
 * @param flag          Flag options, see FSOPT_* constants
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to find files existing in any of
 *                      the Valve search paths, rather than solely files
 *                      existing directly in the gamedir.
 *                      If used, flag option is ignored.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for all search paths
 *
 * @return              If flag is FSOPT_BYTES_COUNT or use_valve_fs to true, the file size in bytes
 *                      If flag is FSOPT_LINES_COUNT, the number of lines in the file
 *                      If flag is FSOPT_END_WITH_LF, 1 is returned if file ends with line feed
 *                      If file doesn't exist, -1
 */
native file_size(const file[], flag = FSOPT_BYTES_COUNT, bool:use_valve_fs = false, const valve_path_id[] = "GAME");

/**
 * Opens or creates a file, returning a file handle on success. File handles
 * should be closed with fclose().
 *
 * @note The open mode may be one of the following strings:
 *         "r": Open an existing file for reading.
 *         "w": Create a file for writing, or truncate (delete the contents of) an
 *              existing file and then open it for writing.
 *         "a": Create a file for writing, or open an existing file such that writes
 *              will be appended to the end.
 *        "r+": Open an existing file for both reading and writing.
 *        "w+": Create a file for reading and writing, or truncate an existing file
 *              and then open it for reading and writing.
 *        "a+": Create a file for both reading and writing, or open an existing file
 *              such that writes will be appended to the end.
 *
 * @note The open mode may also contain an additional character after "r", "w", or "a",
 *       but before any "+" sign. This character may be "b" (indicating binary mode) or
 *       "t" (indicating text mode). By default, "text" mode is implied. On Linux and
 *       Mac, this has no distinction from binary mode. On Windows, it causes the '\n'
 *       character (0xA) to be written as "\r\n" (0xD, 0xA).
 *
 *       Example: "rb" opens a binary file for writing; "at" opens a text file for
 *       appending.
 *
 * @note Registered paths ID are (in priority order) :
 *         GAME           All paths related to current mod, including fallback
 *                        Depending settings, it includes: <gamedir>_lv/_addon/_<language>/_hd
 *                        and <gamedir> itself
 *         GAMECONFIG     The default writable directory (<gamedir>)
 *         GAMEDOWNLOAD   The download directory (<gamedir>_download)
 *         GAME_FALLBACK  All paths related to fallback game, same as GAME
 *         DEFAULTGAME    All paths related to the default game which is "valve", same as GAME
 *         BASE           The base path where server is installed
 *
 *         Note that some paths are non-writable. It includes all <gamedir>_* (expect _download)
 *         and DEFAULTGAME. Any file inside a non-writable path will be ignored if you try to open
 *         it in writing mode.
 *
 * @param filename      File to open
 * @param mode          Open mode
 * @param use_valve_fs  If true, the Valve file system will be used instead
 *                      This can be used to finred files existing in valve
 *                      search paths, rather than solely files existing directly
 *                      in the gamedir.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for all search paths
 *
 * @return              A file handle, or null if the file could not be opened.
 */
native fopen(const filename[], const mode[], bool:use_valve_fs = false, const valve_path_id[] = "GAME");

/**
 * Closes a file handle.
 *
 * @param file          File handle
 */
native fclose(file);

/**
 * Reads a single binary data from a file.
 *
 * @param file          Handle to the file
 * @param data          Variable to store item read
 * @param mode          Size of each element, in bytes, to be read
 *                      See BLOCK_* constants
 *
 * @return              Number of elements read
 */
native fread(file, &any:data, mode);

/**
 * Reads binary data from a file.
 *
 * @param file          Handle to the file
 * @param data          Array to store each item read
 * @param blocks        Number of items to read into the array
 * @param mode          Size of each element, in bytes, to be read
 *                      Valid sizes are 1, 2, or 4. See BLOCK_* constants.
 *
 * @return              Number of elements read
 */
native fread_blocks(file, any:data[], blocks, mode);

/**
 * Reads raw binary data from a file.
 *
 * @param file          Handle to the file
 * @param stream        Array to store each item read
 * @param blocksize     Number of items to read into the array
 * @param blocks        Size of each element, in bytes. The data is read directly.
 *                      That is, in 1 or 2-byte mode, the lower byte(s) in
 *                      each cell are used directly, rather than performing
 *                      any casts from a 4-byte number to a smaller number.
 *
 * @return              Number of elements read
 */
native fread_raw(file, any:stream[], blocksize, blocks);

/**
 * Writes a single binary data to a file.
 *
 * @param file          Handle to the file
 * @param data          Item to write
 * @param mode          Size of each item in the array in bytes
 *                      Valid sizes are 1, 2, or 4. See BLOCK_* constants
 *
 * @return              Number of elements written
 */
native fwrite(file, any:data, mode);

/**
 * Writes binary data to a file.
 *
 * @param file          Handle to the file
 * @param data          Array of items to write
 * @param blocks        Number of items in the array
 * @param mode          Size of each item in the array in bytes
 *                      Valid sizes are 1, 2, or 4. See BLOCK_* constants
 *
 * @return              Number of elements written
 */
native fwrite_blocks(file, const any:data[], blocks, mode);

/**
 * Writes raw binary data to a file.
 *
 * @param file          Handle to the file.
 * @param stream        Array of items to write.  The data is written directly.
 *                      That is, in 1 or 2-byte mode, the lower byte(s) in
 *                      each cell are used directly, rather than performing
 *                      any casts from a 4-byte number to a smaller number.
 * @param blocks        Size of each item in the array in bytes.
 * @param mode          Number of items in the array.
 *
 * @return              Number of elements written
 */
native fwrite_raw(file, const any:stream[], blocks, mode);

/**
 * Tests if the end of file has been reached.
 *
 * @param file          Handle to the file
 *
 * @return              1 if end of file has been reached, 0 otherwise.
 */
native feof(file);

/**
 * Reads a line from a text file.
 *
 * @param file          Handle to the file.
 * @param buffer        String buffer to hold the line
 * @param maxlength     Maximum size of string buffer
 *
 * @return              Total number of characters written on success, 0 otherwise
 */
native fgets(file, buffer[], maxlength);

/**
 * Writes a line of text to a text file.
 *
 * @param file          Handle to the file
 * @param text          String to write
 * @param null_term     True to append NULL terminator, false otherwise
 *
 * @return              0 on success, -1 otherwise
 */
native fputs(file, const text[], bool:null_term = false);

/**
 * Writes a line of formatted text to a text file.
 *
 * @param file          Handle to the file
 * @param format        Formatting rules
 * @param ...           Variable number of format parameters
 *
 * @return              Total number of characters written on success, 0 otherwise
 */
native fprintf(file, const fmt[], any:...);

/**
 * Sets the file position indicator.
 *
 * @param file          Handle to the file
 * @param position      Position relative to what is specified in whence
 * @param start         SEEK_ constant value of where to see from
 *
 * @return              0 on success, a non-zero value otherwise
 */
native fseek(file, position, start);

/**
 * Gets current position in the file.
 *
 * @param file          Handle to the file
 *
 * @return              Value for the file position indicator
 */
native ftell(file);

/**
 * Gets character from file.
 *
 * @param file          Handle to the file
 *
 * @return              Character read on success, -1 otherwise
 */
native fgetc(file);

/**
 * Writes character to file
 *
 * @param file          Handle to the file
 * @param data          Character to put
 *
 * @return              Character written on success, -1 otherwise
 */
native fputc(file, data);

/**
 * Ungets character from file.
 *
 * @param file          Handle to the file
 * @param data          Character to unget
 *
 * @return              On success, the character put back is returned, -1 otherwise
 */
native fungetc(file, data);

/**
 * Flushes a buffered output stream.
 *
 * @param file          File handle, or 0 for all open streams
 *
 * @return              0 on success, -1 on failure
 */
native fflush(file);

/**
 * Gets the formatted file size in bytes.
 *
 * @param filename      Path to the file
 * @param ...           Variable number of format parameters
 *
 * @return              File size in bytes, otherwise -1 if file not found
 */
native filesize(const filename[], any:...);

/**
 * Removes a directory.
 *
 * @note On most Operating Systems you cannot remove a directory which has files inside it.
 *
 * @param path          Path to the directory
 *
 * @return              1 on success, 0 otherwise
 */
native rmdir(const path[]);

/**
 * Creates a directory.
 *
 * @param path          Path to create
 * @param mode          Permissions (default is o=rx,g=rx,u=rwx).  Note that folders must have
 *                      the execute bit set on Linux.  On Windows, the mode is ignored.
 * @param use_valve_fs  If true, the Valve file system will be used instead
 *                      This can be used to create folders in the game's
 *                      Valve search paths, rather than directly in the gamedir.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for default
 *                      In this case, mode is ignored
 *
 * @return              0 on success, -1 otherwise
 */
native mkdir(const dirname[], mode = FPERM_DIR_DEFAULT, bool:use_valve_fs = false, const valve_path_id[] = "GAMECONFIG");

/**
 * Deletes a file (delete_file macro)
 *
 * @param filename      Path of the file to delete
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to delete files existing in the Valve
 *                      search path, rather than solely files existing directly
 *                      in the gamedir.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for all search paths
 *
 * @return              1 on success, 0 on failure or if file not immediately removed
 */
native unlink(const filename[], bool:use_valve_fs = false, const valve_path_id[] = "GAMECONFIG");

/**
 * Opens a directory/folder for contents enumeration.
 *
 * @note Directories are closed with close_dir().
 *
 * @param dir           Path to open.
 * @param firstfile     String buffer to hold first file name
 * @param length        Maximum size of the string buffer
 * @param type          Optional variable to store the file type
 * @param use_valve_fs  If true, the Valve file system will be used instead.
 *                      This can be used to find files existing in any of
 *                      the Valve search paths, rather than solely files
 *                      existing directly in the gamedir.
 * @param valve_path_id If use_valve_fs, a search path from gameinfo or NULL_STRING for all search paths.
 *
 * @return              Handle to the directory, 0 otherwise
 */
native open_dir(dir[], firstfile[], length, &FileType:type = FileType_Unknown, bool:use_valve_fs = false, const valve_path_id[] = "GAME");

/**
 * Reads the next directory entry as a local filename.
 *
 * @note Contents of buffers are undefined when returning false.
 * @note Both the '.' and '..' automatic directory entries will be retrieved for Windows and Linux.
 *
 * @param dirh          Handle to a directory
 * @param buffer        String buffer to hold directory name
 * @param length        Maximum size of string buffer
 * @param type          Optional variable to store the file type. FileType_* constants
 *
 * @return              1 on success, 0 if there are no more files to read.
 */
native next_file(dirh, buffer[], length, &FileType:type = FileType_Unknown);

/**
 * Closes the directory.
 *
 * @param dirh          Handle to a directory
 */
native close_dir(dirh);

/**
 * Loads a file using the LoadFileForMe engine function.
 *
 * The data is truncated if there is not enough space.  No null-terminator
 * is applied; the data is the raw contents of the file.
 *
 * @param file          File to load (may be a file from the GCF)
 * @param buffer        Buffer to store file contents
 * @param maxlength     Maximum size of the file buffer
 * @param length        Variable to store the file length.  This may return
 *                      a number larger than the buffer size
 * @return              -1 if the file could not be loaded.  Otherwise,
 *                      the number of cells actually written to the buffer
 *                      are returned.
 */
native LoadFileForMe(const file[], buffer[], maxlength, &length = 0);

/**
 * Returns a file timestamp as a unix timestamp.
 *
 * @param file          File name
 * @param tmode         Time mode, see FileTime_* constants
 *
 * @return              Returns a file timestamp as a unix timestamp
 */
native GetFileTime(const file[], FileTimeType:tmode);

/**
 * Changes a file or directories permissions.
 *
 * @param path          Path to the file
 * @param mode          Permissions to set, see FPERM_* constants
 *
 * @return              True on success, false otherwise
 */
native bool:SetFilePermissions(const path[], mode);

/**
 * Reads a single int8 (byte) from a file. The returned value is sign-
 * extended to an int32.
 *
 * @param file          Handle to the file
 * @param data          Variable to store the data read
 *
 * @return              True on success, false on failure
 */
native bool:FileReadInt8(file, &any:data);

/**
 * Reads a single uint8 (unsigned byte) from a file. The returned value is
 * zero-extended to an int32.
 *
 * @param file          Handle to the file
 * @param data          Variable to store the data read
 *
 * @return              True on success, false on failure
 */
native bool:FileReadUint8(file, &any:data);

/**
 * Reads a single int16 (short) from a file. The value is sign-extended to
 * an int32.
 *
 * @param file          Handle to the file
 * @param data          Variable to store the data read
 *
 * @return              True on success, false on failure
 */
native bool:FileReadInt16(file, &any:data);

/**
 * Reads a single unt16 (unsigned short) from a file. The value is zero-
 * extended to an int32.
 *
 * @param file          Handle to the file
 * @param data          Variable to store the data read
 *
 * @return              True on success, false on failure
 */
native bool:FileReadUint16(file, &any:data);

/**
 * Reads a single int32 (int/cell) from a file.
 *
 * @param file          Handle to the file
 * @param data          Variable to store the data read
 *
 * @return              True on success, false on failure
 */
native bool:FileReadInt32(file, &any:data);

/**
 * Writes a single int8 (byte) to a file.
 *
 * @param file          Handle to the file
 * @param data          Data to write (truncated to an int8)
 *
 * @return              True on success, false on failure
 */
native bool:FileWriteInt8(file, any:data);

/**
 * Writes a single int16 (short) to a file.
 *
 * @param file          Handle to the file
 * @param data          Data to write (truncated to an int16)
 *
 * @return              True on success, false on failure
 */
native bool:FileWriteInt16(file, any:data);

/**
 * Writes a single int32 (int/cell) to a file.
 *
 * @param file          Handle to the file
 * @param data          Data to write
 *
 * @return              True on success, false on failure
 */
native bool:FileWriteInt32(file, any:data);

