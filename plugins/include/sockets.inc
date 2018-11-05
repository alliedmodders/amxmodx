// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Socket Functions
//

#if defined _socket_included
	#endinput
#endif
#define _socket_included

#pragma reqlib sockets
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib sockets
#endif

/**
 * Socket connection type (TCP/UDP)
 */
#define SOCKET_TCP 1
#define SOCKET_UDP 2

/**
 * Socket flags
 */
#define SOCK_NON_BLOCKING (1 << 0)    /* Set the socket a nonblocking */
#define SOCK_LIBC_ERRORS  (1 << 1)    /* Enable libc error reporting */

/**
 * Error reporting
 */
#define SOCK_ERROR_OK               0 /* No error */
#define SOCK_ERROR_CREATE_SOCKET    1 /* Couldn't create a socket */
#define SOCK_ERROR_SERVER_UNKNOWN   2 /* Server unknown */
#define SOCK_ERROR_WHILE_CONNECTING 3 /* Error while connecting */

/**
 * Connects to the given node and service via TCP/UDP.
 *
 * @note There's 2 types of error reporting on this function that you can use.
 * @note Default error codes:
 *       0 - No error
 *       1 - Error while creating socket
 *       2 - Couldn't resolve hostname
 *       3 - Couldn't connect
 * @note New, more expressive libc error codes:
 *       https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html
 *       https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno.h
 *       https://msdn.microsoft.com/en-us/library/ms740668.aspx
 *
 * @note The currently available bit flags are:
 *       - SOCK_NON_BLOCKING : if set, the socket will be on nonblocking mode
 *       - SOCK_LIBC_ERRORS : if set, the new libc errors will be seen on _error
 *
 * @note If no flags are set, the behaviour of the function will not be modified.
 *
 * @note Multiple flags may be set at the same time using the | operator. 
 *       For example, SOCK_NON_BLOCKING|SOCK_LIBC_ERRORS will create a nonblocking socket with libc error codes.
 *
 * @note If you're creating a new nonblocking socket, _hostname should be numeric to avoid calling the 
 *       name resolution server and potentially blocking the call.
 *
 * @note If the socket is a nonblocking one, the returned socket descriptor may be still connecting and 
 *       further checks should be done with socket_is_writable() before trying to send data.
 *
 * @param _hostname    Node to connect to
 * @param _port        Service to connect to
 * @param _protocol    Connect via SOCKET_TCP or SOCKET_UDP
 * @param _error       Set an error code here if anything goes wrong
 * @param _flags       Optional bit flags that change the behaviour of the function
 *
 * @return             A socket descriptor (a positive integer) on success
 *                     -1 on failure
*/
native socket_open(const _hostname[], _port, _protocol = SOCKET_TCP, &_error, _flags = 0);

/**
 * Closes a socket.
 *
 * @param _socket    Socket descriptor
 *
 * @return           1 on success
 *                   0 on failure
 */
native socket_close(_socket);

/**
 * Receives data.
 *
 * @note The amount of bytes than you end up receiving can be less than the one you expected.
 *
 * @note This function will completely block the server until some data arrives
 *       to the given socket. Use this only if you are sure there is some data
 *       to be retrieved. You can do that by polling with socket_is_readable().
 *
 * @param _socket    Socket descriptor
 * @param _data      Array to save the data
 * @param _length    Length of the array
 *
 * @return           Amount of bytes received
 *                   0 if the peer closed the connection
 *                   -1 on failure
 */
native socket_recv(_socket, _data[], _length);

/**
 * Sends data.
 *
 * @note The amount of bytes that end up being sent may be lower than the total length of the array,
 *       check the return value and send the rest if needed.
 *
 * @param _socket    Socket descriptor
 * @param _data      Array with the data to send
 * @param _length    Length of the array
 *
 * @return           Amount of bytes sent
 *                   -1 on failure
 */
native socket_send(_socket, const _data[], _length);

/**
 * Sends data that can contain null bytes.
 *
 * @note The amount of bytes that end up being sent may be lower than the total length of the array,
 *       check the return value and send the rest if needed.
 *
 * @note strlen(_data) will return the wrong length if the array contains null bytes.
 *
 * @param _socket    Socket descriptor
 * @param _data      Array with the data to send
 * @param _length    Length of the array
 *
 * @return           Amount of bytes sent
 *                   -1 on failure
 */
native socket_send2(_socket, const _data[], _length);

/**
 * Backwards compatible function.
 *
 * @deprecated Renamed to socket_is_readable()
 */
#pragma deprecated Use socket_is_readable() instead
native socket_change(_socket, _timeout = 100000);

/**
 * Checks if a socket is marked as readable.
 *
 * @note You can use this function to make sure there's something on the socket and avoid a blocking call.
 * @note Set _timeout to 0 avoid blocking the call.
 * @note A socket will become readable if there's any data or an EOF.
 *
 * @param _socket    Socket descriptor
 * @param _timeout   Amount of time to block the call waiting for the socket to be marked as readable or
 *                   for the timeout to expire, in µSeconds (1 sec = 1000000 µsec)
 *
 * @return           1 if the socket is marked as readable
 *                   0 otherwise
 */
native socket_is_readable(_socket, _timeout = 100000);

/**
 * Checks if a socket is marked as writable.
 *
 * @note Use this function to check if a nonblocking socket is ready to be used.
 * @note Set _timeout to 0 avoid blocking the call.
 * @note An UDP socket is always writable.
 *
 * @param _socket    Socket descriptor
 * @param _timeout   Amount of time to block the call waiting for the socket to be marked as writable or
 *                   for the timeout to expire, in µSeconds (1 sec = 1000000 µsec)
 *
 * @return           1 if the socket is marked as writable
 *                   0 otherwise
 */
native socket_is_writable(_socket, _timeout = 100000);
