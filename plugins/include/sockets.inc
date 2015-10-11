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

// Backwards compatibility and error reporting
#define ERROR_CREATE_SOCKET       1 // Couldn't create a socket
#define ERROR_SERVER_UNKNOWN      2 // Server unknown
#define ERROR_WHILE_CONNECTING    3 // Error while connecting

#define DISABLE_LIBC_ERRORS 0 // Old, default error reporting
#define ENABLE_LIBC_ERRORS  1 // Enable libc error reporting

// Use SOCKET_TCP for TCP Socket connections
#define SOCKET_TCP 1

// Use SOCKET_UDP for UDP Socket connections
#define SOCKET_UDP 2

/**
 * Connects to the given node and service via TCP/UDP.
 *
 * @note There's 2 types of error reporting on this function that you can use, for backwards compatibility reasons libc errors are disabled by default
 * @note Old, default error codes:
 *       0 - No error
 *       1 - Error while creating socket
 *       2 - Couldn't resolve hostname
 *       3 - Couldn't connect
 * @note New, libc error codes:
 *       https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html
 *       https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno.h
 *       https://msdn.microsoft.com/en-us/library/ms740668.aspx
 *
 * @param _hostname    Node to connect to
 * @param _port        Service to connect to
 * @param _protocol    Connect via SOCKET_TCP or SOCKET_UDP
 * @param _error       Set an error code here if anything goes wrong
 * @param _libc_errors Optional parameter that defaults to DISABLE_LIBC_ERRORS. If set to ENABLE_LIBC_ERRORS the new libc errors will be set on _error instead of the old, made up ones
 *
 * @return             A socket descriptor (a positive integer) on success
 *                     -1 on failure
*/
native socket_open(const _hostname[], _port, _protocol = SOCKET_TCP, &_error, _libc_errors = DISABLE_LIBC_ERRORS);

/**
 * Connects on nonblocking mode to the given node and service via TCP/UDP.
 *
 * @note _hostname should be numeric to avoid calling the name resolution server and potentially blocking the call
 * @note The returned socket descriptor may be still connecting, further checks should be done with socket_is_writable()
 * @note Unlike in socket_open() libc error codes are used. They can be found at:
 *       https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html
 *       https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno.h
 *       https://msdn.microsoft.com/en-us/library/ms740668.aspx
 *
 * @param _hostname    Node to connect to
 * @param _port        Service to connect to
 * @param _protocol    Connect via SOCKET_TCP or SOCKET_UDP
 * @param _error       Set an error code here if anything goes wrong

 * @return             A socket descriptor (a positive integer) on success
 *                     -1 on failure
*/
native socket_open_nb(const _hostname[], _port, _protocol = SOCKET_TCP, &_error);

/**
 * Closes a socket
 *
 * @param _socket    Socket descriptor
 *
 * @return           1 on success
 *                   -1 on failure
 */
native socket_close(_socket);

/**
 * Receives data.
 *
 * @note The ammount of bytes than you end up receiving can be less than the one you expected
 *
 * @param _socket    Socket descriptor
 * @param _data      Array to save the data
 * @param _length    Length of the array
 *
 * @return           Ammount of bytes received
 *                   0 if the peer closed the connection
 *                   -1 on failure
 */
native socket_recv(_socket, _data[], _length);

/**
 * Sends data.
 *
 * @note The ammount of bytes than you end up sending can be less than expected, you're responsible of sending the rest later
 *
 * @param _socket    Socket descriptor
 * @param _data      Array with the data to send
 * @param _length    Length of the array
 *
 * @return           Ammount of bytes sent
 *                   -1 on failure
 */
native socket_send(_socket, const _data[], _length);

/**
 * Sends data that can contain null bytes.
 *
 * @note The ammount of bytes than you end up sending can be less than expected, you're responsible of sending the rest later
 *
 * @param _socket    Socket descriptor
 * @param _data      Array with the data to send
 * @param _length    Length of the array
 *
 * @return           Ammount of bytes sent
 *                   -1 on failure
 */
native socket_send2(_socket, const _data[], _length);

/**
 * Check if a socket is marked as readable.
 *
 * @note You can use this function to make sure there's something on the socket and avoiding a blocking call
 * @note Set _timeout to 0 avoid blocking the call
 * @note A socket will become readable if there's any data or an EOF
 *
 * @param _socket    Socket descriptor
 * @param _timeout   Ammount of time to block the call waiting for the socket to be marked as readable or for the timeout to expire, in µSeconds (1 sec = 1000000 µsec)
 *
 * @return           1 if the socket is marked as readable
 *                   -1 otherwise
 */
native socket_change(_socket, _timeout = 100000);

/**
 * Check if a socket is marked as readable.
 *
 * @note You can use this function to make sure there's something on the socket and avoiding a blocking call
 * @note Set _timeout to 0 avoid blocking the call
 * @note A socket will become readable if there's any data or an EOF
 *
 * @param _socket    Socket descriptor
 * @param _timeout   Ammount of time to block the call waiting for the socket to be marked as readable or for the timeout to expire, in µSeconds (1 sec = 1000000 µsec)
 *
 * @return           1 if the socket is marked as readable
 *                   -1 otherwise
 */
native socket_is_readable(_socket, _timeout = 100000);

/**
 * Check if a socket is marked as writable.
 *
 * @note Use this function to check if a nonblocking socket is ready to be used
 * @note Set _timeout to 0 avoid blocking the call
 * @note An UDP socket is always writable
 *
 * @param _socket    Socket descriptor
 * @param _timeout   Ammount of time to block the call waiting for the socket to be marked as writable or for the timeout to expire, in µSeconds (1 sec = 1000000 µsec)
 *
 * @return           1 if the socket is marked as writable
 *                   -1 otherwise
 */
native socket_is_writable(_socket, _timeout = 100000);
