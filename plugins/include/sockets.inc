/*
 *
 * AMX Mod X Module
 * Basic Socket Functions
 * 
 * Codebase from Ivan, -g-s-ivan@web.de (AMX 0.9.3)
 * Modification by Olaf Reusch, kenterfie@hlsw.de (AMXX 0.16, AMX 0.96)
 * 
 */

#if defined _socket_included
	#endinput
#endif
#define _socket_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib sockets
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib sockets
 #endif
#else
 #pragma library socket
#endif

// Use SOCKET_TCP for TCP Socket connections

#define SOCKET_TCP 1

// Use SOCKET_UDP for UDP Socket connections

#define SOCKET_UDP 2

/* Opens a new connection to hostname:port via protocol (either SOCKET_TCP or SOCKET_UDP),
 * returns a socket (positive) or negative or zero on error.
 * States of error:
 * 0 - no error
 * 1 - error while creating socket
 * 2 - couldn't resolve hostname
 * 3 - couldn't connect to given hostname:port 
*/

native socket_open(const _hostname[], _port, _protocol = SOCKET_TCP, &_error);

/* Closes a Socket */

native socket_close(_socket);

/* Recieves Data to string with the given length */

native socket_recv(_socket, _data[], _length);

/* Sends data to the Socket */

native socket_send(_socket, const _data[], _length);

/* Same as socket_send but Data can contain null bytes */

native socket_send2(_socket, const _data[], _length);

/* This function will return true if the state (buffer content) have changed within the last recieve or
* the timeout, where timeout is a value in µSeconds, (1 sec =1000000 µsec). 
* Use to check if new data is in your socket. */

native socket_change(_socket, _timeout=100000);
