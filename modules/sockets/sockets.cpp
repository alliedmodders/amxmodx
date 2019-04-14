// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Sockets Module
//

#include "amxxmodule.h"
#include <amtl/am-string.h>

#ifdef _WIN32
	#include <WinSock2.h>
	#include <WS2tcpip.h>

	#undef errno
	#undef close

	#define errno WSAGetLastError()
	#define close(sockfd) closesocket(sockfd)

	#undef EINPROGRESS
	#undef EWOULDBLOCK

	#define EINPROGRESS WSAEINPROGRESS
	#define EWOULDBLOCK WSAEWOULDBLOCK
#else
	#include <netinet/in.h>
	#include <sys/socket.h>
	#include <sys/types.h>
	#include <arpa/inet.h>
	#include <unistd.h>
	#include <errno.h>
	#include <netdb.h>
	#include <fcntl.h>
#endif

#ifdef _WIN32
	bool g_winsock_initialized = false;
#endif

static char *g_send2_buffer = nullptr;
static int g_send2_buffer_length = 0;


bool setnonblocking(int sockfd)
{
#ifdef _WIN32
	unsigned long flags = 1;

	return (ioctlsocket(sockfd, FIONBIO, &flags) == 0);
#else

	int flags = -1;

	if((flags = fcntl(sockfd, F_GETFL, 0)) == -1)
		return false;

	if(fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) == -1)
		return false;

	return true;
#endif
}

// native socket_open(_hostname[], _port, _protocol = SOCKET_TCP, &_error, _flags = 0);
static cell AMX_NATIVE_CALL socket_open(AMX *amx, cell *params)
{
	// Socket flags and backwards compatibility
	enum
	{
		SOCK_NON_BLOCKING = (1 << 0),
		SOCK_LIBC_ERRORS = (1 << 1)
	};

	#define SOCK_ERROR_OK               0    // No error
	#define SOCK_ERROR_CREATE_SOCKET    1    // Couldn't create a socket
	#define SOCK_ERROR_SERVER_UNKNOWN   2    // Server unknown
	#define SOCK_ERROR_WHILE_CONNECTING 3    // Error while connecting
	#define ERROR_EHOSTUNREACH          113  // libc error code: No route to host

	int hostname_length = 0;
	char *hostname = MF_GetAmxString(amx, params[1], 0, &hostname_length);

	cell *error = MF_GetAmxAddr(amx, params[4]);
	*error = 0;

	unsigned int flags = 0;
	bool libc_errors = false, nonblocking_socket = false;

	if((*params / sizeof(cell)) == 5)
	{
		flags = params[5];

		nonblocking_socket = (flags & SOCK_NON_BLOCKING) != 0;
		libc_errors = (flags & SOCK_LIBC_ERRORS) != 0;
	}

	if(hostname_length == 0)
	{
		*error = libc_errors ? ERROR_EHOSTUNREACH : SOCK_ERROR_SERVER_UNKNOWN;
		return -1;
	}

	char port_number[6];
	ke::SafeSprintf(port_number, sizeof(port_number), "%d", params[2]);

	int sockfd = -1, getaddrinfo_status = -1, connect_status = -1;
	bool setnonblocking_status = false, connect_inprogress = false;
	struct addrinfo hints, *server_info, *server;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = params[3];

	if((getaddrinfo_status = getaddrinfo(hostname, port_number, &hints, &server_info)) != 0)
	{
		*error = libc_errors ? getaddrinfo_status : SOCK_ERROR_SERVER_UNKNOWN;
		return -1;
	}

	server = server_info;

	do
	{
		if((sockfd = socket(server->ai_family, server->ai_socktype, server->ai_protocol)) != -1)
		{
			if(nonblocking_socket)
				setnonblocking_status = setnonblocking(sockfd);

			if(nonblocking_socket == false || (nonblocking_socket && setnonblocking_status == true))
			{
				if((connect_status = connect(sockfd, server->ai_addr, server->ai_addrlen)) == -1)
				{
					*error = libc_errors ? errno : SOCK_ERROR_WHILE_CONNECTING;

					if(nonblocking_socket && (errno == EINPROGRESS || errno == EWOULDBLOCK))
						connect_inprogress = true;
					else
						close(sockfd);
				}
				else
				{
					*error = 0;
				}
			}
			else
			{
				if(*error == 0)
					*error = libc_errors ? errno : SOCK_ERROR_CREATE_SOCKET;
			}
		}
		else
		{
			if(*error == 0)
				*error = errno;
		}

	} while((nonblocking_socket && connect_inprogress == false) && connect_status != 0 && (server = server->ai_next) != nullptr);
	
	freeaddrinfo(server_info);

	if(sockfd == -1 || server == nullptr)
		return -1;

	return sockfd;
}

// native socket_close(_socket);
static cell AMX_NATIVE_CALL socket_close(AMX *amx, cell *params)
{
	return (close(params[1]) == -1) ? 0 : 1;
}

// native socket_recv(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_recv(AMX *amx, cell *params)
{
	int sockfd = params[1];
	int length = params[3];

	char *recv_buffer = new char[length];

	if(recv_buffer == nullptr)
		return -1;

	memset(recv_buffer, 0, length);

	int bytes_received = -1;
	bytes_received = recv(sockfd, recv_buffer, length - 1, 0);

	if(bytes_received == -1)
	{
		delete[] recv_buffer;
		return -1;
	}

	recv_buffer[bytes_received] = '\0';

	cell* destination = MF_GetAmxAddr(amx, params[2]);

	int current_length = 0;
	int max_length = length - 1;

	const char *buffer = recv_buffer;
	while(max_length-- && current_length < bytes_received)
	{
		*destination++ = (cell)*buffer++;
		current_length++;
	}

	*destination = 0;

	delete[] recv_buffer;

	return bytes_received;
}

// native socket_send(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_send(AMX *amx, cell *params)
{
	int sockfd = params[1];
	int length = 0;
	
	char *data = MF_GetAmxString(amx, params[2], 0, &length);

	return send(sockfd, data, length, 0);
}

// native socket_send2(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_send2(AMX *amx, cell *params)
{
	int sockfd = params[1];
	int length = params[3];
	
	if(length > g_send2_buffer_length)
	{
		delete[] g_send2_buffer;

		g_send2_buffer = new char[length + 1];

		if(g_send2_buffer == nullptr)
		{
			g_send2_buffer_length = 0;
			return -1;
		}
		
		g_send2_buffer_length = length;
	}

	cell *data = MF_GetAmxAddr(amx, params[2]);
	char *buffer = g_send2_buffer;

	while(length--)
		*buffer++ = (char)*data++;

	return send(sockfd, g_send2_buffer, params[3], 0);
}

// native socket_change(_socket, _timeout = 100000);
static cell AMX_NATIVE_CALL socket_change(AMX *amx, cell *params)
{
	int sockfd = params[1];
	unsigned int timeout = params[2];

	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = timeout;

	fd_set readfds;
	FD_ZERO(&readfds);
	FD_SET(sockfd, &readfds);

	return (select(sockfd + 1, &readfds, nullptr, nullptr, &tv) > 0) ? 1 : 0;
}

// native socket_is_readable(_socket, _timeout = 100000);
static cell AMX_NATIVE_CALL socket_is_readable(AMX *amx, cell *params)
{
	return socket_change(amx, params);
}

// native socket_is_writable(_socket, _timeout = 100000);
static cell AMX_NATIVE_CALL socket_is_writable(AMX *amx, cell *params)
{
	int sockfd = params[1];
	unsigned int timeout = params[2];

	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = timeout;

	fd_set writefds;
	FD_ZERO(&writefds);
	FD_SET(sockfd, &writefds);

	return (select(sockfd + 1, nullptr, &writefds, nullptr, &tv) > 0) ? 1 : 0;
}

AMX_NATIVE_INFO sockets_natives[] =
{
	{"socket_open", socket_open},

	{"socket_close", socket_close},

	{"socket_recv", socket_recv},

	{"socket_send", socket_send},
	{"socket_send2", socket_send2},

	{"socket_change", socket_change},
	{"socket_is_readable", socket_is_readable},
	{"socket_is_writable", socket_is_writable},

	{NULL, NULL}
};

void OnAmxxAttach()
{
#ifdef _WIN32  
	WSADATA WSAData;
	int errorcode = WSAStartup(MAKEWORD(2, 2), &WSAData);

	if(errorcode != 0)
	{
		MF_Log("[%s]: WSAStartup failed with error code %d. Natives will not be available.", MODULE_LOGTAG, errorcode);
		return;
	}
	
	g_winsock_initialized = true;
#endif

	MF_AddNatives(sockets_natives);
}

void OnAmxxDetach()
{
#ifdef _WIN32
	if(g_winsock_initialized)
		WSACleanup();
#endif

	delete[] g_send2_buffer;
}
