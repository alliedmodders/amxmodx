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
	#include <winsock2.h>
	#include <ws2tcpip.h>

	#undef errno
	#undef close

	#define errno WSAGetLastError()
	#define close(sockfd) closesocket(sockfd)

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

// Backwards compatibility
#define ERROR_CREATE_SOCKET 1		// Couldn't create a socket
#define ERROR_SERVER_UNKNOWN 2		// Server unknown
#define ERROR_WHILE_CONNECTING 3	// Error while connecting
#define ERROR_EHOSTUNREACH 113		// libc error code: No route to host

#ifdef _WIN32
	bool g_winsock_initialized = false;
#endif

static char *g_send2_buffer = nullptr;
static int g_send2_buffer_length = 0;

// native socket_open(_hostname[], _port, _protocol = SOCKET_TCP, &_error, _libc_errors = DISABLE_LIBC_ERRORS);
static cell AMX_NATIVE_CALL socket_open(AMX *amx, cell *params)
{
	int length = 0;
	char *hostname = MF_GetAmxString(amx, params[1], 0, &length);

	cell *error = MF_GetAmxAddr(amx, params[4]);
	*error = 0;

	bool libc_errors = false;

	if((*params / sizeof(cell)) == 5)
		libc_errors = (params[5] == 1) ? true : false;

	if(length == 0)
	{
		*error = libc_errors ? ERROR_EHOSTUNREACH : ERROR_SERVER_UNKNOWN;
		return -1;
	}

	char port_number[6];
	ke::SafeSprintf(port_number, sizeof(port_number), "%d", params[2]);

	int sockfd = -1, getaddrinfo_status = -1, connect_status = -1;
	struct addrinfo hints, *server_info, *server;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = params[3];

	if((getaddrinfo_status = getaddrinfo(hostname, port_number, &hints, &server_info)) != 0)
	{
		*error = libc_errors ? getaddrinfo_status : ERROR_SERVER_UNKNOWN;
		return -1;
	}

	server = server_info;

	do
	{
		if((sockfd = socket(server->ai_family, server->ai_socktype, server->ai_protocol)) != -1)
		{
			if((connect_status = connect(sockfd, server->ai_addr, server->ai_addrlen)) == -1)
			{
				*error = libc_errors ? errno : ERROR_WHILE_CONNECTING;
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
				*error = libc_errors ? errno : ERROR_CREATE_SOCKET;
		}

	} while(connect_status != 0 && (server = server->ai_next) != nullptr);

	freeaddrinfo(server_info);

	if(sockfd == -1 || server == nullptr)
		return -1;

	return sockfd;
}

int set_nonblocking(int sockfd)
{
#ifdef _WIN32
	unsigned long flags = 1;

	if(ioctlsocket(sockfd, FIONBIO, &flags) == 0)
		return 0;
	else
		return errno;
#else
	int flags = -1;

	if((flags = fcntl(sockfd, F_GETFL, 0)) == -1)
		return errno;

	if(fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) == -1)
		return errno;

	return 0;
#endif
}

// native socket_open_nb(_hostname[], _port, _protocol = SOCKET_TCP, &_error);
static cell AMX_NATIVE_CALL socket_open_nb(AMX *amx, cell *params)
{
	int length = 0;
	char *hostname = MF_GetAmxString(amx, params[1], 0, &length);

	cell *error = MF_GetAmxAddr(amx, params[4]);
	*error = 0;

	if(length == 0)
	{
		*error = ERROR_EHOSTUNREACH;
		return -1;
	}

	char port_number[6];
	ke::SafeSprintf(port_number, sizeof(port_number), "%d", params[2]);

	int sockfd = -1, getaddrinfo_status = -1, connect_status = -1, setnonblocking_status = -1;
	bool connect_inprogress = false;
	struct addrinfo hints, *server_info, *server;

	memset(&hints, 0, sizeof(hints));

	// Both hostname and port should be numeric to prevent the name resolution service from being called and potentially blocking the call for a long time
	hints.ai_flags = AI_NUMERICHOST | AI_NUMERICSERV; 
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = params[3];

	if((getaddrinfo_status = getaddrinfo(hostname, port_number, &hints, &server_info)) != 0)
	{
		*error = getaddrinfo_status;
		return -1;
	}

	server = server_info;

	do
	{
		if((sockfd = socket(server->ai_family, server->ai_socktype, server->ai_protocol)) != -1)
		{
			setnonblocking_status = set_nonblocking(sockfd);

			if(setnonblocking_status == 0)
			{
				if((connect_status = connect(sockfd, server->ai_addr, server->ai_addrlen)) == -1)
				{
					*error = errno;

					if(*error == EINPROGRESS)
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
					*error = setnonblocking_status;
			}
		}
		else
		{
			if(*error == 0)
				*error = errno;
		}

	} while(connect_inprogress == false && connect_status != 0 && (server = server->ai_next) != nullptr);

	freeaddrinfo(server_info);

	if(sockfd == -1 || server == nullptr)
		return -1;

	return sockfd;
}

// native socket_close(_socket);
static cell AMX_NATIVE_CALL socket_close(AMX *amx, cell *params)
{
	return (close(params[1]) == -1) ? -1 : 1;
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
	strncopy(destination, recv_buffer, length - 1);

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
		g_send2_buffer_length = length;
	}

	cell *data = MF_GetAmxAddr(amx, params[2]);

	while(length--)
		*g_send2_buffer++ = (char)*data++;

	return send(sockfd, g_send2_buffer, length, 0);
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

	return (select(sockfd + 1, &readfds, nullptr, nullptr, &tv) > 0) ? 1 : -1;
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

	return (select(sockfd + 1, nullptr, &writefds, nullptr, &tv) > 0) ? 1 : -1;
}

AMX_NATIVE_INFO sockets_natives[] =
{
	{"socket_open", socket_open},
	{"socket_open_nb", socket_open_nb},

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
