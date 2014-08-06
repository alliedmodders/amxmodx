// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// Codebase from Ivan, -g-s-ivan@web.de (AMX 0.9.3)
// Modification by Olaf Reusch, kenterfie@hlsw.de (AMXX 0.16, AMX 0.96)
// Modification by David Anderson, dvander@tcwonline.org (AMXx 0.20)
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Sockets Module
//

#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#ifdef _WIN32
/* Windows */
#include <winsock.h>
#include <io.h>
#define socklen_t int
#else
/* Unix/Linux */
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#define closesocket(s) close(s)
#endif

// AMX Headers
#include "amxxmodule.h"

#define SOCKET_TCP 1
#define SOCKET_UDP 2

// And global Variables:

// native socket_open(_hostname[], _port, _protocol = SOCKET_TCP, &_error);
static cell AMX_NATIVE_CALL socket_open(AMX *amx, cell *params)  /* 2 param */
{ 
    unsigned int port = params[2];
    int len;
    char* hostname = MF_GetAmxString(amx,params[1],0,&len); // Get the hostname from AMX
	cell *err = MF_GetAmxAddr(amx, params[4]);
    if(len == 0) { // just to prevent to work with a nonset hostname
        *err = 2;  // server unknown
        return -1;
    }
    *err = 0; // params[4] is error backchannel
    struct sockaddr_in server;
    struct hostent *host_info;
    unsigned long addr;
    int sock=-1;
    int contr;
    // Create a Socket
    sock = socket(AF_INET, params[3]==SOCKET_TCP?SOCK_STREAM:SOCK_DGRAM, 0);
    if (sock < 0) {
        // Error, couldn't create a socket, so set an error and return.
        *err = 1;
        return -1;
    }
    
    // Clear the server structure (set everything to 0)
    memset( &server, 0, sizeof (server));
    // Test the hostname, and resolve if needed
    if ((addr = inet_addr(hostname)) != INADDR_NONE) {
        // seems like hostname is a numeric ip, so put it into the structure
        memcpy( (char *)&server.sin_addr, &addr, sizeof(addr));
    }
    else {
        // hostname is a domain, so resolve it to an ip
        host_info = gethostbyname(hostname);
        if (host_info == NULL) {
            // an error occured, the hostname is unknown
            *err = 2;  // server unknown
            return -1;
        }
        // If not, put it in the Server structure
        memcpy( (char *)&server.sin_addr, host_info->h_addr, host_info->h_length);
    }
    // Set the type of the Socket
    server.sin_family = AF_INET;
    // Change the port to network byte order, and put it into the structure
    server.sin_port = htons(port);
    
    // Not, let's try to open a connection to the server
    contr = connect(sock, (struct sockaddr*)&server, sizeof( server));
    if (contr < 0) {
            // If an error occured cancel
            *err = 3;  //error while connecting
            return -1;
    }
    // Everything went well, so return the socket
    return sock;
}

// native socket_close(_socket);
static cell AMX_NATIVE_CALL socket_close(AMX *amx, cell *params)  /* 2 param */
{
    int socket = params[1];
    //PRINT_CONSOLE("Function: Close | Socket: %i\n", socket);
    #ifdef _WIN32 // On windows, check whether the sockets are initialized correctly
    closesocket(socket);
    #else
    // Close the socket (linux/unix styled systems)
    close(socket);
    #endif
    return 0;
}

// native socket_change(_socket, _timeout=100000);
// 1 sec =1000000 usec
static cell AMX_NATIVE_CALL socket_change(AMX *amx, cell *params)  /* 2 param */
{
    int socket = params[1];
    unsigned int timeout = params[2];
    //PRINT_CONSOLE("Function: Change | Socket: %i | Timeout: %i\n", socket, timeout);
    // We need both a timeout structure and a fdset for our filedescriptor
    fd_set rfds;
    struct timeval tv;
    // Fill in ...
    FD_ZERO(&rfds);
    FD_SET(socket, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = timeout;
    // Now we "select", which will show us if new data is waiting in the socket's buffer
    if (select(socket+1, &rfds, NULL, NULL, &tv) > 0) 
       return 1; // Ok, new data, return it
     else 
       return 0; // No new data, return it
}

// native socket_recv(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_recv(AMX *amx, cell *params)  /* 2 param */
{
    int socket = params[1];
    int length = params[3];
    int tmp = -1;
    // First we dynamicly allocate a block of heap memory for recieving our data
    char *tmpchar = new char[length];
    if(tmpchar == NULL) return -1; // If we didn't got a block, we have to quit here to avoid sigsegv
    // And set it all to 0, because the memory could contain old trash
    memset(tmpchar, 0, length);
    // Now we recieve
    tmp = recv(socket, tmpchar, length-1, 0);
	if (tmp == -1)
	{
		delete [] tmpchar;
		return -1;
	}
    // And put a copy of our recieved data into amx's string
    tmpchar[tmp]='\0';
    int nlen = 0;
    //int max = params[3];
    int max = length-1;
    const char* src = tmpchar;
    cell* dest = MF_GetAmxAddr(amx,params[2]);
    while(max--&&nlen<tmp){
     *dest++ = (cell)*src++;
     nlen++;
    }
    *dest = 0;
    // And we need to free up the space to avoid wasting memory
    delete [] tmpchar;
    // And finnally, return the what recv returnd
    return tmp;
}

// native socket_send(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_send(AMX *amx, cell *params)  /* 3 param */
{
	// We get the string from amx
	int len;
	int socket = params[1];
	char* data = MF_GetAmxString(amx,params[2],0,&len);
	// And send it to the socket
	return send(socket, data, len, 0);
}

static char *g_buffer = NULL;
static size_t g_buflen = 0;

// native socket_send2(_socket, _data[], _length);
static cell AMX_NATIVE_CALL socket_send2(AMX *amx, cell *params)  /* 3 param */
{
	// We get the string from amx
	int len = params[3];
	int socket = params[1];
	if ((size_t)len > g_buflen)
	{
		delete [] g_buffer;
		g_buffer = new char[len+1];
		g_buflen = len;
	}

	cell *pData = MF_GetAmxAddr(amx, params[2]);
	char *pBuffer = g_buffer;

	while (len--)
		*pBuffer++ = (char)*pData++;

	// And send it to the socket
    return send(socket, g_buffer, params[3], 0);
}

AMX_NATIVE_INFO sockets_natives[] = {
	{"socket_open", socket_open},
	{"socket_close", socket_close},
	{"socket_change", socket_change},
	{"socket_recv", socket_recv},
	{"socket_send", socket_send},
	{"socket_send2", socket_send2},
	{NULL, NULL}
};

void OnAmxxAttach()
{
	MF_AddNatives(sockets_natives);
    // And, if win32, we have to specially start up the winsock environment
    #ifdef _WIN32  
        short wVersionRequested;
        WSADATA wsaData;
        wVersionRequested = MAKEWORD (1, 1);
        if (WSAStartup (wVersionRequested, &wsaData) != 0) {
            MF_Log("Sockets Module: Error while starting up winsock environment.!");
        }
    #endif
    return;
}

void OnAmxxDetach()
{
    #ifdef _WIN32
    WSACleanup();
    #endif
    delete [] g_buffer;
    return;
}
