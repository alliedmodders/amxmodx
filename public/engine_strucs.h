//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _ENGINE_STRUCTS_H_
#define _ENGINE_STRUCTS_H_

#include <extdll.h>
#include <meta_api.h>
#include <osdep.h>
#include <FileSystem.h>
#include <entity_state.h>
#include <usercmd.h>

#define MAX_CLIENTS            32
#define MAX_EDICTS             900
#define MAX_NAME               32
#define MAX_LIGHTSTYLES        64
#define MAX_PACKET_ENTITIES    256
#define MAX_PATH_LENGTH        260

#define MAX_LATENT             32
#define FRAGMENT_SIZE          1400
#define MAX_FRAGMENTS          25000
#define MAX_EVENT_QUEUE        64

#define MAX_FLOWS              2
#define FLOW_OUTGOING          0
#define FLOW_INCOMING          1

#define MAX_KV_LEN             127  // Max key/value length (with a NULL char)
#define MAX_INFO_STRING        256  // Key + value + 2 x slash + NULL
#define INFO_MAX_BUFFER_VALUES 4

#define FEVENT_ORIGIN          (1<<0)  // Event was invoked with stated origin
#define FEVENT_ANGLES          (1<<1)  // Event was invoked with stated angles

#define HL_MODEL_MAX           512
#define HL_GENERIC_MAX         512
#define HL_EVENT_MAX           256
#define HL_SOUND_MAX           512
#define HL_SOUND_HASHLOOKUP_SIZE (HL_SOUND_MAX * 2 - 1)

// Authentication types
#define AUTH_IDTYPE_UNKNOWN	   0
#define AUTH_IDTYPE_STEAM	   1
#define AUTH_IDTYPE_VALVE	   2
#define AUTH_IDTYPE_LOCAL	   3

#define NUM_BASELINES          64

// 0 == regular, 1 == file stream
#define MAX_STREAMS            2

// Flow control bytes per second limits
#define MAX_RATE               20000
#define MIN_RATE               1000

// Default data rate
#define DEFAULT_RATE           (9999.0f)

// This is the packet payload without any header bytes (which are attached for actual sending)
#define NET_MAX_PAYLOAD        3990

typedef enum netsrc_s
{
	NS_CLIENT,
	NS_SERVER,
	NS_MULTICAST

} netsrc_t;

typedef enum server_state_e
{
	ss_dead    = 0,
	ss_loading = 1,
	ss_active  = 2,

} server_state_t;

typedef enum
{
	NA_UNUSED,
	NA_LOOPBACK,
	NA_BROADCAST,
	NA_IP,
	NA_IPX,
	NA_BROADCAST_IPX,

} netadrtype_t;

typedef struct netadr_s
{
	netadrtype_t	type;
	unsigned char	ip[4];
	unsigned char	ipx[10];
	unsigned short	port;

} netadr_t;

typedef struct sizebuf_s
{
	const char* buffername;
	uint16      flags;
	byte*       data;
	int         maxsize;
	int         cursize;

} sizebuf_t;

typedef struct flowstats_s
{
	int    size;  // Size of message sent/received
	double time;  // Time that message was sent/received

} flowstats_t;

typedef struct flow_s
{
	
	flowstats_t stats[MAX_LATENT];  // Data for last MAX_LATENT messages
	int         current;            // Current message position
	double      nextcompute;        // Time when we should recompute k/sec data
	float       kbytespersec;       // Average data
	float       avgkbytespersec;

} flow_t;

typedef struct fragbuf_s
{
	
	fragbuf_s* next;                             // Next buffer in chain
	int        bufferid;                         // Id of this buffer
	
	sizebuf_t  frag_message;                     // Message buffer where raw data is stored
	byte       frag_message_buf[FRAGMENT_SIZE];  // The actual data sits here
	
	qboolean   isfile;                           // Is this a file buffer?
	qboolean   isbuffer;                         // Is this file buffer from memory ( custom decal, etc. ).
	qboolean   iscompressed;
	
	char       filename[MAX_PATH_LENGTH];        // Name of the file to save out on remote host
	int        foffset;                          // Offset in file from which to read data
	int        size;                             // Size of data to read at that offset

} fragbuf_t;

typedef struct fragbufwaiting_s
{
	fragbufwaiting_s* next;          // Next chain in waiting list
	int               fragbufcount;  // Number of buffers in this chain
	fragbuf_t*        fragbufs;      // The actual buffers

} fragbufwaiting_t;

typedef struct netchan_s
{	
	netsrc_t          sock;                          // NS_SERVER or NS_CLIENT, depending on channel.
	netadr_t          remote_address;                // Address this channel is talking to.

	int               player_slot;
	
	float             last_received;                 // For timeouts.  Time last message was received.
	float             connect_time;                  // Time when channel was connected.

	double            rate;                          // Bandwidth choke, Bytes per second
	double            cleartime;                     // If realtime > cleartime, free to send next packet

	// Sequencing variables
	int               incoming_sequence;             // Increasing count of sequence numbers 
	int               incoming_acknowledged;         // # of last outgoing message that has been ack'd.
	int               incoming_reliable_acknowledged;// Toggles T/F as reliable messages are received.
	int               incoming_reliable_sequence;    // single bit, maintained local
	int               outgoing_sequence;             // Message we are sending to remote
	int               reliable_sequence;             // Whether the message contains reliable payload, single bit
	int               last_reliable_sequence;        // Outgoing sequence number of last send that had reliable data

	void*             connection_status;
	int               (*pfnNetchan_Blocksize)(void *);

	sizebuf_t         message;                        // Staging and holding areas
	byte              message_buf[NET_MAX_PAYLOAD];

	
	int               reliable_length;                // Reliable message buffer. We keep adding to it until reliable is acknowledged. Then we clear it.
	byte              reliable_buf[NET_MAX_PAYLOAD];
	
	fragbufwaiting_t* waitlist[MAX_STREAMS];          // Waiting list of buffered fragments to go onto queue. Multiple outgoing buffers can be queued in succession.
	
	int               reliable_fragment[MAX_STREAMS]; // Is reliable waiting buf a fragment?
	unsigned int      reliable_fragid[MAX_STREAMS];   // Buffer id for each waiting fragment

	
	fragbuf_t*        fragbufs[MAX_STREAMS];          // The current fragment being set
	int               fragbufcount[MAX_STREAMS];      // The total number of fragments in this stream
	
	short int         frag_startpos[MAX_STREAMS];     // Position in outgoing buffer where frag data starts
	short int         frag_length[MAX_STREAMS];       // Length of frag data in the buffer
	
	fragbuf_t*        incomingbufs[MAX_STREAMS];      // Incoming fragments are stored here
	qboolean          incomingready[MAX_STREAMS];     // Set to true when incoming data is ready

	char              incomingfilename[MAX_PATH_LENGTH];  // Only referenced by the FRAG_FILE_STREAM component
	                                                  //  Name of file being downloaded
	void*             tempbuffer;
	int               tempbuffersize;
	
	flow_t            flow[MAX_FLOWS];                // Incoming and outgoing flow metrics

} netchan_t;

typedef struct packet_entities_s
{
	int             num_entities;
	unsigned char   flags[32];
	entity_state_t* entities;

} packet_entities_t;


typedef struct client_frame_s
{
	double            senttime;
	float             ping_time;
	clientdata_t      clientdata;
	weapon_data_t     weapondata[64];
	packet_entities_t entities;

} client_frame_t;

typedef struct event_args_s
{
	int		flags;

	int		entindex;  // Transmitted

	float	origin[3];
	float	angles[3];
	float	velocity[3];

	int		ducking;

	float	fparam1;
	float	fparam2;

	int		iparam1;
	int		iparam2;

	int		bparam1;
	int		bparam2;

} event_args_t;

struct event_info_s
{
	unsigned short index;         // 0 implies not in use

	short          packet_index;  // Use data from state info for entity in delta_packet .  -1 implies separate info based on event
	short          entity_index;  // The edict this event is associated with

	float         fire_time;      // if non-zero, the time when the event should be fired ( fixed up on the client )

	event_args_t  args;
	int	          flags;          // Client only - Reliable or not, etc.
};

typedef struct event_state_s
{
	struct event_info_s ei[MAX_EVENT_QUEUE];

} event_state_t;

typedef struct USERID_s
{
	int          idtype;
	uint64       m_SteamID;
	unsigned int clientip;

} USERID_t;

typedef struct consistency_s
{
	char* filename;
	int   issound;
	int   orig_index;
	int   value;
	int   check_type;
	float mins[3];
	float maxs[3];

} consistency_t;

typedef struct event_s
{
	unsigned short	index;
	const char*     filename;
	int				filesize;
	const char*     pszScript;

} event_t;

typedef struct extra_baselines_s
{
	int number;
	int classname[NUM_BASELINES];
	entity_state_t baseline[NUM_BASELINES];
} extra_baselines_t;

typedef struct server_log_s
{
	qboolean active;
	qboolean net_log_;
	netadr_t net_address_;
	void*    file;

} server_log_t;

typedef struct server_stats_s
{
	int   num_samples;

	int   at_capacity;
	int   at_empty;

	float capacity_percent;
	float empty_percent;

	int   minusers;
	int   maxusers;

	float cumulative_occupancy;
	float occupancy;

	int   num_sessions;
	float cumulative_sessiontime;
	float average_session_len;

	float cumulative_latency;
	float average_latency;

} server_stats_t;

typedef struct server_static_s
{
	qboolean          dll_initialized;

	struct client_s*  clients;
	int               maxclients;
	int               maxclientslimit;

	int               spawncount;
	int               serverflags;

	server_log_t      log;

	double            next_cleartime;
	double            next_sampletime;

	server_stats_t    stats;
	qboolean          isSecure;

} server_static_t;

typedef struct server_s
{
	qboolean               active;
	qboolean               paused;
	qboolean               loadgame;

	double                 time;
	double                 oldtime;

	int                    lastcheck;
	double                 lastchecktime;

	char                   name[64];
	char                   oldname[64];
	char                   startspot[64];
	char                   modelname[64];

	struct model_s*        worldmodel;
	CRC32_t                worldmapCRC;

	unsigned char          clientdllmd5[16];

	resource_t             resourcelist[1280];
	int                    num_resources;

	consistency_t          consistency_list[512];
	int                    num_consistency;

	const char*            model_precache[HL_MODEL_MAX];
	struct model_s*        models[HL_MODEL_MAX];
	unsigned char          model_precache_flags[HL_MODEL_MAX];
	struct event_s         event_precache[HL_EVENT_MAX];

	const char*            sound_precache[HL_SOUND_MAX];
	short int              sound_precache_hashedlookup[HL_SOUND_HASHLOOKUP_SIZE];
	qboolean               sound_precache_hashedlookup_built;

	const char*            generic_precache[HL_GENERIC_MAX];
	char                   generic_precache_names[HL_GENERIC_MAX][64];
	int                    num_generic_names;

	char*                  lightstyles[MAX_LIGHTSTYLES];

	int                    num_edicts;
	int                    max_edicts;
	edict_t*               edicts;

	struct entity_state_s* baselines;
	extra_baselines_t*     instance_baselines;
	server_state_t         state;

	sizebuf_t              datagram;
	unsigned char          datagram_buf[4000];

	sizebuf_t              reliable_datagram;
	unsigned char          reliable_datagram_buf[4000];

	sizebuf_t              multicast;
	unsigned char          multicast_buf[1024];

	sizebuf_t              spectator;
	unsigned char          spectator_buf[1024];

	sizebuf_t              signon;
	unsigned char          signon_data[32768];

} server_t;

typedef struct client_s
{
	qboolean        active;
	qboolean        spawned;
	qboolean        fully_connected;
	qboolean        connected;
	qboolean        uploading;
	qboolean        hasusrmsgs;
	qboolean        has_force_unmodified;

	netchan_t       netchan;

	int             chokecount;
	int             delta_sequence;

	qboolean        fakeclient;
	qboolean        proxy;
	usercmd_t       lastcmd;

	double          connecttime;
	double          cmdtime;
	double          ignorecmdtime;

	float           latency;
	float           packet_loss;

	double          localtime;
	double          nextping;
	double          svtimebase;

	sizebuf_t       datagram;
	byte            datagram_buf[4000];

	double          connection_started;
	double          next_messagetime;
	double          next_messageinterval;
	qboolean        send_message;
	qboolean        skip_message;

	client_frame_t* frames;
	event_state_t   events;
	edict_t*        edict;
	const edict_t*  pViewEntity;
	int             userid;
	USERID_t        network_userid;

	char            userinfo[MAX_INFO_STRING];

	qboolean        sendinfo;
	float           sendinfo_time;

	char            hashedcdkey[64];
	char            name[32];
	int             topcolor;
	int             bottomcolor;
	int             entityId;

	resource_t      resourcesonhand;
	resource_t      resourcesneeded;

	FileHandle_t    upload;
	qboolean        uploaddoneregistering;
	customization_t customdata;

	int             crcValue;
	int             lw;
	int             lc;

	char            physinfo[MAX_INFO_STRING];

	qboolean        m_bLoopback;
	uint32          m_VoiceStreams[2];
	double          m_lastvoicetime;
	int             m_sendrescount;

} client_t;

#endif //_ENGINE_STRUCTS_H_
