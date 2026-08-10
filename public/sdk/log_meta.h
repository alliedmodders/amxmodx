#pragma once

enum
{
	MAX_LOGMSG_LEN = 1024,	// max buffer size for printed messages
	MAX_CLIENTMSG_LEN = 128	// max buffer size for client messages
};

extern cvar_t g_meta_debug;

void META_CONS(const char* fmt, ...);
void META_DEV(const char* fmt, ...);
void META_INFO(const char* fmt, ...);
void META_WARNING(const char* fmt, ...);
void META_ERROR(const char* fmt, ...);
void META_LOG(const char* fmt, ...);
void META_CLIENT(edict_t* pEntity, const char* fmt, ...);
void META_DEBUG_(int level, const char* fmt, ...);

template <typename ...t_args>
void META_DEBUG(int level, const char* fmt, t_args ... args)
{
	if (unlikely(g_meta_debug.value >= level))
		META_DEBUG_(level, fmt, args...);
}

void flush_ALERT_buffer();
