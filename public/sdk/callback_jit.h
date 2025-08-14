#pragma once

#define CDATA_ENTRY(s, x, p, h)	{#x, offsetof(s, x), (uint8)getArgsCount(decltype(s::x)()), getRetType(decltype(s::x)()), is_varargs(decltype(s::x)()), p, h}

enum rettype_t : uint8_t
{
	rt_void,
	rt_integer,
	rt_float
};

struct jitdata_t
{
	size_t		pfn_original;
	size_t		pfn_offset; // from fn table
	uint8		args_count;
	rettype_t	rettype;
	bool		has_varargs;
	uint8		mm_hook_time;
	size_t		mm_hook;

	plugins_t*	plugins;
	size_t		table_offset; // from MPlugin
	size_t		post_table_offset; // from MPlugin

#ifdef JIT_DEBUG
	const char*	name;
#endif
};

struct compile_data_t
{
	const char*	name;
	size_t		offset;
	uint8		args_count;
	rettype_t	rettype;
	bool		has_varargs;
	uint8		mm_hook_time;
	size_t		mm_hook;
};

template<typename ret_t, typename ...t_args>
size_t getArgsCount(ret_t (*)(t_args...))
{
	return sizeof...(t_args);
}

template<typename ret_t, typename ...t_args>
size_t getArgsCount(ret_t (*)(t_args..., ...))
{
	return sizeof...(t_args);
}

template<typename ...t_args>
rettype_t getRetType(void (*)(t_args..., ...))
{
	return rt_void;
}

template<typename ...t_args>
rettype_t getRetType(void(*)(t_args...))
{
	return rt_void;
}

template<typename ...t_args>
rettype_t getRetType(float (*)(t_args...))
{
	return rt_float;
}

template<typename ...t_args>
rettype_t getRetType(double (*)(t_args...))
{
	return rt_float;
}

template<typename ...t_args>
rettype_t getRetType(long double (*)(t_args...))
{
	return rt_float;
}

template<typename ret_t, typename ...t_args>
rettype_t getRetType(ret_t (*)(t_args...))
{
	return rt_integer;
}

template<typename ret_t, typename ...t_args>
bool is_varargs(ret_t (*)(t_args...))
{
	return false;
}

template<typename ret_t, typename ...t_args>
bool is_varargs(ret_t (*)(t_args..., ...))
{
	return true;
}

class CJit
{
public:
	CJit();
	size_t compile_callback(jitdata_t* jitdata);
	size_t compile_tramp(size_t ptr_to_func/*, size_t hook, size_t hook_time*/);
	void clear_callbacks();
	void clear_tramps();
	size_t is_callback_retaddr(uint32 addr);
	char* find_callback_pattern(char* pattern, size_t len);

private:
	static bool is_hook_needed(jitdata_t* jitdata);

	static_allocator m_callback_allocator;
	static_allocator m_tramp_allocator;
};

extern CJit g_jit;
