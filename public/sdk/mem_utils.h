#pragma once

class static_allocator
{
public:
	enum memory_protection : uint8
	{
#ifdef _WIN32
		mp_readwrite = PAGE_READWRITE,
		mp_rwx = PAGE_EXECUTE_READWRITE
#else
		mp_readwrite = PROT_READ | PROT_WRITE,
		mp_rwx = PROT_READ | PROT_WRITE | PROT_EXEC
#endif
	};

	static_allocator(memory_protection protection);
	char *allocate(const size_t n);
	char *strdup(const char *string);
	void deallocate_all();
	size_t memory_used() const;
	bool contain(uint32 addr);
	char *find_pattern(char *pattern, size_t len);

	template<typename T>
	T *allocate()
	{
		return (T *)allocate(sizeof(T));
	}

private:
	void allocate_page();

	enum
	{
		Pagesize = 4096
	};

	size_t m_used = 0;
	std::vector<void *> m_pages;
	memory_protection m_protection;

	friend class CJit;
};

bool  mem_compare(const char *addr, const char *pattern, size_t len);
char *mem_find_pattern(char *pos, int range, const char *pattern, size_t len);
char *mem_find_ref(char *pos, char *end, char opcode, uint32 ref, bool relative);
char *mem_find_string_push(char *addr, const char *string, size_t len);
