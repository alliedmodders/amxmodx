#include "CPUInfo.h"


#ifdef _WIN32

//  Windows
#include <intrin.h>
#define cpuid(info, x)	__cpuidex(info, x, 0)

#else

//  GCC Intrinsics
#include <cpuid.h>
void cpuid(int info[4], int InfoType){
	__cpuid_count(InfoType, 0, info[0], info[1], info[2], info[3]);
}

#endif

// CPU Info
CPUInfo::CPUInfo()
{
    int info[4];
	cpuid(info, 0);
	int nIds = info[0];

	cpuid(info, 0x80000000);
	unsigned nExIds = info[0];

	//  Detect Features
	if (nIds >= 0x00000001)
	{
		cpuid(info,0x00000001);
		m_has_cmov		= (info[3] & ((int)1 << 15)) != 0;
		m_has_mmx		= (info[3] & ((int)1 << 23)) != 0;

		m_has_sse		= (info[3] & ((int)1 << 25)) != 0;
		m_has_sse2		= (info[3] & ((int)1 << 26)) != 0;
		m_has_sse3		= (info[2] & ((int)1 <<  0)) != 0;
		m_has_ssse3		= (info[2] & ((int)1 <<  9)) != 0;
		m_has_sse4_1	= (info[2] & ((int)1 << 19)) != 0;
		m_has_sse4_2	= (info[2] & ((int)1 << 20)) != 0;

		m_has_clmul		= (info[2] & ((int)1 <<  1)) != 0;
		m_has_fma3		= (info[2] & ((int)1 << 12)) != 0;
		m_has_aes		= (info[2] & ((int)1 << 25)) != 0;
		m_has_avx		= (info[2] & ((int)1 << 28)) != 0;
		m_has_f16c		= (info[2] & ((int)1 << 29)) != 0;
		m_has_rdrand	= (info[2] & ((int)1 << 30)) != 0;
	}
	if (nIds >= 0x00000007)
	{
		cpuid(info,0x00000007);

		m_has_avx2		= (info[1] & ((int)1 <<  5)) != 0;
		m_has_avx512F	= (info[1] & ((int)1 << 16)) != 0;
		m_has_avx512CD	= (info[1] & ((int)1 << 28)) != 0;
		m_has_avx512PF	= (info[1] & ((int)1 << 26)) != 0;
		m_has_avx512ER	= (info[1] & ((int)1 << 27)) != 0;
		m_has_avx512VL	= (info[1] & ((int)1 << 31)) != 0;
		m_has_avx512BW	= (info[1] & ((int)1 << 30)) != 0;
		m_has_avx512DQ	= (info[1] & ((int)1 << 17)) != 0;
		m_has_avx512IFMA  = (info[1] & ((int)1 << 21)) != 0;
		m_has_avx512VBMI  = (info[2] & ((int)1 <<  1)) != 0;
	}
	if (nExIds >= 0x80000001)
	{
		cpuid(info,0x80000001);
		 	
		m_has_xop = (info[2] & ((int)1 << 11)) != 0;
	}
}


bool CPUInfo::has_cmov()		const { return m_has_cmov; }
bool CPUInfo::has_mmx()			const { return m_has_mmx; }
bool CPUInfo::has_sse()			const { return m_has_sse; }
bool CPUInfo::has_sse2()		const { return m_has_sse2; }
bool CPUInfo::has_sse3()		const { return m_has_sse3; }
bool CPUInfo::has_ssse3()		const { return m_has_ssse3; }
bool CPUInfo::has_sse4_1()		const { return m_has_sse4_1; }
bool CPUInfo::has_sse4_2()		const { return m_has_sse4_2; }
bool CPUInfo::has_clmul()		const { return m_has_clmul; }
bool CPUInfo::has_fma3()		const { return m_has_fma3; }
bool CPUInfo::has_aes()			const { return m_has_aes; }
bool CPUInfo::has_avx()			const { return m_has_avx; }
bool CPUInfo::has_avx2()		const { return m_has_avx2; }
bool CPUInfo::has_f16c()		const { return m_has_f16c; }
bool CPUInfo::has_rdrand()		const { return m_has_rdrand; }
bool CPUInfo::has_xop()			const { return m_has_xop; }
bool CPUInfo::has_avx512F()		const { return m_has_avx512F; }
bool CPUInfo::has_avx512CD()	const { return m_has_avx512CD; }
bool CPUInfo::has_avx512PF()	const { return m_has_avx512PF; }
bool CPUInfo::has_avx512ER()	const { return m_has_avx512ER; }
bool CPUInfo::has_avx512VL()	const { return m_has_avx512VL; }
bool CPUInfo::has_avx512BW()	const { return m_has_avx512BW; }
bool CPUInfo::has_avx512DQ()	const { return m_has_avx512DQ; }
bool CPUInfo::has_avx512IFMA()	const { return m_has_avx512IFMA; }
bool CPUInfo::has_avx512VBMI()	const { return m_has_avx512VBMI; }

