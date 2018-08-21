#ifndef _INCLUDE_AMXMODX_CPUINFO_H
#define _INCLUDE_AMXMODX_CPUINFO_H

class CPUInfo
{
	private:
		bool m_has_cmov;
		bool m_has_mmx;

		bool m_has_sse;
		bool m_has_sse2;
		bool m_has_sse3;
		bool m_has_ssse3;
		bool m_has_sse4_1;
		bool m_has_sse4_2;

		bool m_has_clmul;
		bool m_has_fma3;
		bool m_has_aes;
		bool m_has_avx;
		bool m_has_avx2;
		bool m_has_f16c;
		bool m_has_rdrand;
		bool m_has_xop;

		bool m_has_avx512F;
		bool m_has_avx512CD;
		bool m_has_avx512PF;
		bool m_has_avx512ER;
		bool m_has_avx512VL;
		bool m_has_avx512BW;
		bool m_has_avx512DQ;
		bool m_has_avx512IFMA;
		bool m_has_avx512VBMI;
		
	public:
		CPUInfo();

		bool has_cmov() const;
		bool has_mmx() const;

		bool has_sse() const;
		bool has_sse2() const;
		bool has_sse3() const;
		bool has_ssse3() const;
		bool has_sse4_1() const;
		bool has_sse4_2() const;

		bool has_clmul() const;
		bool has_fma3() const;
		bool has_aes() const;
		bool has_avx() const;
		bool has_avx2() const;
		bool has_f16c() const;
		bool has_rdrand() const;
		bool has_xop() const;

		bool has_avx512F() const;
		bool has_avx512CD() const;
		bool has_avx512PF() const;
		bool has_avx512ER() const;
		bool has_avx512VL() const;
		bool has_avx512BW() const;
		bool has_avx512DQ() const;
		bool has_avx512IFMA() const;
		bool has_avx512VBMI() const;
};


#endif //_INCLUDE_AMXMODX_CPUINFO_H