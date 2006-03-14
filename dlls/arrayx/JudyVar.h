#ifndef _JUDYVAR_H
#define _JUDYVAR_H

#if defined HAVE_STDINT_H
	//#include <stdint.h>
#else
	#if defined __LCC__ || defined __DMC__ || defined LINUX
		#if defined HAVE_INTTYPES_H
			#include <inttypes.h>
		#else
	 		#include <stdint.h>
		#endif
	#elif !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L
		/* The ISO C99 defines the int16_t and int_32t types. If the compiler got
		 * here, these types are probably undefined.
		 */
		#if defined __MACH__
			#include <ppc/types.h>
			typedef unsigned short int	uint16_t;
			typedef unsigned long int	 uint32_t;
		#elif defined __FreeBSD__
			#include <inttypes.h>
		#else
			typedef short int					 int16_t;
			typedef unsigned short int	uint16_t;
			#if defined SN_TARGET_PS2
				typedef int							 int32_t;
				typedef unsigned int			uint32_t;
			#else
				typedef long int					int32_t;
				typedef unsigned long int uint32_t;
			#endif
			#if defined __WIN32__ || defined _WIN32 || defined WIN32
				typedef __int64						int64_t;
				typedef unsigned __int64	uint64_t;
				#define HAVE_I64
			#elif defined __GNUC__
				typedef long long				 int64_t;
				typedef unsigned long long uint64_t;
				#define HAVE_I64
			#endif
		#endif
	#endif
#endif

#if defined _LP64 || defined WIN64 || defined _WIN64
	#if !defined __64BIT__
		#define __64BIT__
	#endif
#endif

#if !defined PAWN_CELL_SIZE
	#define PAWN_CELL_SIZE 32		 /* by default, use 32-bit cells */
#endif
#if PAWN_CELL_SIZE==16
	typedef uint16_t	ucell;
	typedef int16_t	 cell;
#elif PAWN_CELL_SIZE==32
	typedef uint32_t	ucell;
	typedef int32_t	 cell;
#define REAL	float
#elif PAWN_CELL_SIZE==64
	typedef uint64_t	ucell;
	typedef int64_t	 cell;
#define REAL	double
#else
	#error Unsupported cell size (PAWN_CELL_SIZE)
#endif

#endif