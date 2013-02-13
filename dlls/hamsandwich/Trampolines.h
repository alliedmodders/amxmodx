/* Trampolines
 *
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */


#ifndef TRAMPOLINES_H
#define TRAMPOLINES_H

#ifndef NDEBUG
#define TPRINT(msg)  printf msg
#else
#define TPRINT(msg) /* nothing */
#endif

#if defined _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif // WIN32_LEAN_AND_MEAN
#if _MSC_VER >= 1400
#ifdef offsetof
#undef offsetof
#endif // offsetof
#endif // _MSC_VER >= 1400
#include <windows.h>
#elif defined(__linux__) || defined(__APPLE__)
#include <sys/mman.h>
#if defined (__linux__)
#include <malloc.h>
#endif
#endif
#include <stddef.h> // size_t
#include <string.h> // memcpy
#include <stdlib.h> // memalign
#include <stdio.h>


namespace Trampolines
{

	/**
	 * List of x86 bytecodes for creating
	 * basic trampolines at runtime.
	 * -
	 * These are defined here so that, should
	 * the need ever arise, this can be ported
	 * to other architectures fairly painlessly
	 */
	namespace Bytecode
	{
		/**
		 * Prologue for a void function
		 * Clobbers EBX and EAX
		 */
		const unsigned char codeVoidPrologue[] = { 
			0x55,						// push ebp
			0x89, 0xE5,					// mov ebp, esp
			0x50,						// push eax
		};

		/**
		 * Prologue for a function that returns
		 * Clobbers EBX, EAX too but not after call
		 */
		const unsigned char codeReturnPrologue[] = {
			0x55,						// push ebp
			0x89, 0xE5,					// mov ebp, esp
		};
		const unsigned char codeThisReturnPrologue[] = {
			0x55,						// push ebp
			0x89, 0xE5,					// mov ebp, esp
		};


		/**
		 * Takes a paramter from the trampoline's stack
		 * and pushes it onto the target's stack.
		 */
		const unsigned char codePushParam[] = {
			0xFF, 0x75, 0xFF			// pushl [ebp+0xFF]
		};

		/**
		 * Offset of codePushParam to modify at runtime
		 * that contains the stack offset
		 */
		const unsigned int codePushParamReplace = 2;


		/**
		 * Takes the "this" pointer from the trampoline and
		 * pushes it onto the target's stack.
		 */
		const unsigned char codePushThis[] = {
		#if defined(_WIN32)
			0x51						// push ecx
		#elif defined(__linux__) || defined(__APPLE__)
			0xFF, 0x75, 0x04			// pushl [ebp+0x08h]
		#endif
		};

#if defined(__linux__) || defined(__APPLE__)
		const int codePushThisReplace = 2;
#endif

		/**
		 * Pushes a raw number onto the target's stack
		 */
		const unsigned char codePushID[] = {
			0x68, 0xDE, 0xFA, 0xAD, 0xDE	// push	DEADFADEh
		};

		/**
		 * Offset of codePushID to modify at runtime
		 * to contain the number to push
		 */
		const unsigned int codePushIDReplace = 1;

		/**
		 * Call our procedure
		 */
		const unsigned char codeCall[] = {
			0xB8, 0xDE, 0xFA, 0xAD, 0xDE,// mov eax, DEADFADEh
			0xFF, 0xD0					// call eax
		};

		/**
		 * Offset of codeCall to modify at runtime
		 * to contain the pointer to the function
		 */
		const unsigned int codeCallReplace = 1;

		/**
		 * Adds to ESP, freeing up stack space
		 */
		const unsigned char codeFreeStack[] = {
			0x81, 0xC4, 0xFF, 0xFF, 0xFF, 0xFF// add esp REPLACEME
		};

		/**
		 * Offset of codeFreeStack to modify at runtime
		 * to contain how much data to free
		 */
		const unsigned int codeFreeStackReplace = 2;

		/**
		 * Epilogue of a simple return function
		 */
		const unsigned char codeReturnEpilogue[] = {
				0x5D,						// pop ebp
				0xC3						// ret
		};
		const unsigned char codeReturnEpilogueN[] = {
				0x5D,						// pop ebp
				0xC2, 0xCD, 0xAB			// retn 0xABCD
		};
		const int codeReturnEpilogueNReplace = 2;


		/**
		 * Epilogue of a void return function
		 */
		const unsigned char codeVoidEpilogue[] = {
				0x58,						// pop eax
				0x5D,						// pop ebp
				0xC3						// ret
		};

		const unsigned char codeVoidEpilogueN[] = {
				0x58,						// pop eax
				0x5D,						// pop ebp
				0xC2, 0xCD, 0xAB			// retn 0xABCD
		};
		const int codeVoidEpilogueNReplace = 3;



		const unsigned char codeBreakpoint[] = {
			0xCC							// int 3
		};

	}

	/**
	 * Our actual maker of the trampolines!!@$
	 * I've no idea why I made this a class and not a namespace
	 * Oh well!
	 */

	class TrampolineMaker
	{
	private:
		unsigned char		*m_buffer;			// the actual buffer containing the code
		int					 m_size;			// size of the buffer
		int					 m_mystack;			// stack for the trampoline itself
		int					 m_calledstack;		// stack for the target function
		int					 m_paramstart;
		int					 m_thiscall;
		int					 m_maxsize;

		/**
		 * Adds data to the buffer
		 * data must be pre-formatted before hand!
		 */
		void Append(const unsigned char *src, size_t size)
		{
			int orig=m_size;
			m_size+=size;

			if (m_buffer==NULL)
			{
				m_maxsize=512;
				m_buffer=(unsigned char *)malloc(m_maxsize);
			}
			else if (m_size > m_maxsize)
			{
				m_maxsize = m_size + 512;
				m_buffer=(unsigned char *)realloc(m_buffer,m_maxsize);
			}

			unsigned char *dat=m_buffer+orig; // point dat to the end of the prewritten 

			while (orig<m_size)
			{
				*dat++=*src++;

				orig++;
			};

		};
	public:
		TrampolineMaker()
		{
			m_buffer=NULL;
			m_size=0;
			m_mystack=0;
			m_calledstack=0;
			m_paramstart=0;
			m_thiscall=0;
			m_maxsize=0;
		};

		/**
		 * Inserts a breakpoint (int 3) into the trampoline.
		 */
		void Breakpoint()
		{
			Append(&::Trampolines::Bytecode::codeBreakpoint[0],sizeof(::Trampolines::Bytecode::codeBreakpoint));
		};

		/**
		 * Adds the "return prologue", pushes registers and prepares stack
		 */
		void ReturnPrologue()
		{
			Append(&::Trampolines::Bytecode::codeReturnPrologue[0],sizeof(::Trampolines::Bytecode::codeReturnPrologue));
			m_paramstart=0;
			m_thiscall=0;
		};
		void ThisReturnPrologue()
		{
			this->ReturnPrologue();
			m_thiscall=1;
		};

		/**
		 * Adds the void prologue pushes registers, prepares the stack
		 */
		void VoidPrologue()
		{
			Append(&::Trampolines::Bytecode::codeVoidPrologue[0],sizeof(::Trampolines::Bytecode::codeVoidPrologue));
			m_paramstart=0;
			m_thiscall=0;
		};

		/**
		 * Flags this trampoline as a thiscall trampoline, and prepares the void prologue.
		 */
		void ThisVoidPrologue()
		{
			this->VoidPrologue();
			m_thiscall=1;
		};
		/**
		 * Epilogue for a returning function pops registers but does not free any more of the stack!
		 */
		void ReturnEpilogue()
		{
			Append(&::Trampolines::Bytecode::codeReturnEpilogue[0],sizeof(::Trampolines::Bytecode::codeReturnEpilogue));
		};

		/**
		 * Epilogue that also frees it's estimated stack usage.  Useful for stdcall/thiscall/fastcall.
		 */
		void ReturnEpilogueAndFree()
		{
			this->ReturnEpilogue(m_mystack);
		};

		/**
		 * Return epilogue.  Pops registers, and frees given amount of data from the stack.
		 *
		 * @param howmuch			How many bytes to free from the stack.
		 */
		void ReturnEpilogue(int howmuch)
		{

			unsigned char code[sizeof(::Trampolines::Bytecode::codeReturnEpilogueN)];

			memcpy(&code[0],&::Trampolines::Bytecode::codeReturnEpilogueN[0],sizeof(::Trampolines::Bytecode::codeReturnEpilogueN));


			unsigned char *c=&code[0];

			union 
			{
				int		i;
				unsigned char	b[4];
			} bi;

			bi.i=howmuch;

			c+=::Trampolines::Bytecode::codeReturnEpilogueNReplace;
			*c++=bi.b[0];
			*c++=bi.b[1];

			Append(&code[0],sizeof(::Trampolines::Bytecode::codeReturnEpilogueN));
			//Append(&::Trampolines::Bytecode::codeReturnEpilogueN[0],sizeof(::Trampolines::Bytecode::codeReturnEpilogueN));
		};

		/**
		 * Void epilogue, pops registers and frees the estimated stack usage of the trampoline.
		 */
		void VoidEpilogueAndFree()
		{
			this->VoidEpilogue(m_mystack);
		};
		/**
		 * Void epilogue, pops registers, nothing else done with stack.
		 */
		void VoidEpilogue()
		{
			Append(&::Trampolines::Bytecode::codeVoidEpilogue[0],sizeof(::Trampolines::Bytecode::codeVoidEpilogue));
		};
		/**
		 * Void epilogue, pops registers, frees given amount of data off of the stack.
		 *
		 * @param howmuch			How many bytes to free from the stack.
		 */
		void VoidEpilogue(int howmuch)
		{

			unsigned char code[sizeof(::Trampolines::Bytecode::codeVoidEpilogueN)];

			memcpy(&code[0],&::Trampolines::Bytecode::codeVoidEpilogueN[0],sizeof(::Trampolines::Bytecode::codeVoidEpilogueN));


			unsigned char *c=&code[0];

			union 
			{
				int		i;
				unsigned char	b[4];
			} bi;

			bi.i=howmuch;

			c+=::Trampolines::Bytecode::codeVoidEpilogueNReplace;
			*c++=bi.b[0];
			*c++=bi.b[1];

			Append(&code[0],sizeof(::Trampolines::Bytecode::codeVoidEpilogueN));
			Append(&::Trampolines::Bytecode::codeVoidEpilogueN[0],sizeof(::Trampolines::Bytecode::codeVoidEpilogueN));
		};

		/**
		 * Pushes the "this" pointer onto the callee stack.  Pushes ECX for MSVC, and param0 on GCC.
		 */
		void PushThis()
		{

			if (!m_thiscall)
			{
				return;
			}

			unsigned char code[sizeof(::Trampolines::Bytecode::codePushThis)];

			memcpy(&code[0],&::Trampolines::Bytecode::codePushThis[0],sizeof(::Trampolines::Bytecode::codePushThis));


#if defined(__linux__) || defined(__APPLE__)
			unsigned char *c=&code[0];

			union 
			{
				int		i;
				unsigned char	b[4];
			} bi;

			bi.i=m_paramstart+8;

			c+=::Trampolines::Bytecode::codePushThisReplace;
			*c++=bi.b[0];
#endif

			Append(&code[0],sizeof(::Trampolines::Bytecode::codePushThis));

#if defined(__linux__) || defined(__APPLE__)
			m_mystack+=4;
#endif
			m_calledstack+=4;
		};

		/**
		 * Frees what is estimated as the stack usage of the trampoline.
		 */
		void FreeMyStack(void)
		{

			this->FreeStack(m_mystack);
		};

		/**
		 * Frees the estimated stack usage of the callee.
		 */
		void FreeTargetStack(void)
		{
			this->FreeStack(m_calledstack);
		};


		/**
		 * Frees the estimated stack usage of the callee and the trampoline.
		 */
		void FreeBothStacks(void)
		{
			this->FreeStack(m_calledstack + m_mystack);
		};

		/**
		 * Frees a given amount of bytes from the stack.
		 *
		 * @param howmuch			How many bytes to free.
		 */
		void FreeStack(int howmuch)
		{
			unsigned char code[sizeof(::Trampolines::Bytecode::codeFreeStack)];

			memcpy(&code[0],&::Trampolines::Bytecode::codeFreeStack[0],sizeof(::Trampolines::Bytecode::codeFreeStack));

			unsigned char *c=&code[0];

			union 
			{
				int		i;
				unsigned char	b[4];
			} bi;

			bi.i=howmuch;

			c+=::Trampolines::Bytecode::codeFreeStackReplace;
			*c++=bi.b[0];
			*c++=bi.b[1];
			*c++=bi.b[2];
			*c++=bi.b[3];

			Append(&code[0],sizeof(::Trampolines::Bytecode::codeFreeStack));

		};

		/**
		 * Pushes a raw number onto the callee stack.
		 *
		 * @param Number			The number to push onto the callee stack.
		 */
		void PushNum(int Number)
		{
			unsigned char code[sizeof(::Trampolines::Bytecode::codePushID)];

			memcpy(&code[0],&::Trampolines::Bytecode::codePushID[0],sizeof(::Trampolines::Bytecode::codePushID));

			unsigned char *c=&code[0];

			union 
			{
				int		i;
				unsigned char	b[4];
			} bi;

			bi.i=Number;

			c+=::Trampolines::Bytecode::codePushIDReplace;
			*c++=bi.b[0];
			*c++=bi.b[1];
			*c++=bi.b[2];
			*c++=bi.b[3];

			Append(&code[0],sizeof(::Trampolines::Bytecode::codePushID));

			m_calledstack+=4; // increase auto detected stack size

		};


		/**
		 * Takes a parameter passed on the trampoline's stack and inserts it into the callee's stack.
		 *
		 * @param which			The parameter number to push. 1-based.  "thiscall" trampolines automatically compensate for the off-number on GCC.
		 */
		void PushParam(int which)
		{
#if defined(__linux__) || defined(__APPLE__)
			if (m_thiscall)
			{
				which++;
			}
#endif
			which=which*4;
			which+=m_paramstart+4;

			unsigned char value=which;

			unsigned char code[sizeof(::Trampolines::Bytecode::codePushParam)];

			memcpy(&code[0],&::Trampolines::Bytecode::codePushParam[0],sizeof(::Trampolines::Bytecode::codePushParam));

			unsigned char *c=&code[0];


			c+=::Trampolines::Bytecode::codePushParamReplace;

			*c=value;

			Append(&code[0],sizeof(::Trampolines::Bytecode::codePushParam));

			m_calledstack+=4; // increase auto detected stack size
			m_mystack+=4;

		};

		/**
		 * Insert a function to call into the trampoline.
		 * 
		 * @param ptr			The function to call, cast to void*.
		 */
		void Call(void *ptr)
		{
			unsigned char code[sizeof(::Trampolines::Bytecode::codeCall)];

			memcpy(&code[0],&::Trampolines::Bytecode::codeCall[0],sizeof(::Trampolines::Bytecode::codeCall));

			unsigned char *c=&code[0];

			union 
			{
				void	*p;
				unsigned char	 b[4];
			} bp;

			bp.p=ptr;

			c+=::Trampolines::Bytecode::codeCallReplace;

			*c++=bp.b[0];
			*c++=bp.b[1];
			*c++=bp.b[2];
			*c++=bp.b[3];
			Append(&code[0],sizeof(::Trampolines::Bytecode::codeCall));


		};

		/**
		 * Finalizes the trampoline.  Do not try to modify it after this.
		 *
		 * @param size			A pointer to retrieve the size of the trampoline. Ignored if set to NULL.
		 * @return				The trampoline pointer, cast to void*.
		 */
		void *Finish(int *size)
		{
			//void *ret=(void *)m_buffer;

			if (size)
			{
				*size=m_size;
			}

			// Reallocate with proper flags
#if defined(_WIN32)
			void *ret=VirtualAlloc(NULL, m_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#elif defined(__GNUC__)
# if defined(__APPLE__)
			void *ret = valloc(m_size);
# else
			void *ret=memalign(sysconf(_SC_PAGESIZE), m_size);
# endif
			mprotect(ret,m_size,PROT_READ|PROT_WRITE|PROT_EXEC);
#endif
			memcpy(ret, m_buffer, m_size);


			m_size=0;

			free(m_buffer);

			m_buffer=NULL; // so we don't accidentally rewrite!
			m_mystack=0;
			m_calledstack=0;
			m_maxsize=512;

			return ret;
		};
	};
};


/**
 * Utility to make a generic trampoline.
 */
inline void *CreateGenericTrampoline(bool thiscall, bool voidcall, int paramcount, void *extraptr, void *callee)
{
		Trampolines::TrampolineMaker tramp;

	if (voidcall)
	{
		if (thiscall)
		{
			tramp.ThisVoidPrologue();
		}
		else
		{
			tramp.VoidPrologue();
		}
	}
	else
	{
		if (thiscall)
		{
			tramp.ThisReturnPrologue();
		}
		else
		{
			tramp.ReturnPrologue();
		}
	}

	while (paramcount)
	{
		tramp.PushParam(paramcount--);
	}
	if (thiscall)
	{
		tramp.PushThis();
	}
	tramp.PushNum(reinterpret_cast<int>(extraptr));
	tramp.Call(callee);
	tramp.FreeTargetStack();
	if (voidcall)
	{
#if defined(_WIN32)
		tramp.VoidEpilogueAndFree();
#elif defined(__linux__) || defined(__APPLE__)
		tramp.VoidEpilogue();
#endif
	}
	else
	{
#if defined(_WIN32)
		tramp.ReturnEpilogueAndFree();
#elif defined(__linux__) || defined(__APPLE__)
		tramp.ReturnEpilogue();
#endif
	}
	return tramp.Finish(NULL);

};


#endif // TRAMPOLINEMANAGER_H
