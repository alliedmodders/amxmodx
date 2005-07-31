;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (C)2005 by David "BAILOPAN" Anderson			 ;
; register_native functions for amd64			 ;;;;;;
; Based on the concept by Julien "dJeyL" Laurent	  ;
; Thanks to T(+)rget for pushing me to implement this ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Licensed under the GNU General Public License, version 2
;;This is a portion of AMX Mod X 
;; and is maintained by the AMX Mod X development team.

;;Initializes the global variable

BITS 64

section .text

global amxx_DynaInit, _amxx_DynaInit
;void amxx_DynaInit(void *ptr);
amxx_DynaInit:
_amxx_DynaInit:
	mov		rax, rdi			;get pointer, first param is in rdi
	mov		[GLOBAL_GATE], rax	;store
	
	mov		rax, 1
	ret
	
;;Assembles the gateway function
global amxx_DynaMake, _amxx_DynaMake
;int amxx_DynaMake(char *buffer, int id);
amxx_DynaMake:
_amxx_DynaMake:
	;we're not damaging the stack I think so we should be safe with no prologue
	
	;save these two we're about to destroy them
	push	rsi		;push id
	push	rdi		;push buffer
	
	mov		rsi, _amxx_DynaFuncStart
	mov		rcx, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	cld		;clear direction flag (just in case)
	rep		movsb
	
	pop		rdi		;get buffer as destination
	pop		rax		;get id
	;align us to mov rsi, 1234... - on x86-64 this is 2 bytes after the differential
	add		rdi, (_amxx_DynaMoveOffset-_amxx_DynaFuncStart) + 2
	mov		[rdi], qword rax
	
	mov		rax, 1
	ret

;;The gateway function we will re-assemble
;; This is similar to dJeyL's but a tad more elegant, as it's written in pure assembly
;; and NASM > GAS :')
global amxx_DynaFunc, _amxx_DynaFunc
;int amxx_DynaFunc(AMX *amx, cell *params);
amxx_DynaFunc:
_amxx_DynaFunc:
_amxx_DynaFuncStart:
	push	rbp
	mov		rbp, rsp
	
	;we're given an amx and params... we're also hardcoded for this though:
	mov		rdx, rsi	;move 2nd param to 3rd 
	mov		rsi, rdi	;move 1st param to 2nd
	;this old trick, we'll move in the real pointer in a bit.
_amxx_DynaMoveOffset:
	mov		rsi, qword 1234567812345678h
	call	[GLOBAL_GATE]		;pass through teh global gateway.
	
	pop		rbp
	ret
_amxx_DynaFuncEnd:

;;Just returns the buffer size required
global _amxx_DynaCodesize, amxx_DynaCodesize
;int amxx_DynaCodesize()
amxx_DynaCodesize:
_amxx_DynaCodesize:
	; on x86 is this 17 bytes
	mov		rax, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	ret
	
GLOBAL_GATE		DQ		0
