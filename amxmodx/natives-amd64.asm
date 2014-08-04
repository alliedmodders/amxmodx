; vim: set ts=4 sw=4 tw=99 noet ft=nasm:
;
; AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
; Copyright (C) The AMX Mod X Development Team.
;
; This software is licensed under the GNU General Public License, version 3 or higher.
; Additional exceptions apply. For full license details, see LICENSE.txt or visit:
;     https://alliedmods.net/amxmodx-license

;
; register_native functions for amd64
;     Based on the concept by Julien "dJeyL" Laurent
;     Thanks to T(+)rget for pushing to implement this
;

;;Initializes the global variable

BITS 64

section .text

global amxx_DynaInit, _amxx_DynaInit
;void amxx_DynaInit(void *ptr);
amxx_DynaInit:
_amxx_DynaInit:
	mov	[GLOBAL_GATE wrt rip], rdi
	ret
	
;;Assembles the gateway function
global amxx_DynaMake, _amxx_DynaMake
;int amxx_DynaMake(char *buffer, int id);
amxx_DynaMake:
_amxx_DynaMake:
	;we're not damaging the stack I think so we should be safe with no prologue
	
	;save these two we're about to destroy them
	push		rsi		;push id
	push		rdi		;push buffer
	
	mov		rsi, _amxx_DynaFuncStart
	mov		rcx, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	cld		;clear direction flag (just in case)
	rep		movsb
	
	pop		rdi		;get buffer as destination
	pop		rax		;get id
	;align us to mov rsi, 1234... - on x86-64 this is 2 bytes after the differential
	add		rdi, (_amxx_DynaFuncStart.move-_amxx_DynaFuncStart) + 2
	mov		[rdi], qword rax
	;align rdi to the call
	add		rdi, (_amxx_DynaFuncStart.call-_amxx_DynaFuncStart.move) 
	mov		rax, qword [GLOBAL_GATE wrt rip]
	;copy the real address
	mov		[rdi], rax
	
	ret

;;The gateway function we will re-assemble
;; This is similar to dJeyL's but a tad more elegant, as it's written in pure assembly
;; and NASM > GAS :')
global amxx_DynaFunc, _amxx_DynaFunc
;int amxx_DynaFunc(AMX *amx, cell *params);
amxx_DynaFunc:
_amxx_DynaFunc:
_amxx_DynaFuncStart:
	push		rbp
	mov		rbp, rsp
	
	;we're given an amx and params... we're also hardcoded for this though:
	mov		rdx, rsi	;move 2nd param to 3rd 
	mov		rsi, rdi	;move 1st param to 2nd
	;this old trick, we'll move in the real pointer in a bit.
.move:
	mov		rdi, qword 1234567812345678h
.call:
	mov		rcx, qword 1234567812345678h
	call		rcx
	
	pop		rbp
	ret
_amxx_DynaFuncEnd:

;;Just returns the buffer size required
global _amxx_DynaCodesize, amxx_DynaCodesize
;int amxx_DynaCodesize()
amxx_DynaCodesize:
_amxx_DynaCodesize:
	; on x86-64 this is 34 bytes
	mov		rax, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	ret

section .data
	
GLOBAL_GATE		DQ		0

